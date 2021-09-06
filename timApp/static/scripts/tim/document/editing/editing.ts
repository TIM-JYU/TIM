/* eslint-disable no-bitwise */
import {IScope} from "angular";
import $ from "jquery";
import {CURSOR} from "tim/editor/BaseParEditor";
import {
    afterAction,
    beforeAction,
    compileWithViewctrl,
    IPluginInfoResponse,
    ParCompiler,
    replaceAction,
} from "tim/editor/parCompiler";
import {PareditorController} from "tim/editor/pareditor";
import {IModalInstance} from "tim/ui/dialog";
import {documentglobals} from "tim/util/globals";
import {$http, $timeout} from "tim/util/ngimport";
import {
    empty,
    getViewName,
    isMobileDevice,
    markPageDirty,
    TimStorage,
    to,
} from "tim/util/utils";
import {openEditor} from "tim/editor/pareditorOpen";
import {getCurrentEditor} from "tim/editor/editorScope";
import {showDiffDialog} from "tim/document/showDiffDialog";
import {
    isManageResponse,
    showRenameDialog,
} from "tim/document/editing/showRenameDialog";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import * as t from "io-ts";
import {UserSelection} from "tim/document/editing/userSelection";
import {Paragraph} from "tim/document/structure/paragraph";
import {Area} from "tim/document/structure/area";
import {HelpPar} from "tim/document/structure/helpPar";
import {ParSelection} from "tim/document/editing/parSelection";
import {
    getMinimalUnbrokenSelection,
    UnbrokenSelection,
} from "tim/document/editing/unbrokenSelection";
import {ParContext} from "tim/document/structure/parContext";
import {DerefOption} from "tim/document/structure/derefOption";
import {enumPars, nextParContext} from "tim/document/structure/iteration";
import {enumDocParts, PreambleIteration} from "tim/document/structure/parsing";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import {
    createParContext,
    getParContainerElem,
} from "tim/document/structure/create";
import {
    getContextualAreaInfo,
    ParAreaInclusionKind,
} from "tim/document/structure/areaContext";
import {IMenuFunctionEntry, MenuFunctionList} from "../viewutils";
import {ViewCtrl} from "../viewctrl";
import {handleUnread} from "../readings";
import {canEditPar, canSeeSource, getElementByParId} from "../parhelpers";
import {onClick} from "../eventhandlers";
import {
    EditPosition,
    EditType,
    extraDataForServer,
    IExtraData,
    IParResponse,
    ITags,
} from "./edittypes";

export interface IParEditorOptions {
    forcedClasses?: string[];
    showDelete?: boolean;
    localSaveTag?: string;
    initialText?: string;
}

export enum SelectionUpdateType {
    AllowShrink,
    DontAllowShrink,
}

function prepareOptions(
    $this: HTMLElement,
    saveTag: string
): [ParContext, IParEditorOptions] {
    const par = createParContext($($this).closest(".par")[0]);
    const text = par.par.htmlElement.querySelector("pre")?.textContent;
    let forcedClasses: string[] = [];
    const forceAttr = par.par.attrs.forceclass;
    if (forceAttr) {
        forcedClasses = forceAttr.split(" ");
    }
    return [
        par,
        {
            localSaveTag: saveTag,
            initialText: text ?? undefined,
            forcedClasses: forcedClasses,
        },
    ];
}

export function getNextId(params: EditPosition) {
    return params.type === EditType.Edit
        ? params.pars.next()?.originalPar.id
        : params.type === EditType.AddAbove
        ? params.par.originalPar.id
        : params.type === EditType.AddBelow
        ? nextParContext(params.par)?.originalPar.id
        : undefined;
}

export class EditingHandler {
    public viewctrl: ViewCtrl;
    public sc: IScope;
    private currentEditor?: PareditorController;
    private editorLoad?: Promise<IModalInstance<PareditorController>>;
    selection?: UserSelection;

    constructor(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        this.viewctrl.editing = false;

        if (this.viewctrl.item.rights.editable) {
            onClick(".addBottom", ($this, e) => {
                this.viewctrl.closePopupIfOpen();
                this.toggleParEditor({type: EditType.AddBottom}, {});
            });

            onClick(".addAbove", ($this, e) => {
                this.viewctrl.closePopupIfOpen();
                const [par, options] = prepareOptions($this[0], "addAbove");
                return this.showAddParagraphAbove(
                    e.originalEvent,
                    par,
                    options
                );
            });

            onClick(".addBelow", ($this, e) => {
                this.viewctrl.closePopupIfOpen();
                const [par, options] = prepareOptions($this[0], "addBelow");
                return this.showAddParagraphBelow(
                    e.originalEvent,
                    par,
                    options
                );
            });

            onClick(".pasteBottom", ($this, e) => {
                this.viewctrl.closePopupIfOpen();
                this.viewctrl.clipboardHandler.pasteAbove(
                    e.originalEvent,
                    undefined,
                    false
                );
            });

            onClick(".pasteRefBottom", ($this, e) => {
                this.viewctrl.closePopupIfOpen();
                this.viewctrl.clipboardHandler.pasteAbove(
                    e.originalEvent,
                    undefined,
                    true
                );
            });
        }
    }

    setSelection(s: UserSelection | undefined) {
        this.selection = s;
        $(".par.lightselect").removeClass("lightselect");
        $(".par.selected").removeClass("selected");
        $(".par.marked").removeClass("marked");
        if (s) {
            getMinimalUnbrokenSelection(s.sel.start, s.sel.end).addClass(
                "marked"
            );
        }
    }

    async toggleParEditor(params: EditPosition, options: IParEditorOptions) {
        if (getCurrentEditor() || this.viewctrl.editing || this.editorLoad) {
            await showMessageDialog("Some editor is already open.");
            return;
        }
        let areaStart;
        let areaEnd;
        const caption =
            params.type === EditType.Edit ? "Edit paragraph" : "Add paragraph";
        let url: string;
        const ctx = params.type === EditType.Edit ? params.pars : undefined;
        const parNextId = getNextId(params);

        if (params.type === EditType.Edit && params.pars.hasMultiple()) {
            areaStart = params.pars.start.originalPar.id;
            areaEnd = params.pars.end.originalPar.id;
            url = "/postParagraph/";
            options.showDelete = true;
        } else {
            // TODO: Use same route (postParagraph) for both cases, determine logic based on given parameters
            if (params.type !== EditType.Edit) {
                url = "/newParagraph/";
                options.showDelete = false;
            } else {
                url = "/postParagraph/";
                options.showDelete = true;
            }
        }

        const tagKeys: Array<keyof ITags> = ["markread"];
        const tagsDescs: Array<{name: keyof ITags; desc: string}> = [
            {name: "markread", desc: "Mark as read"},
        ];
        const tags: ITags = {markread: false};
        if (this.viewctrl.isTranslation()) {
            tagKeys.push("marktranslated");
            tagsDescs.push({
                name: "marktranslated",
                desc: "Mark as translated",
            });
        }
        for (const k of tagKeys) {
            tags[k] = new TimStorage(k, t.boolean).get() ?? false;
        }

        const extraData: IExtraData = {
            area_end: areaEnd,
            area_start: areaStart,
            docId: this.viewctrl.docId, // current document id
            forced_classes: options.forcedClasses,
            par: ctx?.start, // the id of paragraph on which the editor was opened
            par_next: parNextId, // the id of the paragraph that follows par or null if par is the last one
            tags,
        };
        let initialText = "";
        let cursorPos;
        if (options.initialText) {
            initialText = options.initialText;
            cursorPos = initialText.indexOf(CURSOR);
            initialText = initialText.replace(CURSOR, "");
        } else if (options.showDelete && ctx) {
            const r = await this.getBlock(ctx);
            if (r.ok) {
                initialText = r.result.data.text;
            } else {
                await showMessageDialog(
                    `Failed to open editor: ${r.result.data.error}`
                );
                return;
            }
        }
        this.viewctrl.editing = true;
        this.editorLoad = openEditor({
            viewCtrl: this.viewctrl,
            extraData,
            initialText,
            defaultSize: "lg",
            options: {
                caption,
                deleteMsg: `
This will delete the whole ${
                    ctx?.hasMultiple() ? "area" : "paragraph"
                } from the document. Are you sure?

(If you only want to remove selected text, use backspace.)`,
                localSaveTag: options.localSaveTag ?? "par",
                showDelete: options.showDelete,
                showImageUpload: true,
                showPlugins: true,
                cursorPosition: cursorPos,
                showSettings:
                    params.type === EditType.Edit
                        ? params.pars.start.isSetting()
                        : false,
                tags: tagsDescs,
                touchDevice: isMobileDevice(),
            },
            deleteCb: async () => {
                const r = await to(
                    $http.post<IParResponse>(
                        `/deleteParagraph/${this.viewctrl.docId}`,
                        extraDataForServer(extraData)
                    )
                );
                if (!r.ok) {
                    return {error: r.result.data.error};
                } else {
                    this.handleDelete(params);
                }
                return {};
            },
            previewCb: async (text, proofread) => {
                const r = await to(
                    $http.post<IPluginInfoResponse>(
                        `/preview/${this.viewctrl.docId}`,
                        {
                            text,
                            proofread,
                            ...extraDataForServer(extraData),
                        }
                    )
                );
                if (!r.ok) {
                    throw Error("preview failed");
                }
                return r.result.data;
            },
            saveCb: async (text, data) => {
                const r = await to(
                    $http.post<IParResponse>(url, {
                        text,
                        ...extraDataForServer(data),
                        view: getViewName(),
                    })
                );
                if (!r.ok) {
                    return {error: r.result.data.error};
                } else {
                    const saveData = r.result.data;
                    if (
                        saveData.duplicates &&
                        saveData.duplicates.length > 0 &&
                        saveData.new_par_ids != null
                    ) {
                        const res = await to(
                            showRenameDialog({
                                duplicates: saveData.duplicates,
                                extraData,
                                new_par_ids: saveData.new_par_ids,
                                original_par: saveData.original_par,
                            })
                        );
                        if (res.ok && !isManageResponse(res.result)) {
                            await this.addSavedParToDom(res.result, params);
                        } else {
                            await this.addSavedParToDom(saveData, params);
                        }
                    } else {
                        await this.addSavedParToDom(saveData, params);
                    }
                }
                return {};
            },
            unreadCb: async () => {
                await handleUnread(params);
            },
        });
        const awaited = await this.editorLoad;
        this.currentEditor = await awaited.dialogInstance.promise;
        await to(awaited.result);
        this.currentEditor = undefined;
        this.editorLoad = undefined;
        this.viewctrl.editing = false;
    }

    private async getBlock(sel: ParSelection) {
        return await to(
            $http.get<{text: string}>(
                `/getBlock/${this.viewctrl.docId}/${sel.start.originalPar.id}`,
                {
                    params: sel.hasMultiple()
                        ? {
                              area_start: sel.start.originalPar.id,
                              area_end: sel.end.originalPar.id,
                          }
                        : undefined,
                }
            )
        );
    }

    findSettingsPars() {
        const pars = enumDocParts(
            PreambleIteration.Exclude,
            getParContainerElem()
        );
        const found = [];
        for (const p of pars) {
            if (p instanceof Paragraph && p.isSetting()) {
                found.push(p);
            } else if (
                p instanceof ReferenceParagraph &&
                p.original.isTranslation() &&
                p.original.isSetting()
            ) {
                found.push(p.original);
            } else {
                break;
            }
        }
        if (found.length > 0) {
            const first = found[0];
            const last = found[found.length - 1];
            return getMinimalUnbrokenSelection(
                new ParContext(first),
                new ParContext(last)
            );
        }
    }

    hasNonSettingsPars() {
        let has = false;
        for (const p of enumPars(DerefOption.Deref)) {
            if (!p.isSetting()) {
                has = true;
                break;
            }
        }
        return has;
    }

    async insertHelpPar() {
        await $timeout();
        const helpPar = $(`
<div class="par"
     id="HELP_PAR"
     t=""
     attrs="{}">
    <div class="parContent">
        <tim-help-par-content></tim-help-par-content>
    </div>
    <div class="editline" title="Click to open edit menu"></div>
    <div class="readline read"></div>
</div>
`);
        getParContainerElem()!.prepend(helpPar[0]);
        await compileWithViewctrl(helpPar, this.viewctrl.scope, this.viewctrl);
    }

    async editSettingsPars() {
        const pars = this.findSettingsPars();
        if (!pars) {
            const iter = enumDocParts(
                PreambleIteration.Exclude,
                getParContainerElem()
            ).next();
            let ctx;
            const docPart = (!iter.done && iter.value) || undefined;
            if (docPart) {
                ctx = new ParContext(docPart.getFirstOrigPar());
            }

            await this.toggleParEditor(
                ctx
                    ? {type: EditType.AddAbove, par: ctx}
                    : {type: EditType.AddBottom},
                {
                    initialText: `\`\`\` {settings=""}
auto_number_headings: 0${CURSOR}
\`\`\``,
                }
            );
        } else {
            await this.toggleParEditor({type: EditType.Edit, pars: pars}, {});
        }
    }

    showEditWindow(e: MouseEvent, sel: UnbrokenSelection) {
        this.toggleParEditor({type: EditType.Edit, pars: sel}, {});
    }

    editSelection(e: MouseEvent, sel: UnbrokenSelection) {
        this.toggleParEditor({type: EditType.Edit, pars: sel}, {});
    }

    /**
     * Toggles the edit mode of a table in a given table paragraph.
     */
    toggleTableEditor(e: MouseEvent, par: ParContext) {
        const tableCtrl = this.viewctrl.getTableControllerFromParId(par);

        if (tableCtrl == null) {
            void showMessageDialog("Could not find table controller");
            return;
        }

        tableCtrl.toggleEditMode();
    }

    handleDelete(position: EditPosition) {
        if (position.type === EditType.Edit) {
            position.pars.remove();
        }
        this.viewctrl.areaHandler.cancelSelection();
        this.viewctrl.beginUpdate();
    }

    showAddParagraphAbove(
        e: MouseEvent,
        par: ParContext,
        options: IParEditorOptions = {}
    ) {
        this.toggleParEditor({type: EditType.AddAbove, par: par}, options);
    }

    showAddParagraphBelow(
        e: MouseEvent,
        par: ParContext,
        options: IParEditorOptions = {}
    ) {
        this.toggleParEditor({type: EditType.AddBelow, par: par}, options);
    }

    async addSavedParToDom(data: IParResponse, position: EditPosition) {
        let par: JQuery;
        let action: (e: JQuery, c: JQuery) => void;
        switch (position.type) {
            case EditType.Edit:
                action = replaceAction;
                par = $(position.pars.removeAllButFirst());
                break;
            case EditType.CommentAction:
                action = replaceAction;
                par = $(position.par.par.htmlElement);
                break;
            case EditType.AddBottom:
                action = afterAction;
                par = $(".addBottomContainer").prev();
                break;
            case EditType.AddAbove:
                par = $(position.par.getElementForInsert(EditType.AddAbove));
                action = beforeAction;
                break;
            default:
                par = $(position.par.getElementForInsert(EditType.AddBelow));
                action = afterAction;
                break;
        }

        await ParCompiler.compileAndDOMAction(
            action,
            par,
            data,
            this.viewctrl.scope,
            this.viewctrl
        );

        this.viewctrl.docVersion = data.version;
        this.viewctrl.areaHandler.cancelSelection();
        this.removeDefaultPars();
        markPageDirty();

        // No await here; we want to call getUpdatedPars in the background.
        // After that, it will also call getActiveDocument().rebuildSections(),
        // so the parsed document structure is out of sync with the DOM for a short period.
        this.viewctrl.beginUpdate();
    }

    getParEditor(): PareditorController | undefined {
        return this.currentEditor;
    }

    goToEditor() {
        const editor = this.getParEditor();
        if (!editor) {
            void showMessageDialog("Editor is no longer open.");
            return;
        }
        const dg = editor.getDraggable();
        if (dg.isMinimized()) {
            dg.toggleMinimize();
        }
        editor.scrollIntoView();
    }

    closeAndSave(e: MouseEvent, par: ParContext) {
        const editor = this.getParEditor();
        if (!editor) {
            void showMessageDialog("Editor is no longer open.");
            return;
        }
        editor.saveClicked();
        this.viewctrl.parmenuHandler.showOptionsWindow(e, par);
    }

    closeWithoutSaving(e: MouseEvent, par: ParContext) {
        const editor = this.getParEditor();
        if (!editor) {
            void showMessageDialog("Editor is no longer open.");
            return;
        }
        editor.cancelClicked();
        this.viewctrl.parmenuHandler.showOptionsWindow(e, par);
    }

    /**
     * Checks whether the given paragraph is a table paragraph and in edit mode.
     * @param par The paragraph.
     * @returns True if the paragraph is a table paragraph in edit mode, false
     * if the paragraph is a table paragraph but not in edit mode, undefined if the paragraph is not a table paragraph
     * at all or the table has forced edit mode.
     */
    isTimTableInEditMode(par: ParContext): boolean | undefined {
        const tableCtrl = this.viewctrl.getTableControllerFromParId(par);

        if (tableCtrl == null) {
            return undefined;
        }

        if (tableCtrl.isInForcedEditMode()) {
            return undefined;
        }

        return tableCtrl.isInEditMode();
    }

    isQST(par: ParContext): boolean {
        return par.par.attrs.plugin === "qst";
    }

    /**
     * Checks whether given paragraph is a controller with a custom menu entry
     * @param par - Paragraph to inspect
     * @param editable - whether user can edit the paragraph
     * TODO: Add support for multiple options per controller
     */
    getParMenuEntry(
        par: ParContext,
        editable: boolean
    ): IMenuFunctionEntry | undefined {
        if (!editable) {
            return undefined;
        }
        return this.viewctrl.getParMenuEntry(par)?.getMenuEntry();
    }

    getViewSourceEditorMenuEntry(par: ParContext): IMenuFunctionEntry {
        return {
            desc: "View source",
            func: async () => {
                const r = await this.getBlock(new ParSelection(par, par));
                if (r.ok) {
                    const text = r.result.data.text;
                    await showDiffDialog({
                        left: text,
                        right: text,
                        title: "Source",
                        showToolbar: false,
                    });
                } else {
                    await showMessageDialog("Failed to get paragraph markdown");
                }
            },
            show: canSeeSource(this.viewctrl.item, par),
        };
    }

    getEditorFunctions(par: ParContext | HelpPar): MenuFunctionList {
        if (par.isHelp) {
            return [
                this.getAddParagraphItem({type: EditType.AddBottom}),
                this.getAddQuestionItem({type: EditType.AddBottom}),
                this.getAddLectureQuestionItem({type: EditType.AddBottom}),
            ];
        }
        const parEditable = canEditPar(this.viewctrl.item, par);
        const timTableEditMode = this.isTimTableInEditMode(par);
        const qstPar = this.isQST(par);
        const customParMenuEntry = this.getParMenuEntry(par, parEditable);
        const fns: MenuFunctionList = [];
        fns.push(this.getViewSourceEditorMenuEntry(par));
        const showSingleParFns = par.isDeletableOnItsOwn();
        const {
            areasBeforeRef,
            areasAfterRef,
            refAreaInclusion,
        } = getContextualAreaInfo(par);
        let areaWithSel:
            | {sel: UserSelection<UnbrokenSelection>; area: Area}
            | undefined;
        if (areasAfterRef.length === 0) {
            for (const ctx of areasBeforeRef) {
                if (ctx instanceof Area && ctx.isStartOrEnd(par.par)) {
                    areaWithSel = {
                        sel: new UserSelection(
                            getMinimalUnbrokenSelection(
                                new ParContext(ctx.startPar.par),
                                new ParContext(ctx.endPar.par)
                            ),
                            par
                        ),
                        area: ctx,
                    };
                }
            }
        }

        if (this.viewctrl.editing) {
            fns.push(
                {
                    func: () => this.goToEditor(),
                    desc: "Go to editor",
                    show: true,
                },
                {
                    func: (e) => this.closeAndSave(e, par),
                    desc: "Close editor and save",
                    show: true,
                },
                {
                    func: (e) => this.closeWithoutSaving(e, par),
                    desc: "Close editor and cancel",
                    show: true,
                }
            );
        } else if (this.selection != null && documentglobals().editMode) {
            const s = this.selection;
            const minSel = getMinimalUnbrokenSelection(s.sel.start, s.sel.end);
            fns.push(
                {
                    func: (e) => this.editSelection(e, minSel),
                    desc: "Edit selection",
                    show: true,
                },
                {
                    func: (e) =>
                        this.viewctrl.areaHandler.createArea(e, minSel),
                    desc: "Create area",
                    show: true,
                },
                {
                    func: (e) =>
                        this.viewctrl.clipboardHandler.cutSelection(e, minSel),
                    desc: "Cut selection",
                    show: parEditable,
                },
                {
                    func: (e) =>
                        this.viewctrl.clipboardHandler.copySelection(e, minSel),
                    desc: "Copy selection",
                    show: true,
                },
                {
                    func: () => this.viewctrl.areaHandler.cancelSelection(),
                    desc: "Cancel selection",
                    show: true,
                }
            );
        } else {
            const addAbovePos: EditPosition = {
                par: par,
                type: EditType.AddAbove,
            };
            fns.push(
                {
                    func: (e) =>
                        this.viewctrl.notesHandler.showNoteWindow(e, par),
                    desc: "Comment/note",
                    show: this.viewctrl.item.rights.can_comment,
                },
                {
                    func: () => {
                        const w = window.open(
                            `/view/${par.par.docId}#${par.par.id}`
                        );
                        if (w) {
                            w.focus();
                        }
                    },
                    desc: "Follow reference",
                    show: par.isReference() || areasAfterRef.length > 0,
                }
            );
            if (
                refAreaInclusion === ParAreaInclusionKind.Outside ||
                refAreaInclusion === ParAreaInclusionKind.IsStart
            ) {
                fns.push(this.getAddParagraphItem(addAbovePos));
            }
            if (
                showSingleParFns ||
                refAreaInclusion === ParAreaInclusionKind.IsStart ||
                refAreaInclusion === ParAreaInclusionKind.IsEnd
            ) {
                fns.push({
                    func: (e) =>
                        this.showEditWindow(
                            e,
                            getMinimalUnbrokenSelection(par, par)
                        ),
                    desc: "Edit",
                    show: parEditable,
                });
            } else if (areaWithSel) {
                const temp = areaWithSel;
                fns.push({
                    func: (e) => {
                        this.showEditWindow(e, temp.sel.sel);
                    },
                    desc: `Edit area '${areaWithSel.area.areaname}'`,
                    show: parEditable,
                });
            }
            fns.push(
                {
                    func: (e) => this.toggleTableEditor(e, par),
                    desc: "Edit table",
                    show:
                        parEditable &&
                        timTableEditMode === false &&
                        !par.isReference(),
                },
                {
                    func: (e) => this.toggleTableEditor(e, par),
                    desc: "Close table editor",
                    show: parEditable && timTableEditMode === true,
                }
            );
            if (customParMenuEntry) {
                fns.push(customParMenuEntry);
            }
            if (showSingleParFns) {
                fns.push(
                    {
                        func: (e) =>
                            this.viewctrl.clipboardHandler.cutPar(e, par),
                        desc: "Cut",
                        show:
                            documentglobals().editMode === "par" && parEditable,
                    },
                    {
                        func: (e) =>
                            this.viewctrl.clipboardHandler.copyPar(e, par),
                        desc: "Copy",
                        show: documentglobals().editMode !== "area",
                    }
                );
            } else if (areaWithSel) {
                const temp = areaWithSel;
                fns.push(
                    {
                        func: (e) => {
                            this.viewctrl.clipboardHandler.cutSelection(
                                e,
                                temp.sel.sel
                            );
                        },
                        desc: `Cut area '${areaWithSel.area.areaname}'`,
                        show: parEditable,
                    },
                    {
                        func: (e) => {
                            this.viewctrl.clipboardHandler.copySelection(
                                e,
                                temp.sel.sel
                            );
                        },
                        desc: `Copy area '${areaWithSel.area.areaname}'`,
                        show: parEditable,
                    }
                );
            }
            if (refAreaInclusion !== ParAreaInclusionKind.Inside) {
                fns.push(
                    {
                        func: (e) =>
                            this.viewctrl.clipboardHandler.showPasteMenu(
                                e,
                                par
                            ),
                        desc: "Paste...",
                        show: documentglobals().editMode != null,
                        closeAfter: false,
                    },
                    {
                        func: (e) =>
                            this.viewctrl.clipboardHandler.showMoveMenu(e, par),
                        desc: "Move here...",
                        show: documentglobals().allowMove,
                    },
                    {
                        func: (e) =>
                            this.viewctrl.areaHandler.removeAreaMarking(e, par),
                        desc: "Remove area marking",
                        show: documentglobals().editMode === "area",
                    }
                );
            }
            if (
                refAreaInclusion === ParAreaInclusionKind.Outside ||
                refAreaInclusion === ParAreaInclusionKind.IsStart
            ) {
                fns.push(this.getAddQuestionItem(addAbovePos));
            }
            fns.push(
                {
                    func: (e) => this.viewctrl.questionHandler.editQst(e, par),
                    desc: "Edit question",
                    show:
                        /* this.viewctrl.lectureMode && */ parEditable &&
                        qstPar, // TODO: Condition also that par is a question
                },
                this.getAddLectureQuestionItem(addAbovePos),
                {
                    func: (e) =>
                        this.viewctrl.areaHandler.startSelection(e, par),
                    desc: "Start selection",
                    show:
                        documentglobals().editMode === "par" &&
                        this.selection == null,
                }
            );
        }
        fns.push({func: empty, desc: "Close menu", show: true});
        return fns;
    }

    private getAddParagraphItem(pos: EditPosition) {
        return {
            func: () => this.toggleParEditor(pos, {}),
            desc: "Add paragraph above",
            show: this.viewctrl.item.rights.editable,
        };
    }

    private getAddLectureQuestionItem(pos: EditPosition): IMenuFunctionEntry {
        return {
            func: (e) => this.viewctrl.questionHandler.addQuestion(e, pos),
            desc: "Add lecture question above",
            show:
                this.viewctrl.lectureCtrl.lectureSettings.lectureMode &&
                this.viewctrl.item.rights.editable,
        };
    }

    private getAddQuestionItem(pos: EditPosition): IMenuFunctionEntry {
        return {
            func: (e) => this.viewctrl.questionHandler.addQuestionQst(e, pos),
            desc: "Add question above",
            show: /* this.viewctrl.lectureMode && */ this.viewctrl.item.rights
                .editable,
        };
    }

    removeDefaultPars() {
        getElementByParId("HELP_PAR").remove();
    }

    updateSelection(
        par: ParContext,
        userSelection: UserSelection,
        type: SelectionUpdateType
    ) {
        const anchor = userSelection.anchor;
        const sortFn = (a: ParContext, b: ParContext) => {
            const mask = b.par.htmlElement.compareDocumentPosition(
                a.par.htmlElement
            );
            if (mask & Node.DOCUMENT_POSITION_PRECEDING) {
                return -1;
            }
            if (mask & Node.DOCUMENT_POSITION_FOLLOWING) {
                return 1;
            }
            return 0;
        };

        let newsel;
        if (type === SelectionUpdateType.DontAllowShrink) {
            const ctxs = [
                userSelection.sel.start,
                userSelection.sel.end,
                par,
            ].sort(sortFn);
            newsel = new ParSelection(ctxs[0], ctxs[2]);
        } else {
            const ctxs = [anchor, par].sort(sortFn);
            newsel = new ParSelection(ctxs[0], ctxs[1]);
        }
        this.setSelection(new UserSelection(newsel, anchor));
    }
}
