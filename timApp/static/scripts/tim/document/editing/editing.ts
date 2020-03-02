import {IScope} from "angular";
import $ from "jquery";
import {CURSOR} from "tim/editor/BaseParEditor";
import {compileWithViewctrl, IPluginInfoResponse, ParCompiler} from "tim/editor/parCompiler";
import {openEditor, PareditorController} from "tim/editor/pareditor";
import {showMessageDialog} from "tim/ui/dialog";
import {documentglobals} from "tim/util/globals";
import {$http, $timeout} from "tim/util/ngimport";
import {empty, isMobileDevice, markPageDirty, to} from "tim/util/utils";
import {showDiffDialog} from "../diffDialog";
import {onClick} from "../eventhandlers";
import {
    canEditPar,
    canSeeSource,
    createNewPar,
    getElementByParId,
    getFirstParId,
    getLastParId,
    getNextPar,
    getParAttributes,
    getParId,
    getParIndex,
    getPars,
    getRefAttrs,
    isActionablePar,
    isHelpPar,
    isReference,
    isSettingsPar,
    Paragraph,
} from "../parhelpers";
import {handleUnread} from "../readings";
import {ViewCtrl} from "../viewctrl";
import {MenuFunctionList} from "../viewutils";
import {EditPosition, EditType, IExtraData, IParResponse, ITags} from "./edittypes";
import {isManageResponse, showRenameDialog} from "./pluginRenameForm";

export interface IParEditorOptions {
    forcedClasses?: string[];
    showDelete?: boolean;
    area?: boolean;
    localSaveTag?: string;
    texts?: {beforeText: string, initialText: string, afterText: string};
}

function prepareOptions($this: HTMLElement, saveTag: string): [JQuery, IParEditorOptions] {
    const par = $($this).closest(".par");
    const text = par.find("pre").text();
    let forcedClasses: string[] = [];
    const forceAttr = getParAttributes(par).forceclass;
    if (forceAttr) {
        forcedClasses = forceAttr.split(" ");
    }
    const options = {
        localSaveTag: saveTag,
        texts: {
            beforeText: "alkuun",
            initialText: text,
            afterText: "loppuun",
        },
        forcedClasses: forcedClasses,
    };
    return [par, options];
}

export class EditingHandler {
    public viewctrl: ViewCtrl;
    public sc: IScope;
    private currentEditor?: PareditorController;

    constructor(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        this.viewctrl.editing = false;

        this.viewctrl.selection = {};

        sc.$watchGroup([
            () => this.viewctrl.selection.start,
            () => this.viewctrl.selection.end], (newValues, oldValues, scope) => {
            $(".par.lightselect").removeClass("lightselect");
            $(".par.selected").removeClass("selected");
            $(".par.marked").removeClass("marked");
            if (this.viewctrl.selection.start != null) {
                const start = this.viewctrl.selection.start;
                if (this.viewctrl.selection.end != null &&
                    !this.viewctrl.selection.end.is(this.viewctrl.selection.start)) {
                    const end = this.viewctrl.selection.end;
                    this.viewctrl.selection.pars = getPars(start, end);
                } else {
                    this.viewctrl.selection.pars = start;
                }
                if (this.viewctrl.selection.pars) {
                    this.viewctrl.selection.pars.addClass("marked");
                }
            }
        });

        if (this.viewctrl.item.rights.editable) {
            onClick(".addBottom", ($this, e) => {
                this.viewctrl.closePopupIfOpen();
                this.toggleParEditor({type: EditType.AddBottom}, {});
            });

            onClick(".addAbove", ($this, e) => {
                this.viewctrl.closePopupIfOpen();
                const [par, options] = prepareOptions($this[0], "addAbove");
                return this.showAddParagraphAbove(e, par, options);
            });

            onClick(".addBelow", ($this, e) => {
                this.viewctrl.closePopupIfOpen();
                const [par, options] = prepareOptions($this[0], "addBelow");
                return this.showAddParagraphBelow(e, par, options);
            });

            onClick(".pasteBottom", ($this, e) => {
                this.viewctrl.closePopupIfOpen();
                this.viewctrl.clipboardHandler.pasteAbove(e, undefined, false);
            });

            onClick(".pasteRefBottom", ($this, e) => {
                this.viewctrl.closePopupIfOpen();
                this.viewctrl.clipboardHandler.pasteAbove(e, undefined, true);
            });
        }
    }

    async toggleParEditor(params: EditPosition, options: IParEditorOptions) {
        if (this.viewctrl.editing) {
            await showMessageDialog("Some editor is already open.");
            return;
        }
        let areaStart;
        let areaEnd;
        const caption = params.type === EditType.Edit ? "Edit paragraph" : "Add paragraph";
        let url: string;
        const parId = params.type === EditType.Edit ? getParId(params.pars) : undefined;
        let parNextId =
            params.type === EditType.Edit ? getParId(getNextPar(params.pars))
                : params.type === EditType.AddAbove ? getParId(params.par)
                : params.type === EditType.AddBelow ? getParId(getNextPar(params.par))
                    : undefined;
        if (parNextId === "HELP_PAR") {
            parNextId = undefined;
        }

        if (params.type === EditType.Edit && params.pars.length > 1) {
            areaStart = getFirstParId(params.pars);
            areaEnd = getLastParId(params.pars);
            url = "/postParagraph/";
            options.showDelete = true;
        } else {
            // TODO: Use same route (postParagraph) for both cases, determine logic based on given parameters
            if (parId === "HELP_PAR" || params.type !== EditType.Edit) {
                url = "/newParagraph/";
                options.showDelete = false;
            } else {
                url = "/postParagraph/";
                options.showDelete = true;
            }
        }

        if (options.area) {
            areaEnd = getParId(this.viewctrl.selection.end);
            areaStart = getParId(this.viewctrl.selection.start);
        } else {
            areaStart = undefined;
            areaEnd = undefined;
        }

        const tagKeys: Array<keyof ITags> = ["markread"];
        const tagsDescs: Array<{name: keyof ITags, desc: string}> = [{name: "markread", desc: "Mark as read"}];
        const tags: ITags = {markread: false};
        if (this.viewctrl.isTranslation()) {
            tagKeys.push("marktranslated");
            tagsDescs.push({name: "marktranslated", desc: "Mark as translated"});
        }
        for (const t of tagKeys) {
            const x = window.localStorage.getItem(t) ?? false;
            tags[t] = x === "true";
        }

        const extraData: IExtraData = {
            area_end: areaEnd,
            area_start: areaStart,
            docId: this.viewctrl.docId, // current document id
            forced_classes: options.forcedClasses,
            par: parId ?? "NEW_PAR", // the id of paragraph on which the editor was opened
            par_next: parNextId, // the id of the paragraph that follows par or null if par is the last one
            tags,
            ...(params.type === EditType.Edit ? isReference(params.pars) ? getRefAttrs(params.pars) : {} : {}),
        };
        let initialText = "";
        let cursorPos;
        if (options.texts) {
            initialText = options.texts.initialText;
            cursorPos = initialText.indexOf(CURSOR);
            initialText = initialText.replace(CURSOR, "");
        } else if (options.showDelete && parId !== "HELP_PAR" && parId) {
            const r = await this.getBlock(parId, extraData);
            if (r.ok) {
                initialText = r.result.data.text;
            } else {
                await showMessageDialog(`Failed to open editor: ${r.result.data.error}`);
                return;
            }
        }
        this.viewctrl.editing = true;
        const inst = openEditor({
            viewCtrl: this.viewctrl,
            extraData,
            initialText,
            defaultSize: "lg",
            options: {
                caption,
                deleteMsg: `
This will delete the whole ${options.area ? "area" : "paragraph"} from the document. Are you sure?

(If you only want to remove selected text, use backspace.)`,
                localSaveTag: options.localSaveTag ?? "par",
                showDelete: options.showDelete,
                showImageUpload: true,
                showPlugins: true,
                cursorPosition: cursorPos,
                showSettings: params.type === EditType.Edit ? isSettingsPar(params.pars) : false,
                tags: tagsDescs,
                touchDevice: isMobileDevice(),
            },
            deleteCb: async () => {
                const r = await to($http.post<IParResponse>(`/deleteParagraph/${this.viewctrl.docId}`, extraData));
                if (!r.ok) {
                    return {error: r.result.data.error};
                } else {
                    this.handleDelete(params);
                }
                return {};
            },
            previewCb: async (text) => {
                const r = await to($http.post<IPluginInfoResponse>(`/preview/${this.viewctrl.docId}`, {text, proofread: (this.currentEditor)? this.currentEditor.spellcheck : false, ...extraData}));
                if (!r.ok) {
                    throw Error("preview failed");
                }
                return r.result.data;
            },
            saveCb: async (text, data) => {
                const r = await to($http.post<IParResponse>(url, {text, ...data}));
                if (!r.ok) {
                    return {error: r.result.data.error};
                } else {
                    const saveData = r.result.data;
                    if (saveData.duplicates && saveData.duplicates.length > 0 && saveData.new_par_ids != null) {
                        const res = await to(showRenameDialog({
                            duplicates: saveData.duplicates,
                            extraData,
                            new_par_ids: saveData.new_par_ids,
                            original_par: saveData.original_par,
                        }));
                        if (res.ok && !isManageResponse(res.result)) {
                            this.addSavedParToDom(res.result, params);
                        } else {
                            this.addSavedParToDom(saveData, params);
                        }
                    } else {
                        this.addSavedParToDom(saveData, params);
                    }
                }
                return {};
            },
            unreadCb: async () => {
                await handleUnread(this.viewctrl.item, extraData, params);
            },
        });
        this.currentEditor = await inst.dialogInstance.promise;
        await to(inst.result);
        this.viewctrl.editing = false;
    }

    private async getBlock(parId: string, extraData?: IExtraData) {
        return await to($http.get<{text: string}>(`/getBlock/${this.viewctrl.docId}/${parId}`,
            {params: extraData}));
    }

    findSettingsPars() {
        const pars = $(".par");
        return pars.filter((index, elem) => {
            const p = $(elem);
            return isActionablePar(p) && isSettingsPar(p);
        });
    }

    hasNonSettingsPars() {
        let has = false;
        $(".par").each((index, elem) => {
            const p = $(elem);
            if (!isSettingsPar(p) && !isHelpPar(p)) {
                has = true;
                return false;
            }
        });
        return has;
    }

    async insertHelpPar() {
        await $timeout();
        const spars = this.findSettingsPars();
        const helpPar = $(`
<div class="par"
     id="HELP_PAR"
     t=""
     attrs="{}">
    <div class="parContent">
        <tim-help-par-content></tim-help-par-content>
    </div>
    <div class="editline" title="Click to edit this paragraph"></div>
</div>
`);
        if (spars.length === 0) {
            $("#pars").prepend(helpPar);
        } else {
            spars.last().after(helpPar);
        }
        await compileWithViewctrl(helpPar, this.viewctrl.scope, this.viewctrl);
    }

    async editSettingsPars(recursiveCall: boolean = false) {
        const pars = this.findSettingsPars();
        if (pars.length === 0) {
            if (recursiveCall) {
                throw new Error("Faulty recursion stopped, there should be a settings paragraph already");
            }
            const first = $(".par:not(.preamble):not(#HELP_PAR):first");
            const parNext = getParId(first);

            const r = await
                to($http.post<IParResponse>("/newParagraph/", {
                    text: '``` {settings=""}\nauto_number_headings: 0\n```',
                    docId: this.viewctrl.docId,
                    par_next: parNext,
                }));
            if (!r.ok) {
                await showMessageDialog(r.result.data.error);
                return;
            }
            await this.addSavedParToDom(r.result.data, {type: EditType.AddBottom});
            this.editSettingsPars(true);
        } else if (pars.length === 1) {
            this.toggleParEditor({type: EditType.Edit, pars: pars.first()}, {area: false});
        } else {
            const start = pars.first();
            const end = pars.last();
            this.viewctrl.selection.start = start;
            this.viewctrl.selection.end = end;
            pars.addClass("selected");
            this.toggleParEditor({type: EditType.Edit, pars: pars}, {area: true});
            pars.removeClass("selected");
            this.viewctrl.areaHandler.cancelArea();
        }
    }

    showEditWindow(e: JQuery.Event, par: Paragraph) {
        this.toggleParEditor({type: EditType.Edit, pars: par}, {area: false});
    }

    beginAreaEditing(e: JQuery.Event, par: Paragraph) {
        if (!this.viewctrl.selection.pars) {
            showMessageDialog("Selection was null when trying to edit an area.");
            return;
        }
        this.toggleParEditor({type: EditType.Edit, pars: $(this.viewctrl.selection.pars)}, {area: true});
    }

    /**
     * Toggles the edit mode of a table in a given table paragraph.
     * @param {Event} e
     * @param {Paragraph} par The table paragraph.
     */
    toggleTableEditor(e: JQuery.Event, par: Paragraph) {
        const parId = getParId(par);

        if (parId == null) {
            void showMessageDialog("Could not find paragraph");
            return;
        }

        const tableCtrl = this.viewctrl.getTableControllerFromParId(parId);

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
        this.viewctrl.areaHandler.cancelArea();
        this.viewctrl.beginUpdate();
    }

    showAddParagraphAbove(e: JQuery.Event, par: Paragraph, options: IParEditorOptions = {}) {
        options.area = false;
        this.toggleParEditor({type: EditType.AddAbove, par: par}, options);
    }

    showAddParagraphBelow(e: JQuery.Event, par: Paragraph, options: IParEditorOptions = {}) {
        options.area = false;
        this.toggleParEditor({type: EditType.AddBelow, par: par}, options);
    }

    async addSavedParToDom(data: IParResponse, position: EditPosition) {
        let par: JQuery;
        if (position.type === EditType.Edit) {
            par = position.pars;
        } else {
            const newpar = createNewPar();
            if (position.type === EditType.AddBottom) {
                $(".par").last().after(newpar);
            } else if (position.type === EditType.AddAbove) {
                position.par.before(newpar);
            } else {
                position.par.after(newpar);
            }
            par = newpar;
        }

        // check if we were editing an area
        if (position.type === EditType.Edit && position.pars.length > 1) {
            par = position.pars.first();

            // remove all but the first element of the area because it'll be used
            // when replacing
            const endpar = position.pars.last();
            par.nextUntil(endpar).add(endpar).remove();
        }
        const newPars = await ParCompiler.compileAndReplace(par, data, this.viewctrl.scope, this.viewctrl);
        if (documentglobals().editMode === "area") {
            newPars.find(".editline").removeClass("editline").addClass("editline-disabled");
        }

        this.viewctrl.docVersion = data.version;
        this.viewctrl.areaHandler.cancelArea();
        this.removeDefaultPars();
        markPageDirty();
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

    closeAndSave(e: JQuery.MouseEventBase, par: Paragraph) {
        const editor = this.getParEditor();
        if (!editor) {
            void showMessageDialog("Editor is no longer open.");
            return;
        }
        editor.saveClicked();
        this.viewctrl.parmenuHandler.showOptionsWindow(e, par);
    }

    closeWithoutSaving(e: JQuery.MouseEventBase, par: Paragraph) {
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
     * @param {Paragraph} par The paragraph.
     * @returns {boolean | undefined} True if the paragraph is a table paragraph in edit mode, false
     * if the paragraph is a table paragraph but not in edit mode, undefined if the paragraph is not a table paragraph
     * at all or the table has forced edit mode.
     */
    isTimTableInEditMode(par: Paragraph | undefined): boolean | undefined {
        if (par == null) {
            return undefined;
        }

        const parId = getParId(par);

        if (parId == null) {
            return undefined;
        }

        const tableCtrl = this.viewctrl.getTableControllerFromParId(parId);

        if (tableCtrl == null) {
            return undefined;
        }

        if (tableCtrl.isInForcedEditMode()) {
            return undefined;
        }

        return tableCtrl.isInEditMode();
    }

    isQST(par: Paragraph | undefined): boolean {
        if (par == null) {
            return false;
        }
        return getParAttributes(par).plugin === "qst";
    }

    getEditorFunctions(par?: Paragraph): MenuFunctionList {
        const parEditable = ((!par && this.viewctrl.item.rights.editable) || (par && canEditPar(this.viewctrl.item, par))) || false;
        const timTableEditMode = this.isTimTableInEditMode(par);
        const qstPar = this.isQST(par);
        if (this.viewctrl.editing) {
            return [
                {func: () => this.goToEditor(), desc: "Go to editor", show: true},
                {func: (e, p: Paragraph) => this.closeAndSave(e, p), desc: "Close editor and save", show: true},
                {func: (e, p: Paragraph) => this.closeWithoutSaving(e, p), desc: "Close editor and cancel", show: true},
                {func: empty, desc: "Close menu", show: true},
            ];
        } else if (this.viewctrl.selection.start != null && documentglobals().editMode) {
            return [
                {
                    func: (e: JQuery.Event, p: Paragraph) => this.beginAreaEditing(e, p),
                    desc: "Edit area",
                    show: true,
                },
                {func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.areaHandler.nameArea(e, p), desc: "Name area", show: true},
                {func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.clipboardHandler.cutArea(e, p), desc: "Cut area", show: parEditable},
                {func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.clipboardHandler.copyArea(e, p), desc: "Copy area", show: true},
                {func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.areaHandler.cancelArea(), desc: "Cancel area", show: true},
                {func: empty, desc: "Close menu", show: true},
            ];
        } else {
            return [
                {
                    func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.notesHandler.showNoteWindow(e, p),
                    desc: "Comment/note",
                    show: this.viewctrl.item.rights.can_comment && par != null && !isHelpPar(par),
                },
                {
                    func: async (e: JQuery.Event, p: Paragraph) => {
                        const parId = getParId(p);
                        if (!parId) {
                            await showMessageDialog("Could not get paragraph id");
                            return;
                        }
                        const r = await this.getBlock(parId);
                        if (r.ok) {
                            const text = r.result.data.text;
                            await showDiffDialog({
                                left: text,
                                right: text,
                                title: "Source",
                            });
                        } else {
                            await showMessageDialog("Failed to get paragraph markdown");
                        }
                    },
                    desc: "View source",
                    show: par != null && !isHelpPar(par) && canSeeSource(this.viewctrl.item, par),
                },
                {
                    func: (e, p) => {
                        if (!par) {
                            return;
                        }
                        const attrs = getRefAttrs(par);
                        const docId = attrs["ref-doc-id"];
                        const w = window.open(`/view/${docId}#${attrs["ref-id"]}`);
                        if (w) {
                            w.focus();
                        }
                    },
                    desc: "Follow reference",
                    show: par != null && isReference(par),
                },
                {
                    func: (e: JQuery.Event, p: Paragraph) => this.showAddParagraphAbove(e, p),
                    desc: "Add paragraph above",
                    show: this.viewctrl.item.rights.editable,
                },
                {func: (e: JQuery.Event, p: Paragraph) => this.showEditWindow(e, p), desc: "Edit", show: parEditable},
                {func: (e: JQuery.Event, p: Paragraph) => this.toggleTableEditor(e, p), desc: "Edit table", show: parEditable && timTableEditMode === false && par != null && !isReference(par)},
                {func: (e: JQuery.Event, p: Paragraph) => this.toggleTableEditor(e, p), desc: "Close table editor", show: parEditable && timTableEditMode === true},
                {
                    func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.clipboardHandler.cutPar(e, p),
                    desc: "Cut",
                    show: documentglobals().editMode === "par" && parEditable,
                },
                {
                    func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.clipboardHandler.copyPar(e, p),
                    desc: "Copy",
                    show: documentglobals().editMode !== "area" && par != null && !isHelpPar(par),
                },
                // {func: (e, par) => this.cutArea(e, par), desc: 'Cut area', show: $window.editMode === 'area'},
                // {func: (e, par) => this.copyArea(e, par), desc: 'Copy area', show: $window.editMode === 'area'},
                {
                    func: (e, p: Paragraph) => this.viewctrl.clipboardHandler.showPasteMenu(e, p),
                    desc: "Paste...",
                    show: documentglobals().editMode != null && (this.viewctrl.clipMeta.allowPasteRef || this.viewctrl.clipMeta.allowPasteContent),
                    closeAfter: false,
                },
                {func: (e, p: Paragraph) => this.viewctrl.clipboardHandler.showMoveMenu(e, p), desc: "Move here...", show: documentglobals().allowMove},
                {
                    func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.areaHandler.removeAreaMarking(e, p),
                    desc: "Remove area marking",
                    show: documentglobals().editMode === "area",
                },
                {
                    func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.questionHandler.addQuestionQst(e, p),
                    desc: "Add question above",
                    show: /* this.viewctrl.lectureMode && */ this.viewctrl.item.rights.editable,
                },
                {
                    func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.questionHandler.editQst(e, p),
                    desc: "Edit question",
                    show: /* this.viewctrl.lectureMode && */  parEditable && qstPar,  // TODO: Condition also that par is a question
                },
                {
                    func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.questionHandler.addQuestion(e, p),
                    desc: "Create lecture question",
                    show: this.viewctrl.lectureCtrl.lectureSettings.lectureMode && this.viewctrl.item.rights.editable,
                },
                {
                    func: (e: JQuery.Event, p: Paragraph) => this.viewctrl.areaHandler.startArea(e, p),
                    desc: "Start selecting area",
                    show: documentglobals().editMode === "par" && this.viewctrl.selection.start == null,
                },
                {func: empty, desc: "Close menu", show: true},
            ];
        }
    }

    getAddParagraphFunctions() {
        return [
            {func: (e: JQuery.Event, p: Paragraph) => this.showAddParagraphAbove(e, p), desc: "Above", show: true},
            {func: (e: JQuery.Event, p: Paragraph) => this.showAddParagraphBelow(e, p), desc: "Below", show: true},
            {func: empty, desc: "Cancel", show: true},
        ];
    }

    removeDefaultPars() {
        getElementByParId("HELP_PAR").remove();
    }

    extendSelection(par: Paragraph, allowShrink = false) {
        if (this.viewctrl.selection.start == null) {
            this.viewctrl.selection.start = par;
            this.viewctrl.selection.end = par;
        } else if (this.viewctrl.selection.pars) {
            const n = this.viewctrl.selection.pars.length;
            const startIndex = getParIndex(this.viewctrl.selection.pars.eq(0));
            const endIndex = getParIndex(this.viewctrl.selection.pars.eq(n - 1));
            const newIndex = getParIndex(par);
            if (endIndex == null || startIndex == null || newIndex == null) {
                return;
            }
            const areaLength = endIndex - startIndex + 1;
            if (newIndex < startIndex) {
                this.viewctrl.selection.start = par;
            } else if (newIndex > endIndex) {
                this.viewctrl.selection.end = par;
            } else if (allowShrink && areaLength > 1 && newIndex === startIndex) {
                this.viewctrl.selection.start = $(this.viewctrl.selection.pars[1]);
            } else if (allowShrink && areaLength > 1 && newIndex === endIndex) {
                this.viewctrl.selection.end = $(this.viewctrl.selection.pars[n - 2]);
            }
        }
    }
}
