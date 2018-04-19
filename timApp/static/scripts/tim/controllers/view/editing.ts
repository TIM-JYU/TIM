import {IScope} from "angular";
import $ from "jquery";
import {Coords, markPageDirty} from "tim/utils";
import {isManageResponse, showRenameDialog} from "../../components/pluginRenameForm";
import {showMessageDialog} from "../../dialog";
import {openEditor} from "../../directives/pareditor";
import {IExtraData, IParResponse} from "../../edittypes";
import {$compile, $http, $window} from "../../ngimport";
import {IPluginInfoResponse, ParCompiler} from "../../services/parCompiler";
import {empty, isMobileDevice, to} from "../../utils";
import {getActiveDocument} from "./document";
import {onClick} from "./eventhandlers";
import {
    canEditPar,
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
    isPreamble,
    isReference,
    isSettingsPar,
    Paragraph,
} from "./parhelpers";
import {ViewCtrl} from "./viewctrl";
import {viewCtrlDot} from "./viewutils";

export enum EditType {
    Edit,
    AddAbove,
    AddBelow,
    AddBottom,
}

export type EditPosition =
    | {type: EditType.Edit, pars: JQuery}
    | {type: EditType.AddAbove, par: JQuery}
    | {type: EditType.AddBelow, par: JQuery}
    | {type: EditType.AddBottom};

export interface IParEditorOptions {
    forcedClasses?: string[];
    showDelete?: boolean;
    area?: boolean;
    localSaveTag?: string;
    texts?: {beforeText: string, initialText: string, afterText: string};
}

export interface IParEditorAttrs {
    [key: string]: any;
}

function prepareOptions($this: Element, saveTag: string): [JQuery, IParEditorOptions] {
    // var $par = $('.par').last();
    // return sc.showAddParagraphBelow(e, $par);
    // return sc.showAddParagraphAbove(e, sc.$pars);
    const par = $($this).closest('.par');
    const text = par.find('pre').text();
    // text = text.replace('⁞', '');  // TODO: set cursor to | position
    let forcedClasses: string[] = [];
    const forceAttr = getParAttributes(par).forceclass;
    if (forceAttr) {
        forcedClasses = forceAttr.split(" ");
    }
    const options = {
        'localSaveTag': saveTag,
        'texts': {
            'beforeText': "alkuun",
            'initialText': text,
            'afterText': "loppuun",
        },
        forcedClasses: forcedClasses,
    };
    return [par, options];
}

// Wrap given text to max n chars length lines spliting from space
export function wrapText(s: string, n: number) {
    var lines = s.split("\n");
    var needJoin = false;
    for (var i = 0; i < lines.length; i++) {
        var line = lines[i];
        // lines[i] = "";
        var sep = "";
        if (line.length > n) {
            lines[i] = "";
            while (true) {
                var p = -1;
                if (line.length > n) {
                    p = line.lastIndexOf(" ", n);
                    if (p < 0) p = line.indexOf(" "); // long line
                }
                if (p < 0) {
                    lines[i] += sep + line;
                    break;
                }
                lines[i] += sep + line.substring(0, p);
                line = line.substring(p + 1);
                if (i + 1 < lines.length && (lines[i + 1].length > 0 && (" 0123456789-".indexOf(lines[i + 1][0]) < 0))) {
                    lines[i + 1] = line + " " + lines[i + 1];
                    needJoin = true;
                    break;
                }
                sep = "\n";
                needJoin = true;
            }
        }
    }
    if (needJoin) return {modified: true, s: lines.join("\n")};
    return {modified: false, s: s};
}

export class EditingHandler {
    public viewctrl: ViewCtrl;
    public sc: IScope;

    constructor(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        this.viewctrl.editing = false;
        this.viewctrl.addParagraphFunctions = this.getAddParagraphFunctions();

        this.viewctrl.selection = {start: null, end: null, pars: null};

        sc.$watchGroup([
            () => this.viewctrl.selection.start,
            () => this.viewctrl.selection.end], (newValues, oldValues, scope) => {
            $(".par.lightselect").removeClass("lightselect");
            $(".par.selected").removeClass("selected");
            $(".par.marked").removeClass("marked");
            if (this.viewctrl.selection.start != null) {
                const $start = this.viewctrl.selection.start;
                if (this.viewctrl.selection.end != null &&
                    !this.viewctrl.selection.end.is(this.viewctrl.selection.start)) {
                    const $end = this.viewctrl.selection.end;
                    this.viewctrl.selection.pars = getPars($start, $end);
                } else {
                    this.viewctrl.selection.pars = $start;
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

        const tags = {markread: false};
        const markread = $window.localStorage.getItem("markread") || false;
        tags.markread = markread === "true";

        const extraData: IExtraData = {
            area_end: areaEnd,
            area_start: areaStart,
            docId: this.viewctrl.docId, // current document id
            forced_classes: options.forcedClasses,
            par: parId || "NEW_PAR", // the id of paragraph on which the editor was opened
            par_next: parNextId, // the id of the paragraph that follows par or null if par is the last one
            tags,
            ...(params.type === EditType.Edit ? isReference(params.pars) ? getRefAttrs(params.pars) : {} : {}),
        };
        let initialText = "";
        if (options.showDelete && parId !== "HELP_PAR") {
            initialText = (await $http.get<{text: string}>(`/getBlock/${this.viewctrl.docId}/${parId}`,
                {params: extraData})).data.text;
        }
        this.viewctrl.editing = true;
        await to(openEditor({
            extraData,
            initialText,
            options: {
                caption,
                localSaveTag: options.localSaveTag || "par",
                showDelete: options.showDelete,
                showImageUpload: true,
                showPlugins: true,
                showSettings: params.type === EditType.Edit ? isSettingsPar(params.pars) : false,
                tags: [
                    {name: "markread", desc: "Mark as read"},
                ],
                touchDevice: isMobileDevice(),
            },
            deleteCb: async () => {
                const [err, resp] = await to($http.post<IParResponse>(`/deleteParagraph/${this.viewctrl.docId}`, extraData));
                if (err) {
                    return {error: err.data.error};
                } else if (resp) {
                    this.handleDelete(params);
                }
                return {};
            },
            previewCb: async (text) => (await $http.post<IPluginInfoResponse>(`/preview/${this.viewctrl.docId}`, {text, ...extraData})).data,
            saveCb: async (text, data) => {
                const [err, resp] = await to($http.post<IParResponse>(url, {text, ...data}));
                if (err) {
                    return {error: err.data.error};
                } else if (resp) {
                    const saveData = resp.data;
                    if (saveData.duplicates && saveData.duplicates.length > 0 && saveData.new_par_ids != null) {
                        const [renameErr, result] = await to(showRenameDialog({
                            duplicates: saveData.duplicates,
                            extraData,
                            new_par_ids: saveData.new_par_ids,
                            original_par: saveData.original_par,
                        }));
                        if (result && !isManageResponse(result)) {
                            this.addSavedParToDom(result, params);
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
                if (params.type !== EditType.Edit) {
                    return;
                }
                await $http.post(`/unread/${this.viewctrl.docId}/${extraData.par}`, {});
                params.pars.first().find(".readline").removeClass("read read-modified");
                getActiveDocument().refreshSectionReadMarks();
            },
        }));
        this.viewctrl.editing = false;
    }

    async editSettingsPars(recursiveCall: boolean) {
        const pars: HTMLElement[] = [];
        $(".par").each((index, elem) => {
            if (isPreamble($(elem))) {
                return;
            }
            if (getParAttributes($(elem)).hasOwnProperty("settings")) {
                pars.push(elem);
            }
        });
        if (pars.length === 0) {
            if (recursiveCall) {
                throw new Error("Faulty recursion stopped, there should be a settings paragraph already");
            }
            const $first = $(".par:not(.preamble):first");
            let parNext = getParId($first);
            if (parNext === "HELP_PAR") {
                parNext = undefined;
            }
            try {
                var response = await
                    $http.post<IParResponse>("/newParagraph/", {
                        text: '``` {settings=""}\nexample:\n```',
                        docId: this.viewctrl.docId,
                        par_next: parNext,
                    });
            } catch (e) {
                $window.alert(e.data.error);
                return;
            }
            this.addSavedParToDom(response.data, {type: EditType.AddBottom});
            this.editSettingsPars(true);
        } else if (pars.length === 1) {
            this.toggleParEditor({type: EditType.Edit, pars: $(pars[0])}, {area: false});
        } else {
            const start = pars[0];
            const end = pars[pars.length - 1];
            this.viewctrl.selection.start = $(start);
            this.viewctrl.selection.end = $(end);
            $(pars).addClass("selected");
            this.toggleParEditor({type: EditType.Edit, pars: $(pars)}, {area: true});
            $(pars).removeClass("selected");
            this.viewctrl.areaHandler.cancelArea();
        }
    }

    showEditWindow(e: Event, $par: Paragraph) {
        this.toggleParEditor({type: EditType.Edit, pars: $par}, {area: false});
    }

    beginAreaEditing(e: Event, $par: Paragraph) {
        if (!this.viewctrl.selection.pars) {
            showMessageDialog("Selection was null when trying to edit an area.");
            return;
        }
        this.toggleParEditor({type: EditType.Edit, pars: $(this.viewctrl.selection.pars)}, {area: true});
    }

    handleDelete(position: EditPosition) {
        if (position.type === EditType.Edit) {
            position.pars.remove();
        }
        this.viewctrl.areaHandler.cancelArea();
        this.viewctrl.beginUpdate();
    }

    showAddParagraphAbove(e: Event, $par: Paragraph, options: IParEditorOptions = {}) {
        options.area = false;
        this.toggleParEditor({type: EditType.AddAbove, par: $par}, options);
    }

    showAddParagraphBelow(e: Event, $par: Paragraph, options: IParEditorOptions = {}) {
        options.area = false;
        this.toggleParEditor({type: EditType.AddBelow, par: $par}, options);
    }

    addSavedParToDom(data: IParResponse, position: EditPosition) {
        let $par: JQuery;
        if (position.type === EditType.Edit) {
            $par = position.pars;
        } else {
            const newpar = createNewPar();
            if (position.type === EditType.AddBottom) {
                $(".par").last().after(newpar);
            } else if (position.type === EditType.AddAbove) {
                position.par.before(newpar);
            } else {
                position.par.after(newpar);
            }
            $par = newpar;
        }

        // check if we were editing an area
        if (position.type === EditType.Edit && position.pars.length > 1) {
            $par = position.pars.first();

            // remove all but the first element of the area because it'll be used
            // when replacing
            const $endpar = position.pars.last();
            $par.nextUntil($endpar).add($endpar).remove();
        }
        const $newPars = $($compile(data.texts)(this.viewctrl.reviewCtrlScope, undefined,
            {
                transcludeControllers: {
                    timReview: {instance: this.viewctrl.reviewCtrlScope.$ctrl},
                    timView: {instance: this.viewctrl},
                    ...(this.viewctrl.lectureCtrl != null ? {timLecture: {instance: this.viewctrl.lectureCtrl}} : {}),
                },
            }));
        if ($window.editMode === "area") {
            $newPars.find(".editline").removeClass("editline").addClass("editline-disabled");
        }

        $par.replaceWith($newPars);
        ParCompiler.processAllMathDelayed($newPars);
        this.viewctrl.docVersion = data.version;
        this.viewctrl.areaHandler.cancelArea();
        this.removeDefaultPars();
        markPageDirty();
        this.viewctrl.beginUpdate();
    }

    goToEditor() {
        $("pareditor")[0].scrollIntoView();
    }

    closeAndSave(e: Event, $par: Paragraph) {
        $("pareditor").isolateScope<any>().$ctrl.saveClicked();
        this.viewctrl.parmenuHandler.showOptionsWindow(e, $par);
    }

    closeWithoutSaving(e: Event, $par: Paragraph) {
        $("pareditor").isolateScope<any>().$ctrl.cancelClicked();
        this.viewctrl.parmenuHandler.showOptionsWindow(e, $par);
    }

    getEditorFunctions($par?: Paragraph) {
        const parEditable = ((!$par && this.viewctrl.item.rights.editable) || ($par && canEditPar(this.viewctrl.item, $par)));
        if (this.viewctrl.editing) {
            return [
                {func: () => this.goToEditor(), desc: "Go to editor", show: true},
                {func: (e: Event, par: Paragraph) => this.closeAndSave(e, par), desc: "Close editor and save", show: true},
                {func: (e: Event, par: Paragraph) => this.closeWithoutSaving(e, par), desc: "Close editor and cancel", show: true},
                {func: empty, desc: "Close menu", show: true},
            ];
        } else if (this.viewctrl.selection.start != null && $window.editMode) {
            return [
                {
                    func: (e: Event, par: Paragraph) => this.beginAreaEditing(e, par),
                    desc: "Edit area",
                    show: true,
                },
                {func: (e: Event, par: Paragraph) => this.viewctrl.areaHandler.nameArea(e, par), desc: "Name area", show: true},
                {func: (e: Event, par: Paragraph) => this.viewctrl.clipboardHandler.cutArea(e, par), desc: "Cut area", show: parEditable},
                {func: (e: Event, par: Paragraph) => this.viewctrl.clipboardHandler.copyArea(e, par), desc: "Copy area", show: true},
                {func: (e: Event, par: Paragraph) => this.viewctrl.areaHandler.cancelArea(), desc: "Cancel area", show: true},
                {func: empty, desc: "Close menu", show: true},
            ];
        } else {
            return [
                {
                    func: (e: Event, par: Paragraph) => this.viewctrl.notesHandler.showNoteWindow(e, par),
                    desc: "Comment/note",
                    show: this.viewctrl.item.rights.can_comment,
                },
                {func: (e: Event, par: Paragraph) => this.showEditWindow(e, par), desc: "Edit", show: parEditable},
                {
                    func: (e: Event, par: Paragraph) => this.viewctrl.clipboardHandler.cutPar(e, par),
                    desc: "Cut paragraph",
                    show: $window.editMode === "par" && parEditable,
                },
                {
                    func: (e: Event, par: Paragraph) => this.viewctrl.clipboardHandler.copyPar(e, par),
                    desc: "Copy paragraph",
                    show: $window.editMode !== "area",
                },
                //{func: (e, par) => this.cutArea(e, par), desc: 'Cut area', show: $window.editMode === 'area'},
                //{func: (e, par) => this.copyArea(e, par), desc: 'Copy area', show: $window.editMode === 'area'},
                {
                    func: (e: Event, par: Paragraph) => this.viewctrl.clipboardHandler.showPasteMenu(e, par),
                    desc: "Paste...",
                    show: $window.editMode && (this.viewctrl.allowPasteRef || this.viewctrl.allowPasteContent),
                },
                {func: (e: Event, par: Paragraph) => this.viewctrl.clipboardHandler.showMoveMenu(e, par), desc: "Move here...", show: $window.allowMove},
                {
                    func: (e: Event, par: Paragraph) => this.viewctrl.areaHandler.removeAreaMarking(e, par),
                    desc: "Remove area marking",
                    show: $window.editMode === "area",
                },
                {
                    func: (e: Event, par: Paragraph) => this.showAddParagraphAbove(e, par),
                    desc: "Add paragraph above",
                    show: this.viewctrl.item.rights.editable,
                },
                {
                    func: (e: Event, par: Paragraph) => this.viewctrl.questionHandler.addQuestionQst(e, par),
                    desc: "Add question above",
                    show: this.viewctrl.lectureMode && this.viewctrl.item.rights.editable,
                },
                {
                    func: (e: Event, par: Paragraph) => this.viewctrl.questionHandler.editQst(e, par),
                    desc: "Edit question",
                    show: this.viewctrl.lectureMode && parEditable,
                },
                {
                    func: (e: Event, par: Paragraph) => this.viewctrl.questionHandler.addQuestion(e, par),
                    desc: "Create lecture question",
                    show: this.viewctrl.lectureMode && this.viewctrl.item.rights.editable,
                },
                {
                    func: (e: Event, par: Paragraph) => this.viewctrl.areaHandler.startArea(e, par),
                    desc: "Start selecting area",
                    show: $window.editMode === "par" && this.viewctrl.selection.start == null,
                },
                {func: empty, desc: "Close menu", show: true},
            ];
        }
    }

    showAddParagraphMenu(e: Event, $parOrArea: JQuery, coords: Coords) {
        this.viewctrl.parmenuHandler.showPopupMenu(e, $parOrArea, coords, {actions: viewCtrlDot("addParagraphFunctions"), save: false});
    }

    getAddParagraphFunctions() {
        return [
            {func: (e: Event, $par: Paragraph) => this.showAddParagraphAbove(e, $par), desc: "Above", show: true},
            {func: (e: Event, $par: Paragraph) => this.showAddParagraphBelow(e, $par), desc: "Below", show: true},
            {func: empty, desc: "Cancel", show: true},
        ];
    }

    removeDefaultPars() {
        getElementByParId("HELP_PAR").remove();
    }

    extendSelection($par: Paragraph, allowShrink = false) {
        if (this.viewctrl.selection.start == null) {
            this.viewctrl.selection.start = $par;
            this.viewctrl.selection.end = $par;
        } else if (this.viewctrl.selection.pars) {
            const n = this.viewctrl.selection.pars.length;
            const startIndex = getParIndex(this.viewctrl.selection.pars.eq(0));
            const endIndex = getParIndex(this.viewctrl.selection.pars.eq(n - 1));
            const newIndex = getParIndex($par);
            if (endIndex == null || startIndex == null || newIndex == null) {
                return;
            }
            const areaLength = endIndex - startIndex + 1;
            if (newIndex < startIndex) {
                this.viewctrl.selection.start = $par;
            } else if (newIndex > endIndex) {
                this.viewctrl.selection.end = $par;
            } else if (allowShrink && areaLength > 1 && newIndex === startIndex) {
                this.viewctrl.selection.start = $(this.viewctrl.selection.pars[1]);
            } else if (allowShrink && areaLength > 1 && newIndex === endIndex) {
                this.viewctrl.selection.end = $(this.viewctrl.selection.pars[n - 2]);
            }
        }
    }
}
