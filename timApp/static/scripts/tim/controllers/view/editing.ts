import angular, {IScope} from "angular";
import $ from "jquery";
import {Coords, markPageDirty} from "tim/utils";
import {IExtraData, IParResponse} from "../../edittypes";
import {$compile, $http, $timeout, $window} from "../../ngimport";
import {ParCompiler} from "../../services/parCompiler";
import {empty} from "../../utils";
import {onClick} from "./eventhandlers";
import {INoteEditorOptions} from "./notes";
import {
    canEditPar,
    createNewPar,
    EDITOR_CLASS,
    EDITOR_CLASS_DOT,
    getElementByParId,
    getElementByRefId,
    getFirstPar,
    getLastParId,
    getNextPar,
    getParAttributes,
    getParId,
    getParIndex,
    getPars,
    getRefAttrs,
    isPreamble,
    isReference,
    Paragraph,
} from "./parhelpers";
import {ViewCtrl} from "./viewctrl";
import {editingDot, viewCtrlDot} from "./viewutils";

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
    $(".actionButtons").remove();
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
                $(".actionButtons").remove();
                //var $par = $('.par').last();
                //return this.showAddParagraphBelow(e, $par);
                return this.showAddParagraphAbove(e, $(".addBottomContainer"));
            });

            onClick(".addAbove", ($this, e) => {
                const [par, options] = prepareOptions($this[0], "addAbove");
                return this.showAddParagraphAbove(e, par, options);
            });

            onClick(".addBelow", ($this, e) => {
                const [par, options] = prepareOptions($this[0], "addBelow");
                return this.showAddParagraphBelow(e, par, options);
            });

            onClick(".pasteBottom", ($this, e) => {
                $(".actionButtons").remove();
                this.viewctrl.clipboardHandler.pasteAbove(e, $(".addBottomContainer"), false);
            });

            onClick(".pasteRefBottom", ($this, e) => {
                $(".actionButtons").remove();
                this.viewctrl.clipboardHandler.pasteAbove(e, $(".addBottomContainer"), true);
            });
        }
    }

    toggleParEditor($pars: JQuery, options: IParEditorOptions) {
        const $par = getFirstPar($pars);
        if (!$par) {
            return;
        }
        let areaStart;
        let areaEnd;
        let caption = "Add paragraph";
        const touch = typeof ("ontouchstart" in window || navigator.msMaxTouchPoints) !== "undefined";
        const mobile = touch && (window.screen.width < 1200);
        let url;
        const parId = getParId($par);
        let parNextId = getParId(getNextPar($par));
        if (parNextId === "HELP_PAR") {
            parNextId = undefined;
        }

        if ($pars.length > 1) {
            areaStart = getParId($par);
            areaEnd = getLastParId($pars);
            url = "/postParagraph/";
            options.showDelete = true;
        } else {
            // TODO: Use same route (postParagraph) for both cases, determine logic based on given parameters
            if (parId === "HELP_PAR" || $pars.hasClass("new")) {
                url = "/newParagraph/";
                options.showDelete = false;
            } else {
                url = "/postParagraph/";
                options.showDelete = true;
            }

            areaStart = options.area ? getParId(this.viewctrl.selection.start) : null;
            areaEnd = options.area ? getParId(this.viewctrl.selection.end) : null;
        }

        if (options.area) {
            areaEnd = getParId(this.viewctrl.selection.end);
            areaStart = getParId(this.viewctrl.selection.start);
        } else {
            areaStart = null;
            areaEnd = null;
        }

        const tags = {markread: false};
        const markread = $window.localStorage.getItem("markread") || false;
        tags.markread = markread === "true";

        const attrs = {
            "save-url": url,
            "extra-data": {
                docId: this.viewctrl.docId, // current document id
                par: parId, // the id of paragraph on which the editor was opened
                par_next: parNextId || null, // the id of the paragraph that follows par or null if par is the last one
                area_start: areaStart,
                area_end: areaEnd,
                forced_classes: options.forcedClasses,
                tags,
            },
            "options": {
                localSaveTag: options.localSaveTag ? options.localSaveTag : "par",
                texts: options.texts,
                showDelete: options.showDelete,
                showImageUpload: true,
                showPlugins: true,
                destroyAfterSave: true,
                touchDevice: mobile,
                tags: [
                    {name: "markread", desc: "Mark as read"},
                ],
            },
            "after-save": `${editingDot("addSavedParToDom")}(saveData, extraData)`,
            "after-cancel": `${editingDot("handleCancel")}(extraData)`,
            "after-delete": `${editingDot("handleDelete")}(saveData, extraData)`,
            "preview-url": "/preview/" + this.viewctrl.docId,
            "delete-url": "/deleteParagraph/" + this.viewctrl.docId,
            "unread-url": "/unread/" + this.viewctrl.docId,
            "initial-text-url": options.showDelete && parId !== "HELP_PAR" ? "/getBlock/" + this.viewctrl.docId + "/" + parId : null,
        };
        if (options.showDelete) {
            caption = "Edit paragraph";
        }
        this.toggleEditor($par, options, attrs, caption, "pareditor");
    }

    toggleEditor($par: Paragraph, options: IParEditorOptions | INoteEditorOptions, attrs: IParEditorAttrs, caption: string, directive: string) {
        if (isReference($par)) {
            angular.extend(attrs["extra-data"], getRefAttrs($par));
        }
        Object.keys(attrs).forEach((key, index) => {
            if (typeof attrs[key] === "object" && attrs[key] != null) {
                attrs[key] = JSON.stringify(attrs[key]);
            }
        });
        if ($par.children(EDITOR_CLASS_DOT).length) {
            $par.children().remove(EDITOR_CLASS_DOT);
            this.viewctrl.editing = false;
        } else {
            $(EDITOR_CLASS_DOT).remove();

            const createEditor = (attrs: any) => {
                const $div = $("<" + directive + ">");
                $div.attr(attrs);
                const draggable = $("<div class='editorArea' tim-draggable-fixed>");
                if (caption) {
                    draggable.attr("caption", caption);
                }
                draggable.append($div);
                $par.append(draggable);
                $compile(draggable[0])(this.sc);
                this.viewctrl.editing = true;
                $timeout(() => {
                    this.goToEditor();
                }, 0);
            };

            if (options.showDelete) {
                $(".par.new").remove();
            }
            createEditor(attrs);
        }
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
            $first.before(createNewPar());
            const parToReplace = "NEW_PAR";
            try {
                var response = await
                    $http.post<IParResponse>("/newParagraph/", {
                        text: '``` {settings=""}\nexample:\n```',
                        docId: this.viewctrl.docId,
                        par_next: parNext || null,
                    });
            } catch (e) {
                $window.alert(e.data.error);
                return;
            }
            this.addSavedParToDom(response.data, {par: parToReplace});
            this.editSettingsPars(true);
        } else if (pars.length === 1) {
            this.toggleParEditor($(pars[0]), {area: false});
        } else {
            const start = pars[0];
            const end = pars[pars.length - 1];
            this.viewctrl.selection.start = $(start);
            this.viewctrl.selection.end = $(end);
            $(pars).addClass("selected");
            this.toggleParEditor($(pars), {area: true});
            $(pars).removeClass("selected");
            this.viewctrl.areaHandler.cancelArea();
        }
    }

    showEditWindow(e: Event, $par: Paragraph) {
        $(".par.new").remove();
        this.toggleParEditor($par, {area: false});
    }

    beginAreaEditing(e: Event, $par: Paragraph) {
        $(".par.new").remove();
        this.toggleParEditor($par, {area: true});
    }

    handleCancel(extraData: IExtraData) {
        const $par = getElementByParId(extraData.par);
        if ($par.hasClass("new")) {
            $par.remove();
        }
        this.viewctrl.editing = false;
    }

    handleDelete(data: IParResponse, extraData: IExtraData) {
        let $par = getElementByParId(extraData.par);
        if (extraData.area_start && extraData.area_end) {
            $par = getElementByParId(extraData.area_start);
            const $endpar = getElementByParId(extraData.area_end);
            if (extraData.area_start !== extraData.area_end) {
                $par.nextUntil($endpar).add($endpar).remove();
            }
        }
        $par.remove();
        this.viewctrl.editing = false;
        this.viewctrl.areaHandler.cancelArea();
        this.viewctrl.beginUpdate();
    }

    showAddParagraphAbove(e: Event, $par: Paragraph, options: IParEditorOptions = {}) {
        const $newpar = createNewPar();
        $par.before($newpar);
        options.area = false;
        this.toggleParEditor($newpar, options);
    }

    showAddParagraphBelow(e: Event, $par: Paragraph, options: IParEditorOptions = {}) {
        const $newpar = createNewPar();
        $par.after($newpar);
        options.area = false;
        this.toggleParEditor($newpar, options);
    }

    addSavedParToDom(data: IParResponse, extraData: IExtraData) {
        let $par: JQuery;
        const refId = extraData["ref-id"];
        if (refId) {
            $par = getElementByRefId(refId);
        } else {
            $par = getElementByParId(extraData.par);
        }

        // check if we were editing an area
        if (extraData.area_start && extraData.area_end) {
            $par = getElementByParId(extraData.area_start);

            // remove all but the first element of the area because it'll be used
            // when replacing
            const $endpar = getElementByParId(extraData.area_end);
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
        // const $newPars = $($compile($par)(this.sc.$new(true, this.viewctrl.reviewCtrlScope)));
        if ($window.editMode === "area") {
            $newPars.find(".editline").removeClass("editline").addClass("editline-disabled");
        }

        $par.replaceWith($newPars);
        ParCompiler.processAllMathDelayed($newPars);
        this.viewctrl.docVersion = data.version;
        this.viewctrl.editing = false;
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
        const parEditable = (!$par || canEditPar(this.viewctrl.item, $par));
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
