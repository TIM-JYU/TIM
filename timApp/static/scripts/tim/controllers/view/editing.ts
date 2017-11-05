import angular, {IScope} from "angular";
import $ from "jquery";
import {$compile, $http, $timeout, $window} from "../../ngimport";
import {
    getPars, getParIndex, isReference, getElementByRefId, getElementByParId, getRefAttrs, getNextPar,
    getLastParId,
    getFirstPar, getParAttributes, getParId, EDITOR_CLASS_DOT, EDITOR_CLASS, createNewPar, isPreamble,
} from "./parhelpers";
import {markPageDirty} from "tim/utils";
import {onClick} from "./eventhandlers";
import {ParCompiler} from "../../services/parCompiler";
import {ViewCtrl} from "./ViewCtrl";

function prepareOptions($this: Element, saveTag: string): [JQuery, {}] {
    $(".actionButtons").remove();
    // var $par = $('.par').last();
    // return sc.showAddParagraphBelow(e, $par);
    // return sc.showAddParagraphAbove(e, sc.$pars);
    var par = $($this).closest('.par');
    var text = par.find('pre').text();
    // text = text.replace('⁞', '');  // TODO: set cursor to | position
    let forcedClasses = [];
    const forceAttr = getParAttributes(par).forceclass;
    if (forceAttr) {
        forcedClasses = forceAttr.split(" ");
    }
    var options = {
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
export function wrapText(s, n)
{
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
                if ( i+1 < lines.length && (lines[i+1].length  > 0 && (" 0123456789-".indexOf(lines[i+1][0]) < 0 )  ) ) {
                    lines[i+1] = line + " " + lines[i+1];
                    needJoin = true;
                    break;
                }
                sep = "\n";
                needJoin = true;
            }
        }
    }
    if ( needJoin ) return {modified: true, s: lines.join("\n")};
    return {modified: false, s:s};
}

export class EditingHandler {
    public viewctrl: ViewCtrl;
    public sc: IScope;

    initEditing(sc: IScope, view: ViewCtrl) {
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
            if (this.viewctrl.selection.start !== null) {
                const $start = this.viewctrl.selection.start;
                if (this.viewctrl.selection.end !== null &&
                    !this.viewctrl.selection.end.is(this.viewctrl.selection.start)) {
                    const $end = this.viewctrl.selection.end;
                    this.viewctrl.selection.pars = getPars($start, $end);
                } else {
                    this.viewctrl.selection.pars = $start;
                }
                this.viewctrl.selection.pars.addClass("marked");
            }
        });

        if (this.viewctrl.item.rights.editable) {
            onClick(".addBottom", ($this, e) => {
                $(".actionButtons").remove();
                //var $par = $('.par').last();
                //return this.showAddParagraphBelow(e, $par);
                return this.showAddParagraphAbove(e, $(".addBottomContainer"));
            });

            onClick(".addAbove", function($this, e) {
                const [par, options] = prepareOptions($this[0], "addAbove");
                return this.showAddParagraphAbove(e, par, options);
            });

            onClick(".addBelow", function($this, e) {
                const [par, options] = prepareOptions($this[0], "addBelow");
                return this.showAddParagraphBelow(e, par, options);
            });

            onClick(".pasteBottom", ($this, e) => {
                $(".actionButtons").remove();
                this.viewctrl.pasteAbove(e, $(".addBottomContainer"), false);
            });

            onClick(".pasteRefBottom", ($this, e) => {
                $(".actionButtons").remove();
                this.viewctrl.pasteAbove(e, $(".addBottomContainer"), true);
            });
        }
    }

    toggleParEditor($pars, options) {
        const $par = getFirstPar($pars);
        let areaStart;
        let areaEnd;
        let caption = "Add paragraph";
        const touch = typeof ("ontouchstart" in window || navigator.msMaxTouchPoints) !== "undefined";
        const mobile = touch && (window.screen.width < 1200);
        let url;
        const parId = getParId($par);
        let parNextId = getParId(getNextPar($par));
        if (parNextId === "HELP_PAR") {
            parNextId = null;
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
                par_next: parNextId, // the id of the paragraph that follows par or null if par is the last one
                area_start: areaStart,
                area_end: areaEnd,
                forced_classes: options.forcedClasses,
                tags,
            },
            "options": {
                localSaveTag: options.localSaveTag ? options.localSaveTag : "par" ,
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
            "after-save": "$ctrl.addSavedParToDom(saveData, extraData)",
            "after-cancel": "$ctrl.handleCancel(extraData)",
            "after-delete": "$ctrl.handleDelete(saveData, extraData)",
            "preview-url": "/preview/" + this.viewctrl.docId,
            "delete-url": "/deleteParagraph/" + this.viewctrl.docId,
            "unread-url": "/unread/" + this.viewctrl.docId,
        };
        if (options.showDelete) {
            caption = "Edit paragraph";
            if (parId !== "HELP_PAR") {
                attrs["initial-text-url"] = "/getBlock/" + this.viewctrl.docId + "/" + parId;
            }
        }
        this.toggleEditor($par, options, attrs, caption, "pareditor");
    }

    toggleEditor($par, options, attrs: object, caption, directive) {
        if (isReference($par)) {
            angular.extend(attrs["extra-data"], getRefAttrs($par));
        }
        Object.keys(attrs).forEach((key, index) => {
            if (typeof attrs[key] === "object" && attrs[key] !== null) {
                attrs[key] = JSON.stringify(attrs[key]);
            }
        });
        if ($par.children(EDITOR_CLASS_DOT).length) {
            $par.children().remove(EDITOR_CLASS_DOT);
            this.viewctrl.editing = false;
        } else {
            $(EDITOR_CLASS_DOT).remove();

            const createEditor = (attrs) => {
                const $div = $("<" + directive + ">", {class: EDITOR_CLASS});
                $div.attr(attrs);
                $div.attr("tim-draggable-fixed", "");
                if (caption) {
                    $div.attr("caption", caption);
                }
                $par.append($div);
                $compile($div[0])(this.sc);
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

    async editSettingsPars(recursiveCall) {
        const pars = [];
        $(".par").each((index, elem) => {
            if (isPreamble($(elem))) {
                return;
            }
            if (getParAttributes($(elem)).hasOwnProperty("settings")) {
                pars.push(this);
            }
        });
        if (pars.length === 0) {
            if (recursiveCall) {
                throw new Error("Faulty recursion stopped, there should be a settings paragraph already");
            }
            const $first = $(".par:not(.preamble):first");
            let parNext = getParId($first);
            if (parNext === "HELP_PAR") {
                parNext = null;
            }
            $first.before(createNewPar());
            const parToReplace = "NEW_PAR";
            try {
                var response = await
                    $http.post("/newParagraph/", {
                        text: '``` {settings=""}\nexample:\n```',
                        docId: this.viewctrl.docId,
                        par_next: parNext,
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
            this.viewctrl.cancelArea();
        }
    }

    showEditWindow(e, $par) {
        $(".par.new").remove();
        this.toggleParEditor($par, {area: false});
    }

    beginAreaEditing(e, $par) {
        $(".par.new").remove();
        this.toggleParEditor($par, {area: true});
    }

    handleCancel(extraData) {
        const $par = getElementByParId(extraData.par);
        if ($par.hasClass("new")) {
            $par.remove();
        }
        this.viewctrl.editing = false;
    }

    handleDelete(data, extraData) {
        let $par = getElementByParId(extraData.par);
        if (extraData.area_start !== null && extraData.area_end !== null) {
            $par = getElementByParId(extraData.area_start);
            const $endpar = getElementByParId(extraData.area_end);
            if (extraData.area_start !== extraData.area_end) {
                $par.nextUntil($endpar).add($endpar).remove();
            }
        }
        $par.remove();
        this.viewctrl.editing = false;
        this.viewctrl.cancelArea();
        this.viewctrl.beginUpdate();
    }

    showAddParagraphAbove(e, $par, options = null) {
        const $newpar = createNewPar();
        $par.before($newpar);
        if ( options == null ) options = {};
        options.area = false;
        this.toggleParEditor($newpar, options);
    }

    showAddParagraphBelow(e, $par, options = null) {
        const $newpar = createNewPar();
        $par.after($newpar);
        if ( options == null ) options = {};
        options.area = false;
        this.toggleParEditor($newpar, options);
    }

    addSavedParToDom(data, extraData) {
        let $par: JQuery;
        if (angular.isDefined(extraData["ref-id"])) {
            $par = getElementByRefId(extraData["ref-id"]);
        } else {
            $par = getElementByParId(extraData.par);
        }

        // check if we were editing an area
        if (angular.isDefined(extraData.area_start) &&
            angular.isDefined(extraData.area_start) &&
            extraData.area_start !== null &&
            extraData.area_end !== null) {
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
        this.viewctrl.cancelArea();
        this.removeDefaultPars();
        markPageDirty();
        this.viewctrl.beginUpdate();
    }

    goToEditor() {
        $("pareditor")[0].scrollIntoView();
    }

    closeAndSave(e, $par) {
        $("pareditor").isolateScope<any>().saveClicked();
        this.viewctrl.showOptionsWindow(e, $par);
    }

    closeWithoutSaving(e, $par) {
        $("pareditor").isolateScope<any>().cancelClicked();
        this.viewctrl.showOptionsWindow(e, $par);
    }

    getEditorFunctions() {
        if (this.viewctrl.editing) {
            return [
                {func: () => this.goToEditor(), desc: "Go to editor", show: true},
                {func: (e, par) => this.closeAndSave(e, par), desc: "Close editor and save", show: true},
                {func: (e, par) => this.closeWithoutSaving(e, par), desc: "Close editor and cancel", show: true},
                {func: () => this.viewctrl.nothing(), desc: "Close menu", show: true},
            ];
        } else if (this.viewctrl.selection.start !== null && $window.editMode) {
            return [
                {
                    func: (e, par) => this.beginAreaEditing(e, par),
                    desc: "Edit area",
                    show: true,
                },
                {func: (e, par) => this.viewctrl.nameArea(e, par), desc: "Name area", show: true},
                {func: (e, par) => this.viewctrl.cutArea(e, par), desc: "Cut area", show: true},
                {func: (e, par) => this.viewctrl.copyArea(e, par), desc: "Copy area", show: true},
                {func: (e, par) => this.viewctrl.cancelArea(), desc: "Cancel area", show: true},
                {func: (e, par) => this.viewctrl.nothing(), desc: "Close menu", show: true},
            ];
        } else {
            return [
                {
                    func: (e, par) => this.viewctrl.showNoteWindow(e, par),
                    desc: "Comment/note",
                    show: this.viewctrl.item.rights.can_comment,
                },
                {func: (e, par) => this.showEditWindow(e, par), desc: "Edit", show: this.viewctrl.item.rights.editable},
                {
                    func: (e, par) => this.viewctrl.cutPar(e, par),
                    desc: "Cut paragraph",
                    show: $window.editMode === "par",
                },
                {
                    func: (e, par) => this.viewctrl.copyPar(e, par),
                    desc: "Copy paragraph",
                    show: $window.editMode !== "area",
                },
                //{func: (e, par) => this.cutArea(e, par), desc: 'Cut area', show: $window.editMode === 'area'},
                //{func: (e, par) => this.copyArea(e, par), desc: 'Copy area', show: $window.editMode === 'area'},
                {
                    func: (e, par) => this.viewctrl.showPasteMenu(e, par),
                    desc: "Paste...",
                    show: $window.editMode && (this.viewctrl.allowPasteRef || this.viewctrl.allowPasteContent),
                },
                {func: (e, par) => this.viewctrl.showMoveMenu(e, par), desc: "Move here...", show: $window.allowMove},
                {
                    func: (e, par) => this.viewctrl.removeAreaMarking(e, par),
                    desc: "Remove area marking",
                    show: $window.editMode === "area",
                },
                {
                    func: (e, par) => this.showAddParagraphAbove(e, par),
                    desc: "Add paragraph above",
                    show: this.viewctrl.item.rights.editable,
                },
                {
                    func: (e, par) => this.viewctrl.addQuestionQst(e, par),
                    desc: "Add question above",
                    show: this.viewctrl.lectureMode && this.viewctrl.item.rights.editable,
                },
                {
                    func: (e, par) => this.viewctrl.editQst(e, par),
                    desc: "Edit question",
                    show: (e, par) => this.viewctrl.lectureMode && this.viewctrl.item.rights.editable,
                },
                {
                    func: (e, par) => this.viewctrl.addQuestion(e, par),
                    desc: "Create lecture question",
                    show: this.viewctrl.lectureMode && this.viewctrl.item.rights.editable,
                },
                {
                    func: (e, par) => this.viewctrl.startArea(e, par),
                    desc: "Start selecting area",
                    show: $window.editMode === "par" && this.viewctrl.selection.start === null,
                },
                {func: (e, par) => this.viewctrl.nothing(), desc: "Close menu", show: true},
            ];
        }
    }

    showAddParagraphMenu(e, $parOrArea, coords) {
        this.viewctrl.showPopupMenu(e, $parOrArea, coords, {actions: "$ctrl.addParagraphFunctions"});
    }

    getAddParagraphFunctions() {
        return [
            {func: (e, $par) => this.showAddParagraphAbove(e, $par), desc: "Above", show: true},
            {func: (e, $par) => this.showAddParagraphBelow(e, $par), desc: "Below", show: true},
            {func: (e, $par) => this.viewctrl.nothing(), desc: "Cancel", show: true},
        ];
    }

    removeDefaultPars() {
        getElementByParId("HELP_PAR").remove();
    }

    extendSelection($par, allowShrink) {
        if (this.viewctrl.selection.start === null) {
            this.viewctrl.selection.start = $par;
            this.viewctrl.selection.end = $par;
        } else {
            const n = this.viewctrl.selection.pars.length;
            const startIndex = getParIndex(this.viewctrl.selection.pars.eq(0));
            const endIndex = getParIndex(this.viewctrl.selection.pars.eq(n - 1));
            const areaLength = endIndex - startIndex + 1;
            const newIndex = getParIndex($par);

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
