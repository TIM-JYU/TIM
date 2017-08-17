import angular from "angular";
import $ from "jquery";
import {$compile, $http, $timeout, $window} from "../../ngimport";
import {
    getPars, getParIndex, isReference, getElementByRefId, getElementByParId, getRefAttrs, getNextPar,
    getLastParId,
    getFirstPar, getParAttributes, getParId, EDITOR_CLASS_DOT, EDITOR_CLASS,
} from "./parhelpers";
import {markPageDirty} from "tim/utils";
import {onClick} from "./eventhandlers";
import {ParCompiler} from "../../services/parCompiler";

export function defineEditing(sc) {
    "use strict";

    sc.editing = false;

    sc.toggleParEditor = function($pars, options) {
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

            areaStart = options.area ? getParId(sc.selection.start) : null;
            areaEnd = options.area ? getParId(sc.selection.end) : null;
        }

        if (options.area) {
            areaEnd = getParId(sc.selection.end);
            areaStart = getParId(sc.selection.start);
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
                docId: sc.docId, // current document id
                par: parId, // the id of paragraph on which the editor was opened
                par_next: parNextId, // the id of the paragraph that follows par or null if par is the last one
                area_start: areaStart,
                area_end: areaEnd,
                tags,
            },
            "options": {
                localSaveTag: "par",
                showDelete: options.showDelete,
                showImageUpload: true,
                showPlugins: true,
                destroyAfterSave: true,
                touchDevice: mobile,
                tags: [
                    {name: "markread", desc: "Mark as read"},
                ],
            },
            "after-save": "addSavedParToDom(saveData, extraData)",
            "after-cancel": "handleCancel(extraData)",
            "after-delete": "handleDelete(saveData, extraData)",
            "preview-url": "/preview/" + sc.docId,
            "delete-url": "/deleteParagraph/" + sc.docId,
            "unread-url": "/unread/" + sc.docId,
        };
        if (options.showDelete) {
            caption = "Edit paragraph";
            if (parId !== "HELP_PAR") {
                attrs["initial-text-url"] = "/getBlock/" + sc.docId + "/" + parId;
            }
        }
        sc.toggleEditor($par, options, attrs, caption, "pareditor");
    };

    sc.toggleEditor = function($par, options, attrs: object, caption, directive) {
        if (isReference($par)) {
            angular.extend(attrs["extra-data"], getRefAttrs($par));
        }
        Object.keys(attrs).forEach(function(key, index) {
            if (typeof attrs[key] === "object" && attrs[key] !== null) {
                attrs[key] = JSON.stringify(attrs[key]);
            }
        });
        if ($par.children(EDITOR_CLASS_DOT).length) {
            $par.children().remove(EDITOR_CLASS_DOT);
            sc.editing = false;
        } else {
            $(EDITOR_CLASS_DOT).remove();

            const createEditor = function(attrs: object) {
                const $div = $("<" + directive + ">", {class: EDITOR_CLASS}).attr(attrs);
                $div.attr("tim-draggable-fixed", "");
                if (caption) {
                    $div.attr("caption", caption);
                }
                $par.append($div);
                $compile($div[0])(sc);
                sc.editing = true;
                $timeout(function() {
                    sc.goToEditor();
                }, 0);
            };

            if (options.showDelete) {
                $(".par.new").remove();
            }
            createEditor(attrs);
        }
    };

    sc.editSettingsPars = async function(recursiveCall) {
        const pars = [];
        $(".par").each(function() {
            if (getParAttributes($(this)).hasOwnProperty("settings")) {
                pars.push(this);
            }
        });
        if (pars.length === 0) {
            if (recursiveCall) {
                throw new Error("Faulty recursion stopped, there should be a settings paragraph already");
            }
            const $first = $(".par:first");
            let parNext = getParId($first);
            if (parNext === "HELP_PAR") {
                parNext = null;
            }
            $first.before(sc.createNewPar());
            const parToReplace = "NEW_PAR";
            try {
                var response = await $http.post("/newParagraph/", {
                    text: '``` {settings=""}\nexample:\n```',
                    docId: sc.docId,
                    par_next: parNext,
                });
            } catch (e) {
                $window.alert(e.data.error);
                return;
            }
            sc.addSavedParToDom(response.data, {par: parToReplace});
            sc.editSettingsPars(true);
        } else if (pars.length === 1) {
            sc.toggleParEditor($(pars[0]), {area: false});
        } else {
            const start = pars[0];
            const end = pars[pars.length - 1];
            sc.selection.start = $(start);
            sc.selection.end = $(end);
            $(pars).addClass("selected");
            sc.toggleParEditor($(pars), {area: true});
            $(pars).removeClass("selected");
            sc.cancelArea();
        }
    };

    sc.showEditWindow = function(e, $par) {
        $(".par.new").remove();
        sc.toggleParEditor($par, {area: false});
    };

    sc.beginAreaEditing = function(e, $par) {
        $(".par.new").remove();
        sc.toggleParEditor($par, {area: true});
    };

    sc.createNewPar = function() {
        return $("<div>", {class: "par new", id: "NEW_PAR", attrs: "{}"})
            .append($("<div>", {class: "parContent"}).html("New paragraph"));
    };

    sc.handleCancel = function(extraData) {
        const $par = getElementByParId(extraData.par);
        if ($par.hasClass("new")) {
            $par.remove();
        }
        sc.editing = false;
    };

    sc.handleDelete = function(data, extraData) {
        let $par = getElementByParId(extraData.par);
        if (extraData.area_start !== null && extraData.area_end !== null) {
            $par = getElementByParId(extraData.area_start);
            const $endpar = getElementByParId(extraData.area_end);
            if (extraData.area_start !== extraData.area_end) {
                $par.nextUntil($endpar).add($endpar).remove();
            }
        }
        $par.remove();
        sc.editing = false;
        sc.cancelArea();
        sc.beginUpdate();
    };

    sc.showAddParagraphAbove = function(e, $par) {
        const $newpar = sc.createNewPar();
        $par.before($newpar);
        sc.toggleParEditor($newpar, {area: false});
    };

    sc.showAddParagraphBelow = function(e, $par) {
        const $newpar = sc.createNewPar();
        $par.after($newpar);
        sc.toggleParEditor($newpar, {area: false});
    };

    sc.addSavedParToDom = function(data, extraData) {
        let $par;
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

        const $newPars = $($compile(data.texts)(sc));

        if ($window.editMode === "area") {
            $newPars.find(".editline").removeClass("editline").addClass("editline-disabled");
        }

        $par.replaceWith($newPars);
        ParCompiler.processAllMathDelayed($newPars);
        sc.docVersion = data.version;
        sc.editing = false;
        sc.cancelArea();
        sc.removeDefaultPars();
        markPageDirty();
        sc.beginUpdate();
    };

    sc.goToEditor = function(e, $par) {
        $("pareditor")[0].scrollIntoView();
    };

    sc.closeAndSave = function(e, $par) {
        $("pareditor").isolateScope<any>().saveClicked();
        sc.showOptionsWindow(e, $par);
    };

    sc.closeWithoutSaving = function(e, $par) {
        $("pareditor").isolateScope<any>().cancelClicked();
        sc.showOptionsWindow(e, $par);
    };

    sc.getEditorFunctions = function() {
        if (sc.editing) {
            return [
                {func: sc.goToEditor, desc: "Go to editor", show: true},
                {func: sc.closeAndSave, desc: "Close editor and save", show: true},
                {func: sc.closeWithoutSaving, desc: "Close editor and cancel", show: true},
                {func: sc.nothing, desc: "Close menu", show: true},
            ];
        } else if (sc.selection.start !== null && $window.editMode) {
            return [
                {
                    func: sc.beginAreaEditing,
                    desc: "Edit area",
                    show: true,
                },
                {func: sc.nameArea, desc: "Name area", show: true},
                {func: sc.cutArea, desc: "Cut area", show: true},
                {func: sc.copyArea, desc: "Copy area", show: true},
                {func: sc.cancelArea, desc: "Cancel area", show: true},
                {func: sc.nothing, desc: "Close menu", show: true},
            ];
        } else {
            return [
                {func: sc.showNoteWindow, desc: "Comment/note", show: sc.item.rights.can_comment},
                {func: sc.showEditWindow, desc: "Edit", show: sc.item.rights.editable},
                {func: sc.cutPar, desc: "Cut paragraph", show: $window.editMode === "par"},
                {func: sc.copyPar, desc: "Copy paragraph", show: $window.editMode !== "area"},
                //{func: sc.cutArea, desc: 'Cut area', show: $window.editMode === 'area'},
                //{func: sc.copyArea, desc: 'Copy area', show: $window.editMode === 'area'},
                {
                    func: sc.showPasteMenu,
                    desc: "Paste...",
                    show: $window.editMode && (sc.allowPasteRef || sc.allowPasteContent),
                },
                {func: sc.showMoveMenu, desc: "Move here...", show: $window.allowMove},
                {func: sc.removeAreaMarking, desc: "Remove area marking", show: $window.editMode === "area"},
                {func: sc.showAddParagraphAbove, desc: "Add paragraph above", show: sc.item.rights.editable},
                {func: sc.addQuestionQst, desc: "Add question above", show: sc.lectureMode && sc.item.rights.editable},
                {func: sc.editQst, desc: "Edit question", show: sc.lectureMode && sc.item.rights.editable},
                {
                    func: sc.addQuestion,
                    desc: "Create lecture question",
                    show: sc.lectureMode && sc.item.rights.editable,
                },
                {
                    func: sc.startArea,
                    desc: "Start selecting area",
                    show: $window.editMode === "par" && sc.selection.start === null,
                },
                {func: sc.nothing, desc: "Close menu", show: true},
            ];
        }
    };

    sc.showAddParagraphMenu = function(e, $parOrArea, coords) {
        sc.showPopupMenu(e, $parOrArea, coords, {actions: "addParagraphFunctions"});
    };

    sc.getAddParagraphFunctions = function() {
        return [
            {func: sc.showAddParagraphAbove, desc: "Above", show: true},
            {func: sc.showAddParagraphBelow, desc: "Below", show: true},
            {func: sc.nothing, desc: "Cancel", show: true},
        ];
    };

    sc.removeDefaultPars = function() {
        getElementByParId("HELP_PAR").remove();
    };

    sc.extendSelection = function($par, allowShrink) {
        if (sc.selection.start === null) {
            sc.selection.start = $par;
            sc.selection.end = $par;
        } else {
            const n = sc.selection.pars.length;
            const startIndex = getParIndex(sc.selection.pars.eq(0));
            const endIndex = getParIndex(sc.selection.pars.eq(n - 1));
            const areaLength = endIndex - startIndex + 1;
            const newIndex = getParIndex($par);

            if (newIndex < startIndex) {
                sc.selection.start = $par;
            } else if (newIndex > endIndex) {
                sc.selection.end = $par;
            } else if (allowShrink && areaLength > 1 && newIndex === startIndex) {
                sc.selection.start = $(sc.selection.pars[1]);
            } else if (allowShrink && areaLength > 1 && newIndex === endIndex) {
                sc.selection.end = $(sc.selection.pars[n - 2]);
            }
        }
    };

    if (sc.item.rights.editable) {
        onClick(".addBottom", function($this, e) {
            $(".actionButtons").remove();
            //var $par = $('.par').last();
            //return sc.showAddParagraphBelow(e, $par);
            return sc.showAddParagraphAbove(e, $(".addBottomContainer"));
        });

        onClick(".pasteBottom", function($this, e) {
            $(".actionButtons").remove();
            sc.pasteAbove(e, $(".addBottomContainer"), false);
        });

        onClick(".pasteRefBottom", function($this, e) {
            $(".actionButtons").remove();
            sc.pasteAbove(e, $(".addBottomContainer"), true);
        });
    }

    sc.addParagraphFunctions = sc.getAddParagraphFunctions();

    sc.selection = {start: null, end: null};

    sc.$watchGroup(["selection.start", "selection.end"], function(newValues, oldValues, scope) {
        $(".par.lightselect").removeClass("lightselect");
        $(".par.selected").removeClass("selected");
        $(".par.marked").removeClass("marked");
        if (sc.selection.start !== null) {
            const $start = sc.selection.start;
            if (sc.selection.end !== null && !sc.selection.end.is(sc.selection.start)) {
                const $end = sc.selection.end;
                sc.selection.pars = getPars($start, $end);
            } else {
                sc.selection.pars = $start;
            }
            sc.selection.pars.addClass("marked");
        }
    });
}
