import $ from "jquery";
import {$compile, $log, $timeout, $window} from "../../ngimport";
import {dist} from "../../utils";
import {onClick} from "./eventhandlers";
import {getPreambleDocId, isActionablePar, isPreamble} from "./parhelpers";
import {showDialog} from "../../dialog";

export function closeOptionsWindow() {
    const $actionButtons = $(".actionButtons");
    const $parOrArea = $actionButtons.parent();
    $actionButtons.remove();
    optionsWindowClosed($parOrArea);
}

export function optionsWindowClosed($parOrArea) {
    const $editline = $(".menuopen");
    $editline.removeClass("menuopen");
}

function selectionToStr(selection: JQuery) {
    return selection.toArray().map(function(e) {
        return "#" + e.id;
    }).join(",");
}

export function defineParMenu(sc) {
    sc.showPopupMenu = function(e, $pars, coords, attrs, $rootElement, editcontext) {
        if (!$rootElement) {
            $rootElement = $pars;
        }

        const $popup = $("<popup-menu>");
        $popup.attr("tim-draggable-fixed", "");
        $popup.attr("srcid", selectionToStr($pars));
        $popup.attr("editcontext", editcontext);
        for (const key in attrs) {
            if (attrs.hasOwnProperty(key)) {
                $popup.attr(key, attrs[key]);
            }
        }

        // todo: cache this value if needed
        if ($(".area").length > 0) {
            $popup.attr("areaeditbutton", "true");
        }

        $rootElement.prepend($popup); // need to prepend to DOM before compiling
        $compile($popup[0])(sc);
        // TODO: Set offset for the popup
        const element = $popup;
        const viewport: any = {};
        viewport.top = $(window).scrollTop();
        viewport.bottom = viewport.top + $(window).height();
        const bounds: any = {};
        bounds.top = element.offset().top;
        bounds.bottom = bounds.top + element.outerHeight();
        let y = $(window).scrollTop();
        if (bounds.bottom > viewport.bottom) {
            y += (bounds.bottom - viewport.bottom);
        } else if (bounds.top < viewport.top) {
            y += (bounds.top - viewport.top);
        }
        $("html, body").animate({
            scrollTop: y,
        }, 500);
    };

    onClick(".paragraphs .parContent", function($this, e) {
        if (sc.editing) {
            return false;
        }

        const $target = $(e.target);
        const tag = $target.prop("tagName");
        const $par = $this.parents(".par");
        if (!isActionablePar($par)) {
            return;
        }

        // Don't show paragraph menu on these specific tags or classes
        const ignoredTags = ["BUTTON", "INPUT", "TEXTAREA", "A", "QUESTIONADDEDNEW"];
        const ignoredClasses = ["no-popup-menu", "ace_editor"];
        const classSelector = ignoredClasses.map(function(c) {
            return "." + c;
        }).join(",");
        if (ignoredTags.indexOf(tag) > -1 || $target.parents(classSelector).length > 0) {
            return false;
        }

        if (sc.selection.start !== null) {
            sc.extendSelection($par, true);
        } else {
            const coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top};
            const toggle1 = $par.find(".actionButtons").length === 0;
            const toggle2 = $par.hasClass("lightselect");

            $(".par.selected").removeClass("selected");
            $(".par.lightselect").removeClass("lightselect");
            closeOptionsWindow();
            sc.toggleActionButtons(e, $par, toggle1, toggle2, coords);
        }
        sc.$apply();
        return true;
    }, true);

    sc.toggleActionButtons = function(e, $par, toggle1, toggle2, coords) {
        if (!sc.item.rights.editable && !sc.item.rights.can_comment) {
            return;
        }

        if (toggle2) {
            // Clicked twice successively
            const clicktime = new Date().getTime() - sc.lastclicktime;
            const clickdelta = dist(coords, sc.lastclickplace);
            $par.addClass("selected");

            if (clickdelta > 10) {
                // Selecting text
                $par.removeClass("selected");
                $par.removeClass("lightselect");
            } else if (clicktime < 500 && sc.defaultAction !== null) {
                // Double click
                sc.defaultAction.func(e, $par, coords);
            } else {
                // Two clicks
                sc.showOptionsWindow(e, $par, coords);
            }
        } else if (toggle1) {
            // Clicked once
            $par.addClass("lightselect");
            sc.lastclicktime = new Date().getTime();
            sc.lastclickplace = coords;
        } else {
            $log.info("This line is new: " + $par);
            $par.children().remove(".actionButtons");
            $par.removeClass("selected");
            $par.removeClass("lightselect");
        }
    };

    sc.showOptionsWindow = function(e, $par, coords) {
        sc.updateClipboardStatus();
        $par.children(".editline").addClass("menuopen");
        sc.showPopupMenu(e, $par, coords, sc.popupMenuAttrs, null, "par");
    };

    sc.defaultAction = {func: sc.showOptionsWindow, desc: "Show options window"};

    sc.optionsWindowClosed = optionsWindowClosed;

    sc.updatePopupMenu = function() {
        sc.editorFunctions = sc.getEditorFunctions();
        if (sc.selection.start !== null && $window.editMode) {
            sc.popupMenuAttrs.save = null;
            sc.popupMenuAttrs.editbutton = false;
        } else {
            sc.popupMenuAttrs.save = "defaultAction";
            sc.popupMenuAttrs.editbutton = true;
        }
    };

    onClick(".editline", function($this, e) {
        closeOptionsWindow();
        const $par = $this.parent().filter(".par");
        if (isPreamble($par)) {
            showDialog(`
This paragraph is from a preamble document.
To comment or edit this, go to the corresponding <a href="/view/${getPreambleDocId($par)}">preamble document</a>.`);
        }
        if (!isActionablePar($par)) {
            return;
        }
        if (sc.selection.start !== null) {
            sc.extendSelection($par);
        }
        const coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top};

        // We need the timeout so we don't trigger the ng-clicks on the buttons
        $timeout(function() {
            sc.showOptionsWindow(e, $par, coords);
        }, 80);
        return false;
    }, true);

    sc.popupMenuAttrs = {actions: "editorFunctions", save: "defaultAction", onclose: "optionsWindowClosed"};
    sc.updatePopupMenu();

}
