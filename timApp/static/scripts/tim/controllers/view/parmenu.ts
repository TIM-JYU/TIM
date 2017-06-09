
import $ from "jquery";
export function defineParMenu(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
    "use strict";

    sc.selectionToStr = function(selection) {
        return selection.toArray().map(function(e) {
            return "#" + e.id;
        }).join(",");
    };

    sc.showPopupMenu = function(e, $pars, coords, attrs, $rootElement, editcontext) {
        if (!$rootElement)
            $rootElement = $pars;

        const $popup = $("<popup-menu>");
        $popup.attr("tim-draggable-fixed", "");
        $popup.attr("srcid", sc.selectionToStr($pars));
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
        }
        else if (bounds.top < viewport.top) {
            y += (bounds.top - viewport.top);
        }
        $("html, body").animate({
            scrollTop: y,
        }, 500);
    };

    sc.onClick(".paragraphs .parContent", function($this, e) {
        if (sc.editing) {
            return false;
        }

        const $target = $(e.target);
        const tag = $target.prop("tagName");
        const $par = $this.parents(".par");
        if ($par.parents(".previewcontent").length > 0) {
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
        }
        else {
            const coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top};
            const toggle1 = $par.find(".actionButtons").length === 0;
            const toggle2 = $par.hasClass("lightselect");

            $(".par.selected").removeClass("selected");
            $(".par.lightselect").removeClass("lightselect");
            sc.closeOptionsWindow();
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
            const clickdelta = sc.dist(coords, sc.lastclickplace);
            $par.addClass("selected");

            if (clickdelta > 10) {
                // Selecting text
                $par.removeClass("selected");
                $par.removeClass("lightselect");
            }
            else if (clicktime < 500 && sc.defaultAction !== null) {
                // Double click
                sc.defaultAction.func(e, $par, coords);
            }
            else {
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

    sc.closeOptionsWindow = function() {
        const $actionButtons = $(".actionButtons");
        const $par_or_area = $actionButtons.parent();
        $actionButtons.remove();
        sc.optionsWindowClosed($par_or_area);
    };

    sc.optionsWindowClosed = function($par_or_area) {
        const $editline = $(".menuopen");
        $editline.removeClass("menuopen");
    };

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

    sc.onClick(".editline", function($this, e) {
        sc.closeOptionsWindow();
        const $par = $this.parent().filter(".par");
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
