import $ from "jquery";
import {$compile, $log, $timeout, $window} from "../../ngimport";
import {dist} from "../../utils";
import {onClick} from "./eventhandlers";
import {IScope} from "angular";
import {ViewCtrl} from "./ViewCtrl";

export function closeOptionsWindow() {
    const $actionButtons = $(".actionButtons");
    const $parOrArea = $actionButtons.parent();
    $actionButtons.remove();
    optionsWindowClosed($parOrArea);
}

export function optionsWindowClosed($parOrArea?) {
    const $editline = $(".menuopen");
    $editline.removeClass("menuopen");
}

function selectionToStr(selection: JQuery) {
    return selection.toArray().map((e) => "#" + e.id).join(",");
}

export class ParmenuHandler {
    public sc: IScope;
    public viewctrl: ViewCtrl;
    public lastclicktime: number;
    public lastclickplace: {left, top};

    initParMenu(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        onClick(".paragraphs .parContent", ($this, e) => {
            if (this.viewctrl.editing) {
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
            const classSelector = ignoredClasses.map((c) => "." + c).join(",");
            if (ignoredTags.indexOf(tag) > -1 || $target.parents(classSelector).length > 0) {
                return false;
            }

            if (this.viewctrl.selection.start !== null) {
                this.viewctrl.extendSelection($par, true);
            } else {
                const coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top};
                const toggle1 = $par.find(".actionButtons").length === 0;
                const toggle2 = $par.hasClass("lightselect");

                $(".par.selected").removeClass("selected");
                $(".par.lightselect").removeClass("lightselect");
                closeOptionsWindow();
                this.toggleActionButtons(e, $par, toggle1, toggle2, coords);
            }
            sc.$apply();
            return true;
        }, true);

        this.viewctrl.defaultAction = {
            func: (e, $par) => this.showOptionsWindow(e, $par, null),
            desc: "Show options window"
        };

        onClick(".editline", ($this, e) => {
            closeOptionsWindow();
            const $par = $this.parent().filter(".par");
            if (this.viewctrl.selection.start !== null) {
                this.viewctrl.extendSelection($par);
            }
            const coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top};

            // We need the timeout so we don't trigger the ng-clicks on the buttons
            $timeout(() => {
                this.viewctrl.showOptionsWindow(e, $par, coords);
            }, 80);
            return false;
        }, true);

        this.viewctrl.popupMenuAttrs = {
            actions: "$ctrl.editorFunctions",
            save: "$ctrl.defaultAction",
            onclose: "$ctrl.optionsWindowClosed"
        };
        this.updatePopupMenu();
    }

    showPopupMenu(e, $pars, coords, attrs, $rootElement, editcontext) {
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
        $compile($popup[0])(this.sc);
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
    }

    toggleActionButtons(e, $par, toggle1, toggle2, coords) {
        if (!this.viewctrl.item.rights.editable && !this.viewctrl.item.rights.can_comment) {
            return;
        }

        if (toggle2) {
            // Clicked twice successively
            const clicktime = new Date().getTime() - this.lastclicktime;
            const clickdelta = dist(coords, this.lastclickplace);
            $par.addClass("selected");

            if (clickdelta > 10) {
                // Selecting text
                $par.removeClass("selected");
                $par.removeClass("lightselect");
            } else if (clicktime < 500 && this.viewctrl.defaultAction !== null) {
                // Double click
                this.viewctrl.defaultAction.func(e, $par, coords);
            } else {
                // Two clicks
                this.showOptionsWindow(e, $par, coords);
            }
        } else if (toggle1) {
            // Clicked once
            $par.addClass("lightselect");
            this.lastclicktime = new Date().getTime();
            this.lastclickplace = coords;
        } else {
            $log.info("This line is new: " + $par);
            $par.children().remove(".actionButtons");
            $par.removeClass("selected");
            $par.removeClass("lightselect");
        }
    }

    showOptionsWindow(e, $par, coords) {
        this.viewctrl.updateClipboardStatus();
        $par.children(".editline").addClass("menuopen");
        this.viewctrl.showPopupMenu(e, $par, coords, this.viewctrl.popupMenuAttrs, null, "par");
    }

    optionsWindowClosed() {
        optionsWindowClosed();
    }

    updatePopupMenu() {
        this.viewctrl.editorFunctions = this.viewctrl.getEditorFunctions();
        if (this.viewctrl.selection.start !== null && $window.editMode) {
            this.viewctrl.popupMenuAttrs.save = null;
            this.viewctrl.popupMenuAttrs.editbutton = false;
        } else {
            this.viewctrl.popupMenuAttrs.save = "defaultAction";
            this.viewctrl.popupMenuAttrs.editbutton = true;
        }
    }
}
