import {IScope} from "angular";
import $ from "jquery";
import {showMessageDialog} from "../../dialog";
import {$compile, $log, $timeout, $window} from "../../ngimport";
import {Coords, dist} from "../../utils";
import {onClick} from "./eventhandlers";
import {getPreambleDocId, isActionablePar, isPreamble, Paragraph} from "./parhelpers";
import {ViewCtrl} from "./ViewCtrl";

export function closeOptionsWindow() {
    const $actionButtons = $(".actionButtons");
    const $parOrArea = $actionButtons.parent();
    $actionButtons.remove();
    optionsWindowClosed($parOrArea);
}

export function optionsWindowClosed($parOrArea?: JQuery) {
    const $editline = $(".menuopen");
    $editline.removeClass("menuopen");
}

function selectionToStr(selection: JQuery) {
    return selection.toArray().map((e) => "#" + e.id).join(",");
}

export interface IPopupMenuAttrs {
    actions: string;
    editbutton?: boolean;
    save: string | null;
    onclose: string;
}

export class ParmenuHandler {
    public sc: IScope;
    public viewctrl: ViewCtrl;
    public lastclicktime: number;
    public lastclickplace: Coords;

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
            if (!isActionablePar($par)) {
                return;
            }

            // Don't show paragraph menu on these specific tags or classes
            const ignoredTags = ["BUTTON", "INPUT", "TEXTAREA", "A", "QUESTIONADDEDNEW"];
            const ignoredClasses = ["no-popup-menu", "ace_editor"];
            const classSelector = ignoredClasses.map((c) => "." + c).join(",");
            if (ignoredTags.indexOf(tag) > -1 || $target.parents(classSelector).length > 0) {
                return false;
            }

            if (this.viewctrl.selection.start != null) {
                this.viewctrl.extendSelection($par, true);
            } else {
                const offset = $par.offset() || {left: 0, top: 0};
                const coords = {left: e.pageX - offset.left, top: e.pageY - offset.top};
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
            func: (e: Event, $par: Paragraph) => this.showOptionsWindow(e, $par, {left: 0, top: 0}),
            desc: "Show options window",
            show: true,
        };

        onClick(".editline", ($this, e) => {
            closeOptionsWindow();
            const $par = $this.parent().filter(".par");
            if (isPreamble($par)) {
                showMessageDialog(`
This paragraph is from a preamble document.
To comment or edit this, go to the corresponding <a href="/view/${getPreambleDocId($par)}">preamble document</a>.`);
            }
            if (!isActionablePar($par)) {
                return;
            }
            if (this.viewctrl.selection.start != null) {
                this.viewctrl.extendSelection($par);
            }
            const offset = $par.offset() || {left: 0, top: 0};
            const coords = {left: e.pageX - offset.left, top: e.pageY - offset.top};

            // We need the timeout so we don't trigger the ng-clicks on the buttons
            $timeout(() => {
                this.viewctrl.showOptionsWindow(e, $par, coords);
            }, 80);
            return false;
        }, true);

        this.viewctrl.popupMenuAttrs = {
            actions: "$ctrl.editorFunctions",
            save: "$ctrl.defaultAction",
            onclose: "$ctrl.optionsWindowClosed",
        };
        this.updatePopupMenu();
    }

    showPopupMenu(e: Event,
                  $pars: Paragraph,
                  coords: Coords,
                  attrs: IPopupMenuAttrs,
                  $rootElement: JQuery,
                  editcontext: string) {
        if (!$rootElement) {
            $rootElement = $pars;
        }

        const $popup = $("<popup-menu>");
        $popup.attr("tim-draggable-fixed", "");
        $popup.attr("srcid", selectionToStr($pars));
        $popup.attr("editcontext", editcontext);
        $popup.attr("actions", attrs.actions);
        if (attrs.editbutton) {
            $popup.attr("editbutton", attrs.editbutton.toString());
        }
        $popup.attr("save", attrs.save);
        $popup.attr("on-close", attrs.onclose);

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
        const offset = element.offset() || {top: 0};
        bounds.top = offset.top;
        bounds.bottom = bounds.top + element.outerHeight();
        let y = $(window).scrollTop() || 0;
        if (bounds.bottom > viewport.bottom) {
            y += (bounds.bottom - viewport.bottom);
        } else if (bounds.top < viewport.top) {
            y += (bounds.top - viewport.top);
        }
        $("html, body").animate({
            scrollTop: y,
        }, 500);
    }

    toggleActionButtons(e: Event, $par: Paragraph, toggle1: boolean, toggle2: boolean, coords: Coords) {
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
            } else if (clicktime < 500 && this.viewctrl.defaultAction != null) {
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

    showOptionsWindow(e: Event, $par: Paragraph, coords: Coords) {
        this.viewctrl.updateClipboardStatus();
        this.viewctrl.updatePopupMenu($par);
        $par.children(".editline").addClass("menuopen");
        this.viewctrl.showPopupMenu(e, $par, coords, this.viewctrl.popupMenuAttrs, null, "par");
    }

    optionsWindowClosed() {
        optionsWindowClosed();
    }

    updatePopupMenu($par?: Paragraph) {
        this.viewctrl.editorFunctions = this.viewctrl.getEditorFunctions($par);
        if (this.viewctrl.selection.start != null && $window.editMode) {
            this.viewctrl.popupMenuAttrs.save = null;
            this.viewctrl.popupMenuAttrs.editbutton = false;
        } else {
            this.viewctrl.popupMenuAttrs.save = "defaultAction";
            this.viewctrl.popupMenuAttrs.editbutton = true;
        }
    }
}
