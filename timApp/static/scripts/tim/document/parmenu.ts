import {IScope} from "angular";
import $ from "jquery";
import {showMessageDialog} from "../ui/dialog";
import {$compile, $log, $timeout, $window} from "../util/ngimport";
import {Coords, dist, getPageXYnull, isInViewport} from "../util/utils";
import {onClick} from "./eventhandlers";
import {getParId, getPreambleDocId, isActionablePar, isPreamble, Paragraph} from "./parhelpers";
import {ViewCtrl} from "./viewctrl";
import {createPopupMenuAttrs, getEmptyCoords} from "./viewutils";
import {getCitePar} from "../editor/pareditor";

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
    contenturl?: string;
    save: boolean;
    onclose?: string;
}

export class ParmenuHandler {
    public sc: IScope;
    public viewctrl: ViewCtrl;
    public lastclicktime: number | undefined;
    public lastclickplace: Coords | undefined;

    constructor(sc: IScope, view: ViewCtrl) {
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
                this.viewctrl.editingHandler.extendSelection($par, true);
            } else if (!this.isCloseMenuDefault()) {
                const offset = $par.offset() || getEmptyCoords();
                const coords = {left: e.pageX - offset.left, top: e.pageY - offset.top};
                const toggle1 = $par.find(".actionButtons").length === 0;
                const toggle2 = $par.hasClass("lightselect");

                $(".par.selected").removeClass("selected");
                $(".par.lightselect").removeClass("lightselect");
                this.viewctrl.closePopupIfOpen();
                this.toggleActionButtons(e, $par, toggle1, toggle2, coords);
            }
            sc.$apply();
            return true;
        }, true);

        this.viewctrl.defaultAction = {
            func: (e: JQueryEventObject, $par: Paragraph) => this.showOptionsWindow(e, $par),
            desc: "Show options window",
            show: true,
        };

        onClick(".editline", ($this, e) => {
            this.viewctrl.closePopupIfOpen();
            const $par = $this.parent().filter(".par");
            if (isPreamble($par)) {
                const parId = getParId($par) || "";
                showMessageDialog(`
<p>This paragraph is from a preamble document.
To comment or edit this, go to the corresponding <a href="/view/${getPreambleDocId($par)}">preamble document</a>.</p>

<p>Citation help: <code>${getCitePar(this.viewctrl.item.id, parId)}</code></p>
`);
            }
            if (!isActionablePar($par)) {
                return;
            }
            if (this.viewctrl.selection.start != null) {
                this.viewctrl.editingHandler.extendSelection($par);
            }
            const offset = $par.offset() || getEmptyCoords();
            const coords = {left: e.pageX - offset.left, top: e.pageY - offset.top};

            // We need the timeout so we don't trigger the ng-clicks on the buttons
            $timeout(() => {
                this.showOptionsWindow(e, $par);
            }, 80);
            return false;
        }, true);

        this.viewctrl.popupMenuAttrs = createPopupMenuAttrs();
        this.updatePopupMenu();
    }

    private isCloseMenuDefault() {
        return this.viewctrl.defaultAction && this.viewctrl.defaultAction.desc === "Close menu";
    }

    showPopupMenu(e: JQueryEventObject,
                  $pars: Paragraph,
                  attrs: IPopupMenuAttrs,
                  $rootElement?: JQuery,
                  editcontext?: string) {
        if (!$rootElement) {
            $rootElement = $pars;
        }

        const $popup = $("<popup-menu>");
        const draggable = $("<div class='actionButtons' tim-draggable-fixed>");
        $popup.attr("srcid", selectionToStr($pars));
        if (editcontext) {
            $popup.attr("editcontext", editcontext);
        }
        $popup.attr("actions", attrs.actions);
        if (attrs.editbutton) {
            $popup.attr("editbutton", attrs.editbutton.toString());
        }
        if (attrs.contenturl) {
            $popup.attr("contenturl", attrs.contenturl.toString());
        }
        $popup.attr("save", (attrs.save || true).toString());
        $popup.attr("on-close", attrs.onclose || "");

        // todo: cache this value if needed
        if ($(".area").length > 0) {
            $popup.attr("areaeditbutton", "true");
        }
        draggable.append($popup);
        $rootElement.prepend(draggable); // need to prepend to DOM before compiling
        $compile(draggable[0])(this.sc);
        const pos = getPageXYnull(e);
        if (pos) {
            draggable.offset({left: pos.X, top: pos.Y});
        }

        if (!isInViewport(draggable[0])) {
            draggable[0].scrollIntoView();
        }
    }

    toggleActionButtons(e: JQueryEventObject, $par: Paragraph, toggle1: boolean, toggle2: boolean, coords: Coords) {
        if (!this.viewctrl.item.rights.editable && !this.viewctrl.item.rights.can_comment) {
            return;
        }

        const $target = $(e.target);
        const ignoredClasses = ["no-highlight"];
        const classSelector = ignoredClasses.map((c) => "." + c).join(",");
        if ($target.parents(classSelector).length > 0) {
            return false;
        }

        if (toggle2 && this.lastclickplace && this.lastclicktime) {
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
                // Two clicks but they were too slow; do nothing.
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

    showOptionsWindow(e: JQueryEventObject, $par: Paragraph) {
        this.viewctrl.clipboardHandler.updateClipboardStatus();
        this.updatePopupMenu($par);
        $par.children(".editline").addClass("menuopen");
        this.showPopupMenu(e, $par, this.viewctrl.popupMenuAttrs, undefined, "par");
    }

    optionsWindowClosed() {
        optionsWindowClosed();
    }

    updatePopupMenu($par?: Paragraph) {
        this.viewctrl.editorFunctions = this.viewctrl.editingHandler.getEditorFunctions($par);
        if (this.viewctrl.selection.start != null && $window.editMode) {
            this.viewctrl.popupMenuAttrs.save = false;
            this.viewctrl.popupMenuAttrs.editbutton = false;
        } else {
            this.viewctrl.popupMenuAttrs.save = true;
            this.viewctrl.popupMenuAttrs.editbutton = true;
        }
    }
}
