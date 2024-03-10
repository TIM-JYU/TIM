import type {IScope} from "angular";
import $ from "jquery";
import {showPopupMenu} from "tim/document/showPopupMenu";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {SelectionUpdateType} from "tim/document/editing/editing";
import type {HelpPar} from "tim/document/structure/helpPar";
import {ParContext} from "tim/document/structure/parContext";
import {
    createParContextOrHelp,
    findParentPar,
    tryCreateParContextOrHelp,
} from "tim/document/structure/create";
import type {Coords} from "tim/util/utils";
import {dist, getPageXY, to2} from "tim/util/utils";
import type {OnClickArg} from "tim/document/eventhandlers";
import {onClick} from "tim/document/eventhandlers";
import {getCitePar} from "tim/document/parhelpers";
import type {EditMode} from "tim/document/popup-menu-dialog.component";
import type {ViewCtrl} from "tim/document/viewctrl";
import type {MenuFunctionList} from "tim/document/viewutils";
import {getEmptyCoords} from "tim/document/viewutils";

function checkIfIgnored(
    ignoredTags: string[],
    ignoredClasses: string[],
    element: JQuery
) {
    const classSelector = ignoredClasses.map((c) => "." + c).join(",");
    const tagSelector = ignoredTags.join(",");
    return (
        element.parents(tagSelector).addBack(tagSelector).length > 0 ||
        element.parents(classSelector).addBack(classSelector).length > 0
    );
}

function getEditLineOfParOrHelp(par: ParContext | HelpPar) {
    const editLine =
        par instanceof ParContext ? par.par.getEditLine() : par.getEditLine();
    return editLine;
}

export class ParmenuHandler {
    public sc: IScope;
    public viewctrl: ViewCtrl;
    public lastclicktime: number | undefined;
    public lastclickplace: Coords | undefined;
    public currCtx?: ParContext | HelpPar;

    constructor(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        onClick(
            ".paragraphs .parContent",
            ($this, e) => {
                if (this.viewctrl.editing) {
                    return false;
                }

                const target = $(e.target) as JQuery;
                const par = tryCreateParContextOrHelp($this.parents(".par")[0]);
                if (!par || par.isHelp) {
                    return;
                }
                const editMenuLeft = this.viewctrl.editMenuOnLeft;
                if ((editMenuLeft && !par.preamble) || !editMenuLeft) {
                    this.updateBadgePosition(par, e);
                }
                if (!par.isActionable()) {
                    return;
                }
                // Don't show paragraph menu on these specific tags or classes
                if (
                    checkIfIgnored(
                        [
                            "button",
                            "input",
                            "textarea",
                            "a",
                            "answerbrowser",
                            "select",
                            "label",
                        ],
                        ["no-popup-menu", "ace_editor", "btn"],
                        target
                    )
                ) {
                    return false;
                }

                if (this.viewctrl.editingHandler.selection != null) {
                    this.viewctrl.editingHandler.updateSelection(
                        par,
                        this.viewctrl.editingHandler.selection,
                        SelectionUpdateType.AllowShrink
                    );
                } else {
                    const offset = par.offset() ?? getEmptyCoords();
                    const coords = {
                        left: e.pageX - offset.left,
                        top: e.pageY - offset.top,
                    };
                    const toggle1 = !par.hasClass("lightselect");
                    const toggle2 =
                        par.hasClass("lightselect") &&
                        !this.isCloseMenuDefault();

                    $(".par.selected").removeClass("selected");
                    $(".par.lightselect").removeClass("lightselect");
                    this.viewctrl.closePopupIfOpen();
                    this.toggleActionButtons(
                        e.originalEvent,
                        par,
                        toggle1,
                        toggle2,
                        coords
                    );
                }
                this.viewctrl.reviewCtrl.selectText(par);
                sc.$apply();
                return true;
            },
            true
        );

        onClick(".editline", this.handleLeftEditAreaClick, true);
    }

    private updateBadgePosition(par: ParContext, e: OnClickArg) {
        const parOffset = par.offset() ?? getEmptyCoords();
        const badgeY = e.pageY - parOffset.top;
        this.viewctrl.notesHandler.updateNoteBadge(par, badgeY);
    }

    handleLeftEditAreaClick = async (
        $this: JQuery,
        e: OnClickArg
    ): Promise<boolean | void> => {
        const par = createParContextOrHelp(findParentPar($this));
        if (!this.viewctrl.editMenuOnLeft && !par.isHelp) {
            this.updateBadgePosition(par, e);
            return;
        }
        await this.openParMenu($this, e);
    };

    openParMenu = async (
        $this: JQuery,
        e: OnClickArg
    ): Promise<boolean | void> => {
        await this.viewctrl.closePopupIfOpen();
        const par = createParContextOrHelp(findParentPar($this));
        if (!par.isHelp) {
            if (par.preamble) {
                showMessageDialog(`
    <p>This paragraph is from a preamble document.
    To comment or edit this, go to the corresponding <a href="/view/${
        par.preamble
    }">preamble document</a>.</p>
    
    <p>Citation help: <code>${getCitePar(this.viewctrl.item.id, par)}</code></p>
    `);
            }
            if (!par.isActionable()) {
                return;
            }
            if (this.viewctrl.editingHandler.selection != null) {
                this.viewctrl.editingHandler.updateSelection(
                    par,
                    this.viewctrl.editingHandler.selection,
                    SelectionUpdateType.DontAllowShrink
                );
            }
        }

        if (!this.viewctrl.actionsDisabled) {
            this.showOptionsWindow(e.originalEvent, par);
        }
        return false;
    };

    private isCloseMenuDefault() {
        return this.viewctrl.defaultAction === "Close menu";
    }

    async showPopupMenu(
        e: MouseEvent,
        par: ParContext | HelpPar,
        attrs: {
            actions: MenuFunctionList;
            save: boolean;
            contenturl?: string;
            editbutton: boolean;
        },
        editcontext?: EditMode
    ) {
        const pos = getPageXY(e);
        const p = {
            actions: attrs.actions,
            contenturl: attrs.contenturl,
            editbutton: attrs.editbutton,
            editcontext: editcontext,
            save: attrs.save,
            srcid: par,
            vctrl: this.viewctrl,
        };
        this.currCtx = par;
        if (this.updatePopupMenuIfOpen(p)) {
            return;
        }
        const editLine = getEditLineOfParOrHelp(par);
        editLine.classList.add("menuopen");
        const mi = showPopupMenu(p, pos);
        const dlg = await mi;
        this.viewctrl.registerPopupMenu(dlg);
        await to2(dlg.result);
        this.currCtx = undefined;
        getEditLineOfParOrHelp(dlg.getCtx()).classList.remove("menuopen");
    }

    toggleActionButtons(
        e: MouseEvent,
        par: ParContext,
        toggle1: boolean,
        toggle2: boolean,
        coords: Coords
    ) {
        if (
            !this.viewctrl.item.rights.editable &&
            !this.viewctrl.item.rights.can_comment
        ) {
            return;
        }

        const target = $(e.target as HTMLElement);
        if (checkIfIgnored(["answerbrowser"], ["no-highlight"], target)) {
            return false;
        }

        if (toggle2 && this.lastclickplace && this.lastclicktime) {
            // Clicked twice successively
            const clicktime = new Date().getTime() - this.lastclicktime;
            const clickdelta = dist(coords, this.lastclickplace);
            par.addClass("selected");

            const defAct = this.viewctrl.getDefaultAction(par);
            if (clickdelta > 10) {
                // Selecting text
                par.removeClass("selected", "lightselect");
            } else if (clicktime < 500 && defAct) {
                // Double click
                defAct.func(e, coords);
            } else {
                // Two clicks but they were too slow; do nothing.
            }
        } else if (toggle1) {
            // Clicked once
            par.addClass("lightselect");
            this.lastclicktime = new Date().getTime();
            this.lastclickplace = coords;
        } else {
            par.removeClass("selected");
            par.removeClass("lightselect");
        }
    }

    showOptionsWindow(e: MouseEvent, par: ParContext | HelpPar) {
        const result = this.getPopupAttrs(par);
        this.showPopupMenu(e, par, result, "par");
    }

    updatePopupMenuIfOpen(attrs: {
        actions: MenuFunctionList;
        save: boolean;
        contenturl?: string;
        editbutton: boolean;
    }) {
        if (this.viewctrl.popupmenu) {
            this.viewctrl.popupmenu.updateAttrs(attrs);
            return true;
        }
        return false;
    }

    getPopupAttrs(par: ParContext | HelpPar) {
        const fns = this.viewctrl.editingHandler.getEditorFunctions(par);
        const hasSelectionAndEditMode = this.viewctrl.item.rights.editable;
        return {
            actions: fns,
            editbutton: hasSelectionAndEditMode,
            save: hasSelectionAndEditMode,
        };
    }
}
