/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import $ from "jquery";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {onClick} from "../document/eventhandlers";
import {ViewCtrl} from "../document/viewctrl";
import {IRights} from "../user/IRights";
import {genericglobals} from "../util/globals";
import {Require} from "../util/utils";
import {GenericPluginMarkup, Info, nullable, withDefault} from "./attributes";
import "style-loader!./timMenu.css";

// this.attrs
const TimMenuMarkup = t.intersection([
    t.partial({
        backgroundColor: nullable(t.string),
        textColor: nullable(t.string),
        fontSize: nullable(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        menu: withDefault(t.string, ""),
        hoverOpen: withDefault(t.boolean, true), // Unimplemented.
        topMenu: withDefault(t.boolean, false),
        topMenuTriggerHeight: withDefault(t.number, 200),
        openAbove: withDefault(t.boolean, false),
        keepLinkColors: withDefault(t.boolean, false),
        basicColors: withDefault(t.boolean, false),
        separator: withDefault(t.string, "&nbsp;"), // Non-breaking space
        openingSymbol: withDefault(t.string, "&#9662;"), // Caret
    }),
]);

interface ITimMenuItem {
    height?: string;
    id: string;
    items?: ITimMenuItem[];
    level: number;
    open: boolean;
    rights?: string;
    text: string;
    width?: string;
}

const ITimMenuItem: t.Type<ITimMenuItem> = t.recursion("ITimMenuItem", () =>
    t.intersection([
        t.partial({
            height: t.string,
            items: t.array(ITimMenuItem),
            rights: t.string,
            width: t.string,
        }),
        t.type({
            id: t.string,
            level: t.number,
            open: t.boolean,
            text: t.string,
        })]),
);

const TimMenuAll = t.intersection([
    t.partial({
        menu: t.array(ITimMenuItem),
    }),
    t.type({
        info: Info,
        markup: TimMenuMarkup,
        preview: t.boolean,
    }),
]);

class TimMenuController extends PluginBase<t.TypeOf<typeof TimMenuMarkup>, t.TypeOf<typeof TimMenuAll>, typeof TimMenuAll> {
    private menu: ITimMenuItem[] = [];
    private vctrl?: Require<ViewCtrl>;
    private openingSymbol: string = "";
    private hoverOpen: boolean = true;
    private separator: string = "";
    private topMenu: boolean = false;
    private basicColors: boolean = false;
    private openAbove: boolean = false;
    private keepLinkColors: boolean = false;
    private previousScroll: number | undefined = 0; // Store y-value of previous scroll event for comparison.
    private previouslyClicked: ITimMenuItem | undefined;
    private barStyle: string = "";
    private mouseInside: boolean = false; // Whether mouse cursor is inside the menu.
    private clickedInside: boolean = false; // Whether a menu has been opened with mouse click.
    private userRights: IRights | undefined;
    private previousSwitch: number | undefined;
    private topMenuTriggerHeight: number | undefined; // Pixels to scroll before topMenu appears.
    private topMenuVisible: boolean = false; // Meant to curb unnecessary topMenu state changes.
    private previouslyScrollingDown: boolean = true;
    private userPrefersHoverDisabled: boolean = false;
    private touchMode: boolean = false; // If a touch event has been detected in HTML body.

    getDefaultMarkup() {
        return {};
    }

    $onInit() {
        super.$onInit();
        if (this.vctrl == null || this.vctrl.isSlideView()) {
            return;
        }
        if (!this.attrsall.menu) {
            return;
        }
        this.menu = this.attrsall.menu;
        this.separator = this.attrs.separator;
        this.topMenu = this.attrs.topMenu;
        this.openAbove = this.attrs.openAbove;
        this.keepLinkColors = this.attrs.keepLinkColors;
        this.basicColors = this.attrs.basicColors;
        this.openingSymbol = this.attrs.openingSymbol;
        // Turn default symbol upwards if menu opens above.
        if (this.attrs.openAbove && this.openingSymbol == "&#9662;") {
            this.openingSymbol = "&#9652;";
        }
        if (this.topMenu && !this.attrsall.preview) {
            // Divided by two because the check is half of the user given height towards either direction.
            this.topMenuTriggerHeight = Math.floor(this.attrs.topMenuTriggerHeight / 2);
            if (this.topMenuTriggerHeight == 0) {
                // Logic doesn't work if the height is zero. Negative number makes it stay after appearing
                // until scrolled to its original location in document.
                this.topMenuTriggerHeight = 1;
            }
            window.onscroll = () => {
                this.toggleSticky();
            };
            window.onresize = () => {
                this.setBarStyles();
            };
        }
        onClick("body", ($this, e) => {
            this.onClick(e);
        });
        if (genericglobals().userPrefs.disable_menu_hover) {
            this.userPrefersHoverDisabled = genericglobals().userPrefs.disable_menu_hover;
        }
        this.hoverOpen = this.attrs.hoverOpen && !this.userPrefersHoverDisabled;
        this.userRights = this.vctrl.item.rights;
        this.setBarStyles();
    }

    getAttributeType() {
        return TimMenuAll;
    }

    /**
     * Close other menus and toggle clicked menu open or closed.
     * TODO: Better way to do this (for deeper menus).
     * @param item Clicked menu item.
     * @param parent1 Closest menu item parent.
     * @param parent2 Further menu item parent.
     * @param clicked Toggled by a mouse click.
     */
    toggleSubmenu(item: ITimMenuItem, parent1: ITimMenuItem | undefined, parent2: ITimMenuItem | undefined, clicked: boolean) {
        // If called by mouseenter and either hover open is off or touch mode is on, do nothing.
        if (!clicked && (!this.hoverOpen || this.touchMode)) {
            return;
        }
        // Toggle open menu closed and back again when clicking.
        if (this.previouslyClicked && (this.previouslyClicked === item || item.open)) {
            item.open = !item.open;
            this.previouslyClicked = item;
            return;
        }
        // Close all menus when clicking menu that isn't child of previously clicked.
        if (parent1 && parent1 !== this.previouslyClicked) {
            for (const menu of this.menu) {
                this.closeAllInMenuItem(menu);
            }
            parent1.open = true;
            if (parent2) {
                parent2.open = true;
            }
        }
        // A first level menu doesn't have a parent; close all other menus.
        if (!parent1 && item !== this.previouslyClicked) {
            for (const menu of this.menu) {
                this.closeAllInMenuItem(menu);
            }
        }
        // Unless already open, clicked item always opens.
        item.open = true;
        this.previouslyClicked = item;
    }

    /**
     * Closes all levels of a menu item recursively.
     * @param t1 First level menu item.
     */
    closeAllInMenuItem(t1: ITimMenuItem) {
        t1.open = false;
        if (!t1.items) {
            return;
        }
        for (const t2 of t1.items) {
            this.closeAllInMenuItem(t2);
        }
    }

    /**
     * Makes the element show at top when scrolling towards it from below.
     */
    toggleSticky() {
        // TODO: Multiple topMenus.
        // TODO: Check the trigger height logic.
        const menu = this.element.find(".tim-menu")[0];
        const placeholder = this.element.find(".tim-menu-placeholder")[0];
        const scrollY = $(window).scrollTop();

        if (!menu || !placeholder || !this.topMenuTriggerHeight || !scrollY) {
            return;
        }
        if (!this.previousScroll) {
            this.previousScroll = scrollY;
            return;
        }
        // Placeholder and its content are separate, because when hidden y is 0.
        const placeholderContent = this.element.find(".tim-menu-placeholder-content")[0];
        const belowPlaceholder = placeholder.getBoundingClientRect().bottom < 0;
        const scrollingDown = scrollY > this.previousScroll;
        const scrollDirHasChanged = (this.previouslyScrollingDown && !scrollingDown) || (!this.previouslyScrollingDown && scrollingDown);
        if (scrollDirHasChanged) {
            // If scrolling direction has changed, set the scroll trigger distance to be centered at that y-coordinate.
            this.previousSwitch = scrollY;
        }
        this.previouslyScrollingDown = scrollingDown;
        this.previousScroll = $(window).scrollTop();
        const isSmallScreen = ($(".device-xs").is(":visible") || $(".device-sm").is(":visible"));

        // Sticky can only show when the element's place in document goes outside upper bounds.
        if (belowPlaceholder) {
            // When scrolling downwards, don't show fixed menu and hide placeholder content.
            // Otherwise (i.e. scrolling upwards), show menu as fixed and let placeholder take its place in document
            // to mitigate page length changes.
            if (scrollingDown) {
                // If topMenu is already hidden, don't try hiding it again.
                // If scroll distance is smaller than min height, don't do anything.
                if (!this.topMenuVisible || (!this.previousSwitch || scrollY > this.previousSwitch + this.topMenuTriggerHeight)) {
                    return;
                }
                menu.classList.remove("top-menu");
                menu.classList.remove("top-menu-mobile");
                placeholderContent.classList.add("tim-menu-hidden");
                this.topMenuVisible = false;
            } else {
                // If topMenu is already visible, the rest are redundant.
                if (this.topMenuVisible || (this.previousSwitch &&
                    !(scrollY > this.previousSwitch + this.topMenuTriggerHeight || scrollY < this.previousSwitch - this.topMenuTriggerHeight))) {
                    return;
                }
                menu.classList.add("top-menu");
                if (isSmallScreen) {
                    menu.classList.add("top-menu-mobile");
                }
                placeholderContent.classList.remove("tim-menu-hidden");
                this.topMenuVisible = true;
            }
        } else {
            if (!this.topMenuVisible) {
                return;
            }
            menu.classList.remove("top-menu");
            menu.classList.remove("top-menu-mobile");
            placeholderContent.classList.add("tim-menu-hidden");
            this.topMenuVisible = false;
        }
        this.scope.$evalAsync();
    }

    /**
     * Set styles for menu bar defined in optional attributes.
     * Includes adjusting topMenu width to fit document area width.
     */
    private setBarStyles() {
        if (this.attrs.backgroundColor) {
            this.barStyle += `background-color: ${this.attrs.backgroundColor}; `;
        }
        if (this.attrs.textColor) {
            this.barStyle += `color: ${this.attrs.textColor}; `;
        }
        if (this.attrs.fontSize) {
            this.barStyle += `font-size: ${this.attrs.fontSize}; `;
        }
        // If menu isn't sticky, default width is fine.
        if (this.topMenu) {
            const menuWidth = this.element.parent().width();
            if (menuWidth) {
                this.barStyle += `width: ${menuWidth}px; `;
            } else {
                this.barStyle = `width: 100%; `;
            }
        }
        this.scope.$evalAsync();
    }

    /**
     * Sets style based on the object's attributes.
     * @param item Menu item.
     */
    private setStyle(item: ITimMenuItem) {
        let style = "";
        if (item.width) {
            style += `width: ${item.width}; `;
        }
        if (item.height) {
            style += `height: ${item.height}; `;
        }
        return style;
    }

    /**
     * Checks whether the user has required rights to see the menu item.
     * If no requirements have been set, return true.
     * @param item Menu item.
     */
    private hasRights(item: ITimMenuItem) {
        // TODO: Limit the amount of checks.
        // If item has no set rights, show to everyone.
        if (!item.rights) {
            return true;
        }
        if (this.userRights) {
            if (item.rights == "edit") {
                return this.userRights.editable;
            }
            if (item.rights == "teacher") {
                return this.userRights.teacher;
            }
            if (item.rights == "manage") {
                return this.userRights.manage;
            }
            if (item.rights == "owner") {
                return this.userRights.owner;
            } else {
                // Return true, if user has none of the supported rights.
                // View not included, since it's redundant: without view the whole document is hidden.
                return true;
            }
        } else {
            // Non-logged in users who see the page have only view rights.
            return (!(item.rights == "edit" || item.rights == "manage" || item.rights == "owner"));
        }
    }

    /**
     * Decide what direction submenus open towards.
     * @param id Element id.
     */
    private openDirection(id: string) {
        const horizontal = ""; // Default: centered.
        let vertical = ""; // Default: below.
        // If true, opens all menus above.
        if (this.openAbove) {
            vertical = "tim-menu-up";
        }
        return `${horizontal} ${vertical}`;
    }

    /**
     * Closes the menu structure if mouse is outside all menu elements.
     * @param e Click event.
     */
    private onClick(e: JQuery.Event) {
        // console.log("onClick, " + e.type);
        // Detect touch screen when touch event happens.
        this.touchMode = e.type.includes("touch");
        if (this.mouseInside) {
            this.clickedInside = true;
        } else {
            this.clickedInside = false;
            this.closeMenus();
        }
    }

    /**
     * Handle mouseleave event.
     */
    private mouseLeave() {
        // console.log("mouseLeave");
        this.mouseInside = false;
        // Only close menus on mouseleave, if hoverOpen is enabled and menu hasn't been clicked.
        if (!this.clickedInside && this.hoverOpen) {
            this.closeMenus();
        }
    }

    private mouseEnter() {
        // console.log("mouseEnter");
        this.mouseInside = true;
    }

    /**
     * Closes all menus and handles updating.
     */
    private closeMenus() {
        for (const t1 of this.menu) {
            this.closeAllInMenuItem(t1);
        }
        // Plugin won't update if clicked outside document area without this.
        this.scope.$evalAsync();
    }
}

const menuApp = angular.module("menuApp", ["ngSanitize"]);
export const moduleDefs = [menuApp];

menuApp.component("timmenuRunner", {
    bindings: pluginBindings,
    controller: TimMenuController,
    require: {
        vctrl: "^timView",
    },
    template: `
<tim-markup-error ng-if="::$ctrl.markupError" [data]="::$ctrl.markupError"></tim-markup-error>
<span ng-cloak ng-if="$ctrl.topMenu" class="tim-menu-placeholder"></span>
<span ng-cloak ng-if="$ctrl.topMenu" class="tim-menu-placeholder-content tim-menu-hidden"><br></span>
<div id="{{$ctrl.menuId}}" class="tim-menu" ng-class="{'bgtim white': $ctrl.basicColors, 'hide-link-colors': !$ctrl.keepLinkColors}" style="{{$ctrl.barStyle}}" ng-mouseleave="$ctrl.mouseLeave()" ng-mouseenter="$ctrl.mouseEnter()">
    <span ng-repeat="t1 in $ctrl.menu">
        <span ng-if="t1.items.length > 0 && $ctrl.hasRights(t1)" class="btn-group" style="{{$ctrl.setStyle(t1)}}">
          <span ng-disabled="disabled" ng-bind-html="t1.text+$ctrl.openingSymbol" ng-click="$ctrl.toggleSubmenu(t1, undefined, undefined, true)" ng-mouseenter="$ctrl.toggleSubmenu(t1, undefined, undefined, false)"></span>
          <ul class="tim-menu-dropdown" ng-if="t1.open" ng-class="$ctrl.openDirection(t1.id)" id="{{t1.id}}">
            <li class="tim-menu-list-item" ng-repeat="t2 in t1.items" style="{{$ctrl.setStyle(t2)}}">
                <span class="tim-menu-item" ng-if="t2.items.length > 0 && $ctrl.hasRights(t2)">
                    <span class="tim-menu-item" ng-bind-html="t2.text+$ctrl.openingSymbol" ng-click="$ctrl.toggleSubmenu(t2, t1, undefined, true)" ng-mouseenter="$ctrl.toggleSubmenu(t2, t1, undefined, false)"></span>
                    <ul class="tim-menu-dropdown" id="{{t2.id}}" ng-class="$ctrl.openDirection(t2.id)" ng-if="t2.open">
                        <li class="tim-menu-list-item" ng-repeat="t3 in t2.items" style="{{$ctrl.setStyle(t3)}}">
                            <span class="tim-menu-item" ng-if="t3.items.length > 0 && $ctrl.hasRights(t3)">
                                <span class="tim-menu-item" ng-bind-html="t3.text+$ctrl.openingSymbol" ng-click="$ctrl.toggleSubmenu(t3, t2, t1, true)" ng-mouseenter="$ctrl.toggleSubmenu(t3, t2, t1, false)"></span>
                                <ul class="tim-menu-dropdown" id="{{t3.id}}" ng-class="$ctrl.openDirection(t3.id)" ng-if="t3.open">
                                    <li class="tim-menu-list-item" ng-repeat="t4 in t3.items" ng-bind-html="t4.text" style="{{$ctrl.setStyle(t4)}}" ng-if="$ctrl.hasRights(t4)"></li>
                                </ul>
                            </span>
                            <span class="tim-menu-item" ng-if="t3.items.length < 1  && $ctrl.hasRights(t3)" ng-bind-html="t3.text"></span>
                        </li>
                    </ul>
                </span>
                <span class="tim-menu-item" ng-if="t2.items.length < 1 && $ctrl.hasRights(t2)" ng-bind-html="t2.text"></span>
            </li>
          </ul>
        </span>
        <span ng-if="t1.items.length < 1 && $ctrl.hasRights(t1)" class="btn-group" style="{{$ctrl.setStyle(t1)}}" ng-bind-html="t1.text"></span>
        <span ng-if="!$last && $ctrl.hasRights(t1)" ng-bind-html="$ctrl.separator"></span>
    </span>
</div>
`,
});
