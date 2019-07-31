/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {timApp} from "../app";
import {onClick} from "../document/eventhandlers";
import {GenericPluginMarkup, Info, nullable, withDefault} from "./attributes";

const importDataApp = angular.module("importDataApp", ["ngSanitize"]);
export const moduleDefs = [importDataApp];

// this.attrs
const TimMenuMarkup = t.intersection([
    t.partial({
        // menu: nullable(t.array(t.string)),
        menu: nullable(t.string),
        backgroundColor: nullable(t.string),
        textColor: nullable(t.string),
        fontSize: nullable(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        hoverOpen: withDefault(t.boolean, true),
        topMenu: withDefault(t.boolean, false),
        openAbove: withDefault(t.boolean, false),
        separator: withDefault(t.string, "&nbsp;"), // Non-breaking space
        openingSymbol: withDefault(t.string, "&#9662;"), // Caret
    }),
]);
const TimMenuAll = t.intersection([
    t.partial({
        menu: t.string,
    }),
    t.type({
        info: Info,
        markup: TimMenuMarkup,
        preview: t.boolean,
    }),
]);

interface ITimMenuItem {
    items: ITimMenuItem[] | undefined;
    width: string | undefined;
    text: string;
    level: number;
    open: boolean;
    id: string;
}

// export let activeTopMenu: string | undefined;

class TimMenuController extends PluginBase<t.TypeOf<typeof TimMenuMarkup>, t.TypeOf<typeof TimMenuAll>, typeof TimMenuAll> {
    private menu: any;
    private openingSymbol: string = "";
    private hoverOpen: boolean = true;
    private separator: string = "";
    private topMenu: boolean = false;
    private openAbove: boolean = false;
    private menuId: string = String.fromCharCode(65 + Math.floor(Math.random() * 26)) + Date.now();
    private previousScroll: number | undefined = 0; // Store y-value of previous scroll event for comparison.
    private previouslyClicked: ITimMenuItem | undefined;
    private barStyle: string = "";
    private mouseInside: boolean = false; // Whether mouse cursor is inside the menu.

    getDefaultMarkup() {
        return {};
    }

    $onInit() {
        super.$onInit();
        this.formMenu();
        this.hoverOpen = this.attrs.hoverOpen;
        this.separator = this.attrs.separator;
        this.topMenu = this.attrs.topMenu;
        this.openAbove = this.attrs.openAbove;
        this.openingSymbol = this.attrs.openingSymbol;
        // Turn default symbol upwards if menu opens above.
        if (this.attrs.openAbove && this.openingSymbol == "&#9662;") {
            this.openingSymbol = "&#9652;";
        }
        if (this.topMenu) {
            window.onscroll = () => this.toggleSticky();
        }
        this.setBarStyles();
        onClick("body", ($this, e) => {
            this.onClick(e);
        });
    }

    /**
     * Turn string-list into leveled menuitem-list.
     */
    formMenu() {
        if (!this.attrs.menu) {
            return;
        }
        console.log(this.attrs.menu);
        // TODO: Get rid of eval.
        // tslint:disable-next-line:no-eval
        this.menu = eval(this.attrs.menu);
    }

    protected getAttributeType() {
        return TimMenuAll;
    }

    /**
     * Close other menus and toggle clicked menu open or closed.
     * TODO: Better way to do this (for deeper menus).
     * @param item Clicked menu item.
     * @param parent1 Closest menu item parent.
     * @param parent2 Further menu item parent.
     */
    toggleSubmenu(item: ITimMenuItem, parent1: ITimMenuItem | undefined, parent2: ITimMenuItem | undefined) {
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
        // TODO: Multiple topMenus (storing active topmenu in a global variable failed!).
        // Sticky can only show when the element's place in document goes outside upper bounds.
        const menu = document.getElementById(this.menuId);
        const placeholder = document.getElementById(`${this.menuId}-placeholder-content`);
        const scrollY = $(window).scrollTop();
        if (!menu || !placeholder) {
                return;
        }
        // TODO: Getting rectangle from placeholder-variable doesn't work?
        // @ts-ignore
        const placeholderY = document.getElementById(`${this.menuId}-placeholder`).getBoundingClientRect().bottom;
        if (scrollY && placeholderY < 0) {
            // When scrolling downwards, don't show fixed menu and hide placeholder content.
            // Otherwise (i.e. scrolling upwards), show menu as fixed and let placeholder take its place in document
            // to mitigate page length changes.
            if (this.previousScroll && scrollY > this.previousScroll) {
                menu.classList.remove("top-menu");
                placeholder.classList.add("hidden");
                // activeTopMenu = undefined;
            } else {
                menu.classList.add("top-menu");
                placeholder.classList.remove("hidden");
                // activeTopMenu = this.menuId;
            }
        } else {
            menu.classList.remove("top-menu");
            placeholder.classList.add("hidden");
            // activeTopMenu = undefined;
        }
        this.previousScroll = $(window).scrollTop();
    }

    /**
     * Set styles for menu bar defined in optional attributes.
     */
    private setBarStyles() {
        if (this.attrs.backgroundColor) {
            this.barStyle += `background-color: ${this.attrs.backgroundColor}; `;
        }
        if (this.attrs.textColor) {
            // TODO: Doesn't override links even with !important.
            this.barStyle += `color: ${this.attrs.textColor}; `;
        }
        if (this.attrs.fontSize) {
            this.barStyle += `font-size: ${this.attrs.fontSize}; `;
        }
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
        return style;
    }

    /**
     * Decide what direction submenus open towards.
     * @param id Element id.
     */
    private openDirection(id: string) {
        // tslint:disable-next-line: prefer-const
        let horizontal = ""; // Default: centered.
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
        // TODO: AngularJS doesn't update if clicked outside document text area.
        if (!this.mouseInside) {
            for (const t1 of this.menu) {
                this.closeAllInMenuItem(t1);
            }
        }
    }
}

timApp.component("timmenuRunner", {
    bindings: pluginBindings,
    controller: TimMenuController,
    require: {
        vctrl: "^timView",
    },
    template: `
<tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
<span ng-cloak ng-if="$ctrl.topMenu" id="{{$ctrl.menuId}}-placeholder"></span>
<div ng-cloak ng-if="$ctrl.topMenu" class="hidden" id="{{$ctrl.menuId}}-placeholder-content"><br></div>
<div id="{{$ctrl.menuId}}" class="tim-menu" style="{{$ctrl.barStyle}}" ng-mouseleave="$ctrl.mouseInside = false" ng-mouseenter="$ctrl.mouseInside = true">
    <span ng-repeat="t1 in $ctrl.menu">
        <span ng-if="t1.items.length > 0" class="btn-group" style="{{$ctrl.setStyle(t1)}}">
          <span ng-disabled="disabled" ng-bind-html="t1.text+$ctrl.openingSymbol" ng-click="$ctrl.toggleSubmenu(t1, undefined, undefined)"></span>
          <ul class="tim-menu-dropdown" ng-if="t1.open" ng-class="$ctrl.openDirection(t1.id)" id="{{t1.id}}">
            <li class="tim-menu-list-item" ng-repeat="t2 in t1.items" style="{{$ctrl.setStyle(t2)}}">
                <span class="tim-menu-item" ng-if="t2.items.length > 0">
                    <span class="tim-menu-item" ng-bind-html="t2.text+$ctrl.openingSymbol" ng-click="$ctrl.toggleSubmenu(t2, t1, undefined)"></span>
                    <ul class="tim-menu-dropdown" id="{{t2.id}}" ng-class="$ctrl.openDirection(t2.id)" ng-if="t2.open">
                        <li class="tim-menu-list-item" ng-repeat="t3 in t2.items" style="{{$ctrl.setStyle(t3)}}">
                            <span class="tim-menu-item"ng-if="t3.items.length > 0">
                                <span class="tim-menu-item" ng-bind-html="t3.text+$ctrl.openingSymbol" ng-click="$ctrl.toggleSubmenu(t3, t2, t1)"></span>
                                <ul class="tim-menu-dropdown" id="{{t3.id}}" ng-class="$ctrl.openDirection(t3.id)" ng-if="t3.open">
                                    <li class="tim-menu-list-item" ng-repeat="t4 in t3.items" ng-bind-html="t4.text" style="{{$ctrl.setStyle(t4)}}"></li>
                                </ul>
                            </span>
                            <span class="tim-menu-item" ng-if="t3.items.length < 1" ng-bind-html="t3.text"></span>
                        </li>
                    </ul>
                </span>
                <span class="tim-menu-item" ng-if="t2.items.length < 1" ng-bind-html="t2.text"></span>
            </li>
          </ul>
        </span>
        <span ng-if="t1.items.length < 1" class="btn-group" style="{{$ctrl.setStyle(t1)}}" ng-bind-html="t1.text"></span>
        <span ng-if="!$last" ng-bind-html="$ctrl.separator"></span>
    </span>
</div>
`,
});
