/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {timApp} from "../app";
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
        separator: withDefault(t.string, "&nbsp;"), // Non-breaking space
        openingSymbol: withDefault(t.string, "&#9662;"), // Caret
    }),
]);
const TimMenuAll = t.intersection([
    t.partial({
        // menu: t.array(t.string),
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
    private menuId: string = String.fromCharCode(65 + Math.floor(Math.random() * 26)) + Date.now();
    private previousScroll: number | undefined = 0; // Store y-value of previous scroll event for comparison.
    private previouslyClicked: ITimMenuItem | undefined;
    private menuStyle: string = "";
    private barStyle: string = "";

    getDefaultMarkup() {
        return {};
    }

    $onInit() {
        super.$onInit();
        this.formMenu();
        this.hoverOpen = this.attrs.hoverOpen;
        this.separator = this.attrs.separator;
        this.topMenu = this.attrs.topMenu;
        this.openingSymbol = this.attrs.openingSymbol;
        if (this.topMenu) {
            window.onscroll = () => this.toggleSticky();
        }
        this.setStyles();
    }

    /**
     * Turn string-list into leveled menuitem-list.
     */
    formMenu() {
        if (!this.attrs.menu) {
            return;
        }
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
     * @param parent Menu item parent.
     */
    toggleSubmenu(item: any, parent: ITimMenuItem | undefined) {
        // Toggle open menu closed and back again when clicking.
        if (this.previouslyClicked && (this.previouslyClicked === item || item.open)) {
            item.open = !item.open;
            this.previouslyClicked = item;
            return;
        }
        // Close all menus when clicking menu that isn't child of previously clicked.
        if (parent && parent !== this.previouslyClicked) {
            for (const menu of this.menu) {
                this.closeAllInMenuItem(menu);
            }
            parent.open = true;
        }
        // A first level menu doesn't have a parent; close all other menus.
        if (!parent && item !== this.previouslyClicked) {
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
     * Set styles defined in optional attributes.
     */
    private setStyles() {
        if (this.attrs.backgroundColor) {
            this.menuStyle += `background-color: ${this.attrs.backgroundColor}; `;
            this.barStyle += `background-color: ${this.attrs.backgroundColor}; `;
        }
        if (this.attrs.textColor) {
            // TODO: Doesn't override a?
            this.menuStyle += `color: ${this.attrs.textColor} !important; `;
            this.barStyle += `color: ${this.attrs.textColor} !important; `;
        }
        if (this.attrs.fontSize) {
            this.menuStyle += `font-size: ${this.attrs.fontSize}; `;
            this.barStyle += `font-size: ${this.attrs.fontSize}; `;
        }
    }

    /**
     * Decide what direction submenus open towards (currently disabled).
     * @param id Element id.
     */
    private openDirection(id: string) {
        return ""; // Open straight below and center to the clicked item.
        /*
        const bounds = this.getBounds(id);
        let horizontal = "tim-menu-right";
        let vertical = "tim-menu-down";
        if (bounds.bottom > $(window).height()) {
            vertical = "tim-menu-up";
        }
        if (bounds.right > $(window).width()) {
            horizontal = "";
        }
        return `${horizontal} ${vertical}`;
         */
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
<div id="{{$ctrl.menuId}}" class="tim-menu" style="{{$ctrl.barStyle}}">
    <span ng-repeat="t1 in $ctrl.menu">
        <div ng-if="t1.items.length > 0" class="btn-group" style="cursor: pointer;">
          <span ng-disabled="disabled" ng-bind-html="t1.text+$ctrl.openingSymbol" ng-click="$ctrl.toggleSubmenu(t1, undefined)"></span>
          <ul class="tim-menu-dropdown" ng-if="t1.open" ng-class="$ctrl.openDirection(t1.id)" id="{{t1.id}}" style="{{$ctrl.menuStyle}}">
            <li class="tim-menu-item" ng-repeat="t2 in t1.items" role="menuitem">
                <span class="tim-menu-item" ng-if="t2.items.length > 0">
                    <span ng-bind-html="t2.text+$ctrl.openingSymbol" ng-click="$ctrl.toggleSubmenu(t2, t1)"></span>
                    <ul class="tim-menu-dropdown" id="{{t2.id}}" ng-class="$ctrl.openDirection(t2.id)" ng-if="t2.open" style="{{$ctrl.menuStyle}}">
                        <li class="tim-menu-item" ng-repeat="t3 in t2.items">
                            <span class="tim-menu-item" ng-if="t3.items.length > 0">
                                <span ng-bind-html="t3.text+$ctrl.openingSymbol" ng-click="$ctrl.toggleSubmenu(t3, t2)"></span>
                                <ul class="tim-menu-dropdown" id="{{t3.id}}" ng-class="$ctrl.openDirection(t3.id)" ng-if="t3.open" style="{{$ctrl.menuStyle}}">
                                    <li class="tim-menu-item" ng-repeat="t4 in t3.items" ng-bind-html="t4.text"></li>
                                </ul>
                            </span>
                            <span class="tim-menu-item" ng-if="t3.items.length < 1" ng-bind-html="t3.text"></span>
                        </li>
                    </ul>
                </span>
                <span class="tim-menu-item" ng-if="t2.items.length < 1" ng-bind-html="t2.text"></span>
            </li>
          </ul>
        </div>
        <div ng-if="t1.items.length < 1" class="btn-group" style="cursor: pointer;" ng-bind-html="t1.text"></div>
        <span ng-if="!$last" ng-bind-html="$ctrl.separator"></span>
    </span>
</div>
`,
});
