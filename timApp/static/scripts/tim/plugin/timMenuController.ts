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
        menu: nullable(t.array(t.string)),
    }),
    GenericPluginMarkup,
    t.type({
        hoverOpen: withDefault(t.boolean, true),
        separator: withDefault(t.string, "&nbsp;"), // Non-breaking space
        openingSymbol: withDefault(t.string, "&#9662;"), // Caret
    }),
]);
const TimMenuAll = t.intersection([
    t.partial({
        menu: t.array(t.string),
    }),
    t.type({
        info: Info,
        markup: TimMenuMarkup,
        preview: t.boolean,
    }),
]);

interface IMenuItem {
    items: IMenuItem[] | undefined;
    open: false; // Whether the list of items is shown.
    text: string;
}

class TimMenuController extends PluginBase<t.TypeOf<typeof TimMenuMarkup>, t.TypeOf<typeof TimMenuAll>, typeof TimMenuAll> {
    private menu: any; // IMenuItem[] = [];
    private openingSymbol: string = "";
    private hoverOpen: boolean = true;
    private separator: string = "";

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "Import";
    }

    $onInit() {
        super.$onInit();
        this.hoverOpen = this.attrs.hoverOpen;
        this.separator = this.attrs.separator;
        this.openingSymbol = this.attrs.openingSymbol;
        // const a: IMenuItem = {text: "[Linkki 1](linkki1)", items: [], open: false};
        // const b: IMenuItem = {text: "[Linkki 2](linkki2)", items: [], open: false};
        this.formMenu();
        console.log(this.openingSymbol);
    }

    /**
     * Turn string-list into two-leveled menuitem-list.
     * TODO: Support arbitrary number of levels.
     * TODO: Get this.attrs.menu in better format.
     */
    formMenu() {
        if (!this.attrs.menu) {
            return;
        }
        const menu = [];
        for (const m of this.attrs.menu) {
            const parts = m.split(" - ");
            if (parts.length > 0) {
                if (parts.length > 1) {
                    const tempItems = [];
                    for (const {item, index} of parts.map((it, ind) => ({ item: it, index: ind }))) {
                        if (index !== 0) {
                            tempItems.push({open: false, text: item.trim()});
                        }
                    }
                    const menuitem = {items: tempItems, open: false, text: parts[0].trim()};
                    menu.push(menuitem);
                } else {
                    const menuitem = {items: undefined, open: false, text: parts[0].trim()};
                    menu.push(menuitem);
                }
            }
        }
        this.menu = menu;
    }

    protected getAttributeType() {
        return TimMenuAll;
    }

    isPlainText() {
        return (window.location.pathname.startsWith("/view/"));
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
<div style="margin-top: 5px; margin-bottom: 10px;">
    <span ng-repeat="m in $ctrl.menu">
        <div ng-if="m.items" class="btn-group" uib-dropdown is-open="status.isopen" id="simple-dropdown" style="cursor: pointer;">
          <span uib-dropdown-toggle ng-disabled="disabled" ng-bind-html="m.text + $ctrl.openingSymbol"></span>
          <ul class="dropdown-menu" uib-dropdown-menu aria-labelledby="simple-dropdown">
            <li ng-repeat="i in m.items" role="menuitem" ng-bind-html="i.text"></li>
          </ul>
        </div>
        <div ng-if="!m.items" class="btn-group" style="cursor: pointer;" ng-bind-html="m.text"></div>
        <span ng-bind-html="$ctrl.separator"></span>
    </span>
</div>
`,
});
