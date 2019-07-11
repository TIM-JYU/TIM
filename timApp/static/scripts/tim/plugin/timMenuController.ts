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
        // menu: t.array(t.string),
        menu: t.string,
    }),
    t.type({
        info: Info,
        markup: TimMenuMarkup,
        preview: t.boolean,
    }),
]);

/*
interface IMenuItem {
    items: IMenuItem[] | undefined;
    open: false; // Whether the list of items is shown.
    text: string;
}*/

interface IMenuItem {
    items: IMenuItem[] | undefined;
    text: string;
    level: number;
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
        this.formMenu();
    }

    /**
     * Turn string-list into leveled menuitem-list.
     */
    formMenu() {
        if (!this.attrs.menu) {
            return;
        }
        // const temp: IMenuItem[] = [];
        const evaluedMenu = eval(this.attrs.menu);
        this.menu = evaluedMenu;
        // console.log(this.menu);
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
        <div ng-if="m.items.length > 0" class="btn-group" uib-dropdown is-open="status.isopen" id="simple-dropdown" style="cursor: pointer;">
          <span uib-dropdown-toggle ng-disabled="disabled" ng-bind-html="m.text+$ctrl.openingSymbol"></span>
          <ul class="dropdown-menu" uib-dropdown-menu aria-labelledby="simple-dropdown">
            <li ng-repeat="i in m.items" role="menuitem" ng-bind-html="i.text"></li>
          </ul>
        </div>
        <div ng-if="m.items.length < 1" class="btn-group" style="cursor: pointer;" ng-bind-html="m.text"></div>
        <span ng-if="!$last" ng-bind-html="$ctrl.separator"></span>
    </span>
</div>
`,
});
