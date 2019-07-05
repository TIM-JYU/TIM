/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {ViewCtrl} from "tim/document/viewctrl";
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
        separator: withDefault(t.string, "|"),
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
    items: IMenuItem[];
    open: false; // Whether the list of items is shown.
    text: string;
}

class TimMenuController extends PluginBase<t.TypeOf<typeof TimMenuMarkup>, t.TypeOf<typeof TimMenuAll>, typeof TimMenuAll> {
    private isRunning = false;
    private output: string = "";
    private fieldlist: string = "";
    private vctrl!: ViewCtrl;
    private importText: string = "";
    private error: {message?: string, stacktrace?: string} = {};
    private result: string = "";
    private menu: any; // IMenuItem[] = [];

    private hoverOpen: boolean = true;
    private separator: string = "|";

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
        console.log(this.attrs);
        // const a: IMenuItem = {text: "[Linkki 1](linkki1)", items: [], open: false};
        // const b: IMenuItem = {text: "[Linkki 2](linkki2)", items: [], open: false};
        this.menu = this.attrs.menu; // [a, b];
        console.log(this.menu);
    }

    protected getAttributeType() {
        return TimMenuAll;
    }

    isPlainText() {
        return (window.location.pathname.startsWith("/view/"));
    }

    menuClick(m: any) {
        console.log(m);
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
<div>
{{$ctrl.separator}}&nbsp;<span ng-repeat="m in $ctrl.menu | orderBy:'title'">
<span ng-click="$ctrl.menuClick(m)">{{m}}</span>&nbsp;{{$ctrl.separator}}&nbsp;</span>
</div>
`,
});
