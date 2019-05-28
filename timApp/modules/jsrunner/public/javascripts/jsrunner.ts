/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {GenericPluginMarkup, GenericPluginTopLevelFields, nullable, PluginBase, pluginBindings} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import "../../stylesheets/jsrunner.css";
import {ViewCtrl} from "../../../../static/scripts/tim/document/viewctrl";

const jsrunnerApp = angular.module("jsrunnerApp", ["ngSanitize"]);
export const moduleDefs = [jsrunnerApp];

const JsrunnerMarkup = t.intersection([
    t.partial({
        creditField: t.string,
        defaultPoints: t.number,
        failGrade: t.string,
        fieldhelper: t.boolean,
        fields: t.array(t.string),
        gradeField: t.string,
        gradingScale: t.dictionary(t.string, t.number),
        groups: t.array(t.string),
        program: nullable(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
    }),
]);
const JsrunnerAll = t.intersection([
    GenericPluginTopLevelFields,
    t.type({markup: JsrunnerMarkup}),
]);

class JsrunnerController extends PluginBase<t.TypeOf<typeof JsrunnerMarkup>, t.TypeOf<typeof JsrunnerAll>, typeof JsrunnerAll> {
    private error?: string;
    private isRunning = false;
    private print: string = "";
    private fieldlist: string = "";
    private vctrl!: ViewCtrl;

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "Run script";
    }

    $onInit() {
        super.$onInit();
        if (this.attrs.fieldhelper) {
            const pluginlist = this.vctrl.getTimComponentsByRegex(".*");
            const tasks = "";
            for (const plug of pluginlist) {
                console.log(plug);
                if (plug.getName() !== undefined) {
                    // @ts-ignore TODO
                    tasks += " - " + plug.getTaskId().toString() + "\n";
                }
            }
            this.fieldlist = tasks;
        }
    }

    checkFields() {
        this.doCheckFields(false);
    }

    async doCheckFields(nosave: boolean) {
        this.error =  "... undefined or no rights to fields ...";
        this.isRunning = true;
        const params = {
            input: {
                nosave: false,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to($http.put<{web: {result: string, error?: string, print: string}}>(url, params));
        this.isRunning = false;
        if (r.ok) {
            const data = r.result.data;
            this.error = data.web.error;
            this.print = data.web.print;
        } else {
            this.error = r.result.data.error || "Unknown error occurred";
        }

    }

    protected getAttributeType() {
        return JsrunnerAll;
    }

    protected isFieldHelper() {
        return this.attrs.fieldhelper;
    }
}

jsrunnerApp.component("jsRunner", {
    bindings: pluginBindings,
    controller: JsrunnerController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="jsRunnerDiv">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" ng-bind-html="::$ctrl.stem"></p>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || $ctrl.readonly"
            ng-click="$ctrl.checkFields()">
        {{::$ctrl.buttonText()}}
    </button>
    <a href="" ng-if="$ctrl.edited" ng-click="$ctrl.initCode()">{{::$ctrl.resetText}}</a>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <pre ng-if="$ctrl.print">{{$ctrl.print}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
    <pre ng-if="::$ctrl.isFieldHelper()">{{$ctrl.fieldlist}}</pre>
</div>
`,
});
