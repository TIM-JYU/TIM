/**
 * Defines the client-side implementation of an example plugin (a palindrome checker).
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {
    GenericPluginMarkup,
    GenericPluginTopLevelFields,
    nullable,
    PluginBase,
    pluginBindings,
    withDefault
} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";

const jsrunnerApp = angular.module("jsrunnerApp", ["ngSanitize"]);
export const moduleDefs = [jsrunnerApp];

const JsrunnerMarkup = t.intersection([
    t.partial({
        fields: t.array(t.string),
        program: nullable(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
    }),
]);
const JsrunnerAll = t.intersection([
    t.partial({
        markup: JsrunnerMarkup,
    }),
    GenericPluginTopLevelFields,
    t.type({markup: JsrunnerMarkup}),
]);

class JsrunnerController extends PluginBase<t.TypeOf<typeof JsrunnerMarkup>, t.TypeOf<typeof JsrunnerAll>, typeof JsrunnerAll> {
    private error?: string;
    private isRunning = false;
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"

    getDefaultMarkup() {
        return {};
    }

    get program() {
        return this.attrs.program;
    }

    buttonText() {
        return super.buttonText() || "Laske";
    }

    $onInit() {
        super.$onInit();
        this.modelOpts = {debounce: this.autoupdate};
    }

    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    checkFields() {
        this.doCheckFields(false);
    }

    async doCheckFields(nosave: boolean) {
        this.error = "... undefined or no rights to fields ...";
        this.isRunning = true;
        const params = {
            input: {
                fields: this.attrs.fields,
                nosave: false,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to($http.put<{web: {result: string, error?: string}}>(url, params));
        this.isRunning = false;
        if (r.ok) {
            const data = r.result.data;
            this.error = data.web.error;
        } else {
            this.error = "Infinite loop or some other error?";
        }
    }

    protected getAttributeType() {
        return JsrunnerAll;
    }
}

jsrunnerApp.component("jsRunner", {
    bindings: pluginBindings,
    controller: JsrunnerController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="csRunDiv no-popup-menu">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" ng-bind-html="::$ctrl.stem"></p>
    <!--<div class="form-inline"><label>{{::$ctrl.inputstem}} <span>-->
        <!--<input type="text"-->
               <!--class="form-control"-->
               <!--ng-model="$ctrl.userword"-->
               <!--ng-model-options="::$ctrl.modelOpts"-->
               <!--ng-change="$ctrl.checkPalindrome()"-->
               <!--ng-trim="false"-->
               <!--ng-readonly="::$ctrl.readonly"-->
               <!--placeholder="{{::$ctrl.inputplaceholder}}"-->
               <!--size="{{::$ctrl.cols}}"></span></label>-->
        <!--<span class="unitTestGreen" ng-if="$ctrl.runTestGreen && $ctrl.userword">OK</span>-->
        <!--<span class="unitTestRed" ng-if="!$ctrl.runTestGreen">Wrong</span>-->
    <!--</div>-->
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || $ctrl.readonly"
            ng-click="$ctrl.checkFields()">
        {{::$ctrl.buttonText()}}
    </button>
    <a href="" ng-if="$ctrl.edited" ng-click="$ctrl.initCode()">{{::$ctrl.resetText}}</a>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
