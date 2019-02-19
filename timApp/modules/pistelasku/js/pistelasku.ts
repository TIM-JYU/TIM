/**
 * Defines the client-side implementation of an example plugin (a pistelaskundrome checker).
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {GenericPluginMarkup, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";
import {number} from "../../../static/scripts/jspm_packages/npm/io-ts@1.4.1/lib";

const pistelaskuApp = angular.module("pistelaskuApp", ["ngSanitize"]);
export const moduleDefs = [pistelaskuApp];

const PistelaskuMarkup = t.intersection([
    t.partial({
        initword: t.string,
        inputplaceholder: nullable(t.string),
        inputstem: t.string,
        initword2: t.number,
        inputplaceholder2: nullable(t.number),
        inputstem2: t.number,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const PistelaskuAll = t.intersection([
    t.partial({
        userword: t.string,
        userword2: t.number,
    }),
    t.type({markup: PistelaskuMarkup}),
]);

function isPistelaskundrome(s: string) {
    let sc = s.toLowerCase();
    sc = sc.replace(/[^a-zåöä]/g, "");
    for (let i1 = 0, i2 = sc.length - 1; i1 < i2; i1++, i2--) {
        if (sc[i1] !== sc[i2]) {
            return false;
        }
    }
    return true;
}

class PistelaskuController extends PluginBase<t.TypeOf<typeof PistelaskuMarkup>, t.TypeOf<typeof PistelaskuAll>, typeof PistelaskuAll> {
    private result?: string;
    private error?: string;
    private isRunning = false;
    private userword = "";
    private userword2 = number;
    private runTestGreen = false;
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "Save";
    }

    $onInit() {
        super.$onInit();
        this.userword = this.attrsall.userword || this.attrs.initword || "";
        this.modelOpts = {debounce: this.autoupdate};
        this.checkPistelaskundrome();
    }

    get edited() {
        return this.attrs.initword !== this.userword;
    }

    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    get inputplaceholder() {
        return this.attrs.inputplaceholder || null;
    }

    get inputstem() {
        return this.attrs.inputstem || null;
    }

    get inputstem2() {
        return this.attrs.inputstem2 || null;
    }

    get cols() {
        return this.attrs.cols;
    }

    get resetText() {
        return valueDefu(this.attrs.resetText, "Reset");
    }

    checkPistelaskundrome() {
        const is = isPistelaskundrome(this.userword);
        this.runTestGreen = is;
        return is;
    }

    initCode() {
        this.userword = this.attrs.initword || "";
        this.userword2 = number;
        this.error = undefined;
        this.result = undefined;
        this.checkPistelaskundrome();
    }

    saveText() {
        this.doSaveText(false);
    }

    async doSaveText(nosave: boolean) {
        this.error = "... saving ...";
        this.isRunning = true;
        this.result = undefined;
        const params = {
            input: {
                nosave: false,
                pistelaskuOK: this.checkPistelaskundrome(),
                userword: this.userword,
                userword2: this.userword2,
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
            this.result = data.web.result;
        } else {
            this.error = "Infinite loop or some other error?";
        }
    }

    protected getAttributeType() {
        return PistelaskuAll;
    }
}

pistelaskuApp.component("pistelaskuRunner", {
    bindings: {
        json: "@",
    },
    controller: PistelaskuController,
    template: `
<div class="csRunDiv no-popup-menu">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        <input type="text"
               class="form-control"
               ng-model="$ctrl.userword"
               ng-model-options="::$ctrl.modelOpts"
               ng-change="$ctrl.checkPistelaskundrome()"
               ng-trim="false"
               placeholder="{{::$ctrl.inputplaceholder}}"
               size="{{::$ctrl.cols}}"></span></label>
        <span class="unitTestGreen" ng-if="$ctrl.runTestGreen && $ctrl.userword">OK</span>
        <span class="unitTestRed" ng-if="!$ctrl.runTestGreen">Wrong</span><br>
    </div>
    <div class="form-inline"><label>{{::$ctrl.inputstem2}} <span>   
        <input type="number"
               class="form-control"
               ng-model="$ctrl.userword2"
               ng-model-options="::$ctrl.modelOpts"
               ng-change="$ctrl.checkPistelaskundrome()"
               ng-trim="false"
               placeholder="{{::$ctrl.inputplaceholder2}}"
               size="{{::$ctrl.cols}}"></span></label>
        <span class="unitTestGreen" ng-if="$ctrl.runTestGreen && $ctrl.userword2">OK</span>
        <span class="unitTestRed" ng-if="!$ctrl.runTestGreen">Wrong</span>
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || !$ctrl.userword || !$ctrl.userword2"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    <a href="" ng-if="$ctrl.edited" ng-click="$ctrl.initCode()">{{::$ctrl.resetText}}</a>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
