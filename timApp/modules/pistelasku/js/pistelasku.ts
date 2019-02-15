/**
 * Defines the client-side implementation of an example plugin (a palindrome checker).
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {GenericPluginMarkup, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";

const pistelaskuApp = angular.module("pistelaskuApp", ["ngSanitize"]);

const PistelaskuMarkup = t.intersection([
    t.partial({
        initword: t.number,
        inputplaceholder: nullable(t.number),
        inputstem: t.number,
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
        userword: t.number,
    }),
    t.type({markup: PaliMarkup}),
]);

function isPalindrome(s: string) {
    let sc = s.toLowerCase();
    sc = sc.replace(/[^a-zåöä]/g, "");
    for (let i1 = 0, i2 = sc.length - 1; i1 < i2; i1++, i2--) {
        if (sc[i1] !== sc[i2]) {
            return false;
        }
    }
    return true;
}

class PaliController extends PluginBase<t.TypeOf<typeof PaliMarkup>, t.TypeOf<typeof PaliAll>, typeof PaliAll> {
    private result?: string;
    private error?: string;
    private isRunning = false;
    private userword = "";
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
        this.checkPalindrome();
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

    get cols() {
        return this.attrs.cols;
    }

    get resetText() {
        return valueDefu(this.attrs.resetText, "Reset");
    }

    checkPalindrome() {
        const is = isPalindrome(this.userword);
        this.runTestGreen = is;
        return is;
    }

    initCode() {
        this.userword = this.attrs.initword || "";
        this.error = undefined;
        this.result = undefined;
        this.checkPalindrome();
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
                paliOK: this.checkPalindrome(),
                userword: this.userword,
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
        return PaliAll;
    }
}

paliApp.component("paliRunner", {
    bindings: {
        json: "@",
    },
    controller: PaliController,
    template: `
<div class="csRunDiv no-popup-menu">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        <input type="text"
               class="form-control"
               ng-model="$ctrl.userword"
               ng-model-options="::$ctrl.modelOpts"
               ng-change="$ctrl.checkPalindrome()"
               ng-trim="false"
               placeholder="{{::$ctrl.inputplaceholder}}"
               size="{{::$ctrl.cols}}"></span></label>
        <span class="unitTestGreen" ng-if="$ctrl.runTestGreen && $ctrl.userword">OK</span>
        <span class="unitTestRed" ng-if="!$ctrl.runTestGreen">Wrong</span>
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || !$ctrl.userword"
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
