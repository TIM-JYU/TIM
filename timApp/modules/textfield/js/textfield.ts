/**
 * Defines the client-side implementation of an example plugin (a textfieldndrome checker).
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {GenericPluginMarkup, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";
import {number} from "../../../static/scripts/jspm_packages/npm/io-ts@1.4.1/lib";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";

const textfieldApp = angular.module("textfieldApp", ["ngSanitize"]);
export const moduleDefs = [textfieldApp];

const TextfieldMarkup = t.intersection([
    t.partial({
        followid: t.string, //maybe not needed
        inputplaceholder: nullable(t.string),
        inputstem: t.string,
        initword: t.string,
        buttonText: nullable(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const TextfieldAll = t.intersection([
    t.partial({
        userword: t.string,
    }),
    t.type({markup: TextfieldMarkup}),
]);

class TextfieldController extends PluginBase<t.TypeOf<typeof TextfieldMarkup>, t.TypeOf<typeof TextfieldAll>, typeof TextfieldAll> implements ITimComponent {
    private result?: string;
    private error?: string;
    private isRunning = false;
    private userword = "";
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"
    private vctrl!: ViewCtrl;

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
       return super.buttonText() || null;
    }

    $onInit() {
        super.$onInit();
        this.userword = this.attrsall.userword || this.attrs.initword || "";
        this.modelOpts = {debounce: this.autoupdate};
        if (this.attrs.followid) {
            this.vctrl.addTimComponent(this);
        }
    }

    getContent(): string {
        return this.userword;
    }

    getNumericContent(): number {
        return -1; // not used with textfield plugin, but promised to implement in ITimComponent
    }

    save(): string {
        return this.userword; // not used with textfield plugin, but promised to implement in ITimComponent
    }

    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    get inputstem() {
        return this.attrs.inputstem || "";
    }

    get cols() {
        return this.attrs.cols;
    }

    initCode() {
        this.userword = this.attrs.initword || "";
        this.error = undefined;
        this.result = undefined;
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
        return TextfieldAll;
    }
}

textfieldApp.component("textfieldRunner", {
    bindings: {
        json: "@",
    },
    controller: TextfieldController,
    template: `
<div class="textfieldNoSaveDiv">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>   
        <input type="string"
               class="form-control"
               ng-model="$ctrl.userword"
               ng-model-options="::$ctrl.modelOpts"
               ng-change="$ctrl.checkTextfield()"
               ng-trim="false"
               placeholder="{{::$ctrl.inputplaceholder}}"
               size="{{::$ctrl.cols}}"></span></label>
    </div>
    <button class="timButton"
            ng-if="$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || !$ctrl.userword"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});