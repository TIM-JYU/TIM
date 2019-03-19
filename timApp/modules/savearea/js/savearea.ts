/**
 * Defines the client-side implementation of an example plugin (a saveareandrome checker).
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {GenericPluginMarkup, Info, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";
import {number} from "../../../static/scripts/jspm_packages/npm/io-ts@1.4.1/lib";

const saveareaApp = angular.module("saveareaApp", ["ngSanitize"]);
export const moduleDefs = [saveareaApp];

const SaveareaMarkup = t.intersection([
    t.partial({
        initword2: t.number,
        inputplaceholder2: nullable(t.number),
        inputstem2: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const SaveareaAll = t.intersection([
    t.partial({
        demopisteet: t.number,
    }),
    t.type({
        info: Info,
        markup: SaveareaMarkup,
        preview: t.boolean,
    }),
]);

class SaveareaController extends PluginBase<t.TypeOf<typeof SaveareaMarkup>, t.TypeOf<typeof SaveareaAll>, typeof SaveareaAll> {
    private result?: string;
    private error?: string;
    private isRunning = false;
    private demopisteet = number;
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "Tallenna";
    }

    $onInit() {
        super.$onInit();
        this.modelOpts = {debounce: this.autoupdate};
    }

    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    get inputstem2() {
        return this.attrs.inputstem2 || null;
    }

    get cols() {
        return this.attrs.cols;
    }

    initCode() {
        this.demopisteet = number;
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
                demopisteet: this.demopisteet,
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
        return SaveareaAll;
    }
}

saveareaApp.component("saveareaRunner", {
    bindings: {
        json: "@",
    },
    controller: SaveareaController,
    template: `
<div class="no-popup-menu">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline">
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    </div>
    <a href="" ng-if="$ctrl.edited" ng-click="$ctrl.initCode()">{{::$ctrl.resetText}}</a>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>

`,
});