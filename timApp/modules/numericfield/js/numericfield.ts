/**
 * Defines the client-side implementation of numericfield plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl"
import {GenericPluginMarkup, Info, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils"; //tarvitaan reset-metodille, jos halutaan toteuttaa

const numericfieldApp = angular.module("numericfieldApp", ["ngSanitize"]);
export const moduleDefs = [numericfieldApp];

const NumericfieldMarkup = t.intersection([
    t.partial({
        followid: nullable(t.string),
        inputplaceholder: nullable(t.number),
        inputstem: nullable(t.string),
        initnumber: nullable(t.number),
        buttonText: nullable(t.string),
        autosave: t.boolean
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const NumericfieldAll = t.intersection([
    t.partial({
        numericvalue: t.number,
    }),
    t.type({
        info: Info,
        markup: NumericfieldMarkup,
        preview: t.boolean,
    }),
]);

class NumericfieldController extends PluginBase<t.TypeOf<typeof NumericfieldMarkup>, t.TypeOf<typeof NumericfieldAll>, typeof NumericfieldAll> implements ITimComponent {
    private result?: string;
    private error?: string;
    private isRunning = false;
    private numericvalue?: number;
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"
    private vctrl!: ViewCtrl;

    getDefaultMarkup() {
        return {};
    }

    /**
     * Method returning (user) defined text for the button
     */
    buttonText() {
        return super.buttonText() || null;
    }

    /**
     * Settings on every new page load
     */
    $onInit() {
        super.$onInit();
        this.numericvalue = this.attrsall.numericvalue || this.attrs.initnumber || undefined;
        this.modelOpts = {debounce: this.autoupdate};
        if (this.attrs.followid) {
            this.vctrl.addTimComponent(this);
        }
    }

    /**
     * Will run after $onInit - reserved for possible eventhandlers OR to be removed
     */
    /*
    $postLink() {
        if (this.userword == "") {
            this.userword = this.attrs.initword || "";
        }
    }
    */

    /**
     * Method to return (user) content in string form.
     * Not used in textfield plugin, but promised to be implemented in ITimComponent.
     */
    getContent(): string {
        return;
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Method to return (user) content in numeric form.
     */
    getNumericContent(): number {
        return this.numericvalue;
    }

    /**
     * Save method for other plguins, needed by e.g. multisave plugin
     */
    save(): undefined {
        this.saveText();
        return undefined;
    }

    /**
     * Method used for autoupdating
     */
    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    /**
     * Method to return (user) set inputstem (textfeed before userinput box)
     */
    get inputstem() {
        return this.attrs.inputstem || null;
    }

    /**
     * Method to return (user) set col size (size of the field)
     */
    get cols() {
        return this.attrs.cols;
    }

    /**
     * Method to initialize content
     */
    initCode() {
        this.numericvalue = this.attrs.initnumber || undefined;
        this.error = undefined;
        this.result = undefined;
    }

    /**
     * Method to (re)direct save request to actual save method.
     * Used as e.g. timButton ng-click event.
     */
    saveText() {
        if (this.attrs.autosave == false) return;
        this.doSaveText(false);
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Autosave method is used by ng-blur in textfieldApp component.
     * Needed to seperate from other save methods because of the if-structure.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    autoSave() {
        if (this.attrs.autosave == false) return;
        this.doSaveText(false);
    }

    /**
     * Actual save method, called by different save alternatives implemented above.
     * @param true/false parameter boolean checker for the need to save
     */
    async doSaveText(nosave: boolean) {
        this.error = "... saving ...";
        this.isRunning = true;
        this.result = undefined;
        const params = {
            input: {
                nosave: false,
                numericvalue: this.numericvalue,
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
        return NumericfieldAll;
    }
}

/**
 * numericfieldRunner as HTML component.
 */
numericfieldApp.component("numericfieldRunner", {
    bindings: {
        json: "@",
    },
    controller: NumericfieldController,
    template: `
<div class="numericfieldNoSaveDiv">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>   
        <input type="number"
               class="form-control"
               ng-model="$ctrl.numericvalue"
               ng-blur="$ctrl.saveText()"
               ng-keydown="$event.keyCode === 13 && $ctrl.saveText()"
               ng-model-options="::$ctrl.modelOpts"
               ng-change="$ctrl.checkNumericfield()"
               ng-trim="false"
               placeholder="{{::$ctrl.inputplaceholder}}"
               size="{{::$ctrl.cols}}"></span></label>
    </div>
    <button class="timButton"
            ng-if="$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || !$ctrl.numericvalue"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div> `,
});
