/**
 * Defines the client-side implementation of numericfield/label plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, nullable, PluginBase, pluginBindings, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to, valueOr} from "tim/util/utils";

const numericfieldApp = angular.module("numericfieldApp", ["ngSanitize"]);
export const moduleDefs = [numericfieldApp];

const NumericfieldMarkup = t.intersection([
    t.partial({
        followid: nullable(t.string),
        inputplaceholder: nullable(t.number),
        inputstem: nullable(t.string),
        initnumber: nullable(t.number),
        buttonText: nullable(t.string),
        validinput: nullable(t.string),
        errormessage: nullable(t.string),
        labelStyle: nullable(t.string),
        step: nullable(t.number),
        autosave: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 1),
    }),
]);
const NumericfieldAll = t.intersection([
    t.partial({
        numericvalue: (t.number),
    }),
    t.type({
        info: Info,
        markup: NumericfieldMarkup,
        preview: t.boolean,
    }),
]);

class NumericfieldController extends PluginBase<t.TypeOf<typeof NumericfieldMarkup>, t.TypeOf<typeof NumericfieldAll>, typeof NumericfieldAll> implements ITimComponent {
    private result?: string;
    private isRunning = false;
    private numericvalue?: number;
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"
    private vctrl!: ViewCtrl;
    private initialValue?: number;
    private errormessage = "";
    private hideSavedText = true;
    private redAlert = false;
    private saveResponse: {saved:boolean, message: (string | undefined)} = {saved:false, message:undefined}

    getDefaultMarkup() {
        return { };
    }

    /**
     * Returns (user) defined text for the button.
     */
    buttonText() {
        return super.buttonText() || null;
    }

    /**
     * Settings on every new page load.
     */
    $onInit() {
        super.$onInit();
        this.numericvalue = valueOr(this.attrsall.numericvalue, this.attrs.initnumber || undefined );
        if(this.numericvalue == undefined)
        {
            if(this.attrs.initnumber === 0)
                this.numericvalue = 0;
        }
        this.modelOpts = {debounce: this.autoupdate};
        if(!this.attrs.labelStyle)
            this.vctrl.addTimComponent(this);
        this.initialValue = this.numericvalue;
    }

    /**
     * Returns the name given to the plugin.
     */
    getName(): string | undefined {
        if (this.attrs.followid) return this.attrs.followid;
        const taskId = this.pluginMeta.getTaskId();
        if (taskId) return taskId.split(".")[1];
    }

    /**
     * Will run after $onInit - reserved for possible eventhandlers OR to be removed.
     */
    /*
    $postLink() {
        if (this.userword == "") {
            this.userword = this.attrs.initword || "";
        }
    }
    */

    /**
     * Returns (user) content in string form.
     * Not used in numericfield plugin, but promised to be implemented in ITimComponent.
     */
    getContent(): string {
        return this.numericvalue.toString();
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Returns (user) content in numeric form.
     */
    getNumericContent(): number {
        return this.numericvalue;
    }

    /**
     * Save method for other plguins, needed by e.g. multisave plugin.
     */
    async save() {
        return this.saveText();
    }

    resetField(): undefined {
        this.initCode();
        return undefined;
    }

    /**
     * Method for autoupdating.
     */
    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    /**
     * Returns (user) set inputstem (textfeed before userinput box).
     */
    get inputstem() {
        return this.attrs.inputstem || null;
    }

    /**
     * Returns (user) set col size (size of the field).
     */
    get cols() {
        return this.attrs.cols;
    }

    /**
     * Initialize content.
     */
    initCode() {
        this.numericvalue = this.attrs.initnumber || undefined
        if (this.numericvalue == undefined) {
            if (this.attrs.initnumber === 0)
                this.numericvalue = 0;
        }
        this.initialValue = this.numericvalue;
        this.result = undefined;
    }

    /**
     * Redirects save request to actual save method.
     * Used as e.g. timButton ng-click event.
     */
    saveText() {
        if (this.isUnSaved()) {
            return this.doSaveText(false);
        }
        else {
            this.saveResponse.saved = false;
            this.saveResponse.message = undefined;
            return this.saveResponse;
        }
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Autosaver used by ng-blur in numericfield-Runner component.
     * Needed to separate from other save methods because of the if-structure.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    autoSave() {
        if (this.attrs.autosave) this.doSaveText(false);
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Stepper used by step in numericfield-Runner component.
     * Used to define range of each numeric step for scroll up/down, e.g. 0.25 or 1.0.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    stepCheck() {
        return (this.attrs.step);
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Returns true value, if label is set to plaintext.
     * Used to define labelstyle in angular, either input or span.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isPlainText() {
        return (this.attrs.labelStyle == "plaintext");
    }

    /**
     * Method to check numeric input type for stringified numericfield.
     * Used as e.g. to define negative or positive numeric input [0-9]+.
     * @param re validinput defined by given attribute.
     */
    validityCheck(re: string) {
        if (this.numericvalue === null) {
            return true;
        }
        const regExpChecker = new RegExp(re);
        return regExpChecker.test(this.numericvalue.toString());
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Returns focus on next HTML field.
     * Used by keydown (Enter) in angular.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    changeFocus() {
        const inputfields = document.querySelectorAll("numericfield-runner input, textfield-runner input");
        for (let i = 0; i < inputfields.length; ++i) {
            const selectedfield = inputfields[i] as HTMLInputElement;
            if (selectedfield === document.activeElement && inputfields[i+1]) {
                let nextfield = inputfields[i+1] as HTMLInputElement;
                return nextfield.focus();
            }
        }
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Checking if input has been changed since the last Save or initialization.
     * Displays a red thick marker at the right side of the inputfield to notify users
     * about unsaved changes.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isUnSaved() {
        if (this.initialValue != this.numericvalue) {
            this.hideSavedText = true;
        }
        return (this.initialValue != this.numericvalue);
    }


    /**
     * Actual saver, called by different save alternatives implemented above.
     * @param true/false parameter boolean checker for the need to save
     */
    async doSaveText(nosave: boolean) {
        this.errormessage = "";
        if (this.attrs.validinput) {
            if(!this.validityCheck(this.attrs.validinput)) {
                this.errormessage = this.attrs.errormessage || "Input does not pass the RegEx: " + this.attrs.validinput;
                this.redAlert = true;
                this.saveResponse.message = this.errormessage;
                return this.saveResponse;
            }
        }
        /* No visible text
        this.error = "... saving ..."; */
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
            this.errormessage = data.web.error;
            this.result = data.web.result;
            if (this.result == "saved") {
                this.initialValue = this.numericvalue;
                this.hideSavedText = false;
                this.redAlert = false;
                this.saveResponse.saved = true;
            }
            this.saveResponse.message = this.errormessage;
        } else {
            this.errormessage = "Infinite loop or some other error?";
        }
        return this.saveResponse;
    }

    protected getAttributeType() {
        return NumericfieldAll;
    }
}

/**
 * Introducing numericfieldRunner as HTML component.
 * Attribute style used to force user given cols to determine size.
 * Developers note: attribute step="0.01" can determine lower step size between scroll-up/down.
 */
numericfieldApp.component("numericfieldRunner", {
    bindings: pluginBindings,
    controller: NumericfieldController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="numericfieldNoSaveDiv">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p class="stem" ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline">
    <label>{{::$ctrl.inputstem}} <span>
        <input type="number"
               ng-if="::!$ctrl.isPlainText()"
               style="width: {{::$ctrl.cols}}em"
               step="{{ $ctrl.stepCheck() }}"
               class="form-control"
               ng-model="$ctrl.numericvalue"
               ng-model-options="{ debounce: {'blur': 0} } "
               ng-blur="$ctrl.autoSave()"
               ng-keydown="$event.keyCode === 13 && $ctrl.saveText() && $ctrl.changeFocus()"
               ng-model-options="::$ctrl.modelOpts"
               ng-change="$ctrl.checkNumericfield()"
               ng-trim="false"
               ng-readonly="::$ctrl.readonly"
               uib-tooltip="{{ $ctrl.errormessage }}"
               tooltip-is-open="$ctrl.f.$invalid && $ctrl.f.$dirty"
               tooltip-trigger="mouseenter"
               placeholder="{{::$ctrl.inputplaceholder}}"
               ng-class="{warnFrame: ($ctrl.isUnSaved() && !$ctrl.redAlert), alertFrame: $ctrl.redAlert}">
               </span></label>
        <span ng-if="::$ctrl.isPlainText()" style="float:left;">{{$ctrl.numericvalue}}</span>
    </div>
    <div ng-if="$ctrl.error" style="font-size: 12px" ng-bind-html="$ctrl.error"></div>
    <button class="timButton"
            ng-if="$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || $ctrl.readonly"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    <p class="savedtext" ng-if="!$ctrl.hideSavedText && $ctrl.buttonText()">Saved!</p> 
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div> `,
});
