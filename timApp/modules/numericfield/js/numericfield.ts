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
        inputchecker: nullable(t.string),
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
    private notSavedNumber?: number;
    private errormessage = "";
    private isSaved = true;

    getDefaultMarkup() {
        return {};
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
        this.numericvalue = valueOr(this.attrsall.numericvalue, this.attrs.initnumber || undefined);
        this.modelOpts = {debounce: this.autoupdate};
        this.vctrl.addTimComponent(this);
        this.notSavedNumber = this.numericvalue;
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
    save(): undefined {
        this.saveText();
        return undefined;
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
        this.numericvalue = this.attrs.initnumber || undefined;
        this.notSavedNumber = this.numericvalue;
        this.error = undefined;
        this.result = undefined;
    }

    /**
     * Redirects save request to actual save method.
     * Used as e.g. timButton ng-click event.
     */
    saveText() {
        if (this.notSaved()) {
            this.doSaveText(false);
        }
        else {
            return undefined;
        }
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Autosaver used by ng-blur in textfieldApp component.
     * Needed to seperate from other save methods because of the if-structure.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    autoSave() {
        if (this.attrs.autosave) this.doSaveText(false);
    }

    /**
     * Method to check numeric input type for stringified numericfield.
     * Used as e.g. to define negative or positive numeric input [0-9]+.
     * @param re inputchecker defined by given attribute.
     */
    validityCheck(re: string) {
        let regExpChecker = new RegExp(re);
        return regExpChecker.test(this.numericvalue.toString());
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Checking if input has been changed since the last Save or initialization.
     * Displays a red thick marker at the right side of the inputfield to notify users
     * about unsaved changes.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    notSaved() {
        if (this.notSavedNumber != this.numericvalue) {
            this.isSaved = true;
        }
        return (this.notSavedNumber != this.numericvalue);
    }


    /**
     * Actual saver, called by different save alternatives implemented above.
     * @param true/false parameter boolean checker for the need to save
     */
    async doSaveText(nosave: boolean) {
        this.errormessage = "";
        if (this.attrs.inputchecker) {
            if(!this.validityCheck(this.attrs.inputchecker)) {
                this.errormessage = "Input does not pass the RegEx: " + this.attrs.inputchecker;
                return;
            }
        }
        /* No visible text
        this.error = "... saving ..."; */
        this.errormessage = "";
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
            this.notSavedNumber = this.numericvalue;
            this.isSaved = false;
        } else {
            this.errormessage = "Infinite loop or some other error?";
        }
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
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        <input type="number"
               style="width: {{::$ctrl.cols}}em"
               class="form-control"
               ng-model="$ctrl.numericvalue"
               ng-model-options="{ debounce: {'blur': 0} } "
               ng-blur="$ctrl.autoSave()"
               ng-keydown="$event.keyCode === 13 && $ctrl.saveText() && elem.next().focus()"
               ng-model-options="::$ctrl.modelOpts"
               ng-change="$ctrl.checkNumericfield()"
               ng-trim="false"
               ng-readonly="::$ctrl.readonly"
               uib-tooltip="{{ $ctrl.errormessage }}"
               tooltip-is-open="$ctrl.f.$invalid && $ctrl.f.$dirty"
               tooltip-trigger="mouseenter"
               placeholder="{{::$ctrl.inputplaceholder}}"
               ng-class="{warnFrame: $ctrl.notSaved()}">
        </span></label>
    </div>
    <div ng-if="$ctrl.error" style="font-size: 12px" ng-bind-html="$ctrl.error"></div>
    <button class="timButton"
            ng-if="$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || !$ctrl.numericvalue || $ctrl.readonly"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    <pre class="savedtext" ng-if="$ctrl.isSaved && $ctrl.buttonText()">Saved!</pre> 
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div> `,
});
