/**
 * Defines the client-side implementation of numericfield/label plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, nullable, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to, valueOr} from "tim/util/utils";

const numericfieldApp = angular.module("numericfieldApp", ["ngSanitize"]);
export const moduleDefs = [numericfieldApp];

const NumericfieldMarkup = t.intersection([
    t.partial({
        tag: nullable(t.string),
        inputplaceholder: nullable(t.number),
        inputstem: nullable(t.string),
        initnumber: nullable(t.number),
        validinput: nullable(t.string),
        errormessage: nullable(t.string),
        readOnlyStyle: nullable(t.string),
        step: nullable(t.number),
        arrows: t.boolean,
        wheel: t.boolean,
        verticalkeys: t.boolean,
        autosave: t.boolean,
        nosave: t.boolean,
        ignorestyles: t.boolean,
        clearstyles: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        autoupdate: withDefault(t.number, 500),
        autoUpdateTables: withDefault(t.boolean, true),
        cols: withDefault(t.number, 6),
    }),
]);
const NumericfieldAll = t.intersection([
    t.partial({}),
    t.type({
        info: Info,
        markup: NumericfieldMarkup,
        preview: t.boolean,
        state: nullable(t.type({
            c: t.union([t.string, t.number, t.null]),
            styles: nullable(t.dictionary(t.string, t.string))})),
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
    private saveResponse: { saved: boolean, message: (string | undefined) } = {saved: false, message: undefined};
    private preventedAutosave = false;
    private styles: {[index: string]: string} = {};
    private saveCalledExternally = false;

    getDefaultMarkup() {
        return {};
    }

    /**
     * Returns (user) defined text for the button.
     */
    buttonText() {
        return super.buttonText() || null;
    }

    get valueOrEmpty(): string {
        return valueOr<string | number, string>(this.numericvalue, "").toString();
    }

    $onInit() {
        super.$onInit();
        const state = this.attrsall.state && this.attrsall.state.c;
        if (state !== undefined) {
            if (typeof state === "number") {
                this.numericvalue = state;
            } else if (state !== null) {
                // TODO: parseFloat accepts too much like "6hello", should have a more accurate float check.
                this.numericvalue = parseFloat(state.replace(",", "."));
                if (isNaN(this.numericvalue)) {
                    this.numericvalue = undefined;
                    if (state !== "") {
                        this.errormessage = `State is NaN (${state}); showing empty value.`;
                    }
                }
            }
        }
        if ( !this.attrs.wheel ) { this.element.bind("mousewheel DOMMouseScroll", () => false); }
        if ( !this.attrs.verticalkeys ) { this.element.bind("keydown",
            (e) => { if ( e.which == 38 || e.which == 40 ) { e.preventDefault(); } }); }
        this.modelOpts = {debounce: {blur: 0}};
        this.vctrl.addTimComponent(this, this.attrs.tag );
        this.initialValue = this.numericvalue;
        if (this.attrsall.state && this.attrsall.state.styles && !this.attrs.ignorestyles) {
            this.applyStyling(this.attrsall.state.styles);
        }
    }

    /**
     * Returns the name given to the plugin.
     */
    getName(): string | undefined {
        // if (this.attrs.tag) {
        //     return this.attrs.tag;
        // }
        const taskId = this.pluginMeta.getTaskId();
        if (taskId) {
            return taskId.split(".")[1];
        }
    }

    /**
     * Returns (user) content in string form.
     * Not used in numericfield plugin, but promised to be implemented in ITimComponent.
     */
    getContent(): string {
        return this.valueOrEmpty;
    }

    /**
     * Save method for other plguins, needed by e.g. multisave plugin.
     */
    async save() {
        this.saveCalledExternally = true;
        return this.saveText();
    }

    resetField(): undefined {
        this.initCode();
        this.applyStyling({});
        return undefined;
    }

    // TODO: Do not support setAnswer if an attribute says not to
    supportsSetAnswer(): boolean {
        return true;
    }

    // TODO: Use answer content as arg or entire IAnswer?
    setAnswer(content: { [index: string]: any }): { ok: boolean, message: (string | undefined) } {
        let message;
        let ok = true;
        // TODO: should receiving empty answer reset to defaultnumber or clear field?
        if (Object.keys(content).length == 0) {
            this.resetField();
        } else {
            try {
                const parsed = parseFloat(content.c);
                if (isNaN(parsed)) {
                    this.numericvalue = undefined;
                    ok = false;
                    message = "Value at \"c\" was not a valid number";
                } else {
                    this.numericvalue = parsed;
                }
            } catch (TypeError) {
                this.numericvalue = undefined;
                ok = false;
                message = "Couldn't find related content (\"c\")";
            }
            if (!this.attrs.ignorestyles) {
                this.applyStyling(content.styles);
            }
        }
        this.initialValue = this.numericvalue;
        return {ok: ok, message: message};

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
        if (this.numericvalue == undefined) {
            if (this.attrs.initnumber === 0) {
                this.numericvalue = 0;
            }
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
        } else {
            this.saveResponse.saved = false;
            this.saveResponse.message = undefined;
            return this.saveResponse;
        }
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Autosaver used by ng-blur in textfieldApp component.
     * Needed to seperate from other save methods because of the if-structure.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    autoSave() {
        this.saveCalledExternally = false;
        if (this.preventedAutosave) {
            this.preventedAutosave = false;
            return;
        }
        if (this.attrs.autosave || this.attrs.autosave === undefined) {
            this.saveText();
        }
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
     * Used to define readOnlyStyle in angular, either input or span.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isPlainText() {
        return (this.attrs.readOnlyStyle == "plaintext" && window.location.pathname.startsWith("/view/"));
    }

    /**
     * Parses "styles" from the plugin answer that were saved by tableForm
     * For now only backgroundColor is supported
     * See TODOs at textfield
     */
    applyStyling(styles: {[index: string]: string}) {
        if (!styles || Object.keys(styles).length == 0) {
            this.styles = {};
            return;
        }
        if (styles.backgroundColor) {
            this.styles.backgroundColor = styles.backgroundColor;
        }
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
        return regExpChecker.test(this.valueOrEmpty);
    }

    // noinspection JSUnusedGlobalSymbols,JSMethodCanBeStatic
    /**
     * Returns focus on next HTML field.
     * Used by keydown (Enter) in angular.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    changeFocus() {
        const inputfields = document.querySelectorAll("numericfield-runner input, textfield-runner input");
        for (let i = 0; i < inputfields.length; ++i) {
            const selectedfield = inputfields[i] as HTMLInputElement;
            if (selectedfield === document.activeElement && inputfields[i + 1]) {
                const nextfield = inputfields[i + 1] as HTMLInputElement;
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
        return (!this.attrs.nosave && this.initialValue != this.numericvalue);
    }

    /**
     * Actual saver, called by different save alternatives implemented above.
     * @param nosave true/false parameter boolean checker for the need to save
     */
    async doSaveText(nosave: boolean) {
        this.errormessage = "";
        if (this.attrs.validinput) {
            if (!this.validityCheck(this.attrs.validinput)) {
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
                c: this.numericvalue,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to($http.put<{web: {result: string, error?: string, clear?: boolean}}>(url, params));
        this.isRunning = false;
        if (r.ok) {
            const data = r.result.data;
            if (data.web.error) {
                this.errormessage = data.web.error;
            }
            this.result = data.web.result;
            if (this.result === "saved") {
                this.initialValue = this.numericvalue;
                this.hideSavedText = false;
                this.redAlert = false;
                this.saveResponse.saved = true;
            }
            if (data.web.clear) {
                this.applyStyling({});
            }
            this.saveResponse.message = this.errormessage;
            if (this.vctrl && !this.saveCalledExternally) {
                const tid = this.getTaskId();
                if (tid) {
                    if (this.attrs.autoUpdateTables) {
                        this.vctrl.updateAllTables([tid]);
                    }
                    if (this.vctrl.docSettings.form_mode) {
                        const duplicates = this.vctrl.getTimComponentArray(tid);
                        if (duplicates && duplicates.length > 1) {
                            this.vctrl.updateFields([tid]);
                            // for (const dup of duplicates) {
                            //     dup.setAnswer({"c": this.numericvalue, "styles": this.styles})
                            // }
                        }
                    }
                }
            }
        } else {
            this.errormessage = r.result.data.error;
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
    <p class="stem" ng-if="::$ctrl.stem" ng-bind-html="::$ctrl.stem"></p>
    <div class="form-inline">
     <label><span>
      <span ng-bind-html="::$ctrl.inputstem"></span>
      <span ng-if="::!$ctrl.isPlainText()" ng-class="::{noarrows: (!$ctrl.attrs.arrows)}">
        <input type="number"
               style="width: {{::$ctrl.cols}}em"
               step="{{ $ctrl.stepCheck() }}"
               class="form-control"
               ng-model="$ctrl.numericvalue"
               ng-blur="$ctrl.autoSave()"
               ng-keydown="$event.keyCode === 13 && $ctrl.autoSave() && $ctrl.changeFocus()"
               ng-model-options="::$ctrl.modelOpts"
               ng-change="$ctrl.checkNumericfield()"
               ng-trim="false"
               ng-readonly="::$ctrl.readonly"
               uib-tooltip="{{ $ctrl.errormessage }}"
               tooltip-is-open="$ctrl.f.$invalid && $ctrl.f.$dirty"
               tooltip-trigger="mouseenter"
               placeholder="{{::$ctrl.inputplaceholder}}"
               ng-class="{warnFrame: ($ctrl.isUnSaved() && !$ctrl.redAlert), alertFrame: $ctrl.redAlert}"
               ng-style="$ctrl.styles">
      </span>
      <!--<span ng-if="::$ctrl.isPlainText()" style="float:left;" ng-bind-html="$ctrl.inputstem + " " + $ctrl.numericvalue">{{$ctrl.numericvalue}}</span> -->
      <span ng-if="::$ctrl.isPlainText()" class="plaintext" style="width: {{::$ctrl.cols}}em">{{$ctrl.numericvalue}}</span>
     </span></label>
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
