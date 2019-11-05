/**
 * Defines the client-side implementation of rbfield/label plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, RegexOption, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, nullable, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to, valueOr} from "tim/util/utils";

const rbfieldApp = angular.module("rbfieldApp", ["ngSanitize"]);
export const moduleDefs = [rbfieldApp];

const RbfieldMarkup = t.intersection([
    t.partial({
        tag: nullable(t.string),
        inputplaceholder: nullable(t.string),
        inputstem: nullable(t.string),
        initword: nullable(t.string),
        validinput: nullable(t.string),
        errormessage: nullable(t.string),
        readOnlyStyle: nullable(t.string),
        showname: nullable(t.number),
        autosave: t.boolean,
        isRb: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        cols: withDefault(t.number, 0),
    }),
]);
const RbfieldAll = t.intersection([
    t.partial({
    }),
    t.type({
        info: Info,
        markup: RbfieldMarkup,
        preview: t.boolean,
        state: nullable(t.type({c: t.union([t.string, t.number, t.null])})),
    }),
]);

class RbfieldController extends PluginBase<t.TypeOf<typeof RbfieldMarkup>, t.TypeOf<typeof RbfieldAll>, typeof RbfieldAll> implements ITimComponent {
    private result?: string;
    private isRunning = false;
    private userword: string = "0";
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"
    private vctrl!: ViewCtrl;
    private initialValue: string = "0";
    private errormessage = "";
    private hideSavedText = true;
    private redAlert = false;
    private saveResponse: {saved: boolean, message: (string | undefined)} = {saved: false, message: undefined};
    private preventedAutosave = false;  // looks depracated???
    private rbName: string = "";

    getDefaultMarkup() {
        return {};
    }

    /**
     * Returns (user) defined text for the button.
     */
    buttonText() {
        return super.buttonText() || null;
    }
/*
    makeBoolean(s: string): boolean {
        if ( s == "" ) { return false; }
        if ( s == "0" ) { return false; }
        if ( s == "false" ) { return false; }
        if ( s == "1" ) { return true; }
        return true;
    }
*/
    $onInit() {
        super.$onInit();
        this.rbName = this.rbname;
        const uw = (valueOr(this.attrsall.state && this.attrsall.state.c, this.attrs.initword || "0")).toString();
        this.userword = uw; // this.makeBoolean(uw);

        if (this.attrs.tag) {
            this.vctrl.addTimComponent(this, this.attrs.tag);
        } else {
            this.vctrl.addTimComponent(this);
        }
        this.initialValue = this.userword;
        if (this.attrs.showname ) { this.initCode(); }
    }

    get inputtype(): string {
        return "radio";
    }

    get rbname(): string {
        if ( this.rbName ) { return this.rbName; }
        let n: string = this.getName() || "rb";
        n = n.replace(/[0-9]+/, "");
        this.rbName = n;
        return n;
    }

    /**
     * Returns (user) content in string form.
     */
    getContent(): string {
        return this.userword; //  ? "1" : "0";
    }

    /**
     * Save method for other plugins, needed by e.g. multisave plugin.
     */
    async save() {
        return this.saveText();
    }

    resetField(): undefined {
        this.initCode();
        return undefined;
    }

    supportsSetAnswer(): boolean {
        return true;
    }

    // TODO: Use answer content as arg or entire IAnswer?
    setAnswer(content: { [index: string]: unknown }): { ok: boolean, message: (string | undefined) } {
        let message;
        let ok = true;
        // TODO: should receiving empty answer reset to defaultnumber or clear field?
        if (Object.keys(content).length == 0) {
            this.resetField();
        } else {
            try {
                this.userword = content.c as string;
            } catch (TypeError) {
                this.userword = "";
                ok = false;
                message = "Couldn't find related content (\"c\")";
            }
        }
        this.initialValue = this.userword;
        return {ok: ok, message: message};

    }

    /**
     * Returns (user) set inputstem (textfeed before userinput box).
     */
    get inputstem() {
        return this.attrs.inputstem || "";
    }

    /**
     * Returns (user) set col size (size of the field).
     */
    get cols() {
        if ( !this.attrs.cols ) { return {}; }
        return {width: this.attrs.cols + "em", display: "inline-block"};
    }

    // noinspection JSUnusedGlobalSymbols
    get cbStyle() {
        if ( !this.inputstem && (this.stem || this.header ) ) { return {}; }
        return { // otherwise input stem and cb are vertical
            width: "auto",
        };
    }

    /**
     * Initialize content.
     */
    initCode() {
        this.userword = this.attrs.initword || "";
        this.initialValue = this.userword;
        this.result = undefined;
    }

    /**
     * Redirects save request to actual save method.
     * Used as e.g. timButton ng-click event.
     */
    async saveText() {
        if (this.isUnSaved()) {
            return this.doSaveText(false);
        } else {
            // return {saved: false, message:undefined};
            this.saveResponse.saved = false;
            this.saveResponse.message = undefined;
            return this.saveResponse;
        }
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

    isReadOnly() {
        return (this.attrs.readOnlyStyle == "box" && window.location.pathname.startsWith("/view/")) ? "disable" : "";
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Checking if input has been changed since the last Save or initialization.
     * Displays a red thick marker at the right side of the inputfield to notify users
     * about unsaved changes.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isUnSaved() {
        if (this.initialValue != this.userword) {
            this.hideSavedText = true;
        }
        return (this.initialValue != this.userword);
    }

    setChecked(b: boolean) {
        this.userword = b ? "1" : "0";
        if (this.attrs.autosave || this.attrs.autosave === undefined) {
            // We want to save the plugin regardless of unSaved status to prevent two radio buttons
            // from being checked at the same time.
            this.doSaveText(false);
        }
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Autosaver used by ng-blur in rbfieldApp component.
     * Needed to seperate from other save methods because of the if-structure.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    autoSave() {
        const tid = this.getTaskId();
        if (!tid) {
            return;
        }
        const comps = this.vctrl.getTimComponentsByRegex(`${tid.docId}\.${this.rbname}.*`, RegexOption.DontPrependCurrentDocId);
        const n = this.getName();
        for (const c of comps) {
            if ( c.getName() == n ) { continue; }
            if ( !(c instanceof RbfieldController) ) { continue; }
            c.setChecked(false);
        }
        if (this.preventedAutosave) {
            this.preventedAutosave = false;
            return;
        }
        if (this.attrs.autosave || this.attrs.autosave === undefined) {
            this.saveText();
        }
    }

    /**
     * Actual save method, called by different save alternatives implemented above.
     * @param nosave true/false parameter boolean checker for the need to save
     */
    async doSaveText(nosave: boolean) {
        this.errormessage = "";
        this.isRunning = true;
        const c = this.userword;
        this.result = undefined;
        const params = {
            input: {
                nosave: false,
                c: c,
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
            // TODO: Make angular to show tooltip even without user having to move cursor out and back into the input
            // (Use premade bootstrap method / add listener for enter?)
            this.errormessage = data.web.error || "";
            this.result = data.web.result;
            this.initialValue = this.userword;
            this.hideSavedText = false;
            this.saveResponse.saved = true;
            this.saveResponse.message = this.errormessage;
        } else {
            this.errormessage = r.result.data.error || "Syntax error, infinite loop or some other error?";
        }
        return this.saveResponse;
    }

    protected getAttributeType() {
        return RbfieldAll;
    }
}

/**
 * Introducing rbfieldRunner as HTML component.
 */
rbfieldApp.component("rbfieldRunner", {
    bindings: pluginBindings,
    controller: RbfieldController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="textfieldNoSaveDiv" ng-style="::$ctrl.cols">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p class="stem" ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <!--<form name="$ctrl.f" class="form-inline"> -->
     <!--<label>-->
     <span style="width: 100%">
      <span class="inputstem" ng-bind-html="::$ctrl.inputstem"></span>
      <span  ng-if="::!$ctrl.isPlainText()" ng-class="{warnFrame: ($ctrl.isUnSaved() )  }">
        <!-- <span ng-if="$ctrl.isUnSaved()"  ng-class="{warnFrame: ($ctrl.isUnSaved() )  }">&nbsp;</span> -->
        <input type="radio"
               ng-if="::!$ctrl.isPlainText()"
               name="{{::$ctrl.getName()}}"
               id="{{::$ctrl.getName()}}"
               value="1"
               ng-style="::$ctrl.cbStyle"
               class="form-control"
               ng-model="$ctrl.userword"
               ng-change="$ctrl.autoSave()"
               ng-disabled="::$ctrl.readonly"
               ng-model-options="::$ctrl.modelOpts"
               ng-readonly="::$ctrl.readonly"
               uib-tooltip="{{ $ctrl.errormessage }}"
               tooltip-is-open="$ctrl.f.$invalid && $ctrl.f.$dirty"
               tooltip-trigger="mouseenter"
               >
         </span>
         <span ng-if="::$ctrl.isPlainText()" style="">{{$ctrl.userword}}</span>
         </span>
         <!--</label>-->
    <!--</form> -->
    <div ng-if="$ctrl.error" style="font-size: 12px" ng-bind-html="$ctrl.error"></div>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
