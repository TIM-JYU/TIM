/**
 * Defines the client-side implementation of textfield/label plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, nullable, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$http, $timeout} from "tim/util/ngimport";
import {to, valueOr} from "tim/util/utils";

const textfieldApp = angular.module("textfieldApp", ["ngSanitize"]);
export const moduleDefs = [textfieldApp];

const TextfieldMarkup = t.intersection([
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
        nosave: t.boolean,
        ignorestyles: t.boolean,
        clearstyles: t.boolean,
        textarea: t.boolean,
        autogrow: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        autoupdate: withDefault(t.number, 500),
        autoUpdateTables: withDefault(t.boolean, true),
        form: withDefault(t.boolean, true),
        cols: withDefault(t.number, 6),
        rows: withDefault(t.number, 1),
    }),
]);
const TextfieldAll = t.intersection([
    t.partial({}),
    t.type({
        info: Info,
        markup: TextfieldMarkup,
        preview: t.boolean,
        state: nullable(
            t.intersection([
                t.type({
                    c: t.union([t.string, t.number, t.null]),
                }),
                t.partial({
                    styles: nullable(t.record(t.string, t.string)),
                }),
            ]),
        ),
    }),
]);

class TextfieldController extends PluginBase<t.TypeOf<typeof TextfieldMarkup>, t.TypeOf<typeof TextfieldAll>, typeof TextfieldAll> implements ITimComponent {
    private changes = false;
    private result?: string;
    private isRunning = false;
    private userword = "";
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"
    private vctrl!: ViewCtrl;
    private initialValue = "";
    private errormessage = "";
    private hideSavedText = true;
    private redAlert = false;
    private saveResponse: {saved: boolean, message: (string | undefined)} = {saved: false, message: undefined};
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
        return super.buttonText() ?? null;
    }

    $onInit() {
        super.$onInit();
        this.userword = (valueOr(this.attrsall.state && this.attrsall.state.c, this.attrs.initword ?? "")).toString();
        // this.modelOpts = {debounce: this.autoupdate};
        this.modelOpts = {debounce: { blur: 0}};
        this.vctrl.addTimComponent(this, this.attrs.tag);
        this.initialValue = this.userword;
        if (this.attrs.showname) { this.initCode(); }
        if (this.attrsall.state && this.attrsall.state.styles && !this.attrs.ignorestyles) {
            this.applyStyling(this.attrsall.state.styles);
        }
        if (this.attrs.textarea && this.attrs.autogrow) {
            this.autoGrow();
        }
    }

    /**
     * Returns (user) content in string form.
     */
    getContent(): string {
        return this.userword;
    }

    /**
     * Save method for other plugins, needed by e.g. multisave plugin.
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

    supportsSetAnswer(): boolean {
        return true;
    }

    // TODO: Use answer content as arg or entire IAnswer?
    // TODO: get rid of any (styles can arrive as object)
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    setAnswer(content: { [index: string]: any }): { ok: boolean, message: (string | undefined) } {
        let message;
        let ok = true;
        // TODO: should receiving empty answer reset to defaultnumber or clear field?
        if (Object.keys(content).length == 0) {
            this.resetField();
        } else {
            try {
                // eslint-disable-next-line @typescript-eslint/tslint/config
                this.userword = content.c;
            } catch (e) {
                this.userword = "";
                ok = false;
                message = "Couldn't find related content (\"c\")";
            }
            if (!this.attrs.ignorestyles) {
                // eslint-disable-next-line @typescript-eslint/tslint/config
                this.applyStyling(content.styles);
            }
        }
        this.changes = false;
        this.updateListenerMultisaves(true);
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
        return this.attrs.inputstem ?? "";
    }

    /**
     * Returns (user) set col size (size of the field).
     */
    get cols() {
        return this.attrs.cols;
    }

    get rows() {
        return this.attrs.rows;
    }

    /**
     * Initialize content.
     */
    initCode() {
        if (this.attrs.showname) {
            const u = this.vctrl.selectedUser;
            if (this.attrs.showname == 1) {
                this.userword = u.real_name ?? "";
            }
            if (this.attrs.showname == 2) {
                this.userword = u.name;
            }
        } else { this.userword = this.attrs.initword ?? ""; }
        this.initialValue = this.userword;
        this.result = undefined;
        this.changes = false;
        this.updateListenerMultisaves(true);
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
     * Returns validinput attribute, if one is defined.
     * Used by pattern checker in angular.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    getPattern() {
        if (this.attrs.validinput) {
            return this.attrs.validinput;
        }
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Returns focus on next HTML field.
     * Used by keydown (Enter) in angular.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    changeFocus() {
        const inputfields = document.querySelectorAll("textfield-runner input, numericfield-runner input");
        for (let i = 0; i < inputfields.length; ++i) {
            const selectedfield = inputfields[i] as HTMLInputElement;
            if (selectedfield === document.activeElement && inputfields[i + 1]) {
                const nextfield = inputfields[i + 1] as HTMLInputElement;
                this.preventedAutosave = true;
                return nextfield.focus();
            }
        }
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Returns true value, if label is set to plaintext.
     * Used to define readOnlyStyle in angular, either input or span.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isPlainText() {
        if (this.attrs.showname) { return true; }
        return (this.attrs.readOnlyStyle == "plaintext" && window.location.pathname.startsWith("/view/"));
    }

    isTextArea() {
        if (this.attrs.textarea) {
            return true;
        }
        return false;
    }

    /**
     * Parses "styles" from the plugin answer that were saved by tableForm
     * For now only backgroundColor is supported
     * TODO: Extend styling for all attributes in timTable's cellStyles?
     *  For now tableForm is only able to define backgroundColor or textAlign
     *  Could also define (and import) generic tim-wide inputstyles
     * TODO: Could also just apply given styles as they are
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
     * Method to check grading input type for textfield.
     * Used as e.g. grading checker for hyv | hyl | 1 | 2 | 3 | 4 | 5.
     * @param re validinput defined by given attribute.
     */
    validityCheck(re: string) {
        if (this.userword === "") {
            return new RegExp("").test(this.userword);
        }
        const regExpChecker = new RegExp(re);
        return regExpChecker.test(this.userword);
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Checking if input has been changed since the last Save or initialization.
     * Displays a red thick marker at the right side of the inputfield to notify users
     * about unsaved changes.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isUnSaved() {
        // if (this.initialValue != this.userword) {
        //         //     this.hideSavedText = true;
        //         // }
        return (!this.attrs.nosave && this.changes);
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
            // noinspection JSIgnoredPromiseFromCall
            this.saveText();
        }
    }

    /**
     * Actual save method, called by different save alternatives implemented above.
     * @param nosave: true/false parameter boolean checker for the need to save
     */
    async doSaveText(nosave: boolean) {
        if (!this.isUnSaved()) {
            this.saveResponse.saved = false;
            this.saveResponse.message = "No changes";
            return this.saveResponse;
        }
        this.errormessage = "";
        if (this.attrs.validinput) {
            if (!this.validityCheck(this.attrs.validinput)) {
                this.errormessage = this.attrs.errormessage ?? "Input does not pass the RegEx: " + this.attrs.validinput;
                this.redAlert = true;
                this.saveResponse.message = this.errormessage;
                return this.saveResponse;
            }
        }
        this.isRunning = true;
        this.result = undefined;
        const params = {
            input: {
                nosave: false,
                c: this.userword.trim(),
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
            // TODO: Make angular to show tooltip even without user having to move cursor out and back into the input
            // (Use premade bootstrap method / add listener for enter?)
            this.errormessage = data.web.error ?? "";
            this.result = data.web.result;
            this.initialValue = this.userword;
            this.changes = false;
            this.updateListenerMultisaves(true);
            this.hideSavedText = false;
            this.redAlert = false;
            this.saveResponse.saved = true;
            this.saveResponse.message = this.errormessage;
            if (data.web.clear) {
                this.applyStyling({});
            }

            if (this.vctrl && !this.saveCalledExternally) {
                const taskId = this.getTaskId();
                if (taskId) {
                    const tid = taskId.docTask();
                    if (this.attrs.autoUpdateTables) {
                        this.vctrl.updateAllTables([tid]);
                    }
                    if (this.vctrl.docSettings.form_mode) {
                        const duplicates = this.vctrl.getTimComponentArray(tid);
                        if (duplicates && duplicates.length > 1) {
                            this.vctrl.updateFields([tid]);
                            // for (const dup of duplicates) {
                            //     dup.setAnswer({"c": this.userword, "styles": this.styles})
                            // }
                        }
                    }
                }
            }
        } else {
            this.errormessage = r.result.data.error || "Syntax error or no reply from server?";
        }
        return this.saveResponse;
    }

    getAttributeType() {
        return TextfieldAll;
    }

    async autoGrow() {
        await $timeout(0);
        const ele = this.element.find(".textarea").first();
        const scrollHeight = ele[0].scrollHeight;
        const prevHeight = parseFloat(ele.css("height"));
        if (scrollHeight < prevHeight) {
            return;
        }
        ele.css("height",  ele[0].scrollHeight + "px");
    }

    isForm(): boolean {
        return this.attrs.form;
    }

    updateInput() {
        if (!this.changes) {
            this.changes = true;
            this.updateListenerMultisaves(false);
        }
    }

    // TODO: Generic, move
    updateListenerMultisaves(saved: boolean) {
        if (this.attrs.hasListeners || !this.vctrl) {
            return;
        }
        const taskId = this.pluginMeta.getTaskId();
        if (!taskId) {
            return;
        }
        const doctask = taskId.docTask();
        this.vctrl.informMultisavesAboutChanges(doctask, saved, (this.attrs.tag ? this.attrs.tag : undefined));
    }
}

// noinspection CssInvalidFunction
/**
 * Introducing textfieldRunner as HTML component.
 */
textfieldApp.component("textfieldRunner", {
    bindings: pluginBindings,
    controller: TextfieldController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="textfieldNoSaveDiv">
    <tim-markup-error ng-if="::$ctrl.markupError" [data]="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p class="stem" ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <form name="$ctrl.f" class="form-inline">
     <label><span>
      <span class="inputstem" ng-bind-html="::$ctrl.inputstem"></span>
      <span ng-if="::!$ctrl.isPlainText()" >
        <input type="text"
               style="width: {{::$ctrl.cols}}em"
               ng-if="::!$ctrl.isTextArea()"
               class="form-control"
               ng-model="$ctrl.userword"
               ng-blur="::$ctrl.autoSave()"
               ng-keydown="$event.keyCode === 13 && $ctrl.autoSave() && $ctrl.changeFocus()"
               ng-change="$ctrl.updateInput()"
               ng-model-options="::$ctrl.modelOpts"
               ng-trim="false"
               ng-pattern="$ctrl.getPattern()"
               ng-readonly="::$ctrl.readonly"
               uib-tooltip="{{ $ctrl.errormessage }}"
               tooltip-is-open="$ctrl.f.$invalid && $ctrl.f.$dirty"
               tooltip-trigger="mouseenter"
               placeholder="{{::$ctrl.inputplaceholder}}"
               ng-class="{warnFrame: ($ctrl.isUnSaved() && !$ctrl.redAlert), alertFrame: $ctrl.redAlert }"
               ng-style="$ctrl.styles">
       <textarea
               style="width: {{::$ctrl.cols}}em;"
               rows="{{::$ctrl.rows}}"
               ng-if="::$ctrl.isTextArea()"
               class="form-control textarea"
               ng-model="$ctrl.userword"
               ng-blur="::$ctrl.autoSave()"
               ng-keydown="::$ctrl.attrs.autogrow && $ctrl.autoGrow()"
               ng-keyup="::$ctrl.attrs.autogrow && $ctrl.autoGrow()"
               ng-model-options="::$ctrl.modelOpts"
               ng-trim="false"
               ng-pattern="$ctrl.getPattern()"
               ng-readonly="::$ctrl.readonly"
               uib-tooltip="{{ $ctrl.errormessage }}"
               tooltip-is-open="$ctrl.f.$invalid && $ctrl.f.$dirty"
               tooltip-trigger="mouseenter"
               placeholder="{{::$ctrl.inputplaceholder}}"
               ng-class="{warnFrame: ($ctrl.isUnSaved() && !$ctrl.redAlert), alertFrame: $ctrl.redAlert }"
               ng-style="$ctrl.styles">
               </textarea>
         </span>
         <span ng-if="::$ctrl.isPlainText()" ng-bind-html="$ctrl.userword" class="plaintext" style="width: {{::$ctrl.cols}}em; max-width: 100%"></span>
         </span></label>
    </form>
    <div ng-if="$ctrl.errormessage" class="error" style="font-size: 12px" ng-bind-html="$ctrl.errormessage"></div>
    <button class="timButton"
            ng-if="$ctrl.buttonText()"
            ng-disabled="!$ctrl.isUnSaved() || $ctrl.isRunning || $ctrl.readonly"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    <p class="savedtext" ng-if="!$ctrl.hideSavedText && $ctrl.buttonText()">Saved!</p>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
