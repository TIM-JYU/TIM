/**
 * Defines the client-side implementation of textfield/label plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {GenericPluginMarkup, Info, nullable, PluginBase, pluginBindings, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {valueDefu} from "tim/util/utils"; //tarvitaan reset-metodille, jos halutaan toteuttaa

const textfieldApp = angular.module("textfieldApp", ["ngSanitize"]);
export const moduleDefs = [textfieldApp];

const TextfieldMarkup = t.intersection([
    t.partial({
        followid: nullable(t.string),
        inputplaceholder: nullable(t.string),
        inputstem: nullable(t.string),
        initword: nullable(t.string),
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
const TextfieldAll = t.intersection([
    t.partial({
        userword: t.string,
    }),
    t.type({
        info: Info,
        markup: TextfieldMarkup,
        preview: t.boolean,
    }),
]);

class TextfieldController extends PluginBase<t.TypeOf<typeof TextfieldMarkup>, t.TypeOf<typeof TextfieldAll>, typeof TextfieldAll> implements ITimComponent {
    private result?: string;
    private error?: string;
    private isRunning = false;
    private userword = "";
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"
    private vctrl!: ViewCtrl;
    private notSavedWord = "";

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
        this.userword = this.attrsall.userword || this.attrs.initword || "";
        this.modelOpts = {debounce: this.autoupdate};
        this.vctrl.addTimComponent(this);
        this.notSavedWord = this.userword;
    }

    /**
     *  Will run after $onInit - reserved for possible eventhandlers OR to be removed.
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
     */
    getContent(): string {
        return this.userword;
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Returns (user) content in numeric form.
     * Not used in textfield plugin, but promised to be implemented in ITimComponent.
     * Unused method warning is suppressed.
     */
    getNumericContent(): number {
        return -1;
    }

    /**
     * Save method for other plugins, needed by e.g. multisave plugin.
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
        return this.attrs.inputstem || "";
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
        this.userword = this.attrs.initword || "";
        this.error = undefined;
        this.result = undefined;
    }

    /**
     * Redirects save request to actual save method.
     * Used as e.g. timButton ng-click event.
     */
    saveText() {
        this.doSaveText(false);
    }

    /**
     * NOTE: This functionality is defined as low-priority and therefore TODO.
     * Method to check grading input type for textfield.
     * Used as e.g. grading checker for hyv | hyl | 1 | 2 | 3 | 4 | 5.
     * @param re TODO!
     */
    /*
    checkInputRegex(re: string) {
        let regExpChecker = new RegExp(re);
        return regExpChecker.test(this.userword);
    }
    */

    // noinspection JSUnusedGlobalSymbols
    /**
     * Checking if input has been changed since the last Save or initialization.
     * Displays a red thick marker at the right side of the inputfield to notify users
     * about unsaved changes.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    notSaved() {
        return (this.notSavedWord != this.userword);
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
     * Actual save method, called by different save alternatives implemented above.
     * @param true/false parameter boolean checker for the need to save
     */
    async doSaveText(nosave: boolean) {
        /* if (this.attrs.inputchecker) {
            if(!this.checkInputRegex(this.attrs.inputchecker)) {
                this.error = "Input does not pass the RegExp checker!";
                return;
            }
        } */
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
            this.notSavedWord = this.userword;
        } else {
            this.error = "Infinite loop or some other error?";
        }
    }

    protected getAttributeType() {
        return TextfieldAll;
    }
}

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
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p class="stem" ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>   
        <input type="string"
               class="form-control"
               ng-model="$ctrl.userword"
               ng-model-options="{ debounce: {'blur': 0} } "
               ng-blur="$ctrl.autoSave()"
               ng-keydown="$event.keyCode === 13 && $ctrl.saveText()"
               ng-model-options="::$ctrl.modelOpts"
               ng-trim="false"
               ng-readonly="::$ctrl.readonly"
               placeholder="{{::$ctrl.inputplaceholder}}"
               size="{{::$ctrl.cols}}" 
               ng-class="{warnFrame: $ctrl.notSaved()}">
               </span></label>
    </div>
    <button class="timButton"
            ng-if="$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || !$ctrl.userword || $ctrl.readonly"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});