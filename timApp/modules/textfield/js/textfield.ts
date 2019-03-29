/**
 * Defines the client-side implementation of textfield/label plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {GenericPluginMarkup, Info, nullable, PluginBase, withDefault} from "tim/plugin/util";
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
        autosave: t.boolean
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
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
        this.userword = this.attrsall.userword || this.attrs.initword || "";
        this.modelOpts = {debounce: this.autoupdate};
        this.vctrl.addTimComponent(this);
    }

    /**
     *  Will run after $onInit - reserved for possible eventhandlers OR to be removed
     */
    /*
    $postLink() {
        if (this.userword == "") {
            this.userword = this.attrs.initword || "";
        }
    }
    */

    /**
     * Method to return (user) content in string form
     */
    getContent(): string {
        return this.userword;
    }

    /**
     * Method to return (user) content in numeric form.
     * Not used in textfield plugin, but promised to be implemented in ITimComponent
     */
    getNumericContent(): number {
        return -1;
    }

    /**
     * Save method for other plugins, needed by e.g. multisave plugin
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
        return this.attrs.inputstem || "";
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
        this.userword = this.attrs.initword || "";
        this.error = undefined;
        this.result = undefined;
    }

    /**
     * Method to (re)direct save request to actual save method.
     * Used as e.g. timButton ng-click event.
     */
    saveText() {
        this.doSaveText(false);
    }

    checkInputRegex(re: string) {
        let regExpChecker = new RegExp(re);
        return regExpChecker.test(this.userword);
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Autosave method is used by ng-blur in textfieldApp component.
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
        if (this.attrs.inputchecker) {
            if(!this.checkInputRegex(this.attrs.inputchecker)) {
                this.error = "Input does not pass the RegExp checker!";
                return;
            }
        }
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

/**
 * textfieldRunner as HTML component.
 */
textfieldApp.component("textfieldRunner", {
    bindings: {
        json: "@",
    },
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
               ng-model-options="{ debounce: 0} "
               ng-blur="$ctrl.autoSave()"
               ng-keydown="$event.keyCode === 13 && $ctrl.saveText()"
               ng-model-options="::$ctrl.modelOpts"
               ng-change="$ctrl.checkTextfield()"
               ng-trim="false"
               ng-readonly="::$ctrl.readonly"
               placeholder="{{::$ctrl.inputplaceholder}}"
               size="{{::$ctrl.cols}}"> </span></label>
    </div>
    <button class="timButton"
            ng-if="$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || !$ctrl.userword || $ctrl.readonly"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    <!-- <pre class="hidepre" ng-if="$ctrl.result">{{$ctrl.result}}</pre> -->
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});