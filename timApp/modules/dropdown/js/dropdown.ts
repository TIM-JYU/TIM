/**
 * Defines the client-side implementation of a dropdown plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";

const dropdownApp = angular.module("dropdownApp", ["ngSanitize"]);
export const moduleDefs = [dropdownApp];

const DropdownMarkup = t.intersection([
    t.partial({
        initword: t.string,
        inputplaceholder: nullable(t.string),
        inputstem: t.string,
        words: t.array(t.string),
        followid: t.string,
        item: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const DropdownAll = t.intersection([
    t.partial({
        userword: t.string,
    }),
    t.type({markup: DropdownMarkup}),
]);

class DropdownController extends PluginBase<t.TypeOf<typeof DropdownMarkup>, t.TypeOf<typeof DropdownAll>, typeof DropdownAll> implements ITimComponent {
    private result?: string;
    private error?: string;
    private userword = "";
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"
    private wordlist?: string[];
    private item = "";
    private selectedWord?: string;
    private vctrl!: ViewCtrl;
    private parts!: string[];

    getDefaultMarkup() {
        return {};
    }

    $onInit() {
        super.$onInit();
        this.userword = this.attrsall.userword || this.attrs.initword || "";
        this.modelOpts = {debounce: this.autoupdate};
        this.wordlist = this.attrs.words || [];
        this.addToCtrl();
        this.item = this.attrs.item || "";
        this.makeSentence();
    }

    addToCtrl() {
        const taskid = this.pluginMeta.getTaskId() || ""; // TODO: fix this dirty stuff
        const name = taskid.split(".");
        this.vctrl.addTimComponent(this, this.attrs.followid || name[1] || "");
    }

    makeSentence() {
        this.parts = this.item.split("$choice");

    }

    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    get inputstem() {
        return this.attrs.inputstem || null;
    }

    initCode() {
        this.userword = this.attrs.initword || "";
        this.error = undefined;
        this.result = undefined;
        this.selectedWord = undefined;
        //this.wordlist = this.attrs.words || [];
    }

    getContent(): string {
        const part1 = this.parts[0];
        const part2 = this.parts[1];
        const selected = this.selectedWord || "";
        return part1 + selected + part2;

        // return this.selectedWord || "Nothing selected";
    }

    save(): string {
      return "";
    }

    getSelectedWord() {
        const timComponent = this.vctrl.getTimComponent("item1");
        if(timComponent) {
            //this.error = this.pluginMeta.getTaskId();
            this.error = timComponent.getContent();
        }
        return;
    }

    protected getAttributeType() {
        return DropdownAll;
    }
}

dropdownApp.component("dropdownRunner", {
    bindings: {
        json: "@",
    },
    controller: DropdownController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        {{$ctrl.parts[0]}}
        <select ng-model="$ctrl.selectedWord" ng-options="item for item in $ctrl.wordlist">
        </select>
        {{$ctrl.parts[1]}}
        </span></label>
    </div>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
/*
    template: `
<div class="csRunDiv no-popup-menu">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        <input type="text"
               class="form-control"
               ng-model="$ctrl.userword"
               ng-model-options="::$ctrl.modelOpts"
               ng-change="$ctrl.checkPalindrome()"
               ng-trim="false"
               placeholder="{{::$ctrl.inputplaceholder}}"
               size="{{::$ctrl.cols}}"></span></label>
        <span class="unitTestGreen" ng-if="$ctrl.runTestGreen && $ctrl.userword">OK</span>
        <span class="unitTestRed" ng-if="!$ctrl.runTestGreen">Wrong</span>
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || !$ctrl.userword"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    <a href="" ng-if="$ctrl.edited" ng-click="$ctrl.initCode()">{{::$ctrl.resetText}}</a>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`, */
});
