/**
 * Defines the client-side implementation of an example plugin (a omapluginndrome checker).
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";

const omapluginApp = angular.module("omapluginApp", ["ngSanitize"]);
export const moduleDefs = [omapluginApp];

const omapluginMarkup = t.intersection([
    t.partial({
        initword: t.string,
        inputplaceholder: nullable(t.string),
        followid: t.string,
        inputstem: t.string,
        inputstem2: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const omapluginAll = t.intersection([
    t.partial({
        userword: t.string,
    }),
    t.type({markup: omapluginMarkup}),
]);

function isomapluginndrome(s: string) {
    let sc = s.toLowerCase();
    sc = sc.replace(/[^a-zåöä]/g, "");
    for (let i1 = 0, i2 = sc.length - 1; i1 < i2; i1++, i2--) {
        if (sc[i1] !== sc[i2]) {
            return false;
        }
    }
    return true;
}

export class OmapluginController extends PluginBase<t.TypeOf<typeof omapluginMarkup>, t.TypeOf<typeof omapluginAll>, typeof omapluginAll> implements ITimComponent{
    private result?: string;
    private error?: string;
    private isRunning = false;
    private userword = "";
    private runTestGreen = false;
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"
    private vctrl!: ViewCtrl;

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "Save";
    }

    $onInit() {
        super.$onInit();
        this.userword = this.attrsall.userword || this.attrs.initword || "";
        this.modelOpts = {debounce: this.autoupdate};
        this.checkomapluginndrome();
            //this.vctrl.addOmaplugin(this, this.attrs.followid);
            //this.vctrl.addTimComponent(this, this.attrs.followid || this.pluginMeta.getTaskId() || "");
            //this.vctrl.addTimComponentToList(this, this.attrs.followid);
        this.vctrl.addTimComponent(this);
    }

    get userword1(): string {
        return this.userword;
    }

    get initword(): string {
        return this.initword;
    }

    get edited() {
        return this.attrs.initword !== this.userword;
    }

    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    get inputplaceholder() {
        return this.attrs.inputplaceholder || null;
    }

    get inputstem() {
        return this.attrs.inputstem || null;
    }

    get inputstem2() {
        return this.attrs.inputstem2 || null;
    }

    get cols() {
        return this.attrs.cols;
    }

    get resetText() {
        return valueDefu(this.attrs.resetText, "Reset");
    }

    checkomapluginndrome() {
        const is = isomapluginndrome(this.userword);
        this.runTestGreen = is;
        return is;
    }

    initCode() {
        this.userword = this.attrs.initword || "";
        this.error = undefined;
        this.result = undefined;
        this.checkomapluginndrome();
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
                omapluginOK: this.checkomapluginndrome(),
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
            const timComponent = this.vctrl.getTimComponentByName("FOLLOWID");
            if (!this.attrs.followid && timComponent) {
                //this.error = this.vctrl.getOmapluginControllerFromName("FOLLOWID").userword1;//jotain toiselta pluginilta?
                this.error = timComponent.getContent();
            }
        } else {
            this.error = "Infinite loop or some other error?";
        }
    }

    protected getAttributeType() {
        return omapluginAll;
    }

    getName(): string | undefined {
        return this.attrs.followid || this.pluginMeta.getTaskId(); //TODO parse taskid / followid
    }

    getContent(): string {
        return this.userword;
    }

    save(): string {
        this.userword = "I'm saved";
        this.getGroups();
        return "";
    }

    getGroups(): string[]{
        let returnList: string[] = [];
        const parents = this.element.parents('.area'); //Palauttaa vain yhden koska divit ei sisäkkäin?
        //Parsetaan toistaiseksi manuaalisesti "area area_ulompi area_sisempi"
        if(parents[0]){
            let areaList = parents[0].classList;
            areaList.forEach(
                function(value){
                    if(value.match("area_")){
                        returnList.push(value.replace("area_", ""));
                    }
                }
            )
        }
        //console.log(this.attrs.followid + ": " + returnList);
        return returnList;
    }

}

omapluginApp.component("omapluginRunner", {
    bindings: {
        json: "@",
    },
    controller: OmapluginController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="csRunDiv no-popup-menu">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} , {{::$ctrl.inputstem2}}<span>
        <input type="text"
               class="form-control"
               ng-model="$ctrl.userword"
               ng-model-options="::$ctrl.modelOpts"
               ng-change="$ctrl.checkomapluginndrome()"
               ng-trim="false"
               placeholder="{{::$ctrl.inputplaceholder}}"
               size="{{::$ctrl.cols}}"></span></label>
        <span class="unitTestGreen" ng-if="$ctrl.runTestGreen && $ctrl.userword">OK</span>
        <span class="unitTestRed" ng-if="!$ctrl.runTestGreen">Wrong lol</span>
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || !$ctrl.userword"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}} aa
    </button>
    <a href="" ng-if="$ctrl.edited" ng-click="$ctrl.initCode()">{{::$ctrl.resetText}}</a>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
