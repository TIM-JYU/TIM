/**
 * Defines the client-side implementation of an example plugin (a multisavendrome checker).
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";

const multisaveApp = angular.module("multisaveApp", ["ngSanitize"]);
export const moduleDefs = [multisaveApp];

const multisaveMarkup = t.intersection([
    t.partial({
        //initword: t.string,
        //inputplaceholder: nullable(t.string),
        followid: t.string,
        fields: t.array(t.string)
        //inputstem: t.string,
        //inputstem2: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const multisaveAll = t.intersection([
    t.partial({
        //userword: t.string,
    }),
    t.type({markup: multisaveMarkup}),
]);

function ismultisavendrome(s: string) {
    let sc = s.toLowerCase();
    sc = sc.replace(/[^a-zåöä]/g, "");
    for (let i1 = 0, i2 = sc.length - 1; i1 < i2; i1++, i2--) {
        if (sc[i1] !== sc[i2]) {
            return false;
        }
    }
    return true;
}

export class MultisaveController extends PluginBase<t.TypeOf<typeof multisaveMarkup>, t.TypeOf<typeof multisaveAll>, typeof multisaveAll> {
   // private result?: string;
   // private error?: string;
   // pr//ivate isRunning = false;
   // private userword = "";
   // private runTestGreen = false;
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
        //this.userword = this.attrsall.userword || this.attrs.initword || "";
        this.modelOpts = {debounce: this.autoupdate};
        //this.checkmultisavendrome();
        // if (this.attrs.followid) {
        //     //this.vctrl.addMultisave(this, this.attrs.followid);
        //     this.vctrl.addTimComponent(this, this.attrs.followid || this.pluginMeta.getTaskId() || "");
        // }
    }

    //get userword1(): string {
    //    return this.userword;
    //}

    //get initword(): string {
    //    return this.initword;
    //}

    //get edited() {
    //    return this.attrs.initword !== this.userword;
    //}

    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    //get inputplaceholder() {
    //    return this.attrs.inputplaceholder || null;
    //}

    //get inputstem() {
    //    return this.attrs.inputstem || null;
    //}

    //get inputstem2() {
    //    return this.attrs.inputstem2 || null;
    //}

    get cols() {
        return this.attrs.cols;
    }

    get resetText() {
        return valueDefu(this.attrs.resetText, "Reset");
    }

    //checkmultisavendrome() {
    //    const is = ismultisavendrome(this.userword);
    //    this.runTestGreen = is;
    //    return is;
    //}

    initCode() {
        //this.userword = this.attrs.initword || "";
        //this.error = undefined;
        //this.result = undefined;
        //this.checkmultisavendrome();
    }

    //saveText() {
    //    this.doSaveText(false);
    //}

    //async doSaveText(nosave: boolean) {
//
    //    this.error = "... saving ...";
    //    this.isRunning = true;
    //    this.result = undefined;
    //    const params = {
    //        input: {
    //            nosave: false,
    //            multisaveOK: this.checkmultisavendrome(),
    //            userword: this.userword,
    //        },
    //    };
//
    //    if (nosave) {
    //        params.input.nosave = true;
    //    }
    //    const url = this.pluginMeta.getAnswerUrl();
    //    const r = await to($http.put<{web: {result: string, error?: string}}>(url, params));
    //    this.isRunning = false;
    //    if (r.ok) {
    //        const data = r.result.data;
    //        this.error = data.web.error;
    //        this.result = data.web.result;
    //        const timComponent = this.vctrl.getTimComponent("FOLLOWID");
    //        if (!this.attrs.followid && timComponent) {
    //            //this.error = this.vctrl.getMultisaveControllerFromName("FOLLOWID").userword1;//jotain toiselta pluginilta?
    //            this.error = timComponent.getContent();
    //        }
    //    } else {
    //        this.error = "Infinite loop or some other error?";
    //    }
    //}

    protected getAttributeType() {
        return multisaveAll;
    }

    getContent(): string {
        return "";
    }

    save(): string {
        if(this.attrs.fields){
            for (let i of this.attrs.fields){
                //TODO tarkista ettei kutsuta olion tallennusta kahdesti
                //Tee oma lista johon keräät uniikit oliot, save oman listan pohjalta
                //let timComponents = this.vctrl.getTimComponentsByRegex("^" + i + "$");
                let timComponents = this.vctrl.getTimComponentsByGroup(i);
                for (const v of timComponents)
                {
                    v.save();
                    //v.getGroups();
                }
            }
        }

        // //Vie listan nimi joka palauttaa listan
        // if(this.attrs.fields){
        //     for (let i of this.attrs.fields){
        //         alert("asd" + i);
        //         let list = this.vctrl.getTimComponentByList(i);
        //         for(let j of list)
        //         {
        //            if(j) j.save();
        //         }
        //     }
        //}
        // Vie lista ja saa takaisin lista componenteista
        //if(this.attrs.fields){
        //    let list = this.vctrl.getTimComponents(this.attrs.fields)
        //    for(let i of list)
        //    {
        //       if(i) i.save();
        //    }
        //}
        //Käy lista itse ja kysy jokainen erikseen
        //for(let i of this.attrs.fields)
        //{
        //    let timComponent = this.vctrl.getTimComponent(i);
        //    if(timComponent) timComponent.save();
        //}
        return this.attrs.followid || this.pluginMeta.getTaskId() || ""; //return n onnistunutta save()-kutsua?
    }
}

multisaveApp.component("multisaveRunner", {
    bindings: {
        json: "@",
    },
    controller: MultisaveController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="no-popup-menu">
    <!-- <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>-->
    <div class="form-inline"><label>{{::$ctrl.inputstem}}</label>
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-click="$ctrl.save()">
        {{::$ctrl.buttonText()}}
    </button>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
