/**
 * Defines the client-side implementation of an example plugin (a tableFormndrome checker).
 */
import angular, {INgModelOptions, IRootElementService, IScope} from "angular";
import * as t from "io-ts";
import {
    GenericPluginMarkup,
    GenericPluginTopLevelFields,
    nullable,
    PluginBase,
    pluginBindings, PluginMeta,
    withDefault,
} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";
import {timApp} from "../app";
import {None} from "../../jspm_packages/npm/fp-ts@1.11.1/lib/Option";
import enumerate = Reflect.enumerate;

const tableFormApp = angular.module("tableFormApp", ["ngSanitize"]);
export const moduleDefs = [tableFormApp];

const TableFormMarkup = t.intersection([
    t.partial({
        initword: t.string
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const TableFormAll = t.intersection([
    t.partial({
        rows: t.Dictionary,
        fields: t.Dictionary,
    }),
    GenericPluginTopLevelFields,
    t.type({markup: TableFormMarkup}),
]);


class TableFormController extends PluginBase<t.TypeOf<typeof TableFormMarkup>, t.TypeOf<typeof TableFormAll>, typeof TableFormAll> {
    private result?: string;
    private error?: string;
    private isRunning = false;
    private userfilter = "";
    private runTestGreen = false;
    private data: any = {};
    private rows: any = {};
    private tabledata: any
    private timTableData!: {};
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "TODO";
    }


    $onInit() {
        super.$onInit();
        this.userfilter = "";
        this.rows = this.attrsall.rows;
        //this.fields = this.attrsall.fields;
        this.setDataMatrix();
    }

    setDataMatrix() {
        this.timTableData = {'task': 'true'}
        // for (const row in this.rows){
        //      console.log("eaa");
        //  }
        this.data.userdata = {};
        // for (let i = 0; i < Object.keys(this.rows).length; i++){ // TODO parempi silmukka - object.keys pois
        //     this.data.userdata['A' + (i+1).toString()] = Object.keys(this.rows)[i];
        //     for (let j = 0; j < Object.keys(Object.keys(this.rows)[i]).length; j++)
        //     {
        //         if(Object.keys(Object.keys(this.rows)[i])[j]){
        //             this.data.userdata[this.colnumToLetters(j+1) + (i+2).toString()] = Object.keys(Object.values(this.rows)[i])[j]
        //         }
        //     }
        // }
        for (const f in this.attrsall.fields) {
            for(const r in this.rows){
                console.log("ea");
            }
        }

        console.log("asd");
    }

    /** TODO SIIRRÄ jonnekin
     * Transforms column index to letter.
     * @param colIndex ex. 2
     * @return column index as letter
     */
    public colnumToLetters(colIndex: number): string {
        const ASCII_OF_A = 65;
        const ASCII_CHAR_COUNT = 26;
        const lastChar = String.fromCharCode(ASCII_OF_A + (colIndex % ASCII_CHAR_COUNT));
        const remainder = Math.floor(colIndex / ASCII_CHAR_COUNT);

        if (remainder == 0) {
            return lastChar;
        } else if (remainder <= ASCII_CHAR_COUNT) {
            return String.fromCharCode(ASCII_OF_A + remainder - 1) + lastChar;
        }
        // recursive call to figure out the rest of the letters
        return this.colnumToLetters(remainder - 1) + lastChar;
    }

    initCode() {
        this.userfilter = "";
        this.error = undefined;
        this.result = undefined;
    }

    saveText() {
        this.doSaveText(false);
    }

    updateFilter() {
        console.log("asdd");
    }

    async doSaveText(nosave: boolean) {
        this.error = "... saving ...";
        this.isRunning = true;
        this.result = undefined;
        const params = {
            input: {
                nosave: false,
                userfilter: this.userfilter,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to($http.put<{ web: { result: string, error?: string } }>(url, params));
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
        return TableFormAll;
    }
}

timApp.component("tableformRunner", {
    bindings: pluginBindings,

    controller: TableFormController,
    template: `
<div class="csRunDiv no-popup-menu">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" ng-bind-html="::$ctrl.stem"></p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        <input type="text"
               class="form-control"
               ng-model="$ctrl.userfilter"
               ng-model-options="::$ctrl.modelOpts"
               ng-trim="false"
               ng-change="$ctrl.updateFilter()"
               ng-readonly="::$ctrl.readonly"
               size="{{::$ctrl.cols}}"></span></label>
        <tim-table data="$ctrl.data" userdata="$ctrl.userdata" task-url="{{$ctrl.pluginMeta.getAnswerUrl()}}"></tim-table>
        <!-- TODO: taskid="{{ $ctrl.pluginm }}", vie pluginmeta & taskid-->
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || !$ctrl.userfilter || $ctrl.readonly"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
