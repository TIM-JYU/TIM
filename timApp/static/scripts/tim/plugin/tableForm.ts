/**
 * Defines the client-side implementation of an example plugin (tableForm).
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
import {ViewCtrl} from "../document/viewctrl";
import {valueDefu} from "tim/util/utils";
import {timApp} from "../app";
import {None} from "../../jspm_packages/npm/fp-ts@1.11.1/lib/Option";
import enumerate = Reflect.enumerate;
import {getParId} from "../document/parhelpers";

const tableFormApp = angular.module("tableFormApp", ["ngSanitize"]);
export const moduleDefs = [tableFormApp];

const TableFormMarkup = t.intersection([
    t.partial({
        initword: t.string,
        table: nullable(t.boolean),
        report: nullable(t.boolean),
        seperator: nullable(t.string), /* TODO! Seperate columns with user given character for report */
        usednames: nullable(t.string), /* TODO! username and full name, username or anonymous */
        sortBy: nullable(t.string), /* TODO! Username and task, or task and username -- what about points? */
        /* answerAge: nullable(t.string), /* TODO! Define time range from which answers are fetched. Maybe not to be implemented! */
        dataCollection: nullable(t.string), /* TODO! Filter by data collection consent: allowed, denied or both */
        print: nullable(t.string), /* TODO! Headers and answers, headers, answers, answers w/o separator line, (or Korppi export?) */

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
        fields: t.array(t.string)
    }),
    GenericPluginTopLevelFields,
    t.type({markup: TableFormMarkup}),
]);


class TableFormController extends PluginBase<t.TypeOf<typeof TableFormMarkup>, t.TypeOf<typeof TableFormAll>, typeof TableFormAll> {
    public viewctrl?: ViewCtrl;
    private result?: string;
    private error?: string;
    private isRunning = false;
    private userfilter = "";
    private data: any = {};
    private rows!: {};
    // private hiderows: number[] = [];
    private allRows!: {};
    private tabledata: any
    private timTableData!: {};
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"
    //private table = false;
    //private report = false;

    getDefaultMarkup() {
        return {};
    }
    
    // THIS WILL BE REMOVED AS WE IMPLEMENT A 2 BUTTON SOLUTION
    //
    // buttonText() {
    //     return super.buttonText() || "Tallenna taulukko";
    // }



    $onInit() {
        super.$onInit();
        this.userfilter = "";
        this.allRows = this.attrsall.rows || {};
        this.rows = this.allRows;
        this.data.task = true;
        this.data.hiderows = [];
        this.data.tableForm = true;
        this.data.lockedCells = ["A1"];
        //this.fields = this.attrsall.fields;
        this.setDataMatrix();
    }


    setDataMatrix() {
        // for (const row in this.rows){
        //      console.log("eaa");
        //  }
        this.data.userdata = {'cells': {}};
        // for (let i = 0; i < Object.keys(this.rows).length; i++){ // TODO parempi silmukka - object.keys pois
        //     this.data.userdata['A' + (i+1).toString()] = Object.keys(this.rows)[i];
        //     for (let j = 0; j < Object.keys(Object.keys(this.rows)[i]).length; j++)
        //     {
        //         if(Object.keys(Object.keys(this.rows)[i])[j]){
        //             this.data.userdata[this.colnumToLetters(j+1) + (i+2).toString()] = Object.keys(Object.values(this.rows)[i])[j]
        //         }
        //     }
        // }
        // if(this.attrsall.fields){
        //     for(const r in this.rows){
        //         for (const f of this.attrsall.fields) {
        //             console.log(this.rows[r][f])
        //             if(this.rows[r][f] != undefined){
        //                 console.log("asd");
        //             }
        //         }
        //     }
        // }
        var x = 2;
        for (const r of Object.keys(this.rows)) {
            this.data.userdata['cells']['A' + x] = r;
            this.data.lockedCells.push('A' + x);
            x++;
        }
        if (this.attrsall.fields) {
            for (var y = 0; y < this.attrsall.fields.length; y++) {
                this.data.userdata['cells'][this.colnumToLetters(y + 1) + 1] = this.attrsall.fields[y];
                this.data.lockedCells.push(this.colnumToLetters(y + 1) + 1)
                x = 0;
                for (const [u,r] of Object.entries(this.rows)) {
                    // @ts-ignore
                    if (r[this.attrsall.fields[y]]) {
                        // @ts-ignore
                        this.data.userdata['cells'][this.colnumToLetters(y + 1) + (x + 2)] = r[this.attrsall.fields[y]];

                    }
                    x++;
                }
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

    // noinspection JSUnusedGlobalSymbols
    /**
     * Returns true value, if table attribute is true.
     * Used to define table view & relative save button in angular, true or false.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    tableCheck() {
        return (this.attrs.table == true);
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Returns true value, if report attribute is true.
     * Used to define create report button in angular, true or false.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    reportCheck() {
        return (this.attrs.report == true);
    }

    /**
     * Generates report based on the table. TODO!
     * Used if report report is set to true and create report button is clicked.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    generateReport() {
        console.log("Will Generate actual report based on the table!")
    }

    updateFilter() {
        // if (this.userfilter == "") {
        //     this.rows = this.allRows;
        // } else {
        //     this.rows = {}
        //     const reg = new RegExp(this.userfilter)
        //     for (const [key, value] of Object.entries(this.allRows)) {
        //         if (reg.test(key)) {
        //             // @ts-ignore
        //             this.rows[key] = value;
        //         }
        //     }
        // }
        //
        // this.setDataMatrix();
        //
        // const parId = getParId(this.element.parents(".par"));
        // if (parId != null && this.viewctrl != null) {
        //     const table = this.viewctrl.getTableControllerFromParId(parId);
        //     if(!table) return;
        //     table.reInitialize();
        //     table.processDataBlock(this.data.userdata.cells);
        //     //TODO: Re-initalize table
        // }
        this.data.hiderows = [];
        if (this.userfilter != "" && this.userfilter != undefined) {
            const reg = new RegExp(this.userfilter.toLowerCase())
            let rowi = 1;
            for (const [key, value] of Object.entries(this.allRows)) {
                if (!reg.test(key.toLowerCase())) {
                    this.data.hiderows.push(rowi);
                }
                rowi++;
            }
        }
    }

    async doSaveText(nosave: boolean) {
        this.error = "... saving ...";
        const keys = Object.keys(this.data.userdata.cells)
        keys.sort();
        const userLocations = {}
        const taskLocations = {}
        const replyRows = {}
        for(const coord of keys)
        {
            const alphaRegExp = new RegExp("([A-Z]*)");
            const alpha = alphaRegExp.exec(coord);
            const value = coord;

            if (alpha == null) {
                continue;
            }
            const columnPlace = alpha[0];
            const numberPlace = coord.substring(columnPlace.length);
            var cellContent = this.data.userdata.cells[coord];
            //TODO: Save cell attributes (e.g backgroundColor) as plugin's own answer
            if(typeof cellContent != "string") cellContent = cellContent.cell;
            if (columnPlace === "A"){
                if (numberPlace === "1") continue;
                // @ts-ignore
                userLocations[numberPlace] = cellContent;
                // @ts-ignore
                replyRows[cellContent] = {};
            }
            else if (numberPlace === "1"){
                // @ts-ignore
                taskLocations[columnPlace] = cellContent;
            }
            else{
                // @ts-ignore
                replyRows[userLocations[numberPlace]][taskLocations[columnPlace]] = cellContent;
            }
        }
        console.log("asd");
        // this.isRunning = true;
        // this.result = undefined;
        const params = {
            input: {
                nosave: false,
                replyRows: replyRows,
            },
        };
        //
        // if (nosave) {
        //     params.input.nosave = true;
        // }
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
    require: {
        viewctrl: "?^timView",
    },
    template: `
<div class="csRunDiv no-popup-menu">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" ng-bind-html="::$ctrl.stem"></p>
    <div class="form-inline" ng-if="::$ctrl.tableCheck()"><label>{{::$ctrl.inputstem}} <span> &nbsp;
        <input type="text"
               class="form-control"
               ng-model="$ctrl.userfilter"
               ng-model-options="::$ctrl.modelOpts"
               ng-trim="false"
               ng-change="$ctrl.updateFilter()"
               ng-readonly="::$ctrl.readonly"
               size="{{::$ctrl.cols}}"></span></label>
        <tim-table data="::$ctrl.data" taskid="{{$ctrl.pluginMeta.getTaskId()}}" plugintype="{{$ctrl.pluginMeta.getPlugin()}}" ></tim-table>
        <!-- TODO: taskid="{{ $ctrl.pluginm }}", vie pluginmeta & taskid-->
    </div> &nbsp;
    <button class="timButton"
            ng-if="::$ctrl.tableCheck()"
            ng-click="$ctrl.saveText()">
        Tallenna taulukko
    </button> &nbsp;
    <button class="timButton"
            ng-if="::$ctrl.reportCheck()"
            ng-click="$ctrl.generateReport()">
        Luo Raportti 
    </button>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
