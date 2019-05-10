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
import {$http, $timeout} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";
import {None} from "../../jspm_packages/npm/fp-ts@1.11.1/lib/Option";
import {timApp} from "../app";
import enumerate = Reflect.enumerate;
import {getParId} from "../document/parhelpers";
import {ViewCtrl} from "../document/viewctrl";

const tableFormApp = angular.module("tableFormApp", ["ngSanitize"]);
export const moduleDefs = [tableFormApp];

const TableFormMarkup = t.intersection([
    t.partial({
        initword: t.string,
        table: nullable(t.boolean),
        report: nullable(t.boolean),
        separator: nullable(t.string), /* TODO! Separate columns with user given character for report */
        usednames: nullable(t.string), /* TODO! username and full name, username or anonymous */
        sortBy: nullable(t.string), /* TODO! Username and task, or task and username -- what about points? */
        /* answerAge: nullable(t.string), /* TODO! Define time range from which answers are fetched. Maybe not to be implemented! */
        dataCollection: nullable(t.string), /* TODO! Filter by data collection consent: allowed, denied or both */
        print: nullable(t.string), /* TODO! Headers and answers, headers, answers, answers w/o separator line, (or Korppi export?) */
        autosave: t.boolean,

    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const Rows = t.dictionary(t.string, t.dictionary(t.string, t.union([t.string, t.null])));

interface RowsType extends t.TypeOf<typeof Rows> {
}

const TableFormAll = t.intersection([
    t.partial({
        rows: Rows,
        fields: t.array(t.string),
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
    private rows!: RowsType;
    // private hiderows: number[] = [];
    private allRows!: {};
    private tabledata: any;
    private timTableData!: {};
    private modelOpts!: INgModelOptions;
    private oldCellValues!: string;

    // initialized in $onInit, so need to assure TypeScript with "!"

    getDefaultMarkup() {
        return {};
    }

    // THIS WILL BE REMOVED AS WE IMPLEMENT A 2 BUTTON SOLUTION
    //
    // buttonText() {
    //     return super.buttonText() || "Tallenna taulukko";
    // }

    async $onInit() {
        super.$onInit();
        this.userfilter = "";
        this.allRows = this.attrsall.rows || {};
        this.rows = this.allRows;
        this.data.task = true;
        this.data.hiderows = [];
        this.data.tableForm = true;
        this.data.lockedCells = ["A1"];
        // this.fields = this.attrsall.fields;
        this.setDataMatrix();
        const parId = getParId(this.getPar());
        if (parId && this.viewctrl) {
            await $timeout(0);
            const t = this.viewctrl.getTableControllerFromParId(parId);
            //console.log(t);
        }
        this.oldCellValues = JSON.stringify(this.data.userdata.cells);

    }

    $doCheck() {
        if(this.attrs.autosave && this.oldCellValues)
        {
           //TODO: Create proper object for comparing new and old celldata
           const currAsString = JSON.stringify(this.data.userdata.cells);
           if(this.oldCellValues != currAsString)
           {
               this.oldCellValues = currAsString
               //TODO: Find cell difference and send only minimum amount of data to the server
               //this.saveCells(~);
               this.doSaveText(false);
           }

        }
    }

    setDataMatrix() {
        // for (const row in this.rows){
        //      console.log("eaa");
        //  }
        this.data.userdata = {cells: {}};
        let x = 2;
        for (const r of Object.keys(this.rows)) {
            this.data.userdata.cells["A" + x] = {cell: r, backgroundColor: "#efecf1"};
            this.data.lockedCells.push("A" + x);
            x++;
        }
        // TODO: Load default cell colors from tableForm's private answer?
        if (this.attrsall.fields) {
            for (let y = 0; y < this.attrsall.fields.length; y++) {
                this.data.userdata.cells[this.colnumToLetters(y + 1) + 1] =  {cell: this.attrsall.fields[y], backgroundColor: "#efecf1"};
                this.data.lockedCells.push(this.colnumToLetters(y + 1) + 1);
                x = 0;
                for (const [u, r] of Object.entries(this.rows)) {

                    if (r[this.attrsall.fields[y]]) {
                        // @ts-ignore
                        this.data.userdata.cells[this.colnumToLetters(y + 1) + (x + 2)] = r[this.attrsall.fields[y]];

                    }
                    x++;
                }
            }
        }
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

    /**
     * Returns true value, if table attribute is true.
     * Used to define table view & relative save button in angular, true or false.
     */
    tableCheck() {
        return (this.attrs.table == true);
    }

    /**
     * Returns true value, if report attribute is true.
     * Used to define create report button in angular, true or false.
     */
    reportCheck() {
        return (this.attrs.report == true);
    }

    /**
     * Generates report based on the table. TODO!
     * Used if report is set to true and create report button is clicked.
     */
    generateReport() {
        console.log(this.separator(), this.names());
    }

    /**
     * String (or character) to separate fields in report.
     * Used in report to define how fields/values are separated.
     */
    separator() {
        return (this.attrs.separator || ";");
    }

    /**
     * String to determinate how user names are viewed in report.
     * Choises are username, username and full name and anonymous.
     */
    names() {
        return (this.attrs.usednames || "username");
    }

    /**
     * TODO! VERY MUCH !!
     * String to determinate how user names are viewed in report.
     * Choises are username, username and full name and anonymous.
     */
    sortBy() {
        return (this.attrs.sortBy || "username");
    }

    updateFilter() {
        this.data.hiderows = [];
        if (this.userfilter != "" && this.userfilter != undefined) {
            const reg = new RegExp(this.userfilter.toLowerCase());
            let rowi = 1;
            for (const [key, value] of Object.entries(this.allRows)) {
                if (!reg.test(key.toLowerCase())) {
                    this.data.hiderows.push(rowi);
                }
                rowi++;
            }
        }
        //TODO: Call timTable and close the editor if open
    }

    async doSaveText(nosave: boolean) {
        this.error = "... saving ...";
        const keys = Object.keys(this.data.userdata.cells);
        keys.sort();
        const userLocations = {};
        const taskLocations = {};
        const replyRows = {};
        for (const coord of keys) {
            const alphaRegExp = new RegExp("([A-Z]*)");
            const alpha = alphaRegExp.exec(coord);
            const value = coord;

            if (alpha == null) {
                continue;
            }
            const columnPlace = alpha[0];
            const numberPlace = coord.substring(columnPlace.length);
            let cellContent = this.data.userdata.cells[coord];
            // TODO: Save cell attributes (e.g backgroundColor) as plugin's own answer
            if (typeof cellContent != "string") { cellContent = cellContent.cell; }
            if (columnPlace === "A") {
                if (numberPlace === "1") { continue; }
                // @ts-ignore
                userLocations[numberPlace] = cellContent;
                // @ts-ignore
                replyRows[cellContent] = {};
            } else if (numberPlace === "1") {
                // @ts-ignore
                taskLocations[columnPlace] = cellContent;
            } else {
                // @ts-ignore
                replyRows[userLocations[numberPlace]][taskLocations[columnPlace]] = cellContent;
            }
        }
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
