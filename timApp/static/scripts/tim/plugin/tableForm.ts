/**
 * Defines the client-side implementation of an example plugin (tableForm).
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {
    GenericPluginMarkup,
    GenericPluginTopLevelFields,
    nullable,
    PluginBase,
    pluginBindings,
    withDefault,
} from "tim/plugin/util";
import {$http, $httpParamSerializer, $timeout} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {timApp} from "../app";
import {getParId} from "../document/parhelpers";
import {ViewCtrl} from "../document/viewctrl";
import {CellType, colnumToLetters, DataEntity, ICell, isPrimitiveCell, TimTable, TimTableController} from "./timTable";
import "./tableForm.css";


const tableFormApp = angular.module("tableFormApp", ["ngSanitize"]);
export const moduleDefs = [tableFormApp];

const TableFormMarkup = t.intersection([
    t.partial({
        initword: t.string,
        table: nullable(t.boolean),
        report: nullable(t.boolean),
        separator: nullable(t.string), /* TODO! Separate columns with user given character for report */
        shownames: t.boolean,
        sortBy: nullable(t.string), /* TODO! Username and task, or task and username -- what about points? */
        /* answerAge: nullable(t.string), /* TODO! Define time range from which answers are fetched. Maybe not to be implemented! */
        dataCollection: nullable(t.string), /* TODO! Filter by data collection consent: allowed, denied or both */
        autosave: t.boolean,

    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const Rows = t.dictionary(t.string, t.dictionary(t.string, t.union([t.string, t.null, t.number])));

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
    // public cellDataMatrix: ICell[][] = [];
    private result?: string;
    private error?: string;
    private isRunning = false;
    private userfilter = "";
    private data: TimTable & {userdata: DataEntity} = {
        hiderows:[],
        table:{countRow:0, countCol:0},
        hideSaveButton:true,
        lockedCells:[],
        hid:{edit:false},
        userdata:{type:"Relative", cells:{}},
        task: true,
        //saveCallBack: this.singleCellSave
    };
    private rows!: RowsType;
    private allRows!: {};
    private modelOpts!: INgModelOptions;
    private oldCellValues!: string;

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
        this.setDataMatrix();
        this.oldCellValues = JSON.stringify(this.data.userdata.cells);
        if(this.attrs.autosave) this.data.saveCallBack = this.singleCellSave;
        console.log("eaaa");
    }

    getTimTable() {
        const parId = getParId(this.getPar());
        if (this.viewctrl && parId) {
            return this.viewctrl.getTableControllerFromParId(parId);
        }
    }

    $doCheck() {
        //TODO: Possibly obsolete after this.singleCellSave() implemented and data.saveCallback given to timtable
        if(this.attrs.autosave && this.oldCellValues)
        {
           //TODO: Create proper object for comparing new and old celldata
           const currAsString = JSON.stringify(this.data.userdata.cells);
           if(this.oldCellValues != currAsString)
           {
               this.oldCellValues = currAsString
               //TODO: Find cell difference and send only minimum amount of data to the server
               this.doSaveText(false);
           }

        }
    }

    setDataMatrix() {
        this.data.lockedCells.push("A1");
        if(this.attrsall.fields) this.data.table.countCol = this.attrsall.fields.length + 1;
        this.data.table.countRow = Object.keys(this.rows).length + 1;
        let x = 2;
        for (const r of Object.keys(this.rows)) {
            this.data.userdata.cells["A" + x] = {cell: r, backgroundColor: "#efecf1"};
            this.data.lockedCells.push("A" + x);
            x++;
        }
        // TODO: Load default cell colors from tableForm's private answer?
        if (this.attrsall.fields) {
            for (let y = 0; y < this.attrsall.fields.length; y++) {
                this.data.userdata.cells[colnumToLetters(y + 1) + 1] =  {cell: this.attrsall.fields[y], backgroundColor: "#efecf1"};
                this.data.lockedCells.push(colnumToLetters(y + 1) + 1);
                x = 0;
                for (const [u, r] of Object.entries(this.rows)) {
                    if (r[this.attrsall.fields[y]]) {
                        this.data.userdata.cells[colnumToLetters(y + 1) + (x + 2)] = r[this.attrsall.fields[y]];

                    }
                    x++;
                }
            }
        }
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
     * String (or character) to separate fields in report.
     * Used in report to define how fields/values are separated, ';' as default.
     */
    separator() {
        return (this.attrs.separator || ";");
    }

    /**
     * Boolean to determinate if usernames are viewed in report.
     * Choises are true for username and false for anonymous. Username/true as default.
     */
    shownames() {
        return (this.attrs.shownames || true);
    }

    /**
     * String to determinate how usernames are filtered in report.
     * Choises are username, username and full name and anonymous. Username as default.
     */
    sortBy() {
        return (this.attrs.sortBy || "username");
    }

    /**
     * String to determinate what kind of data can be collected to the report.
     * Choises are allowed, denied and both. Allowed as default.
     */
    taskIDs() {
        return (this.attrs.dataCollection || "any");
    }

    // /**
    //  * String to determinate how the CSV is printed.
    //  * Choises are true to show name. All as default.
    //  */
    // print() {
    //     return (this.attrs.print || true);
    // }

    /**
     * Generates report based on the table. TODO!
     * Used if report is set to true and create report button is clicked.
     */
    generateReport() {
        console.log(this.separator(), this.shownames(), this.sortBy());
        const dataTable = this.generateCSVTable();
        console.log(dataTable);
        // window.open(this.pluginMeta.getAnswerUrl() + "/report.csv");
        const win = window.open("/tableForm/generateCSV?" + $httpParamSerializer({data: dataTable}), "WINDOWID");
        if (win != null) {
            const doc = win.document;
            // doc.open("text/plain");
            // doc.close();
        }
        else (this.error);
    }

    generateCSVTable() {
        const timTable = this.getTimTable();
        if (timTable == null) {
            console.log("timtable was undefined");
            return;
        }
        var rowcount = Object.keys(this.allRows).length + 1;
        var colcount = 0;
        if (this.attrsall.fields && this.attrsall.fields.length) {
            colcount = this.attrsall.fields.length +1;
        }
        console.log(timTable.cellDataMatrix);
        return timTable.cellDataMatrix[1][0].cell;
    }

    updateFilter() {
        const timTable = this.getTimTable();
        if (timTable == null) {
            console.log("timtable was undefined");
            return;
        }
        //TODO check if better way to save than just making saveAndCloseSmallEditor public and calling it
        if(timTable) timTable.saveAndCloseSmallEditor();
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
    }

    singleCellSave(){
        console.log("fgsdsfd");
    }

    async doSaveText(nosave: boolean) {
        const timTable = this.getTimTable();
        if (timTable != null) {
            timTable.saveAndCloseSmallEditor();
        }
        this.error = "... saving ...";
        const keys = Object.keys(this.data.userdata.cells);
        keys.sort();
        const userLocations: {[index: string]: string} = {};
        const taskLocations: {[index: string]: string} = {};
        const replyRows: {[index: string]: {[index: string]: CellType}} = {};
        for (const coord of keys) {
            const alphaRegExp = new RegExp("([A-Z]*)");
            const alpha = alphaRegExp.exec(coord);

            if (alpha == null) {
                continue;
            }
            const columnPlace = alpha[0];
            const numberPlace = coord.substring(columnPlace.length);
            const cell = this.data.userdata.cells[coord];
            let cellContent;
            // TODO: Save cell attributes (e.g backgroundColor) as plugin's own answer
            if (!isPrimitiveCell(cell)) {
                cellContent = cell.cell;
            } else {
                cellContent = cell;
            }
            if (cellContent === null) {
                cellContent = "";
            } else if (typeof cellContent === "boolean") {
                throw new Error("cell was boolean?");
            } else if (typeof cellContent === "number") {
                cellContent = cellContent.toString();
            }
            if (columnPlace === "A") {
                if (numberPlace === "1") { continue; }
                userLocations[numberPlace] = cellContent;
                replyRows[cellContent] = {};
            } else if (numberPlace === "1") {
                taskLocations[columnPlace] = cellContent;
            } else {
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
            //this.result = "Saved";
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
<div class="tableform" style="overflow-x: scroll">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" ng-bind-html="::$ctrl.stem"></p>
    <div class="form-inline" ng-if="::$ctrl.tableCheck()"><label> Suodata {{::$ctrl.inputstem}} <span>
        <input type="text"
               class="form-control"
               ng-model="$ctrl.userfilter"
               ng-model-options="::$ctrl.modelOpts"
               ng-trim="false"
               ng-change="$ctrl.updateFilter()"
               ng-readonly="::$ctrl.readonly"
               size="{{::$ctrl.cols}}"></span></label>
        <tim-table data="::$ctrl.data" taskid="{{$ctrl.pluginMeta.getTaskId()}}" plugintype="{{$ctrl.pluginMeta.getPlugin()}}"></tim-table>
        <!-- TODO: taskid="{{ $ctrl.pluginm }}", vie pluginmeta & taskid-->
    </div>
    <button class="timButton"
            ng-if="::$ctrl.tableCheck()"
            ng-click="$ctrl.saveText()">
        Tallenna taulukko
    </button>
    <button class="timButton"
            ng-if="::$ctrl.reportCheck()"
            ng-click="$ctrl.generateReport()">
        Luo Raportti 
    </button>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <pre ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
