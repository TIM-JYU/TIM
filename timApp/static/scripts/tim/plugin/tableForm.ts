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
import {
    CellDataEntity,
    CellType,
    colnumToLetters,
    DataEntity,
    isPrimitiveCell,
    TimTable,
    TimTableController
} from "./timTable";
import "./tableForm.css";


const tableFormApp = angular.module("tableFormApp", ["ngSanitize"]);
export const moduleDefs = [tableFormApp];

const TableFormMarkup = t.intersection([
    t.partial({
        initword: t.string,
        table: nullable(t.boolean),
        report: nullable(t.boolean),
        separator: nullable(t.string),
        shownames: nullable(t.boolean),
        sortBy: nullable(t.string), /* TODO! Username and task, or task and username -- what about points? */
        buttonText: nullable(t.string),
        reportButton: nullable(t.string),
        autosave: t.boolean,
        realnames: t.boolean,

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
        aliases: t.dictionary(t.string,t.string),
        contentMap: t.dictionary(t.string,t.string),
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
    private realnames = false;

    getDefaultMarkup() {
        return {};
    }

    // noinspection JSUnusedGlobalSymbols
    /**
    * Used to define table view & relative save button in angular, true or false.
    */
     buttonText() {
        return (this.attrs.buttonText || "Tallenna taulukko");
    }

    // noinspection JSUnusedGlobalSymbols
    /**
    * Used to define table view & relative save button in angular, true or false.
    */
     reportButton() {
        return (this.attrs.reportButton || "Luo Raportti");
    }

    async $onInit() {
        super.$onInit();
        this.userfilter = "";
        this.allRows = this.attrsall.rows || {};
        this.realnames = true;
        if (this.attrs.realnames == false) this.realnames = false;
        this.rows = this.allRows;
        this.setDataMatrix();
        this.oldCellValues = JSON.stringify(this.data.userdata.cells);
        if (this.attrs.autosave) this.data.saveCallBack = (rowi, coli, content) => this.singleCellSave(rowi, coli, content);
        console.log("eaaa");
    }

    getTimTable() {
        const parId = getParId(this.getPar());
        if (this.viewctrl && parId) {
            return this.viewctrl.getTableControllerFromParId(parId);
        }
    }

    //$doCheck() {
    //    //TODO: Possibly obsolete after this.singleCellSave() implemented and data.saveCallback given to timtable
    //    if(this.attrs.autosave && this.oldCellValues)
    //    {
    //       //TODO: Create proper object for comparing new and old celldata
    //       const currAsString = JSON.stringify(this.data.userdata.cells);
    //       if(this.oldCellValues != currAsString)
    //       {
    //           this.oldCellValues = currAsString
    //           //TODO: Find cell difference and send only minimum amount of data to the server
    //           this.doSaveText(false);
    //       }

    //    }
    //}

    setDataMatrix() {
        this.data.userdata.cells["A1"] = {cell: "Käyttäjänimi", backgroundColor: "#efecf1"};
        if (this.realnames) this.data.userdata.cells["B1"] = {cell: "Henkilön Nimi", backgroundColor: "#efecf1"};
        if(this.attrsall.fields && this.realnames) this.data.table.countCol = this.attrsall.fields.length + 2;
        else if(this.attrsall.fields) this.data.table.countCol = this.attrsall.fields.length + 1;
        this.data.table.countRow = Object.keys(this.rows).length + 1;
        let y = 2;
        for (const r of Object.keys(this.rows)) {
            this.data.userdata.cells["A" + y] = {cell: r, backgroundColor: "#efecf1"};
            this.data.lockedCells.push("A" + y);
            if (this.realnames) {
                this.data.userdata.cells["B" + y] = {cell: this.rows[r]['realname'], backgroundColor: "#efecf1"};
                this.data.lockedCells.push("B" + y);
            }
            y++;
        }
        // TODO: Load default cell colors from tableForm's private answer?
        let xOffset = 1;
        if(this.realnames) xOffset = 2
        if (this.attrsall.fields) {
            for (let x = 0; x < this.attrsall.fields.length; x++) {
                this.data.userdata.cells[colnumToLetters(x + xOffset) + 1] =  {cell: this.attrsall.fields[x], backgroundColor: "#efecf1"};
                this.data.lockedCells.push(colnumToLetters(x + xOffset) + 1);
                y = 0;
                for (const [u, r] of Object.entries(this.rows)) {
                    if (r[this.attrsall.fields[x]]) {
                        this.data.userdata.cells[colnumToLetters(x + xOffset) + (y + 2)] = r[this.attrsall.fields[x]];
                    }
                    y++;
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
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        timTable.saveAndCloseSmallEditor();
        this.doSaveText([]);
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Returns true value, if table attribute is true.
     * Used to define table view & relative save button in angular, true or false.
     */
    tableCheck() {
        return (this.attrs.table == true);
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Returns true value, if report attribute is true.
     * Used to define create report button in angular, true or false.
     */
    reportCheck() {
        return (this.attrs.report == true);
    }

    /**
     * Boolean to determinate if usernames are viewed in report.
     * Choises are true for username and false for anonymous. Username/true as default.
     */
    shownames() {
        if (this.attrs.shownames) {
            return this.attrs.shownames;
        }
        else return false;
    }

    /**
     * String to determinate how usernames are filtered in report.
     * Choises are username, username and full name and anonymous. Username as default.
     */
    sortBy() {
        return (this.attrs.sortBy || "username");
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Generates report based on the table.
     * Used if report is set to true and create report button is clicked.
     * Used to define table view & relative save button in angular, true or false.
     */
    generateReport() {
        const dataTable = this.generateCSVTable();
        const win = window.open("/tableForm/generateCSV?" + $httpParamSerializer({data: JSON.stringify(dataTable), separator: (this.attrs.separator || ",")}), "WINDOWID");
        if (win == null) {
            this.error;
        }
    }

    generateCSVTable() {
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        let result: CellType[][] = [];
        let rowcount = Object.keys(this.allRows).length + 1;
        let colcount = 0;
        if (this.attrsall.fields && this.attrsall.fields.length) {
            colcount = this.attrsall.fields.length +1;
        }
        for (let i = 0; i < rowcount; i++) {
            const row: CellType[] = [];
            result.push(row);
            for(let j = 0; j < colcount; j++) {
                if (!this.shownames() && j == 0 && i > 0) {
                    row.push("Anonymous" + [i]);
                    continue;
                }
                row.push(timTable.cellDataMatrix[i][j].cell);
            }
        }
        return result;
    }

    updateFilter() {
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        //TODO check if better way to save than just making saveAndCloseSmallEditor public and calling it
        timTable.saveAndCloseSmallEditor();
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
    // singleCellSave(rowi: number, coli: number, content: string)
    singleCellSave(rowi: number, coli: number, content: string){
        const cells = ["A" + (rowi+1),colnumToLetters(coli) + 1,colnumToLetters(coli)+(rowi+1)]
        this.doSaveText(cells)
    }

    async doSaveText(cells: string[]) {
        //TODO: Optimise save? (keep track of userLocations and taskLocations at SetDataMatrix)
        // would need new location info if timtable implements sort
        this.error = "... saving ...";
        let keys;
        if(cells && cells.length > 0)
            keys = cells;
        else
            keys = Object.keys(this.data.userdata.cells);
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
            } else if(this.realnames && columnPlace === "B") {
                continue;
            }
            else if (numberPlace === "1") {
                let contentalias;
                if (this.attrsall.aliases && cellContent in this.attrsall.aliases)
                    contentalias = this.attrsall.aliases[cellContent]
                else contentalias = cellContent
                let contentfield = "";
                if (this.attrsall.contentMap && contentalias in this.attrsall.contentMap)
                    contentfield = "|" + this.attrsall.contentMap[contentalias]
                taskLocations[columnPlace] = contentalias + contentfield;
            } else {
                // let contentAlias = "";
                // if (this.attrsall.contentMap && taskLocations[columnPlace] in this.attrsall.contentMap)
                //     contentAlias = "|" + this.attrsall.contentMap[taskLocations[columnPlace]]
                // replyRows[userLocations[numberPlace]][taskLocations[columnPlace]] = cellContent + contentAlias;
                replyRows[userLocations[numberPlace]][taskLocations[columnPlace]] = cellContent
            }
        }
        const params = {
            input: {
                nosave: false,
                replyRows: replyRows,
            },
        };
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
    <div class="form-inline" ng-if="::$ctrl.tableCheck()"><label>Suodata {{::$ctrl.inputstem}} <span>
        <input type="text"
               class="form-control"
               ng-model="$ctrl.userfilter"
               ng-model-options="::$ctrl.modelOpts"
               ng-trim="false"
               ng-change="$ctrl.updateFilter()"
               ng-readonly="::$ctrl.readonly"
               size="{{::$ctrl.cols}}"></span></label>
        <tim-table disabled="!$ctrl.tableCheck()" data="::$ctrl.data" taskid="{{$ctrl.pluginMeta.getTaskId()}}" plugintype="{{$ctrl.pluginMeta.getPlugin()}}"></tim-table>
        <!-- TODO: taskid="{{ $ctrl.pluginm }}", vie pluginmeta & taskid-->
    </div>
    <button class="timButton"
            ng-if="::$ctrl.tableCheck()"
            ng-click="$ctrl.saveText()">
            {{ ::$ctrl.buttonText() }}
    </button>
    <button class="timButton"
            ng-if="::$ctrl.reportCheck()"
            ng-click="$ctrl.generateReport()">
            {{ ::$ctrl.reportButton() }}
    </button>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <pre ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
