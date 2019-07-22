/**
 * Defines the client-side implementation of a plugin for editing other plugins' answers in a formatted table
 */
import angular from "angular";
import * as t from "io-ts";
import {
    PluginBase,
    pluginBindings,
} from "tim/plugin/util";
import {$http, $httpParamSerializer} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {timApp} from "../app";
import {getParId} from "../document/parhelpers";
import {ViewCtrl} from "../document/viewctrl";
import {IDocument} from "../item/IItem";
import {showInputDialog} from "../ui/inputDialog";
import {GenericPluginMarkup, GenericPluginTopLevelFields, nullable, withDefault} from "./attributes";
import "./tableForm.css";
import {CellToSave, CellType, colnumToLetters, DataEntity, isPrimitiveCell, TimTable} from "./timTable";

const tableFormApp = angular.module("tableFormApp", ["ngSanitize"]);
export const moduleDefs = [tableFormApp];

const TableFormMarkup = t.intersection([
    t.partial({
        anonNames: nullable(t.boolean),
        autosave: t.boolean,
        hideButtonText: nullable(t.string),
        openButtonText: withDefault(t.string, "Avaa Taulukko/Raporttinäkymä"),

        hiddenColumns: t.array(t.number),
        hiddenRows: t.array(t.number),
        maxWidth: t.string,
        minWidth: t.string,
        maxRows: t.string,
        maxCols: withDefault(t.string, "fit-content"),
        open: t.boolean,
        filterRow: t.boolean,
        toolbarTemplates: t.array(t.object),

        cbColumn: t.boolean,
        nrColumn: t.boolean,
        groups: t.array(t.string),
        usernames: withDefault(t.boolean, true),
        realnames: withDefault(t.boolean, true),
        emails: withDefault(t.boolean, false),
        report: nullable(t.boolean),
        reportButton: nullable(t.string),
        separator: nullable(t.string),
        singleLine: t.boolean,
        sortBy: nullable(t.string), /* TODO! Username and task, or task and username -- what about points? */
        table: nullable(t.boolean),
        removeDocIds: withDefault(t.boolean, true),
        taskBorders: withDefault(t.boolean, false),
        removeUsersButtonText: nullable(t.string),
        userListButtonText: nullable(t.string),
        emailUsersButtonText: nullable(t.string),
        fontSize: withDefault(t.string, "smaller"),
        fixedColor: withDefault(t.string, "#f0f0f0"),
        saveStyles: withDefault(t.boolean, true),
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);

const Rows = t.dictionary(t.string, t.dictionary(t.string, t.union([t.string, t.null, t.number])));
const Styles = t.dictionary(t.string, t.dictionary(t.string, t.union([t.null, t.dictionary(t.string, t.string)])));

interface IRowsType extends t.TypeOf<typeof Rows> {
}

const TableFormAll = t.intersection([
    t.partial({
        aliases: t.dictionary(t.string, t.string),
        fields: t.array(t.string),
        realnamemap: t.dictionary(t.string, t.string),
        emailmap: t.dictionary(t.string, t.string),
        rows: Rows,
        styles: Styles,
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
    private data: TimTable & { userdata: DataEntity } = {
        hid: {edit: false, insertMenu: true, editMenu: true},
        hiddenRows: [],
        hiddenColumns: [],
        hideSaveButton: true,
        // lockCellCount: true,
        lockedCells: [],
        table: {countRow: 0, countCol: 0, columns: []},
        // TODO: give rows (and maybe colums) in data.table
        task: true,
        userdata: {type: "Relative", cells: {}},

        // saveCallBack: this.singleCellSave
    };
    // TODO: Change row format to properly typed format (maybe userobject:IRowstype) format
    private rows!: IRowsType;
    private styles!: t.TypeOf<typeof Styles>;
    private realnames = false;
    private usernames = false;
    private emails = false;
    private showTable = true;
    private realNameColumn = "A";
    private userNameColumn = "B";
    private emailColumn = "C";
    private realNameColIndex = 0;
    private userNameColIndex = 1;
    private emailColIndex = 2;
    private rowKeys!: string[];
    private userLocations: { [index: string]: string } = {};
    private taskLocations: { [index: string]: string } = {};
    private changedCells: string[] = []; // Use same type as data.userdata?
    private userlist: string = "";
    private emaillist: string = "";
    private emailsubject: string = "";
    private emailbody: string = "";
    private emailbcc: boolean = false;
    private emailbccme: boolean = true;
    private emailtim: boolean = true;
    private emailMsg: string = "";
    private listSep: string = "-";
    private listName: boolean = false;
    private listUsername: boolean = true;
    private listEmail: boolean = false;
    private fixedColor: string = "#f0f0f0";
    private cbCount: number = 0;

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

    addHiddenIndex(i: number) {
        if (!this.data.hiddenColumns) {
            this.data.hiddenColumns = [i];
        } else {
            this.data.hiddenColumns.push(i);
        }
    }

    checkToShow(param: boolean|undefined, i: number, def: boolean): boolean {
        if ( param == undefined) { param = def; }
        if ( param ) { return true; }

        this.addHiddenIndex(i);
        return false;
    }

    $onInit() {
        super.$onInit();
        const d: any =  this.data;
        const table: any =  this.data.table;
        if ( this.attrs.fontSize ) { table.fontSize = this.attrs.fontSize; }
        d.taskBorders = this.attrs.taskBorders;
        this.fixedColor = this.attrs.fixedColor || this.fixedColor;

        this.data.hiddenRows = this.attrs.hiddenRows;
        this.data.hiddenColumns = this.attrs.hiddenColumns;
        this.userfilter = "";
        this.realnames = this.checkToShow(this.attrs.realnames, this.realNameColIndex, true);
        this.usernames = this.checkToShow(this.attrs.usernames, this.userNameColIndex, true);
        this.emails    = this.checkToShow(this.attrs.emails,    this.emailColIndex,    false);

        this.rows = this.attrsall.rows || {};
        this.rowKeys = Object.keys(this.rows);
        this.styles = this.attrsall.styles || {};
        this.setDataMatrix();

        this.data.saveCallBack = (cellsTosave, colValuesAreSame) => this.cellChanged(cellsTosave, colValuesAreSame);
        if (this.attrs.saveStyles) {
            this.data.saveStyleCallBack = (cellsTosave, colValuesAreSame) => this.cellChanged(cellsTosave, colValuesAreSame);
        }
        this.data.cbCallBack = (cbs, n, index) => this.cbChanged(cbs, n, index);

        if (this.attrs.minWidth) {
            this.data.minWidth = this.attrs.minWidth;
        }
        if (this.attrs.maxWidth !== undefined) {
            this.data.maxWidth = this.attrs.maxWidth;
        }
        if (this.attrs.singleLine) {
            this.data.singleLine = this.attrs.singleLine;
        }
        if (this.attrs.open != undefined) {
            this.showTable = this.attrs.open;
        }

        this.data.cbColumn = this.attrs.cbColumn;
        this.data.nrColumn = this.attrs.nrColumn;
        this.data.filterRow = this.attrs.filterRow;
        this.data.maxRows = this.attrs.maxRows;
        this.data.maxCols = this.attrs.maxCols;
        this.data.toolbarTemplates = this.attrs.toolbarTemplates;
    }

    /**
     * Returns the TimTableController within the tableForm
     */
    getTimTable() {
        const parId = getParId(this.getPar());
        if (this.viewctrl && parId) {
            return this.viewctrl.getTableControllerFromParId(parId);
        }
    }

    /**
     * Sorts row key values (usernames) by their real name attribute in this.realnamemap
     * @param a username to compare with b
     * @param b username to compare with a
     */
    sortByRealName(a: string, b: string) {
        if (!this.attrsall.realnamemap) {
            return 0;
        }
        try {
            return this.attrsall.realnamemap[a].localeCompare(this.attrsall.realnamemap[b]);
        } catch (e) {
            return 0;
        }
    }

    sortByEmail(a: string, b: string) {
        if (!this.attrsall.emailmap) {
            return 0;
        }
        try {
            return this.attrsall.emailmap[a].localeCompare(this.attrsall.emailmap[b]);
        } catch (e) {
            return 0;
        }
    }

    /**
     * Clears tableForm rows and fetches new data to be put into rows
     * Basically just a reset
     */
    public async updateTable() {
        // TODO: Save before reset?
        // TODO: Emails etc?
        const timtab = this.getTimTable();
        if (!timtab) {
            return;
        }
        const tableResponse = await $http.get <{
            // TODO: All of these are probably not needed
            // TODO: Common type/interface for these?
            aliases: { [index: string]: string },
            fields: string[];
            realnamemap: { [index: string]: string },
            emailmap: { [index: string]: string },
            rows: IRowsType,
        }>("/tableForm/fetchTableData?" + $httpParamSerializer({
            taskid: this.getTaskId(),
        }));
        // TODO: Generic reset function
        console.log("debug");
        this.rows = tableResponse.data.rows || {};
        this.rowKeys = Object.keys(tableResponse.data.rows);
        this.userLocations = {};
        this.taskLocations = {};
        this.data.table.countCol = 0;
        this.data.table.countRow = 0;
        this.data.table.columns = [];
        this.data.userdata.cells = {};
        this.setDataMatrix();
        timtab.reInitialize();
        console.log("debug");

    }

    /**
     * Transforms user/task combination defined in this.rows into cell format and sets up the table
     * TODO: generate rows/columns for this.data.table, possibly needed for more easily maintained layout handling
     */
    setDataMatrix() {
        try {
            if ( this.realnames ) {
                this.rowKeys.sort((a, b) => this.sortByRealName(a, b));
            } else if ( this.usernames ) {

            } else {
                this.rowKeys.sort((a, b) => this.sortByEmail(a, b));
            }

            this.data.headers = ["Henkilön nimi", "Käyttäjänimi", "eMail"];
            this.data.headersStyle = {"backgroundColor": this.fixedColor,
                                      "font-weight": "bold"};

            if (this.attrsall.fields ) { this.data.table.countCol = this.attrsall.fields.length + 3; }
            this.data.table.countRow = Object.keys(this.rows).length;
            let y = 1;
            if (!this.data.lockedCells) {
                this.data.lockedCells = [];
            }
            for (const r of this.rowKeys) {
                this.data.userdata.cells[this.userNameColumn + y] = {cell: r, backgroundColor: this.fixedColor};
                this.data.lockedCells.push(this.userNameColumn + y);
                this.userLocations[y] = r;
                if ( this.attrsall.realnamemap ) {
                    this.data.userdata.cells[this.realNameColumn + y] = {
                        cell: this.attrsall.realnamemap[r],
                        backgroundColor: this.fixedColor,
                    };
                    this.data.lockedCells.push(this.realNameColumn + y);
                }
                if ( this.attrsall.emailmap ) {
                    this.data.userdata.cells[this.emailColumn + y] = {
                        cell: this.attrsall.emailmap[r],
                        backgroundColor: this.fixedColor,
                    };
                    this.data.lockedCells.push(this.emailColumn + y);
                }
                y++;
            }
            // TODO: Load default cell colors from tableForm's private answer?
            const xOffset = 3;
            if (this.attrsall.fields) {
                for (let x = 0; x < this.attrsall.fields.length; x++) {

                    const colheader = this.attrsall.fields[x];
                    this.data.headers.push(colheader);
                    /*
                    this.data.userdata.cells[colnumToLetters(x + xOffset) + 1] = {
                        cell: colheader,
                        backgroundColor: this.fixedColor,
                    };
                    */

                    let contentalias;
                    if (this.attrsall.aliases && colheader in this.attrsall.aliases) {
                        contentalias = this.attrsall.aliases[colheader];
                    } else {
                        contentalias = colheader;
                    }
                    this.taskLocations[colnumToLetters(x + xOffset)] = contentalias;
                    // this.data.lockedCells.push(colnumToLetters(x + xOffset) + 1);
                    // y = 0;
                    // for (const [u, r] of Object.entries(this.rows)) {
                    //     if (r[this.attrsall.fields[x]]) {
                    //         this.data.userdata.cells[colnumToLetters(x + xOffset) + (y + 2)] = r[this.attrsall.fields[x]];
                    //     }
                    //     y++;
                    // }
                    for (y = 0; y < this.rowKeys.length; y++) {
                        // this.data.userdata.cells[colnumToLetters(x + xOffset) + (y + 1)] = this.rows[this.rowKeys[y]][this.attrsall.fields[x]];
                        this.data.userdata.cells[colnumToLetters(x + xOffset) + (y + 1)] = Object.assign(
                            {cell: this.rows[this.rowKeys[y]][this.attrsall.fields[x]]},
                            this.styles[this.rowKeys[y]][this.attrsall.fields[x]],
                        );
                    }
                }
            }
        } catch (e) {
            console.log(e);
            this.error = "Error in setDataMatrix" + "\n" + e;
        }
    }

    /**
     * Clears the usernamefilter
     */
    initCode() {
        this.userfilter = "";
        this.error = undefined;
        this.result = undefined;
    }

    /**
     * Closes timTable's editor and saves the cell that is being currently edited
     */
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
        // return (this.attrs.table === true);
        if (this.attrs.table != undefined) {
            return this.attrs.table;
        } else {
            return true;
        }
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
    anonNames() {
        if (this.attrs.anonNames) {
            return this.attrs.anonNames;
        } else {
            return false;
        }
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
        const win = window.open("/tableForm/generateCSV?" + $httpParamSerializer({
            data: JSON.stringify(dataTable),
            separator: (this.attrs.separator || ","),
        }), "WINDOWID");
        if (win == null) {
            this.error = "Failed to open report window.";
        }
    }

    generateCSVTable() {
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        const result: CellType[][] = [];
        const rowcount = Object.keys(this.rows).length + 1;
        let colcount = 0;
        if (this.attrsall.fields && this.attrsall.fields.length) {
            colcount = this.attrsall.fields.length + 1;
        }
        for (let i = 0; i < rowcount; i++) {
            // TODO: In future: change hiddenRows check if hiddenRows is changed from number[] to IRows
            // TODO: Check for hiddenColumns
            if (this.data.hiddenRows && this.data.hiddenRows.includes(i)) { continue; }
            const row: CellType[] = [];
            result.push(row);
            for (let j = 0; j < colcount; j++) {
                if (this.data.hiddenColumns && this.data.hiddenColumns.includes(j)) { continue; }
                if (this.anonNames() && j == this.userNameColIndex && i > 0) {
                    row.push("Anonymous" + [i]);
                    continue;
                }
                if (this.anonNames() && j == this.realNameColIndex && i > 0) {
                    row.push("Unknown" + [i]);
                    continue;
                }
                row.push(timTable.cellDataMatrix[i][j].cell);
            }
        }
        return result;
    }

    /**
     * Make list of users colIndex.  Separate items by separators
     * @param users array of users
     * @param colIndex what index to use for list
     * @param preseparator what comes before evyry item
     * @param midseparator what comes between items
     */
    static makeUserList(users: string[][], colIndex: number, preseparator: string, midseparator: string): string {
        let result = "";
        let sep = "";
        for (const r of users) {
            result += sep + preseparator + r[colIndex];
            sep = midseparator;
        }
        return result;
    }

    /**
     * Removes selected users from the group
     */
    // @ts-ignore
    async removeUsers() {
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        const selUsers = timTable.getCheckedRows(0, true);
        let msg = "";
        for (const r of selUsers) {
            msg += r.join(", ") + "<br>";
        }
        if ( msg == "" ) { return; }

        if ( !this.attrs.groups ) { return; }
        const group = this.attrs.groups[0];

        await showInputDialog({
            defaultValue: "",
            text: "<b>Really remove following users from group:</b> " + group + "<br>\n<pre>\n" + msg + "\n</pre>",
            title: "Remove user from group " + group,
            isInput: false,
            validator: async () => {
                const ulist = TableFormController.makeUserList(selUsers, 1, "", ",");
                // /groups/removemember/group/ ulist
                const r = await to($http.get<IDocument>(`/groups/removemember/${group}/${ulist}`));
                if (r.ok) {
                    return {ok: true, result: r.result.data};
                } else {
                    return {ok: false, result: r.result.data.error};
                }
            },
        });
        location.reload();
    }

    listUsernames() {
        const timTable = this.getTimTable();
        if (timTable == null) { return; }
        let preseparator = " - ";
        let midseparator = "\n";
        let sep = this.listSep;
        const colindex = 0;
        const selUsers = timTable.getCheckedRows(0, true);
        const ulist = [];
        let usep = "";
        if ( !this.attrsall.realnamemap ) { return; }
        if ( !this.attrsall.emailmap ) { return; }
        for (const u of selUsers) {
            const un = u[this.userNameColIndex];
            let s = "";
            if ( this.listName ) { s = this.attrsall.realnamemap[un]; usep = ", "; }
            if ( this.listUsername ) { s += usep + un; usep = ", "; }
            if ( this.listEmail ) { s += usep + this.attrsall.emailmap[un]; usep = ", "; }
            usep = "";
            ulist.push([s]);
        }
        // if ( this.listEmail ) { midseparator = "\n"; preseparator = "";  }
        if ( sep == "" ) { sep = "\n"; }  // radio could not give \n?
        if ( sep != "-") { midseparator = sep; preseparator = ""; }
        this.userlist = TableFormController.makeUserList(ulist, colindex, preseparator, midseparator);
    }

    // @ts-ignore
    copyList() {
        const ta = this.element.find("#userlist");
        ta.focus(); ta.select(); document.execCommand("copy");
        // TODO: myös iPad toimimaan, ks GeoGebra tai csPlugin jaa tee yleinen copy
    }

    // @ts-ignore
    emailUsers() {
        const timTable = this.getTimTable();
        if (timTable == null) { return; }
        const selUsers = timTable.getCheckedRows(0, true);
        this.emaillist = TableFormController.makeUserList(selUsers, this.emailColIndex, "", "\n");
    }

    async sendEmailTim() {
        this.emailMsg = ""; // JSON.stringify(response);
        const url = this.pluginMeta.getAnswerUrl().replace("answer", "multiSendEmail");
        const response = await $http.post<string[]>(url, {
            rcpt: this.emaillist.replace(/\n/g, ";"),
            subject: this.emailsubject,
            msg: this.emailbody,
            bccme: this.emailbccme,
        });
        this.emailMsg = "Sent"; // JSON.stringify(response);
        return;
    }

    // @ts-ignore
    public async sendEmail() {
        if ( this.emailtim ) {
            this.sendEmailTim();
            return;
        }
        const w: any = window;
        // TODO: iPad do not like ;
        let  addrs = this.emaillist.replace(/\n/g, ",");
        let bcc = "";
        if ( this.emailbcc ) {
            bcc = addrs;
            addrs = "";
        }
        if ( this.emailbccme ) {
            if ( bcc ) { bcc += ","; }
            bcc += w.current_user.email;
        }
        window.location.href = "mailto:" + addrs
              + "?" + "subject=" + this.emailsubject
              + "&" + "body=" + this.emailbody
              + "&" + "bcc=" + bcc;
    }

    /**
     * Callback function to be noticed when check boxes are changed in table
     * @param cbs boolean list of cb-values
     * @param n number of visible checked cbs
     * @param index index of clicked cb, may be -1 if header row cb clicked
     */
    cbChanged(cbs: boolean[], n: number, index: number) {
        this.cbCount = n;
    }

    /**
     * Callback function that gets called when timTable saves a cell
     * @param cellsToSave list of cells that needs to be saved
     * @param colValuesAreSame if all values in on column has same value
     */
    cellChanged(cellsToSave: CellToSave[], colValuesAreSame: boolean) {
        // TODO make better implementation so singleCellSave is not called one by one
        // TODO: maybe done so that push cells to chengedCells and call save
        // TODO: but first check if saved to person or group and to that column by column
        for (const c of cellsToSave) {
            const coli = c.col;
            const rowi = c.row;
            const content = c.c;
            if (this.attrs.autosave) {
                this.singleCellSave(rowi, coli, content);
            } else {
                this.changedCells.push(colnumToLetters(coli) + (rowi + 1));
            }
        }
    }

    /**
     * Calls the actual save function with given cell
     * @param rowi row number
     * @param coli col number
     * @param content unused
     */
    singleCellSave(rowi: number, coli: number, content: string) {
        const cells = [colnumToLetters(coli) + (rowi + 1)];
        this.doSaveText(cells);
    }

    openTable() {
        this.showTable = true;
    }

    closeTable() {
        this.showTable = false;
    }

    /**
     * Transforms the cell format back to row format and saves the table input
     * @param cells
     */
    async doSaveText(cells: string[]) {
        this.error = "... saving ...";
        let keys;
        if (cells && cells.length > 0) {
            keys = cells;
        } else {
            // TODO: Force save all?
            // keys = Object.keys(this.data.userdata.cells);
            keys = this.changedCells;
        }
        const replyRows: { [index: string]: { [index: string]: CellType } } = {};
        const styleRows: { [index: string]: { [index: string]: string } } = {};
        try {
            for (const coord of keys) {
                const alphaRegExp = new RegExp("([A-Z]*)");
                const alpha = alphaRegExp.exec(coord);
                if (alpha == null) {
                    continue;
                }
                const columnPlace = alpha[0];
                const numberPlace = coord.substring(columnPlace.length);
                if (columnPlace === this.userNameColumn
                    || columnPlace === this.realNameColumn  // TODO: Do we need this anymore?
                    || columnPlace === this.emailColumn) {  // TODO: Do we need this anymore?
                    continue;
                }
                const cell = this.data.userdata.cells[coord];
                let cellContent;
                let cellStyle = null;
                // TODO: Save cell attributes (e.g backgroundColor) as plugin's own answer to let users take advantage
                //  of timTable's cell layout editing
                if (!isPrimitiveCell(cell)) {
                    cellContent = cell.cell;
                    if (this.attrs.saveStyles) {
                        const cellcopy = JSON.parse(JSON.stringify(cell));
                        delete cellcopy.cell;
                        // cellStyle = JSON.stringify(cellcopy);
                        cellStyle = cellcopy;
                    }
                } else {
                    cellContent = cell;
                }
                if (cellContent === null) {
                    cellContent = "";
                } else if (typeof cellContent === "boolean" || typeof cellContent === "number") {
                    cellContent = cellContent.toString();
                }
                // else if (typeof cellContent === "boolean") {
                //     throw new Error("cell was boolean?");

                try {
                    replyRows[this.userLocations[numberPlace]][this.taskLocations[columnPlace]] = cellContent;
                } catch (TypeError) {
                    replyRows[this.userLocations[numberPlace]] = {};
                    replyRows[this.userLocations[numberPlace]][this.taskLocations[columnPlace]] = cellContent;
                }
                if (cellStyle != null && Object.keys(cellStyle).length != 0) {
                    // TODO
                    const taskWithField = this.taskLocations[columnPlace].split(".");
                    const docTaskStyles = taskWithField[0] + "." + taskWithField[1] + ".styles";
                    replyRows[this.userLocations[numberPlace]][docTaskStyles] = cellStyle;
                }
            }
        } catch (e) {
            console.log(e);
            this.error = "Error in doSaveText" + "\n" + e;
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
            // this.result = "Saved";
        } else {
            this.error = r.result.data.error; // "Infinite loop or some other error?";
        }
        // TODO: Clear changedCells?
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
<div class="tableform" ng-if="$ctrl.showTable">
    <!--<button class="timButton"
            ng-click="$ctrl.updateTable()">
            Reset table
    </button>-->
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" ng-bind-html="::$ctrl.stem"></p>
    <tim-table disabled="!$ctrl.tableCheck()" data="::$ctrl.data"
               taskid="{{$ctrl.pluginMeta.getTaskId()}}" plugintype="{{$ctrl.pluginMeta.getPlugin()}}"></tim-table>

    <div class="hidden-print">
    <button class="timButton"
            ng-if="::$ctrl.tableCheck() && !$ctrl.attrs.autosave"
            ng-click="$ctrl.saveText()">
            {{ ::$ctrl.buttonText() }}
    </button>
    <button class="timButton"
            ng-if="::$ctrl.reportCheck()"
            ng-click="$ctrl.generateReport()">
            {{ ::$ctrl.reportButton() }}
    </button>
    <button class="timButton"
            ng-click="$ctrl.closeTable()"
            ng-if="::$ctrl.attrs.hideButtonText">
            {{::$ctrl.attrs.hideButtonText}}
    </button>
    <button class="timButton"
            ng-click="$ctrl.removeUsers()"
            ng-if="$ctrl.attrs.removeUsersButtonText && $ctrl.cbCount">
            {{::$ctrl.attrs.removeUsersButtonText}}
    </button>
    <button class="timButton"
            ng-click="$ctrl.listUsernames()"
            ng-if="$ctrl.attrs.userListButtonText && $ctrl.cbCount">
            {{::$ctrl.attrs.userListButtonText}}
    </button>
    <button class="timButton"
            ng-click="$ctrl.emailUsers()"
            ng-if="$ctrl.attrs.emailUsersButtonText && $ctrl.cbCount">
            {{::$ctrl.attrs.emailUsersButtonText}}
    </button>
    </div>
    <div class="csRunDiv tableUsers" style="padding: 1em;" ng-if="$ctrl.userlist"> <!-- userlist -->
        <p class="closeButton" ng-click="$ctrl.userlist=''"></p>
        <p>Separator:
        <label><input type="radio" name="listsep" ng-model="$ctrl.listSep" value = "-" ng-change="$ctrl.listUsernames()">-</label>&nbsp;
        <label><input type="radio" name="listsep" ng-model="$ctrl.listSep" value = "," ng-change="$ctrl.listUsernames()">,</label>&nbsp;
        <label><input type="radio" name="listsep" ng-model="$ctrl.listSep" value = "|" ng-change="$ctrl.listUsernames()">|</label>&nbsp;
        <label><input type="radio" name="listsep" ng-model="$ctrl.listSep" value = ";" ng-change="$ctrl.listUsernames()">;</label>&nbsp;
        <label><input type="radio" name="listsep" ng-model="$ctrl.listSep" value = "\n" ng-change="$ctrl.listUsernames()">\\n</label>&nbsp;
        </p>
        <label><input type="checkbox" ng-model="$ctrl.listName" ng-change="$ctrl.listUsernames()">Name</label>&nbsp;
        <label><input type="checkbox" ng-model="$ctrl.listUsername" ng-change="$ctrl.listUsernames()">Username</label>&nbsp;
        <label><input type="checkbox" ng-model="$ctrl.listEmail" ng-change="$ctrl.listUsernames()">Email</label>&nbsp;
        <br>
        <textarea id="userlist" ng-model="$ctrl.userlist" rows="10" cols="60"></textarea>
        <button class="timButton"
                ng-click="$ctrl.copyList()">
                Copy
        </button>
    </div>
    <div class="csRunDiv tableEmail" style="padding: 1em;" ng-if="$ctrl.emaillist"> <!-- email -->
        <p class="closeButton" ng-click="$ctrl.emaillist=''"></p>
        <p><textarea id="emaillist" ng-model="$ctrl.emaillist" rows="4" cols="40"></textarea><p>
        <p>
        <label title="Send so that names are not visible (works only non-TIM send)"><input type="checkbox" ng-model="$ctrl.emailbcc">BCC</label>&nbsp;
        <label title="Send also a copy for me"><input type="checkbox" ng-model="$ctrl.emailbccme" >BCC also for me</label>&nbsp;
        <label title="Send using TIM.  Every mail is send as a personal mail."><input type="checkbox" ng-model="$ctrl.emailtim" >use TIM to send</label>&nbsp;
        </p>
        <p>Subject: <input ng-model="$ctrl.emailsubject" size="60"></p>
        <p>eMail content:</p>
        <p><textarea id="emaillist" ng-model="$ctrl.emailbody" rows="10" cols="70"></textarea></p>
        <p>
        <button class="timButton"
                ng-click="$ctrl.sendEmail()">
                Lähetä
        </button>
        <!-- <span class="emailMsg" ng-model="$ctrl.emailMsg"></span> -->
        <span class="savedtext" ng-if="$ctrl.emailMsg">Sent!</span>
        </p>
    </div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <pre ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
<div class="tableOpener" ng-if="!$ctrl.showTable">
    <button class="timButton"
            ng-click="$ctrl.openTable()">
            {{::$ctrl.attrs.openButtonText}}
    </button>
</div>
<br>
`,
});
