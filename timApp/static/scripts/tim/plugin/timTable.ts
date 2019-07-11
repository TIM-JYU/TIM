/**
 * Implementation for TimTable
 *
 * The table can be sorted by clicking the header row.
 * The sort order is kept in permutation table permTable and the real data is
 * not sorted, only showed using the perumtation table.
 *
 * That means that there is 3 types on coordinates:
 * - spredsheet-like coodinates, example D7
 * - cell indecies by orgininal indecies of cells
 * - screen indecies for the shell
 *
 * So one must be carafull what coordinates to use and when.
 * Especially for areas there is type SelectedCells that keeps track from both.
 * At the moment it is supposed that selection must allways be rectangular area and
 * columns are not sorted. If one adds posibility to change order of columns,
 * then the behaviour of SelelctedCell-interface must be changed.
 *
 * To change from original coord to sceencoord there is permTableToScreen
 * Mostly the screencoordinates starts in code by s like sy, srow and so on.
 */
// TODO: Static headers and filter rows so they do not scroll
// TODO: Toolbar visible only when hit an editable field (not locked)
// TODO: Toolbar shall not steal focus when created first time
// TODO: save filter and sort conditions
// TODO: Show sort icons weakly, so old icons with gray
// TODO: set styles also by list of cells like value

import {IController, IRootElementService, IScope} from "angular";
import {getParId} from "tim/document/parhelpers";
import {timApp} from "../app";
import {onClick} from "../document/eventhandlers";
import {ViewCtrl} from "../document/viewctrl";
import {ParCompiler} from "../editor/parCompiler";
import {openEditorSimple} from "../editor/pareditor";
import {DestroyScope} from "../ui/destroyScope";
import {isArrowKey, KEY_DOWN, KEY_ENTER, KEY_ESC, KEY_F2, KEY_LEFT, KEY_RIGHT, KEY_TAB, KEY_UP} from "../util/keycodes";
import {$http, $timeout} from "../util/ngimport";
import {Binding} from "../util/utils";
import {hideToolbar, isToolbarEnabled, openTableEditorToolbar} from "./timTableEditorToolbar";
import {PluginMeta} from "./util";

/**
 * NumFilter class for filtering numbers.
 *
 * <2     => find all numbers less than 2
 * <=2    => find all numbers less or equal than 2
 * >2     => find all numbers greter than 2
 * >=2    => find all numbers greter or equal than 2
 * =2     => find all numbers equal to 2
 * >2<4   => find all numbers in range ]2,4[
 * >=2<=4 => find all numbers in range [2,4]
 * >2<4!  => find all numbers not in range ]2,4[
 * See: https://regex101.com/r/yfHbaH/3
 */
const numFilterEx: RegExp = /([<=>!]=?) *(-?[\w\.,]+) *(!?) */g;

type NumStr = number | string;
type FilterComparator = (a: NumStr, b: NumStr) => boolean;

const filterComparatorOperators = {
    "<" : ((a: NumStr, b: NumStr): boolean => a < b),
    "<=": ((a: NumStr, b: NumStr): boolean => a <= b),
    "=" : ((a: NumStr, b: NumStr): boolean => a == b),
    "!=": ((a: NumStr, b: NumStr): boolean => a != b),
    "!" : ((a: NumStr, b: NumStr): boolean => a != b),
    "==": ((a: NumStr, b: NumStr): boolean => a == b),
    ">" : ((a: NumStr, b: NumStr): boolean => a > b),
    ">=": ((a: NumStr, b: NumStr): boolean => a >= b),
};

class ComparatorFilter {
    values: NumStr[] = [];
    funcs: FilterComparator[] = [];
    negate: boolean = false;
    // m: RegExpExecArray |
    // null = [];
    constructor(fltr: string) {
        // this.m = numFilterEx.exec(fltr);
        // if ( !this.m ) { return; }
        // if ( fltr.indexOf("!") >= 0 ) { fltr = fltr.replace("!", ""); this.negate = true; }
        for (let result = numFilterEx.exec(fltr); result !== null; result = numFilterEx.exec(fltr)) {
            const op = result[1];
            // @ts-ignore
            this.funcs.push(filterComparatorOperators[op]);
            const vs = result[2];
            if ( result[3] ) { this.negate = true; }
            let v: NumStr;
            try {
                v = Number.parseFloat(vs);
                if ( isNaN(v) ) { v = vs.toLowerCase(); }
            } catch (e) {
                v = vs.toLowerCase();
            }
            this.values.push(v);
        }
    }

    public static isNumFilter(s: string): boolean {
        if ( !s ) { return false; }
        // return ( !!numFilterEx.exec(s) ); // eats first match?
        return "!<=>".indexOf(s[0]) >= 0;
    }

    public static makeNumFilter(s: string): ComparatorFilter | null {
        if ( !this.isNumFilter(s) ) { return null; }
        return new ComparatorFilter(s);
    }

    public isMatch(s: string): boolean {
        let n: NumStr = "";
        try {
            n = Number.parseFloat(s);
            if ( isNaN(n) ) { n = s.toLowerCase(); }
        } catch {
            n = s.toLowerCase();
        }
        let res = true;
        for (let i = 0; i < this.values.length; i++) {
            if ( !this.funcs[i](n, this.values[i]) ) { res = false; break; }
        }
        return this.negate ? !res : res;
    }
}

const styleToHtml: {[index: string]: string} = {
    backgroundColor: "background-color",
    border: "border",
    borderBottom: "border-bottom",
    borderLeft: "border-left",
    borderRight: "border-right",
    borderTop: "border-top",
    color: "color",
    colspan: "colspan",
    fontFamily: "font-family",
    fontSize: "font-size",
    fontWeight: "font-weight",
    height: "height",
    horizontalAlign: "horizontal-align",
    maxWidth: "max-width",
    minWidth: "min-width",
    rowspan: "rowspan",
    textAlign: "text-align",
    verticalAlign: "vertical-align",
    visibility: "visibility",
    width: "width",
};

export interface CellResponse {
    row: number;
    col: number;
    cellHtml: string;
}

export interface TimTable {
    table: ITable;
    id?: string;
    headers?: string[];
    headersStyle?: object;
    addRowButtonText?: string;
    forcedEditMode?: boolean;
    globalAppendMode?: boolean;
    dataInput?: boolean;
    task?: boolean;
    taskBorders?: boolean;
    userdata?: DataEntity;
    editorBottom?: boolean;
    editorButtonsBottom?: boolean;
    editorButtonsRight?: boolean;
    toolbarTemplates?: any[];
    hid: {edit?: boolean, insertMenu?: boolean, editMenu?: boolean};
    hideSaveButton?: boolean;
    // hiddenRows?: IRow[];
    hiddenRows?: number[];
    hiddenColumns?: number[];
    lockedCells?: string[];
    saveCallBack?: (rowi: number, coli: number, content: string) => void;
    cbCallBack?: (cbs: boolean[], n: number, index: number) => void;
    maxWidth?: string; // Possibly obsolete if cell/column layout can be given in data.table.colums
    minWidth?: string;
    singleLine?: boolean;
    filterRow?: boolean;
    cbColumn?: boolean;
    nrColumn?: boolean;
    maxRows?: string;
    maxCols?: string;
    // lockCellCount?: boolean;
}

export interface ITable { // extends ITableStyles
    countRow?: number;
    countCol?: number;
    defrows?: {[index: string]: string};
    defcols?: {[index: string]: string};
    defcells?: {[index: string]: string};
    defcolsrange?: any;  // TODO: { range: [0,2], def: { } }
    defrowsrange?: any;
    defcellsrange?: any; // TODO: { range: [0,2, 3,-2], def: { } }
    rows?: IRow[];
    columns?: IColumn[];
    tabledatablock?: DataEntity;
}

export interface DataEntity {
    type: "Relative" | "Abstract";
    cells: CellDataEntity;
}

export interface CellIndex {
    x: number;
    y: number;
}

export interface SelectedCells {
    cells: CellIndex[];   // List of original cell indecies inside selected area
    srows: boolean[];     // table for screen indecies selected
    scol1: number;        // screen index for first selected column
    scol2: number;        // screen index for last selected column
}

export interface CellDataEntity {
    [key: string]: CellEntity;
}

export type CellType = string | number | boolean | null;
export type CellEntity = ICell | CellType;

export interface IRow { // extends IRowStyles
    row?: CellEntity[];
    id?: string;
}

export interface IColumn { // extends IColumnStyles
    id?: string;
    span?: number;
    formula?: string;
}

export interface ICell { // extends ICellStyles
    cell: CellType;
    editing?: boolean;
    editorOpen?: boolean;
    type?: string;
    colspan?: number;
    rowspan?: number;
    id?: string;
    formula?: string;
    row?: number;
    col?: number;
    underSpanOf?: {row: number, col: number};
    renderIndexX?: number;
    renderIndexY?: number;
    inputScope?: boolean | undefined;

    [key: string]: any;
}

/**
 * Styles
 */

const tableStyles: Set<string> = new Set<string>([
    "backgroundColor",
    "border",
    "borderTop",
    "borderBottom",
    "borderLeft",
    "borderRight",
    "verticalAlign",
    "textAlign",
    "color",
    "fontFamily",
    "fontSize",
    "visibility",
    "width",
]);

const rowStyles: Set<string> = new Set<string>([
    "backgroundColor",
    "border",
    "borderTop",
    "borderBottom",
    "borderLeft",
    "borderRight",
    "verticalAlign",
    "textAlign",
    "color",
    "fontFamily",
    "fontSize",
    "fontWeight",
    "height",
]);

const cellStyles: Set<string> = new Set<string>([
    "verticalAlign",
    "fontSize",
    "border",
    "borderTop",
    "borderBottom",
    "borderLeft",
    "borderRight",
    "backgroundColor",
    "textAlign",
    "fontFamily",
    "color",
    "fontWeight",
    "width",
    "height",
    "colspan",
    "rowspan",
]);

const columnStyles: Set<string> = new Set<string>([
    "width",
    "backgroundColor",
    "border",
    "borderTop",
    "borderBottom",
    "borderLeft",
    "borderRight",
]);

const columnCellStyles: Set<string> = new Set<string>([
    "fontSize",
    "verticalAlign",
    "textAlign",
    "fontFamily",
    "color",
    "fontWeight",
]);

enum Direction {
    Up = 1,
    Down = 2,
    Left = 4,
    Right = 8,
}

const UP_OR_DOWN = [Direction.Up, Direction.Down];
const LEFT_OR_RIGHT = [Direction.Left, Direction.Right];

export function isPrimitiveCell(cell: CellEntity): cell is CellType {
    return cell == null || (cell as ICell).cell === undefined;
}

/**
 * Transforms column index to letter.
 * @param colIndex ex. 2
 * @return column index as letter
 */
export function colnumToLetters(colIndex: number): string {
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
    return colnumToLetters(remainder - 1) + lastChar;
}

export class TimTableController extends DestroyScope implements IController {
    static $inject = ["$scope", "$element"];
    private error: string = "";
    private taskUrl: string = "";

    public viewctrl?: ViewCtrl;
    public cellDataMatrix: ICell[][] = [];
    public columns: IColumn[] = [];
    public data!: Binding<TimTable, "<">;
    public disabled?: Binding<boolean, "<?">;
    private editRight: boolean = false;
    private userdata?: DataEntity = undefined;
    private editing: boolean = false;
    private forcedEditMode: boolean = false;
    private task: boolean = false;
    // private hideSaveButton?: boolean = false;
    private hiddenRows: number[] = [];
    // private lockCellCount: boolean = false;
    // private saveCallBack: () => void;
    private isRunning: boolean = false;
    public taskBorders: boolean = false;
    private editedCellContent: string | undefined;
    private editedCellInitialContent: string | undefined;
    private currentCell?: {row: number, col: number, editorOpen: boolean};
    private activeCell?: {row: number, col: number};
    private startCell?: {row: number, col: number};
    private selectedCells: SelectedCells = {cells: [], srows: [], scol1: 0, scol2: 0};
    public shiftDown: boolean = false;
    public cbAllFilter: boolean = false;
    public cbs: boolean[] = [];
    public filters: string[] = [];
    public sortDir: number[] = [];
    public sortSymbol: string[] = [];
    public sortSymbols: string[] = [" ▼", "", " ▲" ];  // ["🢓", "", "🢑"];  // this small does not work in iPad/Android
    public originalHiddenRows: number[] = [];
    private rowDelta = 0;
    private colDelta = 0;
    private cbFilter: boolean = false;
    private filterRow: boolean = false;
    private maxRows: string = "2000em";
    private maxCols: string = "auto";
    private totalrows: number = 0;
    private visiblerows: string = "";
    private permTable: number[] = [];
    private permTableToScreen: number[] = []; // inverse perm table to get screencoordinate for row

    /**
     * Stores the last direction that the user moved towards with arrow keys
     * or Enter / Tab. Used for saving and retrieving the "coordinate" of a cell that is
     * embedded in another cell through colspan / rowspan so it can be used in navigation.
     */
    private lastDirection?: {direction: Direction, coord: number};
    private mouseInTable?: boolean;
    private bigEditorOpen: boolean = false;

    private addRowButtonText: string = "";
    private pluginMeta: PluginMeta;

    constructor(private scope: IScope, private element: IRootElementService) {
        super(scope, element);
        this.pluginMeta = new PluginMeta(element);
        // if ( !this.data.hid ) this.data.hid = {};
    }

    getTaskUrl(): string {
        if (this.taskUrl) {
            return this.taskUrl;
        }
        const url = this.pluginMeta.getAnswerUrl();
        this.taskUrl = url;
        return url;
    }

    protected getParentAttr(name: string) {
        return this.element.parent().attr(name);
    }

    protected getTaskId() {
        return this.getParentAttr("id");
    }

    protected getPlugin() {
        return this.getParentAttr("data-plugin");
    }

    protected getRootElement() {
        return this.element[0];
    }

    private getHid() {
        if (!this.data.hid) {
            this.data.hid = {};
        }
        return this.data.hid;
    }

    /**
     * Set listener and initializes tabledatablock
     */
    async $onInit() {

        if ( this.data.maxRows ) { this.maxRows = this.data.maxRows; }
        if ( this.data.maxCols ) { this.maxCols = this.data.maxCols; }

        if (this.data.singleLine) {
            if ( !this.data.headersStyle ) { this.data.headersStyle = new Object(); }
            const hs: any = this.data.headersStyle;
            hs["white-space"] = "nowrap";
        }

        this.initializeCellDataMatrix();
        this.processDataBlockAndCellDataMatrix();
        this.clearSortOrder();
        this.userdata = this.data.userdata;
        if (this.userdata) {
            this.processDataBlock(this.userdata.cells);
        } else {
            this.userdata = {
                type: "Relative",
                cells: {},
            };
        }

        this.filterRow = this.data.filterRow || false;
        if ( this.cellDataMatrix.length <= 2) { this.filterRow = false; }
        this.colDelta = 0;
        this.rowDelta = 0;

        if ( this.data.nrColumn ) { this.colDelta += 1; }
        if ( this.data.cbColumn ) { this.colDelta += 1; }
        if ( this.data.headers ) { this.rowDelta += 1; }
        if ( this.filterRow ) { this.rowDelta += 1; }

        let tb = false;
        if (this.data.taskBorders) {
            tb = true;
        } else if (this.data.taskBorders == false) {
            tb = false;
        } else {
            tb = this.data.task == true;
        }
        this.taskBorders = tb;

        if (this.viewctrl == null) {
            return;
        } else {
            this.editRight = this.viewctrl.item.rights.editable;

            if (this.data.task) {
                this.task = true;
            }

            // if(this.data.lockCellCount){
            //     this.lockCellCount = this.data.lockCellCount;
            // }
            if (this.data.addRowButtonText) {
                this.addRowButtonText = " " + this.data.addRowButtonText;
            }
            if (this.data.forcedEditMode) {
                this.forcedEditMode = this.data.forcedEditMode && (this.editRight || this.task);
                this.editing = this.forcedEditMode;
            }

            if (this.data.task) {
                this.task = true;
                this.forcedEditMode = true;
                const id = this.getTaskId(); // TODO: why this could be undefined?
                if (id && id.indexOf("..") >= 0) {
                    this.error = "If task, should also have taskId!";
                }
            }

            await $timeout(500);
            const parId = getParId(this.element.parents(".par"));
            if (parId == null) {
                console.log("parid null in timtable");
                return;
            }
            this.viewctrl.addTable(this, parId);
        }
        if (this.disabled) { return; }
        document.addEventListener("keyup", this.keyUpTable);
        document.addEventListener("keydown", this.keyDownTable);
        document.addEventListener("keypress", this.keyPressTable);
        // document.addEventListener("click", this.onClick);
        onClick("body", ($this, e) => {
            this.onClick(e);
        });
        let hl = 0;
        if ( this.data.hiddenRows ) {
            this.originalHiddenRows = this.data.hiddenRows.slice();
            hl = this.data.hiddenRows.length;
        }
        this.totalrows = this.cellDataMatrix.length;
        if ( hl > 0 ) { this.visiblerows = "" + (this.totalrows - hl); }
    }

    $doCheck() {
        // TODO reference timTableEditorToolbar and ask if the color is different

    }

    /**
     * Removes listener and cleans up
     */
    $onDestroy() {
        document.removeEventListener("keyup", this.keyUpTable);
        document.removeEventListener("keydown", this.keyDownTable);
        document.removeEventListener("keypress", this.keyPressTable);
        // document.removeEventListener("click", this.onClick);
    }

    private onClick(e: JQuery.Event) {
        if (this.mouseInTable) {
            if (this.isInEditMode() && isToolbarEnabled()) {
                openTableEditorToolbar({
                    callbacks: {
                        setCell: (val) => this.setCell(val),
                        addToTemplates: () => this.addToTemplates(),
                        addColumn: (offset) => this.addColumnFromToolbar(offset),
                        addRow: (offset) => this.addRowFromToolbar(offset),
                        removeColumn: () => this.removeColumnFromToolbar(),
                        removeRow: () => this.removeRowFromToolbar(),
                    }, activeTable: this,
                });
            } else {
                // Hide the toolbar if we're not in edit mode
                hideToolbar(this);
            }
        } else {
            const target = e.target;

            if (target) {
                // Do not hide the toolbar if the user clicks on it
                if ($(target).parents(".modal-dialog").length > 0) {
                    return;
                }

                if ($(target).parents(".timTableEditor").length > 0) {
                    return;
                }

                this.activeCell = undefined;

                // Do not hide the toolbar if the user clicks on another TimTable
                if ($(target).parents(".timTableTable").length > 0) {
                    return;
                }
            }

            hideToolbar(this);
        }
    }

    /**
     * Checks whether the table is set to be always in edit mode
     * (assuming the user has edit rights).
     * @returns {boolean} True if the table is always in edit mode, otherwise false.
     */
    public isInForcedEditMode() {
        return this.forcedEditMode;
    }

    /**
     * Changes all visible check boxes on this column to same value than header row cb
     * if header row cb clicked. Otherwise just update hidden rows if cbFilter is on.
     * @param rowi index of chacbox changed
     */
    updateCBs(rowi: number) {
        if ( rowi == -1 ) {
            const b = this.cbAllFilter;
            for (let i = 0; i < this.cellDataMatrix.length; i++) {
                if ( this.data.hiddenRows && this.data.hiddenRows.includes(i) ) { continue; }
                this.cbs[i] = b;
            }
        }
        if ( this.cbFilter ) {
            this.updateFilter();
        } else {
            this.countCBs(rowi);
        }
    }

    private countCBs(rowi: number) {
        let n = 0;
        for (let i = 0; i < this.cellDataMatrix.length; i++) {
            if ( this.data.hiddenRows && this.data.hiddenRows.includes(i) ) { continue; }
            if ( this.cbs[i] ) { n++; }
        }
        if ( this.data.cbCallBack ) { this.data.cbCallBack(this.cbs, n, rowi); }
    }

    public static rowToList(row: ICell[]) {
        const rlist: string[] = [];
        for (const c of row) {
            rlist.push(c ? c.cell ? c.cell.toString() : "" : "");
        }
        return rlist;
    }

    public getCheckedRows(startFrom: number, visible: boolean) {
        const crows = [];
        for (let i = startFrom; i < this.cellDataMatrix.length; i++) {
            if ( this.data.hiddenRows && this.data.hiddenRows.includes(i) && visible ) { continue; }
            if ( this.cbs[i] ) {
                crows.push(TimTableController.rowToList(this.cellDataMatrix[i]));
            }
        }
        return crows;
    }

    public static makeNumFilter(fltr: string) {

    }

    /**
     * Check if all regexp's in regs maches to row values. It's kind of and
     * over all regs if tehere is some condition
     * TODO: add value compare by < and > operators
     * @param regs list of regexp to check
     * @param row where to check values
     */
    public static isMatch(regs: RegExp[], cmpfltrs: ComparatorFilter[], row: any[]) {
        for (let c = 0; c < row.length; c++) {
            if ( !regs[c] ) { continue; }
            const s: string = row[c].cell.toLowerCase();
            if ( !regs[c].test(s) ) { return false; }
        }
        for (let c = 0; c < row.length; c++) {
            if ( !cmpfltrs[c] ) { continue; }
            const s: string = row[c].cell;
            if ( cmpfltrs[c] && !cmpfltrs[c].isMatch(s) ) { return false; }
        }
        return true;
    }

    /**
     * Adds rows to this.hiddenRows if their row values matches the given filters
     * TODO: add also < and > compare
     */
    updateFilter() {
        this.saveAndCloseSmallEditor();
        this.disableStartCell();
        // TODO check if better way to save than just making saveAndCloseSmallEditor public and calling it
        // this.saveAndCloseSmallEditor();
        if (this.originalHiddenRows) { this.data.hiddenRows = this.originalHiddenRows.slice(); } else { this.data.hiddenRows = []; }

        this.totalrows = this.cellDataMatrix.length;
        this.visiblerows = "";
        let hl = this.data.hiddenRows.length;
        if ( hl > 0 ) { this.visiblerows += (this.totalrows - hl); }

        let isFilter = false;
        const regs = [];
        const cmpfltrs = [];
        for (let c = 0; c < this.filters.length; c++) {
            if ( !this.filters[c] ) { continue; }
            isFilter = true;
            const fltr = this.filters[c];
            const cmpfltr = ComparatorFilter.makeNumFilter(fltr);
            if ( cmpfltr ) {
                cmpfltrs[c] = cmpfltr;
            } else {
                regs[c] = new RegExp(fltr.toLowerCase());
            }
        }
        if ( !this.cbFilter && !isFilter ) { return; }

        for (let i = 0; i < this.cellDataMatrix.length; i++) {
            if ( this.cbFilter && !this.cbs[i] ) { this.data.hiddenRows.push(i); continue; }
            if ( TimTableController.isMatch(regs, cmpfltrs, this.cellDataMatrix[i])) { continue; }
            this.data.hiddenRows.push(i);
        }
        hl = this.data.hiddenRows.length;
        if ( hl > 0 ) { this.visiblerows += (this.totalrows - hl); }
        this.countCBs(-1);
    }

    sortByColumn(ai: number, bi: number, col: number, dir: number): number {
        // TODO: numeric sort also
        const a = this.cellDataMatrix[ai];
        const b = this.cellDataMatrix[bi];
        const ca = a[col];
        const cb = b[col];
        const cca = ca.cell;
        const ccb = cb.cell;
        const va = "" + cca;
        const vb = "" + ccb;
        const na = Number.parseFloat(va);
        const nb = Number.parseFloat(vb);
        if ( isNaN(na) || isNaN(nb)) {
            return va.localeCompare(vb) * dir;
        }
        let ret = 0;
        if ( na > nb ) {
            ret = 1;
        } else if ( na < nb ) {
            ret = -1;
        }
        return ret * dir;
    }

    clearSortOrder() {
        this.permTable = [];
        for (let i = 0; i < this.cellDataMatrix.length; i++) {
            this.permTable[i] = i;
            this.permTableToScreen[i] = i;
        }
        this.sortDir = [];
        this.sortSymbol = [];
    }

    sortData(col: number) {
        this.saveAndCloseSmallEditor();
        let dir = this.sortDir[col];
        if ( !dir ) { dir = -1; }
        dir = -dir;
        this.sortDir[col] = dir;
        this.sortSymbol[col] = this.sortSymbols[dir + 1];
        // this.rowKeys.sort((a, b) => this.sortByRealName(a, b));
        this.permTable.sort((a, b) => this.sortByColumn(a, b, col, dir) );
        for (let i = 0; i < this.permTable.length; i++) {
            this.permTableToScreen[this.permTable[i]] = i;
        }
        this.disableStartCell();
    }

    public taskBordersf() {
        if (this.data.taskBorders) {
            return true;
        }
        if (this.data.taskBorders == false) {
            return false;
        }
        return this.data.task;
    }

    /**
     * Checks whether the table is in edit mode.
     * @returns {boolean} True if the table is in edit mode, otherwise false.
     */
    public isInEditMode() {
        return this.editing || this.forcedEditMode;
    }

    public addRowEnabled() {
        // return !this.task && this.editRight && this.isInEditMode() && !this.lockCellCount
        return !this.task && this.editRight && this.isInEditMode();
    }

    public delRowEnabled() {
        return !this.task && this.editRight && this.isInEditMode();
    }

    public addColEnabled() {
        return !this.task && this.editRight && this.isInEditMode();
    }

    public delColEnabled() {
        return !this.task && this.editRight && this.isInEditMode();
    }

    /**
     * Set attributes value to correct ones when saved cell values
     */
    public editSave() {
        this.editing = false;
        if (this.currentCell) {
            this.currentCell = undefined;
        }
    }

    /**
     * Returns true if the simple cell content editor is open.
     */
    public isSomeCellBeingEdited() {
        return this.currentCell;
    }

    /**
     * Sets mouseInTable attribute to true
     */
    public mouseInsideTable() {
        this.mouseInTable = true;
    }

    /**
     * Sets mouseInTable attribute to false
     */
    public mouseOutTable() {
        this.mouseInTable = false;
    }

    public sendDataBlock() {
        if (this.isRunning) {
            return;
        }
        this.saveAndCloseSmallEditor();
        this.sendDataBlockAsync();
    }

    async sendDataBlockAsync() {
        if (!this.task) {
            return;
        }
        this.error = "";
        this.isRunning = true;
        const url = this.getTaskUrl();
        const params = {
            input: {
                answers: {
                    userdata: this.userdata,
                },
            },
        };

        /*
        const r = await to($http<{
            // web: {stackResult: StackResult},
        }>({method: "PUT", url: url, data: params, timeout: 20000},
        ));
        */
        const r = await $http.put<string[]>(url, params);

        this.isRunning = false;
        /*
                if (!r.ok) {
                    this.error = r.result.data.error;
                    return;
                }
        */
    }

    /*
     * Set attribute to user object.  If key == CLEAR, remove attribute in value
     * IF key == CLEAR and value = ALL, clear all attributes
     */
    setUserAttribute(row: number, col: number, key: string, value: string) {
        if (key != "CLEAR") {
            this.cellDataMatrix[row][col][key] = value;
        }
        if (!this.userdata) {
            return;
        }
        const coordinate = colnumToLetters(col) + "" + (row + 1);
        if (!this.userdata.cells[coordinate]) {
            if (key == "CLEAR") {
                return;
            } // nothing to do
            const data: CellEntity = {cell: null};
            data[key] = value;
            this.userdata.cells[coordinate] = data;
            return;
        }
        const cellValue = this.userdata.cells[coordinate];
        if (isPrimitiveCell(cellValue)) {
            if (key == "CLEAR") {
                return;
            } // nothing to do
            const data: CellEntity = {cell: this.cellToString(cellValue)};
            data[key] = value;
            this.userdata.cells[coordinate] = data;
            return;
        }
        if (key != "CLEAR") {
            cellValue[key] = value;
            return;
        }
        if (value == "ALL") {
            for (key in cellValue) {
                if (key == "cell") {
                    continue;
                }
                delete this.cellDataMatrix[row][col][key];
            }
            const data = cellValue.cell;
            this.userdata.cells[coordinate] = data;
            return;
        }

        delete cellValue[value];
        delete this.cellDataMatrix[row][col][value];
    }

    setUserContent(row: number, col: number, content: string) {
        this.cellDataMatrix[row][col].cell = content;
        if (!this.userdata) {
            return;
        }
        const coordinate = colnumToLetters(col) + "" + (row + 1);
        if (!this.userdata.cells[coordinate]) {
            this.userdata.cells[coordinate] = content;
            return;
        }
        const cellValue = this.userdata.cells[coordinate];
        if (isPrimitiveCell(cellValue)) {
            this.userdata.cells[coordinate] = content;
            return;
        }
        cellValue.cell = content;
    }

    disableStartCell() {
        this.startCell = undefined;
        this.shiftDown = false;
        this.selectedCells.srows = [];
    }

    getSelectedCells(row: number, col: number): SelectedCells {
        const ret: CellIndex[] = [];
        const srows: boolean[] = [];

        const scol = col;
        const srow = this.permTableToScreen[row];
        if ( scol == undefined || srow < 0 ) { return { cells: ret, srows: [], scol1: 0, scol2: 0 }; }

        let sx1 = scol;
        let sx2 = scol;
        let sy1 = srow;
        let sy2 = srow;
        if (this.startCell) {
            const sscol = this.startCell.col; // start screen...
            const ssrow = this.permTableToScreen[this.startCell.row];
            sx1 = Math.min(scol, sscol);
            sx2 = Math.max(scol, sscol);
            sy1 = Math.min(srow, ssrow);
            sy2 = Math.max(srow, ssrow);
        }

        for (let sy = sy1; sy <= sy2; sy++) {
            const y = this.permTable[sy];
            if ( (this.data.hiddenRows && this.data.hiddenRows.includes(y)) ) { continue; }
            for (let sx = sx1; sx <= sx2; sx++) {
                if ( (this.data.hiddenColumns && this.data.hiddenColumns.includes(sx)) ) { continue; }
                srows[sy] = true;
                ret.push({x: sx, y: y});
            }
        }
        return { cells: ret, srows: srows, scol1: sx1, scol2: sx2 };
    }

    /**
     * Saves cell content
     * @param {string} cellContent Saved value
     * @param {number} docId  Document id
     * @param {string} parId Paragraph id
     * @param {number} row  Row index
     * @param {number} col Column index
     */
    async saveCells(cellContent: string, docId: number, parId: string, row: number, col: number) {
        if (this.task) {
            // const cells = this.getSelectedCells(row, col);
            for (const c of this.selectedCells.cells) {
                this.setUserContent(c.y, c.x, cellContent);
                if (this.data.saveCallBack) {
                    this.data.saveCallBack(c.y, c.x, cellContent);
                }
            }
            return;
        }
        // TODO: tee iso tietue ja sille oma reitti ja paluussa kaikki uudet paikat
        const cellsToSave = [];
        for (const c of this.selectedCells.cells) {
            cellsToSave.push({c: cellContent, row: c.y, col: c.x});
        }
        const response = await $http.post<CellResponse[]>("/timTable/saveMultiCell", {
            docId,
            parId,
            cellsToSave: cellsToSave,
        });
        const cellHtmls = response.data;
        for (const c of cellHtmls) {
            this.cellDataMatrix[c.row][c.col].cell = c.cellHtml;
        }

        /*
        for (const c of this.selectedCells.cells) {
            const response = await $http.post<string[]>("/timTable/saveCell", {
                cellContent,
                docId,
                parId,
                row: c.y,
                col: c.x,
            });
            const cellHtml = response.data[0];
            this.cellDataMatrix[c.y][c.x].cell = cellHtml;
        }
        */

    }

    /**
     * Get cell data
     * @param {CellEntity} cell Handled cell
     * @param {number} docId Document id
     * @param {string} parId Paragraph id
     * @param {number} row Row index
     * @param {number} col Column index
     * @returns {Promise<string>}
     */
    async getCellData(cell: CellEntity, docId: number, parId: string, row: number, col: number) {
        const response = await $http<CellType[]>({
            url: "/timTable/getCellData",
            method: "GET",
            params: {docId, parId, row, col},
        });

        const data = response.data;
        return this.cellToString(data[0]);
    }

    /**
     * Opens editor
     * @param {CellEntity} cell
     * @param {number} docId Document id
     * @param {string} value Value that editor will show
     * @param {string} parId Paragraph id
     * @param {number} row Row index
     * @param {number} col Column index
     */
    async openEditor(cell: CellEntity, docId: number, value: string, parId: string, row: number, col: number) {
        if (this.currentCell) {
            this.currentCell.editorOpen = true;
        }
        if (this.editedCellContent == undefined) {
            return;
        }
        this.bigEditorOpen = true;
        const result = await openEditorSimple(docId, this.editedCellContent,
            "Edit table cell", "timTableCell");
        this.bigEditorOpen = false;
        if (this.currentCell) {
            this.currentCell.editorOpen = false;
        }
        if (result.type == "save" && result.text != this.editedCellInitialContent) {
            this.saveCells(result.text, docId, parId, row, col);
            // ctrl.cellDataMatrix[row][col] = result.text
            this.editedCellContent = result.text;
            this.closeSmallEditor();
        }
        if (result.type == "cancel") {
            this.closeSmallEditor();
        }
        if (isPrimitiveCell(cell)) {
        } else {
            cell.editorOpen = false;
        }
    }

    /**
     * Opens advanced editor
     * @param {CellEntity} cell Opened cell
     * @param {number} rowi Row index
     * @param {number} coli Column ndex
     */
    private editorOpen(cell: CellEntity, rowi: number, coli: number) {
        if (this.currentCell) {
            this.currentCell.editorOpen = true;
        }
        const parId = getParId(this.element.parents(".par"));
        if (parId === undefined || !this.viewctrl) {
            return;
        }
        this.openEditor(cell, this.viewctrl.item.id, this.getCellContentString(rowi, coli), parId, rowi, coli);
        const edit = this.element.find(".editInput");
        edit.focus();
    }

    /**
     * Opens advanced editor
     */
    private openBigEditor() {
        if (this.editedCellContent == undefined || this.bigEditorOpen) {
            return;
        }

        const modal: CellEntity = {
            cell: this.editedCellContent,
        };
        if (this.currentCell != undefined) {
            this.editorOpen(modal, this.currentCell.row, this.currentCell.col);
        }
    }

    /**
     * Check if defcols that columns array has as many items than celldatamatrix
     */
    private ensureColums() {
        if (!this.data.table) {
            return;
        }
        if (!this.data.table.defcols && !this.data.table.columns) {
            return;
        }
        if (this.data.table.defcols) {
            const n = this.cellDataMatrix[0].length;
            if (!this.data.table.columns) {
                this.data.table.columns = [];
            }
            for (let i = this.data.table.columns.length; i < n; i++) {
                this.data.table.columns.push({});
            }
        }
        this.columns = Object.assign([], this.data.table.columns);
    }

    /**
     * Initialize celldatamatrix with the values from yaml and yaml only
     * @constructor
     */
    private initializeCellDataMatrix() {
        this.cellDataMatrix = [];
        if (!this.data.table) { this.data.table = {}; }
        if (!this.data.table.rows) {
            this.data.table.rows = [];
        }

        let nrows = this.data.table.countRow || 0;
        let ncols = this.data.table.countCol || 0;
        nrows = Math.max(this.data.table.rows.length, nrows);

        for (const row of this.data.table.rows) {
            if (row.row) {
                ncols = Math.max(row.row.length, ncols);
            }
        }
        for (let iy = 0; iy < nrows; iy++) {
            this.cellDataMatrix[iy] = [];
            for (let ix = 0; ix < ncols; ix++) {
                this.cellDataMatrix[iy][ix] = this.createDummyCell();
            }

            const row = this.data.table.rows[iy];
            if (!row || !row.row) {
                continue;
            }
            for (let ix = 0; ix < row.row.length; ix++) {
                const itemInRow = row.row[ix];
                this.applyCellEntityAttributesToICell(itemInRow, this.cellDataMatrix[iy][ix]);
            }

        }
        this.ensureColums();
        this.clearSortOrder();
    }

    /**
     * Applies a cell entity's possible attributes to an ICell instance.
     * @param {CellEntity} sourceCell The source CellEntity from which the attributes are taken.
     * @param {ICell} targetCell The ICell instance to which the attributes are applied to.
     */
    private applyCellEntityAttributesToICell(sourceCell: CellEntity, targetCell: ICell) {
        if (isPrimitiveCell(sourceCell)) {
            targetCell.cell = this.cellToString(sourceCell);
            return;
        }

        for (const key of Object.keys(sourceCell)) {
            const value = sourceCell[key];
            if (value != null) {
                targetCell[key] = value;
            }
        }

    }

    /**
     * Returns a cell's content as a string.
     * @param {CellEntity} cell The cell.
     * @returns {string | string} The cell's content.
     */
    private cellEntityToString(cell: CellEntity) {
        if (isPrimitiveCell(cell)) {
            return this.cellToString(cell);
        }

        return this.cellToString(cell.cell);
    }

    /**
     * Transforms cell to string
     * @param {CellType} cell Changed cell
     * @returns {string}
     */
    private cellToString(cell: CellType) {
        if (cell == null) {
            return "";
        }
        return cell.toString();
    }

    /**
     * Returns a cell's content from the datablock.
     * @param {number} rowi: Table row index
     * @param {number} coli: Table column index
     * @returns {string | string}
     */
    private getCellContentString(rowi: number, coli: number) {
        return this.cellToString(this.cellDataMatrix[rowi][coli].cell);
    }

    /**
     * Combines datablock data
     * @param {CellDataEntity} cells: cells part of tabledatablock
     */
    public processDataBlock(cells: CellDataEntity) {
        for (const [item, value] of Object.entries(cells)) {

            const alphaRegExp = new RegExp("([A-Z]*)");
            const alpha = alphaRegExp.exec(item);

            if (alpha == null) {
                continue;
            }
            const numberPlace = item.substring(alpha[0].length);

            const address = TimTableController.getAddress(alpha[0], numberPlace);
            if (this.checkThatAddIsValid(address)) {
                this.setValueToMatrix(address.row, address.col, value);
            }
        }
    }

    /**
     * Combines datablock data with YAML table data.
     * Also processes rowspan and colspan and sets the table up for rendering.
     */
    private processDataBlockAndCellDataMatrix() {
        if (this.data.table.tabledatablock) {   // reads tabledatablock and sets all values to datacellmatrix
            this.processDataBlock(this.data.table.tabledatablock.cells);
            /*
            for (const item in this.data.table.tabledatablock.cells) {

                const alphaRegExp = new RegExp("([A-Z]*)");
                const alpha = alphaRegExp.exec(item);
                const value = this.data.table.tabledatablock.cells[item];

                if (alpha == null) { continue; }
                const numberPlace = item.substring(alpha[0].length);

                const address = this.getAddress(alpha[0], numberPlace);
                if (this.checkThatAddIsValid(address)) {
                    this.setValueToMatrix(address.row, address.col, value);
                }
            }
            */
        }

        // Process cell col/rowspan and figure out which cells should be rendered as part of another cell
        // (or, in terms of HTML, should not be rendered at all)

        for (let y = 0; y < this.cellDataMatrix.length; y++) {
            const row = this.cellDataMatrix[y];

            if (!row) {
                continue;
            }

            let renderIndexX = 0;

            for (let x = 0; x < row.length; x++) {
                const cell = row[x];
                if (cell.underSpanOf) {
                    continue;
                }
                cell.renderIndexX = renderIndexX;
                cell.renderIndexY = y;
                renderIndexX++;

                const colspan = cell.colspan ? cell.colspan : 1;
                const rowspan = cell.rowspan ? cell.rowspan : 1;

                if (colspan === 1 && rowspan === 1) {
                    continue;
                } // might enhance performance?

                for (let spanCellY = 0; spanCellY < rowspan; spanCellY++) {
                    if (y + spanCellY >= this.cellDataMatrix.length) {
                        break;
                    }

                    const spanRow = this.cellDataMatrix[y + spanCellY];

                    for (let spanCellX = 0; spanCellX < colspan; spanCellX++) {
                        if (spanCellY == 0 && spanCellX == 0) {
                            continue;
                        }
                        if (x + spanCellX >= spanRow.length) {
                            break;
                        }

                        const spanCell = spanRow[x + spanCellX];
                        if (spanCell.underSpanOf) {
                            // console.log("Found intersecting colspan / rowspan areas");
                            break;
                        }

                        spanCell.underSpanOf = {row: y, col: x};
                    }
                }
            }
        }
    }

    /**
     * Coordinates validation
     * @param {{col: number, row: number}} address row and column index
     * @returns {boolean} true if valid
     */
    checkThatAddIsValid(address: { col: number; row: number }): boolean {
        if (address.col >= 0 && address.row >= 0) {
            return true;
        }
        return false;
    }

    /**
     * Get placement, ex. A1 -> 0,0
     * ex. C5 -> 2,4
     * @param {string} colValue Column value, ex. 'A'
     * @param {string} rowValue  Row value, ex. '1'
     * @returns {{col: number, row: number}} Coordinates as index numbers
     */
    static getAddress(colValue: string, rowValue: string) {
        const charCodeOfA = "A".charCodeAt(0);
        const asciiCharCount = 26;
        let reversedCharacterPlaceInString = 0;
        let columnIndex = 0;
        for (let charIndex = colValue.length - 1; charIndex >= 0; charIndex--) {
            columnIndex += (colValue.charCodeAt(charIndex) - charCodeOfA + 1) * Math.pow(asciiCharCount, reversedCharacterPlaceInString);
            reversedCharacterPlaceInString++;
        }
        columnIndex = columnIndex - 1;
        const rowIndex = parseInt(rowValue, 10) - 1;
        return {col: columnIndex, row: rowIndex};
    }

    /**
     * Sets a value to specific index in cellDataMatrix
     * @param {number} row Row index
     * @param {number} col Column index
     * @param {string} value Stored value
     */
    private setValueToMatrix(row: number, col: number, value: CellEntity) {
        if (row >= this.cellDataMatrix.length) {
            this.resizeCellDataMatrixHeight(row + 1);
        }
        if (col >= this.cellDataMatrix[row].length) {
            this.resizeRowWidth(row, col + 1);
        }

        this.applyCellEntityAttributesToICell(value, this.cellDataMatrix[row][col]);
    }

    /**
     * Increases the height of the cell data matrix to the specified number.
     * @param {number} length The new height of the cell data matrix.
     */
    private resizeCellDataMatrixHeight(length: number) {
        for (let i = this.cellDataMatrix.length; i < length; i++) {
            this.cellDataMatrix[i] = [];
            if (i < 1) {
                continue;
            }
            for (let j = 0; j < this.cellDataMatrix[i - 1].length; j++) {
                this.cellDataMatrix[i][j] = this.createDummyCell();
            }
        }
    }

    /**
     * Increases the width of a row in the cell data matrix.
     * @param {number} rowIndex The index of the row to expand.
     * @param {number} width The new width of the row.
     */
    private resizeRowWidth(rowIndex: number, width: number) {
        for (const row of this.cellDataMatrix) {
            for (let i = row.length; i < width; i++) {
                row[i] = this.createDummyCell();
            }
        }
        this.ensureColums();
    }

    /**
     * Creates and returns an "empty" ICell with no content.
     * @returns {{cell: string}}
     */
    private createDummyCell() {
        return {cell: ""};
    }

    /**
     * Deals with key events
     * @param {KeyboardEvent} ev Pressed key event
     */
    private keyUpPressedInSmallEditor(ev: KeyboardEvent) {
        // Arrow keys
        if (ev.ctrlKey && (ev.keyCode == 40 || ev.keyCode == 39 || ev.keyCode == 38 || ev.keyCode == 37)) {
            this.handleArrowMovement(ev);
        }
    }

    private keyDownTable = (ev: KeyboardEvent) => {
        this.shiftDown = ev.shiftKey;

        // if (!this.mouseInTable) return;
        // TODO: Check properly if table has focus when preventing default tab behavior
        if (this.currentCell == undefined) { return; }
        if (ev.keyCode === KEY_TAB) {
            ev.preventDefault();
        }

    }

    private keyPressTable = (ev: KeyboardEvent) => {
        // if (!this.mouseInTable) return;
        if (ev.keyCode === KEY_TAB) {
            ev.preventDefault();
        }
    }

    /**
     * Deals with key events inside the table.
     * @param {KeyboardEvent} ev KeyboardEvent
     */
    private keyUpTable = (ev: KeyboardEvent) => {
        // this.shiftDown = ev.shiftKey;
        this.shiftDown = ev.shiftKey;

        if (!this.mouseInTable) {
            return;
        }

        if (ev.keyCode === KEY_F2) {
            if (this.getHid().edit) {
                return;
            }
            const modal: CellEntity = {
                cell: "",
            };
            if (this.currentCell != undefined && !this.bigEditorOpen) {
                this.editorOpen(modal, this.currentCell.row, this.currentCell.col);
                return;
            }

            // if no cell is being edited, open the last-edited cell for editing
            if (this.activeCell != undefined) {
                this.openCell(this.activeCell.row, this.activeCell.col);
                return;
            }
        }

        if (ev.keyCode === KEY_ENTER) {
            if (!this.isInEditMode() || !this.viewctrl) {
                return;
            }
            ev.preventDefault();

            if (ev.shiftKey) {
                this.doCellMovement(Direction.Up);
                this.disableStartCell();
                return;
            }

            const parId = getParId(this.element.parents(".par"));

            if (parId && this.currentCell !== undefined && this.currentCell.row !== undefined && this.currentCell.col !== undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
                const sy = this.permTableToScreen[this.currentCell.row];
                if (sy === this.cellDataMatrix.length - 1) {
                    this.doCellMovement(Direction.Right);
                } else {
                    this.doCellMovement(Direction.Down);
                }
                return;
            }

            if (this.activeCell) {
                this.openCell(this.activeCell.row, this.activeCell.col);
            }
        }

        if (ev.keyCode === KEY_TAB) {
            ev.preventDefault();
            if (ev.shiftKey) {
                this.doCellMovement(Direction.Left);
                this.disableStartCell();
            } else {
                this.doCellMovement(Direction.Right);
            }
            return;
        }

        if (ev.keyCode === KEY_ESC) {
            ev.preventDefault();
            this.currentCell = undefined;
            this.scope.$apply();
            return;
        }

        // Arrow keys
        if (!this.currentCell && ev.ctrlKey && isArrowKey(ev.keyCode)) {
            if (this.handleArrowMovement(ev)) {
                ev.preventDefault();
            }
        }
    }

    /**
     * Handles arrow movement inside table
     * @param {KeyboardEvent} ev Keyboardevent
     */
    private handleArrowMovement(ev: KeyboardEvent): boolean {
        const parId = getParId(this.element.parents(".par"));
        if (!(this.editing || this.task) || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) {
            return false;
        }

        if (ev.keyCode === KEY_DOWN) {
            return this.doCellMovement(Direction.Down);
        } else if (ev.keyCode === KEY_RIGHT) {
            return this.doCellMovement(Direction.Right);
        } else if (ev.keyCode === KEY_LEFT) {
            return this.doCellMovement(Direction.Left);
        } else if (ev.keyCode === KEY_UP) {
            return this.doCellMovement(Direction.Up);
        }
        return false;
    }

    /**
     * Switches the edit mode to another cell relative to either the current
     * or last edited cell.
     * @param direction The direction that the cell edit mode should move to.
     */
    private doCellMovement(direction: Direction): boolean {
        if (this.activeCell) {
            let x = this.activeCell.col;
            let y = this.activeCell.row;
            if (this.lastDirection) {
                if (UP_OR_DOWN.includes(this.lastDirection.direction)) {
                    if (UP_OR_DOWN.includes(direction)) {
                        x = this.lastDirection.coord;
                    }
                } else {
                    if (LEFT_OR_RIGHT.includes(direction)) {
                        y = this.lastDirection.coord;
                    }
                }
            }

            let nextCellCoords = this.getNextCell(x, y, direction);
            /*
            Iterate towards direction until next non-locked cell in a non-hidden row or column is found
            or until iterator arrives at the same cell
             */
            while (nextCellCoords) {
                if (nextCellCoords.row == y && nextCellCoords.col == x) { break; }
                if (!(this.data.hiddenRows && this.data.hiddenRows.includes(nextCellCoords.row))
                    && !(this.data.hiddenColumns && this.data.hiddenColumns.includes(nextCellCoords.col))
                && !(this.data.lockedCells && this.data.lockedCells.includes(colnumToLetters(nextCellCoords.col) + (nextCellCoords.row + 1)))) { break; }
                nextCellCoords = this.getNextCell(nextCellCoords.col, nextCellCoords.row, direction);
            }

            if (!nextCellCoords) {
                return true;
            }

            if (this.currentCell) {
                this.openCell(nextCellCoords.row, nextCellCoords.col);
                return true;
            }

            this.setActiveCell(nextCellCoords.row, nextCellCoords.col);
        }
        return true;
    }

    /**
     * Gets the next cell in a given direction from a cell.
     * Takes rowspan and colspan into account.
     * @param x The original X coordinate (column index) of the source cell.
     * @param y The original Y coordinate (original row index) of the source cell.
     * @param direction The direction.
     */
    private getNextCell(x: number, y: number, direction: Direction): {row: number, col: number} | null {
        let sourceCell = this.cellDataMatrix[y][x];
        while (sourceCell.underSpanOf) {
            sourceCell = this.cellDataMatrix[sourceCell.underSpanOf.row][sourceCell.underSpanOf.col];
        }

        let nextRow;
        let nextColumn;
        let cell;
        const sy = this.permTableToScreen[y];  // index in screen
        switch (direction) {
            case Direction.Up:
                nextRow = this.constrainRowIndex(sy - 1);
                nextColumn = this.constrainColumnIndex(nextRow, x);
                nextRow = this.permTable[nextRow];
                this.lastDirection = {direction: direction, coord: nextColumn};
                break;
            case Direction.Left:
                nextRow = this.constrainRowIndex(sy);
                nextColumn = this.constrainColumnIndex(nextRow, x - 1);
                nextRow = this.permTable[nextRow];
                this.lastDirection = {direction: direction, coord: nextRow};
                break;
            case Direction.Down:
                const sourceRowspan = sourceCell.rowspan ? sourceCell.rowspan : 1;
                nextRow = this.constrainRowIndex(sy + sourceRowspan);
                nextColumn = this.constrainColumnIndex(nextRow, x);
                nextRow = this.permTable[nextRow];
                this.lastDirection = {direction: direction, coord: nextColumn};
                break;
            case Direction.Right:
                const sourceColspan = sourceCell.colspan ? sourceCell.colspan : 1;
                nextRow = this.constrainRowIndex(sy);
                nextColumn = this.constrainColumnIndex(nextRow, x + sourceColspan);
                nextRow = this.permTable[nextRow];
                this.lastDirection = {direction: direction, coord: nextRow};
                break;
            default:
                return null;
        }

        cell = this.cellDataMatrix[nextRow][nextColumn];
        while (cell.underSpanOf) {
            nextRow = cell.underSpanOf.row;
            nextColumn = cell.underSpanOf.col;
            cell = this.cellDataMatrix[nextRow][nextColumn];
        }

        return {row: nextRow, col: nextColumn};
    }

    private constrainRowIndex(rowIndex: number) {
        if (rowIndex >= this.cellDataMatrix.length) {
            return 0;
        }
        if (rowIndex < 0) {
            return this.cellDataMatrix.length - 1;
        }
        return rowIndex;
    }

    private constrainColumnIndex(rowIndex: number, columnIndex: number) {
        const row = this.cellDataMatrix[rowIndex];
        if (columnIndex >= row.length) {
            return 0;
        }
        if (columnIndex < 0) {
            return row.length - 1;
        }

        return columnIndex;
    }

    private setActiveCell(rowi: number, coli: number) {
        let cell = this.cellDataMatrix[rowi][coli];
        while (cell.underSpanOf) {
            rowi = cell.underSpanOf.row;
            coli = cell.underSpanOf.col;
            cell = this.cellDataMatrix[rowi][coli];
        }
        this.activeCell = {row: rowi, col: coli};
        if (!this.shiftDown) {
            this.startCell = {row: rowi, col: coli};
        }
        this.selectedCells = this.getSelectedCells(rowi, coli);
        this.scope.$applyAsync();
    }

    /**
     * Clicks specified cell or hops opposite side of the table
     * @param {number} rowi Row index
     * @param {number} coli Column index
     */
    private openCell(rowi: number, coli: number) {
        const modal: CellEntity = {
            cell: "",
        };

        rowi = this.constrainRowIndex(rowi);
        coli = this.constrainColumnIndex(rowi, coli);

        this.openCellForEditing(modal, rowi, coli);
    }

    /**
     * Deals with cell clicking
     * @param {CellEntity} cell Cell that was clicked
     * @param {number} rowi Row index
     * @param {number} coli Column index
     * @param {MouseEvent} event If mouse was clikced
     */
    private async cellClicked(cell: CellEntity, rowi: number, coli: number, event?: MouseEvent) {
        this.openCellForEditing(cell, rowi, coli, event);
        this.lastDirection = undefined;
    }

    /**
     * Opens a cell for editing.
     * @param cell The cell.
     * @param rowi The row index.
     * @param coli The column index.
     * @param event The mouse event, if the cell was clicked.
     */
    private async openCellForEditing(cell: CellEntity, rowi: number, coli: number, event?: MouseEvent) {

        const parId = getParId(this.element.parents(".par"));
        if (!this.isInEditMode() || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) {
            return;
        }

        if (this.currentCell) {
            if (this.currentCell.row === rowi && this.currentCell.col === coli) {
                return;
            }
        }

        const cellCoordinate = colnumToLetters(coli) + (rowi + 1);
        if (this.data.lockedCells && this.data.lockedCells.includes(cellCoordinate)) { return; }

        const activeCell = this.activeCell;
        if (this.getHid().edit) {
            this.setActiveCell(rowi, coli);
            return;
        }
        if (this.currentCell ||
            (activeCell && this.activeCell &&
                activeCell.row === this.activeCell.row && activeCell.col === this.activeCell.col)) {
            await this.saveCurrentCell();
            let value: string = "";
            if (!this.task) {
                value = await this.getCellData(cell, this.viewctrl.item.id, parId, rowi, coli);
            } else {
                value = this.getCellContentString(rowi, coli);
            }
            this.editedCellContent = value;
            this.editedCellInitialContent = value;
            this.currentCell = {row: rowi, col: coli, editorOpen: false};
            this.calculateElementPlaces(rowi, coli, event);
        }
        this.setActiveCell(rowi, coli);
}

    /**
     * Saves the possible currently edited cell.
     */
    private async saveCurrentCell() {
        const parId = getParId(this.element.parents(".par"));

        if (this.viewctrl &&
            parId &&
            this.currentCell != undefined &&
            this.currentCell.row != undefined &&
            this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
            const value = this.editedCellContent;

            if (typeof value === "string" && this.editedCellInitialContent != value) {
                await this.saveCells(value, this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
                await ParCompiler.processAllMath(this.element);
                return true;
            }
        }
        return false;
    }

    private async saveToCurrentCell(value: string) {
        if (!this.viewctrl || !this.activeCell) {
            return;
        }
        const parId = this.getOwnParId();
        if (!parId) {
            return;
        }
        const docId = this.viewctrl.item.id;
        const rowId = this.activeCell.row;
        const colId = this.activeCell.col;

        if (typeof value === "string") {
            await this.saveCells(value, docId, parId, rowId, colId);
            await ParCompiler.processAllMath(this.element);
            return true;
        }
        return false;
    }

    /**
     * Calculates new places for plus-icons, input element and pen icon
     * @param {number} rowi Row Index
     * @param {number} coli Column index
     * @param {MouseEvent} event MouseEvent
     */
    private async calculateElementPlaces(rowi: number, coli: number, event?: MouseEvent) {
        await $timeout();
        const sr = this.permTableToScreen[rowi];
        const table = this.element.find(".timTableTable").first();
        const cell = this.cellDataMatrix[rowi][coli];
        if (cell.renderIndexX === undefined || cell.renderIndexY === undefined) {
            return; // we should never be able to get here
        }
        const ry = this.permTable[cell.renderIndexY];
        const tablecell = table.children("tbody").last().children("tr").eq(sr + this.rowDelta).children("td").eq(cell.renderIndexX + this.colDelta);
        // const tableCellOffset = tablecell.offset(); // Ihmeellisesti tämä antoi väärän tuloksen???
        const tableCellOffset = table.children("tbody").last().children("tr").eq(sr + this.rowDelta).children("td").eq(cell.renderIndexX + this.colDelta).offset();

        let cell2y = 0;
        if (sr > 0) {
            const tablecell2 = table.children("tbody").last().children("tr").eq(sr - 1 + this.rowDelta).children("td");
            const off2 = tablecell2.offset();
            if (off2) {
                cell2y = off2.top;
            }
        }

        /*let off;
        if (event && event.target) {
            let obj = $(event.target);
            if (obj.prop("tagName") !== "TD") {
                obj = obj.parents("td").last();
            }
            off = obj.offset();
            if (!off) { return; }
        } else {
            off = tablecell.offset();
            if (!off) { return; }
        }*/
        if (!tableCellOffset) {
            return;
        }
        if (!table) {
            return;
        }

        // this.element.find(".editInput").offset(off);
        const inlineEditorDiv = this.element.find(".inlineEditorDiv");
        inlineEditorDiv.height(1);
        inlineEditorDiv[0].style.position = "relative";
        const edit = this.element.find(".editInput");
        await $timeout();
        // edit.focus();
        const toff = table.offset()!;
        inlineEditorDiv.offset({left: toff.left, top: toff.top + table.height()!});
        try {
            if (this.data.editorBottom) {
                // edit.focus();
                return;
            }
            edit.offset(tableCellOffset);

            const editOffset = edit.offset();
            const tableCellWidth = tablecell.innerWidth();

            // const editOuterWidth = edit.outerWidth();

            const minEditWidth = 20;

            let editOuterWidth;
            if (tableCellWidth) {
                editOuterWidth = Math.max(minEditWidth, tableCellWidth);
            } else {
                editOuterWidth = minEditWidth;
            }

            edit.width(editOuterWidth);
            edit.height(tablecell.innerHeight()! - 2);

            const inlineEditorButtons = this.element.find(".inlineEditorButtons");
            if (this.data.editorButtonsBottom) {
                inlineEditorButtons.offset({left: toff.left, top: toff.top + table.height()! + 5});
                return;
            }
            if (this.data.editorButtonsRight) {
                inlineEditorButtons.offset({
                    left: tableCellOffset.left + editOuterWidth + 5,
                    top: tableCellOffset.top + 5,
                });
                return;
            }
            const editOuterHeight = edit.outerHeight();
            const buttonOpenBigEditor = this.element.find(".buttonOpenBigEditor");
            const h = buttonOpenBigEditor.height() || 20;
            if (editOffset && editOuterHeight && tableCellOffset && editOuterWidth) {
                const mul = sr == 0 ? 1 : 2;
                inlineEditorButtons.offset({
                    left: tableCellOffset.left,
                    top: (cell2y ? cell2y : editOffset.top) - h - 5,
                });
                /*
                const buttonOpenBigEditor = this.element.find(".buttonOpenBigEditor");
                buttonOpenBigEditor.offset({
                    left: tableCellOffset.left + editOuterWidth,
                    top: editOffset.top + editOuterHeight ,
                });

                const buttonOpenBigEditorWidth = buttonOpenBigEditor.outerWidth();
                const buttonOpenBigEditorOffset = buttonOpenBigEditor.offset();

                const buttonAcceptEdit = this.element.find(".buttonAcceptEdit");

                buttonAcceptEdit.offset({
                    left: tableCellOffset.left + editOuterWidth,
                    top: editOffset.top,
                });

                const buttonAcceptEditOffset = buttonAcceptEdit.offset();
                const buttonAcceptEditWidth = buttonAcceptEdit.outerWidth();

                if (buttonAcceptEditOffset && buttonAcceptEditWidth) {
                    this.element.find(".buttonCloseSmallEditor").offset({
                        left: buttonAcceptEditOffset.left + buttonAcceptEditWidth,
                        top: editOffset.top,
                    });
                }
                */
            }

        } finally {
            edit.focus();
        }
    }

    /**
     * Sets style attributes for cells
     * @param {CellEntity} cell Styled cell
     * @param {number} rowi Table row index
     * @param {number] coli Table column index
     */
    private stylingForCell(rowi: number, coli: number) {
        const styles = this.stylingForCellOfColumn(coli);
        // const styles: {[index: string]: string} = {};

        if (this.getCellContentString(rowi, coli) === "") {
            styles.height = "2em";
            styles.width = "1.5em";
        }

        const def = this.data.table.defcells;
        if (def) {
            this.applyStyle(styles, def, cellStyles);
        }

        const defrange = this.data.table.defcellsrange;
        if (defrange) {
            const rown = this.cellDataMatrix.length;
            const coln = this.cellDataMatrix[0].length;
            for (const dra of defrange) {
                const dr = dra; // TODO: korvaa kunnon tyypillä!
                this.checkRange(dr);
                if (this.checkIndex2(dr.range, rown, coln, rowi, coli)) {
                    this.applyStyle(styles, dr.def, columnStyles);
                }
            }
        }

        const cell = this.cellDataMatrix[rowi][coli];

        if (!isPrimitiveCell(cell)) {
            this.applyStyle(styles, cell, cellStyles);
        }

        if (this.data.maxWidth) {
            styles["max-width"] = this.data.maxWidth;
            styles.overflow = "hidden";
        }
        if (this.data.minWidth) { styles["min-width"] = this.data.minWidth; }
        if (this.data.singleLine) {
            styles["white-space"] = "nowrap";
        }
        return styles;
    }

    /**
     * Parses cell style attributes for a column
     * @param {number} coli The index of the column
     */
    private stylingForCellOfColumn(coli: number) {
        const styles: {[index: string]: string} = {};
        const table = this.data.table;

        if (!table.columns) {
            return styles;
        }

        if (table.columns.length <= coli) {
            return styles;
        }

        const col = table.columns[coli];

        if (!col) {
            return styles;
        }

        this.applyStyle(styles, col, columnCellStyles);
        return styles;
    }

    /**
     * Makex r[i] to index if possible, otherwise return def
     * @param r ange to check
     * @param i index to take from r
     * @param n max value
     * @param def in case no item
     */
    private static toIndex(r: number[], i: number, n: number, def: number) {
        if (r.length <= i) {
            return def;
        }
        let idx = r[i];
        if (idx < 0) {
            idx = n + idx;
        }
        if (idx < 0) {
            idx = 0;
        }
        if (idx >= n) {
            idx = n - 1;
        }
        return idx;
    }

    /**
     * Checks if dr.range is valid range.  If it is string, change it to array.
     * To prevent another check, mark it checked.
     * @param dr default range to check
     */
    private checkRange(dr: any) {
        if (dr.ok) {
            return;
        }
        let r = dr.range;
        if (!r) {
            dr.ok = true;
            return;
        }
        if (typeof r === "number") {
            dr.range = [r];
            dr.ok = true;
            return;
        }
        if (typeof r !== "string") {
            dr.ok = true;
            return;
        }

        r = "[" + r.replace("[", "").replace("]", "") + "]";
        try {
            r = JSON.parse(r);
        } catch (e) {
            dr.range = [];
            dr.ok = true;
            return;
        }
        dr.range = r;
        dr.ok = true;
        return;
    }

    /**
     * Check if index is between r[0]-r[1] where negative means i steps backward
     * @param r range to check, may be like [1,-1]
     * @param rown max_value
     * @param coln max_value
     * @param rowi index to check
     * @param coli index to check
     */
    private checkIndex2(r: number[], rown: number, coln: number, rowi: number, coli: number): boolean {
        if (!r) {
            return false;
        }
        if (r.length == 0) {
            return false;
        }
        const ir1 = TimTableController.toIndex(r, 0, rown, 0);
        if (rowi < ir1) {
            return false;
        }
        const ic1 = TimTableController.toIndex(r, 1, coln, 0);
        if (coli < ic1) {
            return false;
        }
        const ir2 = TimTableController.toIndex(r, 2, rown, ir1);
        if (ir2 < rowi) {
            return false;
        }
        const ic2 = TimTableController.toIndex(r, 3, coln, ic1);
        return ic2 >= coli;
    }

    /**
     * Check if index is between r[0]-r[1] where negative means i steps backward
     * @param r range to check, may be like [1,-1]
     * @param n max_value
     * @param index index to check
     */
    private checkIndex(r: number[], n: number, index: number): boolean {
        if (!r) {
            return false;
        }
        if (r.length == 0) {
            return false;
        }
        const i1 = TimTableController.toIndex(r, 0, n, 0);
        if (index < i1) {
            return false;
        }
        const i2 = TimTableController.toIndex(r, 1, n, i1);
        return i2 >= index;
    }

    /**
     * Sets style attributes for columns
     * @param {IColumn} col The column to be styled
     * @param index col index
     */
    private stylingForColumn(col: IColumn, index: number) {
        const styles: {[index: string]: string} = {};

        const def = this.data.table.defcols;
        if (def) {
            this.applyStyle(styles, def, columnStyles);
        }

        const defrange = this.data.table.defcolsrange;
        if (defrange) {
            const n = this.cellDataMatrix[0].length;
            for (const dra of defrange) {
                const dr = dra; // TODO: korvaa kunnon tyypillä!
                this.checkRange(dr);
                if (this.checkIndex(dr.range, n, index)) {
                    this.applyStyle(styles, dr.def, columnStyles);
                }
            }
        }

        this.applyStyle(styles, col, columnStyles);
        return styles;
    }

    /**
     * Sets style attributes for rows
     * @param {IRow} rowi The row to be styled
     */
    private stylingForRow(rowi: number) {
        const styles: {[index: string]: string} = {};
        if (!this.data.table) {
            return styles;
        }

        const def = this.data.table.defrows;
        if (def) {
            this.applyStyle(styles, def, rowStyles);
        }
        const defrange = this.data.table.defrowsrange;
        if (defrange) { // todo: do all this on init
            const n = this.cellDataMatrix.length;
            for (const dra of defrange) {
                const dr = dra; // TODO: korvaa kunnon tyypillä!
                this.checkRange(dr);
                if (this.checkIndex(dr.range, n, rowi)) {
                    this.applyStyle(styles, dr.def, rowStyles);
                }
            }
        }

        if (!this.data.table.rows || rowi >= this.data.table.rows.length) {
            return styles;
        }

        const row = this.data.table.rows[rowi];
        this.applyStyle(styles, row, rowStyles);
        return styles;
    }

    /**
     * Sets style attributes for the whole table
     * @returns {{[p: string]: string}}
     */
    private stylingForTable(tab: ITable) {
        const styles: {[index: string]: string} = {};
        this.applyStyle(styles, tab, tableStyles);
        return styles;
    }

    /**
     * Generic function for setting style attributes.
     * Verifies that given style attributes are valid and applies them.
     * Non-valid style attributes are not applied.
     * @param {{[p: string]: string}} styles The dictionary that will contain the final object styles
     * @param object The object that contains the user-given style attributes
     * @param {Set<string>} validAttrs A set that contains the accepted style attributes
     */
    private applyStyle(styles: {[index: string]: string}, object: any, validAttrs: Set<string>) {
        if (!object) {
            return;
        }
        for (const key of Object.keys(object)) {
            if (!validAttrs.has(key)) {
                continue;
            }

            const property = styleToHtml[key];
            if (!property) {
                continue;
            }

            styles[property] = object[key];
        }
    }

    /**
     * Toggles the table's edit mode on or off.
     */
    public async toggleEditMode() {
        await this.saveCurrentCell();
        this.currentCell = undefined;
        if (!this.editing) {
            this.editSave();
        }
        this.editing = !this.editing;
    }

    /**
     * Tells the server to add a new row into this table.
     */
    async addRow(rowId: number) {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        if (rowId == -1) {
            rowId = this.cellDataMatrix.length;
        }

        let response;

        if (this.isInGlobalAppendMode()) {
            response = await $http.post<TimTable>("/timTable/addUserSpecificRow",
                {docId, parId});
        } else {
            const route = this.isInDataInputMode() ? "/timTable/addDatablockRow" : "/timTable/addRow";
            response = await $http.post<TimTable>(route,
                {docId, parId, rowId});
        }

        this.data = response.data;
        this.reInitialize();
    }

    /**
     * Tells the server to remove a row from this table.
     */
    async removeRow(rowId: number) {
        if (this.viewctrl == null || !this.data.table.rows) {
            return;
        }

        const datablockOnly = this.isInDataInputMode();

        if (rowId == -1) {
            if (datablockOnly) {
                rowId = this.cellDataMatrix.length - 1;
            } else {
                rowId = this.data.table.rows.length - 1;
            }
        }

        const docId = this.viewctrl.item.id;
        const parId = this.getOwnParId();

        if (rowId < 0 || this.cellDataMatrix.length < 2) {
            return;
        }

        const response = await $http.post<TimTable>("/timTable/removeRow",
            {docId, parId, rowId, datablockOnly});
        this.data = response.data;
        this.reInitialize();
    }

    /**
     * Tells the server to add a new column into this table.
     */
    async addColumn(colId: number) {
        if (this.viewctrl == null) {
            return;
        }

        const route = this.isInDataInputMode() ? "/timTable/addDatablockColumn" : "/timTable/addColumn";
        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        const rowLen = this.cellDataMatrix[0].length;
        if ( colId < 0 ) { colId = rowLen; }
        const response = await $http.post<TimTable>(route,
            {docId, parId, colId, rowLen});
        this.data = response.data;
        this.reInitialize();
    }

    /**
     * Tells the server to remove a column from this table.
     */
    async removeColumn(colId: number) {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        if (colId == -1) {
            colId = this.getColumnCount() - 1;
        }
        const datablockOnly = this.isInDataInputMode();

        if (colId < 0) {
            return;
        }

        const response = await $http.post<TimTable>("/timTable/removeColumn",
            {docId, parId, colId, datablockOnly});
        this.data = response.data;
        this.reInitialize();
    }

    /**
     * Initializes the cell data matrix, reads the data block and sets its values
     * to the cell data matrix and processes all math.
     * Call this when the whole table's content is refreshed.
     */
    public reInitialize() {
        this.initializeCellDataMatrix();
        this.processDataBlockAndCellDataMatrix();
        this.clearSortOrder();

        ParCompiler.processAllMathDelayed(this.element);

        if (this.currentCell) {
            this.calculateElementPlaces(this.currentCell.row, this.currentCell.col);
        }
    }

    /**
     * Calculates and returns the number of columns in the table.
     */
    private getColumnCount() {
        let highestCellCount = 0;

        this.cellDataMatrix.forEach((row) => {
            if (row.length > highestCellCount) {
                highestCellCount = row.length;
            }
        });

        return highestCellCount;
    }

    /**
     * Checks whether the table is in global append mode.
     * @returns {boolean} True if the table is in global append mode, otherwise false.
     */
    private isInGlobalAppendMode() {
        if (this.data.globalAppendMode) {
            return this.data.globalAppendMode;
        }

        return false;
    }

    /**
     * Checks whether the table is in data input mode.
     * In data input mode, new rows are added to the datablock instead of the regular YAML.
     */
    private isInDataInputMode() {
        return this.data.dataInput === true;
    }

    /**
     * Returns the ID of the paragraph related to the current table instance.
     */
    private getOwnParId() {
        return getParId(this.element.parents(".par"));
    }

    /**
     * Closes the simple cell content editor.
     */
    private closeSmallEditor() {
        this.currentCell = undefined;
    }

    /**
     * Saves the currently edited cell and closes the simple cell content editor.
     */
    public saveAndCloseSmallEditor() {
        this.saveCurrentCell();
        this.closeSmallEditor();
    }

    async addColumnFromToolbar(offset: number) {
        if (this.activeCell) {
            return this.addColumn(this.activeCell.col + offset);
        }
    }

    async addRowFromToolbar(offset: number) {
        if (this.activeCell) {
            return this.addRow(this.activeCell.row + offset);
        }
    }

    async removeColumnFromToolbar() {
        if (this.activeCell) {
            return this.removeColumn(this.activeCell.col);
        }
    }

    async removeRowFromToolbar() {
        if (this.activeCell) {
            return this.removeRow(this.activeCell.row);
        }
    }

    async setCell(value: object) {
        for (const [key, s] of Object.entries(value)) {
            if (key.indexOf("$$") == 0) {
                continue;
            }
            if (key === "cell") {
                this.editedCellContent = s;
                await this.saveToCurrentCell(s);
            } else {
                await this.setCellStyleAttribute("setCell", key, s);
            }
        }
    }

    async addToTemplates() {
        const parId = getParId(this.element.parents(".par"));
        if ( !this.activeCell || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) { return; }
        const cell: CellEntity = {
            cell: "",
        };

        const rowId = this.activeCell.row;
        const colId = this.activeCell.col;
        const obj = this.cellDataMatrix[rowId][colId];
        const templ: any = {};
        let value = "";

        if (!this.task) {
            value = await this.getCellData(cell, this.viewctrl.item.id, parId, rowId, colId);
        } else {
            value = this.getCellContentString(rowId, colId);
        }

        for (const key in obj) {
            if (key.indexOf("render") == 0 || key.indexOf("border") == 0) {
                continue;
            }
            if (key.indexOf("$$") == 0) {
                continue;
            }
            if (key == "cell" && !obj[key]) {
                continue;
            }
            templ[key] = obj[key];
        }
        templ.cell = value;
        if (this.data.toolbarTemplates === undefined) {
            this.data.toolbarTemplates = [];
        }
        for (const ob of this.data.toolbarTemplates) {
            delete ob.$$hashKey; // TODO: is this needed? AngularJS internal attributes should never be touched.
            if (JSON.stringify(ob) == JSON.stringify(templ)) {
                return;
            }
        }
        this.data.toolbarTemplates.push(templ);
    }

    /**
     * Tells the server to set a cell style attribute.
     * @param route The route to call.
     * @param key The name of the attribute to set.
     * @param value The value of the attribute.
     */
    async setCellStyleAttribute(route: string, key: string, value: string) {
        if (!this.viewctrl || !this.activeCell) {
            return;
        }

        // sometimes there is extra # in colors?
        if (value.indexOf("##") == 0) {
            value = value.substr(1);
        }
        // const cells = this.getSelectedCells(this.activeCell.row, this.activeCell.col);

        if (this.task) {
            for (const c of this.selectedCells.cells) {
                this.setUserAttribute(c.y, c.x, key, value);
            }
            return;
        }

        // TODO; Old code, change protocol to use cells
        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        const rowId = this.activeCell.row;
        const colId = this.activeCell.col;
        let x1 = colId;
        let x2 = colId;
        let y1 = rowId;
        let y2 = rowId;
        if (this.startCell) {
            x1 = Math.min(this.activeCell.col, this.startCell.col);
            x2 = Math.max(this.activeCell.col, this.startCell.col);
            y1 = Math.min(this.activeCell.row, this.startCell.row);
            y2 = Math.max(this.activeCell.row, this.startCell.row);
        }

        // TODO: Does not take account hidden rows! Change protocol to use cells
        let data = {docId, parId, y1, y2, x1, x2, [key]: value};
        if (route === "setCell") {
            data = {docId, parId, y1, y2, x1, x2, key: key, value: value};
        }
        const response = await $http.post<TimTable>("/timTable/" + route, data);
        const toolbarTemplates = this.data.toolbarTemplates;
        this.data = response.data;
        this.data.toolbarTemplates = toolbarTemplates;
        this.reInitialize();
    }

    /**
     * Checks whether a cell is the currently active cell of the table.
     * The active cell is the cell that is being edited, or if no cell is being edited,
     * the cell that was edited last.
     * Suppose only rectangle areas
     * @param {number} rowi Table row index.
     * @param {number} coli Table column index.
     * @returns {boolean} True if the cell is active, otherwise false.
     */
    private isActiveCell(rowi: number, coli: number) {
        if (!this.isInEditMode()) {
            return false;
        }

        /*if (this.currentCell && this.currentCell.editorOpen) {
            return this.currentCell.row === rowi && this.currentCell.col === coli;
        }*/
        if (!this.activeCell) { return false; }

        const srow = this.permTableToScreen[rowi];
        const scol = coli;

        if ( scol < this.selectedCells.scol1 || this.selectedCells.scol2 < scol ) { return false; }
        if ( this.selectedCells.srows[srow] ) { return true; }

        /*  too slow:
        for (const c of this.selectedCells.cells ) {
            if ( c.x == coli && c.y == rowi ) { return true; }
        }
        */

        return false;
    }

    /**
     * Returns cell content HTML as trusted through AngularJS's SCE service.
     * Disables AngularJS HTML sanitizing for TimTable cells which breaks some attributes
     * of markdown tables placed inside TimTables (and possibly other things as well).
     * @param rowi Row index
     * @param coli Column index
     */
    private getTrustedCellContentHtml(rowi: number, coli: number) {
        return this.cellDataMatrix[rowi][coli].cell;
    }

    private showCell(cell: ICell) {
        return !cell.underSpanOf;
    }

    /**
     * Returns true if given row should be visible
     * @param index row number
     */
    private showRow(index: number) {
        // TODO: Change to use proper type
        // return this.data.hiddenRows.includes(this.data.table.rows[index]);
        return (!this.data.hiddenRows || !this.data.hiddenRows.includes(index));
    }

    /**
     * Returns true if given column should be visible
     * @param index row number
     */
    private showColumn(index: number) {
        // TODO: Change to use proper type
        return (!this.data.hiddenColumns || !this.data.hiddenColumns.includes(index));
    }

    private tableStyle() {
        return {"border": "none", "overflow": "auto", "max-height": this.maxRows, "width": this.maxCols};
    }

    private isHidden(coli: number) {
        if ( !this.data.hiddenColumns ) { return false; }
        return this.data.hiddenColumns.includes(coli);
    }

    private clearFilters() {
        for (let i = 0; i < this.filters.length; i++) {
            this.filters[i] = "";
        }
        this.cbFilter = false;
        this.updateFilter();
        this.clearSortOrder();
    }

}

timApp.component("timTable", {
    controller: TimTableController,
    bindings: {
        data: "<",
        plugintype: "@?",
        taskid: "@?",
        disabled: "<?",
    },
    require: {
        viewctrl: "?^timView",
    },
    template: `<div ng-if="!$ctrl.disabled" ng-mouseenter="$ctrl.mouseInsideTable()"
     ng-mouseleave="$ctrl.mouseOutTable()">
<div ng-cloak ng-class="{
          'csRunDiv': $ctrl.taskBorders}" class=" no-popup-menu" ng-style="::$ctrl.tableStyle()" >
    <h4 ng-if="::$ctrl.data.header" ng-bind-html="::$ctrl.data.header"></h4>
    <p ng-if="::$ctrl.data.stem" class="stem" ng-bind-html="::$ctrl.data.stem"></p>
    <div class="timTableContentDiv no-highlight">
    <button class="timTableEditor timButton buttonAddCol" title="Add column" ng-show="$ctrl.addColEnabled()"
            ng-click="$ctrl.addColumn(-1)"><span class="glyphicon glyphicon-plus"></span></button>
    <button class="timTableEditor timButton buttonRemoveCol" title="Remove column" ng-show="$ctrl.delColEnabled()"
            ng-click="$ctrl.removeColumn(-1)"><span class="glyphicon glyphicon-minus"></span></button>
    <table ng-class="{editable: $ctrl.isInEditMode() && !$ctrl.isInForcedEditMode(), forcedEditable: $ctrl.isInForcedEditMode()}" class="timTableTable"
     ng-style="$ctrl.stylingForTable($ctrl.data.table)" id={{$ctrl.data.table.id}}>
        <col ng-repeat="c in $ctrl.columns" ng-attr-span="{{c.span}}}" id={{c.id}}
             ng-style="$ctrl.stylingForColumn(c, $index)"/>

        <tr ng-if="$ctrl.data.headers"> <!-- Header row -->
            <td class="nrcolumn totalnr" ng-if="::$ctrl.data.nrColumn"
                ng-click="$ctrl.clearFilters()"
                title="Click to show all"
                ">{{$ctrl.totalrows}}</td>
            <td ng-if="::$ctrl.data.cbColumn"><input type="checkbox" ng-model="$ctrl.cbAllFilter"
             ng-change="$ctrl.updateCBs(-1)" title="Check for all visible rows"> </td>
            <td class="headers"
             ng-repeat="c in $ctrl.data.headers"  ng-init="coli = $index"
             ng-show="::$ctrl.showColumn(coli)"
             ng-click="$ctrl.sortData(coli)"
             title="Click to sort"
             ng-style="$ctrl.data.headersStyle" ng-click="" >{{c}}{{$ctrl.sortSymbol[coli]}}
            </td>
        </tr>

        <tr ng-if="$ctrl.filterRow" ng-init="irowi = 2"> <!-- Filter row -->
            <td class="nrcolumn totalnr" ng-if="::$ctrl.data.nrColumn" >{{$ctrl.visiblerows}}</td>
            <td ng-if="::$ctrl.data.cbColumn"><input type="checkbox" ng-model="$ctrl.cbFilter" ng-change="$ctrl.updateFilter()" title="Check to show only checked rows"> </td>

            <td ng-class=""
             ng-show="::$ctrl.showColumn(coli)"
             ng-repeat="c in $ctrl.cellDataMatrix[0]" ng-attr-span="{{c.span}}}" ng-init="coli = $index"
             ng-style="" ng-click="">
               <div class="filterdiv">
                 <input type="text" ng-change="$ctrl.updateFilter()" ng-model="$ctrl.filters[coli]" title="Write filter condition">
               </div>
            </td>

        </tr> <!-- Now the matrix -->
        <tr ng-repeat="rowi in $ctrl.permTable"
            ng-style="$ctrl.stylingForRow(rowi)" ng-show="$ctrl.showRow(rowi)">
                <td class="nrcolumn" ng-if="::$ctrl.data.nrColumn" >{{$index+1}}</td>
                <td ng-if="::$ctrl.data.cbColumn">
                   <input type="checkbox" ng-model="$ctrl.cbs[rowi]" ng-change="$ctrl.updateCBs(rowi)"> </td>
                <td ng-class="{'activeCell': $ctrl.isActiveCell(rowi, coli)}"
                 ng-show="$ctrl.showColumn(coli)"
                 ng-repeat="td in $ctrl.cellDataMatrix[rowi]" ng-init="coli = $index" ng-if="$ctrl.showCell(td)"
                 colspan="{{td.colspan}}" rowspan="{{td.rowspan}}"
                    ng-style="$ctrl.stylingForCell(rowi, coli)" ng-click="$ctrl.cellClicked(td, rowi, coli, $event)">
                    <div ng-bind-html="$ctrl.getTrustedCellContentHtml(rowi, coli)"></div>
                    <!-- {{rowi+1}}{{irowi+1}} -->
                </td> <!-- one cell -->
        </tr> <!-- the matrix -->
    </table>
    <button class="timTableEditor timButton buttonAddRow" title="Add row" ng-show="$ctrl.addRowEnabled()" ng-click="$ctrl.addRow(-1)"><span
            class="glyphicon glyphicon-plus" ng-bind="$ctrl.addRowButtonText"></span></button>
    <button class="timTableEditor timButton buttonRemoveRow" title="Remove row" ng-show="$ctrl.delRowEnabled()" ng-click="$ctrl.removeRow(-1)"><span
            class="glyphicon glyphicon-minus"></span></button>
    </div>
    <div class="timTableEditor inlineEditorDiv no-highlight" ng-show=$ctrl.isSomeCellBeingEdited()>
        <input class="editInput"  ng-show="$ctrl.isSomeCellBeingEdited()"
                   ng-keydown="$ctrl.keyDownPressedInSmallEditor($event)"
                   ng-keyup="$ctrl.keyUpPressedInSmallEditor($event)" ng-model="$ctrl.editedCellContent"><!--
             --><span class="inlineEditorButtons" style="position: fixed;" ng-show="$ctrl.isSomeCellBeingEdited()" ><!--
                 --><button class="timButton buttonOpenBigEditor"
                        ng-click="$ctrl.openBigEditor()" class="timButton"><span class="glyphicon glyphicon-pencil"></span>
                 </button><!--
                 --><button class="timButton buttonCloseSmallEditor"
                        ng-click="$ctrl.closeSmallEditor()"
                        class="timButton"><span class="glyphicon glyphicon-remove"></span>
                 </button><!--
                 --><button class="timButton buttonAcceptEdit"
                        ng-click="$ctrl.saveAndCloseSmallEditor()"
                         class="timButton"><span class="glyphicon glyphicon-ok"></span>
                 </button>
             </span>


    </div>
<div class="csRunMenuArea" ng-show="::($ctrl.task && !$ctrl.data.hideSaveButton)">
  <p class="csRunMenu"><button class="timButton" ng-show="::$ctrl.task" ng-click="$ctrl.sendDataBlock()" >Tallenna</button></p>
</div>
  <p class="plgfooter" ng-if="::$ctrl.data.footer" ng-bind-html="::$ctrl.data.footer"></p>
  <span class="error" ng-show="$ctrl.error" ng-bind="$ctrl.error"></span>
</div>

</div>
`,
});