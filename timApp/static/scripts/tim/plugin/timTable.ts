import {IController, IRootElementService, IScope, ISCEService} from "angular";
import {getParId} from "tim/document/parhelpers";
import {timApp} from "../app";
import {ViewCtrl} from "../document/viewctrl";
import {ParCompiler} from "../editor/parCompiler";
import {openEditorSimple} from "../editor/pareditor";
import {$http, $timeout} from "../util/ngimport";
import {Binding} from "../util/utils";

const styleToHtml: { [index: string]: string } = {
        backgroundColor: "background-color",
        border: "border",
        borderBottom: "border-bottom",
        borderLeft: "border-left",
        borderRight: "border-right",
        borderTop: "border-top",
        color: "color",
        fontFamily: "font-family",
        fontSize: "font-size",
        fontWeight: "font-weight",
        height: "height",
        horizontalAlign: "horizontal-align",
        textAlign: "text-align",
        verticalAlign: "vertical-align",
        visibility: "visibility",
        width: "width",
    };

export interface TimTable {
    table: ITable;
    id?: string;
    addRowButtonText?: string;
    forcedEditMode?: boolean;
    globalAppendMode?: boolean;
    dataInput?: boolean;
}

export interface ITable extends ITableStyles {
    rows?: IRow[];
    columns?: IColumn[];
    tabledatablock?: DataEntity;
}

/**
 * Styles
 */
export interface ITableStyles {
    backgroundColor?: string;
    border?: string;
    borderTop?: string;
    borderBottom?: string;
    borderLeft?: string;
    borderRight?: string;
    verticalAlign?: string;
    textAlign?: string;
    color?: string;
    fontFamily?: string;
    fontSize?: string;
    visibility?: string;
}

export interface DataEntity {
    type: "Relative" | "Abstract";
    cells: CellDataEntity;
}

export interface CellDataEntity {
    [key: string]: CellEntity;
}

export type CellType = string | number | boolean | null;
export type CellEntity = ICell | CellType;

export interface IRow extends IRowStyles {
    row?: CellEntity[];
    id?: string;
}

export interface IRowStyles {
    backgroundColor?: string;
    border?: string;
    borderTop?: string;
    borderBottom?: string;
    borderLeft?: string;
    borderRight?: string;
    verticalAlign?: string;
    textAlign?: string;
    color?: string;
    fontFamily?: string;
    fontSize?: string;
    fontWeight?: string;
}

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
    ]);

/*export interface ICellStyles {
    verticalAlign?: string;
    fontSize?: string;
    border?: string;
    borderTop?: string;
    borderBottom?: string;
    borderLeft?: string;
    borderRight?: string;
    backgroundColor?: string;
    textAlign?: string;
    fontFamily?: string;
    color?: string;
    fontWeight?: string;
} */

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
    inputScope?: boolean | undefined;
    [key: string]: any;
}

export interface IColumn { // extends IColumnStyles
    id?: string;
    span?: number;
    formula?: string;
}

const columnStyles: Set<string> = new Set<string>([
        "width",
        "backgroundColor",
        "border",
        "borderTop",
        "borderBottom",
        "borderLeft",
        "borderRight",
    ]);

/*export interface IColumnStyles {
    width?: string;
    backgroundColor?: string;
    border?: string;
    borderTop?: string;
    borderBottom?: string;
    borderLeft?: string;
    borderRight?: string;
} */

const columnCellStyles: Set<string> = new Set<string>(
    [
            "fontSize",
            "verticalAlign",
            "textAlign",
            "fontFamily",
            "color",
            "fontWeight",
            ]);

/** Using this for property validating failed
export interface IColumnCellStyles {
    verticalAlign: string;
    fontSize: string;
    textAlign: string;
    fontFamily: string;
    color: string;
    fontWeight: string;
} */

function isPrimitiveCell(cell: CellEntity): cell is CellType {
    return cell == null || (cell as ICell).cell === undefined;
}

export class TimTableController implements IController {
    private static $inject = ["$scope", "$element", "$sce"];
    public viewctrl?: ViewCtrl;
    public cellDataMatrix: ICell[][] = [];
    private data!: Binding<TimTable, "<">;
    private editing: boolean = false;
    private forcedEditMode: boolean = false;
    private editedCellContent: string | undefined;
    private editedCellInitialContent: string | undefined;
    private currentCell?: { row: number, col: number, editorOpen: boolean };
    private lastEditedCell?: {row: number, col: number};
    private mouseInTable?: boolean;
    private bigEditorOpen: boolean = false;

    private addRowButtonText: string = "";

    constructor(private scope: IScope, private element: IRootElementService, private sceService: ISCEService) {
        this.keyDownPressedTable = this.keyDownPressedTable.bind(this);
    }

    /**
     * Set listener and initializes tabledatablock
     */
    $onInit() {

        this.initializeCellDataMatrix();
        this.readDataBlockAndSetValuesToDataCellMatrix();

        if (this.viewctrl == null) {
            return;
        } else {
            const parId = getParId(this.element.parents(".par"));
            if (parId == null) {
                return;
            }
            if (this.data.addRowButtonText) {
                this.addRowButtonText = " " + this.data.addRowButtonText;
            }
            if (this.data.forcedEditMode) {
                this.forcedEditMode = this.data.forcedEditMode && this.viewctrl.item.rights.editable;
                this.editing = this.forcedEditMode;
            }

            this.viewctrl.addTable(this, parId);
        }
        document.addEventListener("keyup", this.keyDownPressedTable);
    }

    /**
     * Removes listener and cleans up
     */
    $onDestroy() {
        document.removeEventListener("keyup", this.keyDownPressedTable);
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
     * Checks whether the table is in edit mode.
     * @returns {boolean} True if the table is in edit mode, otherwise false.
     */
    public isInEditMode() {
        return this.editing || this.forcedEditMode;
    }

    /**
     * Set attirbutes value to correct ones when saved cell values
     */
    public editSave() {
        this.editing = false;
        if (this.currentCell) { this.currentCell = undefined; }
    }

    /**
     * Returns true if currentcell is not undefined
     * @returns {{row: number; col: number; editorOpen: boolean} | undefined}
     */
    public isSomeCellBeingEdited() {
        return this.currentCell;
    }

    /**
     * Sets mouseInTable attribut to true
     */
    public mouseInsideTable() {
        this.mouseInTable = true;
    }

    /**
     * Sets mouseInTable attribut to false
     */
    public mouseOutTable() {
        this.mouseInTable = false;
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
        const response = await $http.post<string[]>("/timTable/saveCell", {
            cellContent,
            docId,
            parId,
            row,
            col,
        });
        const cellHtml = response.data[0];
        this.cellDataMatrix[row][col].cell = cellHtml;
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
        const value = this.cellToString(data[0]);
        this.editedCellContent = value;
        this.editedCellInitialContent = value;
        return data[0];
    }

    /**
     * Opens editor
     * @param {CellEntity} cell
     * @param {number} docId Document id
     * @param {string} value Value that editor will show
     * @param {string} parId Pargharph id
     * @param {number} row Row index
     * @param {number} col Column index
     * @returns {Promise<void>}
     */
    async openEditor(cell: CellEntity, docId: number, value: string, parId: string, row: number, col: number) {
        if (this.currentCell) { this.currentCell.editorOpen = true; }
        if (this.editedCellContent == undefined) {
            return;
        }
        this.bigEditorOpen = true;
        const result = await openEditorSimple(docId, this.editedCellContent, "Edit table cell",
            "timTableCell",
            () => { this.bigEditorOpen = false; this.closeSmallEditor(); });
        this.bigEditorOpen = false;
        if (this.currentCell) { this.currentCell.editorOpen = false; }
        if (result.type == "save" && result.text != this.editedCellInitialContent) {
            this.saveCells(result.text, docId, parId, row, col);
            // ctrl.cellDataMatrix[row][col] = result.text
            this.editedCellContent = result.text;
            this.closeSmallEditor();
        }
        if (result.type == "cancel") {
            // this code path seems to be unused, because the result type is never cancel?
            // when the user clicks 'cancel' in the paragraph editor, the dialog is just dismissed
            // and no code after "await openEditorSimple" is executed here
            this.closeSmallEditor();
        }
        if (isPrimitiveCell(cell)) {
        } else { cell.editorOpen = false; }
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
        if (parId === undefined || !this.viewctrl) { return; }
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
        if (this.currentCell != undefined) { this.editorOpen(modal, this.currentCell.row, this.currentCell.col); }
    }

    /**
     * Initialize celldatamatrix with the values from yaml and yaml only
     * @constructor
     */
    private initializeCellDataMatrix() {
        this.cellDataMatrix = [];
        if (this.data.table.rows) {
            this.data.table.rows.forEach((item, index) => {
                this.cellDataMatrix[index] = [];
                if (item.row) {
                    item.row.forEach((item2, index2) => {
                        if (item.row) {
                            const itemInRow = item.row[index2];
                            // this.cellDataMatrix[index][index2] = this.cellEntityToString(itemInRow);
                            this.cellDataMatrix[index][index2] = this.createDummyCell();
                            this.applyCellEntityAttributesToICell(itemInRow, this.cellDataMatrix[index][index2]);
                        }
                    });
                }
            });
        }
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
            targetCell[key] = sourceCell[key];
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
        if (cell == null) { return ""; }
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
     * Reads DataBlock and sets all values to DataCellMatrix
     */
    private readDataBlockAndSetValuesToDataCellMatrix() {
        if (this.data.table.tabledatablock) {   // reads tabledatablock and sets all values to datacellmatrix
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
        }

        const cdm = this.cellDataMatrix;
    }

    /**
     * Coordinates validation
     * @param {{col: number; row: number}} address row and column index
     * @returns {boolean} true if valid
     */
    private checkThatAddIsValid(address: { col: number, row: number }) {
        if (address.col >= 0 && address.row >= 0) { return true; }
    }

    /**
     * Get placement, ex. A1 -> 0,0
     * ex. C5 -> 2,4
     * @param {string} colValue Column value, ex. 'A'
     * @param {string} rowValue  Row value, ex. '1'
     * @returns {{col: number; row: number}} Coordinates as index numbers
     */
    private getAddress(colValue: string, rowValue: string) {
        const charCodeOfA = "A".charCodeAt(0);
        const asciiCharCount = 26;
        let reversedCharacterPlaceInString = 0;
        let columnIndex = 0;
        for (let charIndex = colValue.length - 1; charIndex >= 0; charIndex--) {
            columnIndex += (colValue.charCodeAt(charIndex) - charCodeOfA + 1) * Math.pow(asciiCharCount, reversedCharacterPlaceInString);
            reversedCharacterPlaceInString++;
        }
        columnIndex = columnIndex - 1;
        const rowIndex: number = parseInt(rowValue) - 1;
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
        }
    }

    /**
     * Increases the width of a row in the cell data matrix.
     * @param {number} rowIndex The index of the row to expand.
     * @param {number} width The new width of the row.
     */
    private resizeRowWidth(rowIndex: number, width: number) {
        const row = this.cellDataMatrix[rowIndex];
        for (let i = row.length; i < width; i++) {
            row[i] = this.createDummyCell();
        }
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

    /**
     * Deals with keyevents inside div
     * @param {KeyboardEvent} ev KeyboardEvent
     */
    private keyDownPressedTable(ev: KeyboardEvent) {
        if (!this.mouseInTable) {
            return;
        }

        if (ev.keyCode === 113) { // F2
            const modal: CellEntity = {
                cell: "",
            };
            if (this.currentCell != undefined && !this.bigEditorOpen) {
                this.editorOpen(modal, this.currentCell.row, this.currentCell.col);
                return;
            }

            // if no cell is being edited, open the last-edited cell for editing
            if (this.lastEditedCell != undefined) {
                this.openCell(this.lastEditedCell.row, this.lastEditedCell.col);
                return;
            }
        }

        if (ev.keyCode === 13) { // Enter
            if (!this.isInEditMode() || !this.viewctrl) {
                return;
            }

            const parId = getParId(this.element.parents(".par"));

            if (parId && this.currentCell !== undefined && this.currentCell.row !== undefined && this.currentCell.col !== undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
                const value = this.editedCellContent;
                if (typeof value === "string" && this.editedCellInitialContent !== value) {
                    this.saveCells(value, this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
                }
                if (this.currentCell.row === this.cellDataMatrix.length - 1) {
                    this.openCellNextRowOrColumn(this.currentCell.row, this.currentCell.col + 1);
                } else {
                    this.openCellNextRowOrColumn(this.currentCell.row + 1, this.currentCell.col);
                }
                return;
            }

            if (this.lastEditedCell) {
                this.openCell(this.lastEditedCell.row, this.lastEditedCell.col);
            }
        }

        if (ev.keyCode === 9) { // Tab
            const parId = getParId(this.element.parents(".par"));
            if (!this.editing || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) {
                return;
            }
            if (this.currentCell !== undefined && this.currentCell.row !== undefined && this.currentCell.col !== undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
                const value = this.editedCellContent;
                if (typeof value === "string" && this.editedCellInitialContent !== value) {
                    this.saveCells(value, this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
                }
            }

            if (this.currentCell) {
                this.openCellNextRowOrColumn(this.currentCell.row, this.currentCell.col + 1);
            }

            return;
        }

        if (ev.keyCode === 27) { // esc
            this.currentCell = undefined;
            this.scope.$apply();
            return;
        }

        // Arrow keys
        if (!this.currentCell && ev.ctrlKey && (ev.keyCode === 40 || ev.keyCode === 39 || ev.keyCode === 38 || ev.keyCode === 37)) {
            this.handleArrowMovement(ev);
            this.scope.$apply();
        }
    }

    /**
     * Handles arrow movement inside table
     * @param {KeyboardEvent} ev Keyboardevent
     */
    private handleArrowMovement(ev: KeyboardEvent) {
        const modal: CellEntity = {
            cell: "",
        };

        const parId = getParId(this.element.parents(".par"));
        if (!this.editing || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) { return; }

        this.saveCurrentCell();

        if (ev.keyCode === 40) { // down arrow
            this.doCellMovement(0, 1);
            return;
        }
        if (ev.keyCode === 39) {
            this.doCellMovement(1, 0);
            return;
        }
        if (ev.keyCode === 37) {
            this.doCellMovement(-1, 0);
            return;
        }
        if (ev.keyCode === 38) {
            this.doCellMovement(0, -1);
            return;
        }
    }

    /**
     * Switches the edit mode to another cell relative to either the current
     * or last edited cell.
     * @param {number} x
     * @param {number} y
     */
    private doCellMovement(x: number, y: number) {
        if (this.currentCell) {
            this.openCell(this.currentCell.row + y, this.currentCell.col + x);
            return;
        }

        if (this.lastEditedCell) {
            const newRow = this.constrainRowIndex(this.lastEditedCell.row + y);
            const newColumn = this.constrainColumnIndex(newRow, this.lastEditedCell.col + x);
            this.lastEditedCell = {row: newRow, col: newColumn};
        }
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

        this.cellClicked(modal, rowi, coli);
    }

    private constrainRowIndex(rowIndex: number) {
        if (rowIndex >= this.cellDataMatrix.length) { return 0; }
        if (rowIndex < 0) { return this.cellDataMatrix.length - 1; }
        return rowIndex;
    }

    private constrainColumnIndex(rowIndex: number, columnIndex: number) {
        const row = this.cellDataMatrix[rowIndex];
        if (columnIndex >= row.length) { return 0; }
        if (columnIndex < 0) { return row.length - 1; }

        return columnIndex;
    }

    /**
     * Clicks given or hops opposite side of the table
     * @param {number} rowi Row index
     * @param {number} coli Column index
     */
    private openCellNextRowOrColumn(rowi: number, coli: number) {
        const modal: CellEntity = {
            cell: "",
        };

        if (rowi >= this.cellDataMatrix.length) {
            rowi = 0;
        } else if (rowi < 0) {
            rowi = this.cellDataMatrix.length - 1;
        }

        if (coli >= this.cellDataMatrix[rowi].length) {
            coli = 0;
            if (rowi + 1 < this.cellDataMatrix.length) { rowi++; }
        } else if (coli < 0) {
            coli = this.cellDataMatrix[rowi].length - 1;
        }

        this.cellClicked(modal, rowi, coli);
    }

    /**
     * Deals with cell clicking
     * @param {CellEntity} cell Cell that was clicked
     * @param {number} rowi Row index
     * @param {number} coli Column index
     * @param {MouseEvent} event If mouse was clikced
     */
    private async cellClicked(cell: CellEntity, rowi: number, coli: number, event?: MouseEvent) {
        const parId = getParId(this.element.parents(".par"));
        if (!this.isInEditMode() || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) { return; }

        if (this.currentCell) {
            if (this.currentCell.row === rowi && this.currentCell.col === coli) {
                return;
            }
        }

        this.saveCurrentCell();
        const cellData = this.getCellContentString(rowi, coli);
        this.editedCellContent = cellData;
        this.editedCellInitialContent = cellData;
        this.getCellData(cell, this.viewctrl.item.id, parId, rowi, coli);
        this.lastEditedCell = {row: rowi, col: coli};
        this.currentCell = {row: rowi, col: coli, editorOpen: false};
        this.calculateElementPlaces(rowi, coli, event);
    }

    /**
     * Saves the possible currently edited cell.
     */
    private saveCurrentCell() {
        const parId = getParId(this.element.parents(".par"));

        if (this.viewctrl && parId && this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
            const value = this.editedCellContent;

            if (typeof value === "string" && this.editedCellInitialContent != value) {
                this.saveCells(value, this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
            }
        }

        ParCompiler.processAllMathDelayed(this.element);
    }

    /**
     * Calculates new places for plus-icons, input element and pen icon
     * @param {number} rowi Row Index
     * @param {number} coli Column index
     * @param {MouseEvent} event MouseEvent
     * @returns {Promise<void>}
     */
    private async calculateElementPlaces(rowi: number, coli: number, event?: MouseEvent) {
        await $timeout();
        const table = this.element.find(".timTableTable").first();
        const tablecell = table.children("tbody").last().children("tr").eq(rowi).children("td").eq(coli);
        const off = tablecell.offset();
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
        if (off) {
            this.element.find(".editInput").offset(off);
            await $timeout();
            const edit = this.element.find(".editInput");
            edit.focus();
            const editOffset = edit.offset();
            const editOuterHeight = edit.outerHeight();
            const tableCellOffset = tablecell.offset();
            const tableCellWidth = tablecell.outerWidth();

            // const editOuterWidth = edit.outerWidth();

            const minEditWidth = 200;

            let editOuterWidth;
            if (tableCellWidth) {
                editOuterWidth = Math.max(minEditWidth, tableCellWidth);
            } else {
                editOuterWidth = minEditWidth;
            }

            edit.width(editOuterWidth);


            if (editOffset && editOuterHeight && tableCellOffset && editOuterWidth) {
                this.element.find(".buttonOpenBigEditor").offset({
                    left: tableCellOffset.left,
                    top: editOffset.top + editOuterHeight,
                });

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

            }
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

        if (this.getCellContentString(rowi, coli) === "") {
            styles["height"] = "2em";
            styles["width"] = "1.5em";
        }

        const cell = this.cellDataMatrix[rowi][coli];

        if (!isPrimitiveCell(cell)) {
            this.applyStyle(styles, cell, cellStyles);
        }

        return styles;
    }

    /**
     * Parses cell style attributes for a column
     * @param {number} coli The index of the column
     */
    private stylingForCellOfColumn(coli: number) {
        const styles: { [index: string]: string } = {};
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
     * Sets style attributes for columns
     * @param {IColumn} col The column to be styled
     */
    private stylingForColumn(col: IColumn) {
        const styles: { [index: string]: string } = {};

        this.applyStyle(styles, col, columnStyles);
        return styles;
    }

    /**
     * Sets style attributes for rows
     * @param {IRow} row The row to be styled
     */
    private stylingForRow(rowi: number) {
        const styles: { [index: string]: string } = {};

        if (!this.data.table.rows || rowi >= this.data.table.rows.length) {
            return styles;
        }

        const row = this.data.table.rows[rowi];

        // TODO decide what to do with this old implementation
        for (const key of Object.keys(row)) {
            const property: string = styleToHtml[key];
            if (property === undefined) {
                continue;
            }
            const c = row[key as keyof IRowStyles];
            if (!c) {
                continue;
            }
            styles[property] = c;
        }
        return styles;
    }

    /**
     * Sets style attributes for the whole table
     * @returns {{[p: string]: string}}
     */
    private stylingForTable(tab: ITable) {
        const styles: { [index: string]: string } = {};

        for (const key of Object.keys(tab)) {
            const property: string = styleToHtml[key];
            if (property === undefined) {
                continue;
            }
            const c = tab[key as keyof ITableStyles];
            if (!c) {
                continue;
            }
            styles[property] = c;
        }
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
    private applyStyle(styles: { [index: string]: string }, object: any, validAttrs: Set<string>) {
        for (const key of Object.keys(object)) {
            if (!validAttrs.has(key)) {
                continue;
            }

            const property: string = styleToHtml[key];
            if (!property) {
                continue;
            }

            styles[property] = object[key];
        }
    }

    /**
     * Toggles the table's edit mode on or off.
     */
    public toggleEditMode() {
        if (this.currentCell != undefined && this.editedCellContent != undefined) {
            if (this.editedCellInitialContent != this.editedCellContent && this.viewctrl) {
                const parId = getParId(this.element.parents(".par"));
                if (parId) {
                    this.saveCells(this.editedCellContent, this.viewctrl.item.id, parId, this.currentCell.row,
                        this.currentCell.col);
                }
            }
            this.currentCell = undefined;
        }
        if (!this.editing) { this.editSave(); }
        this.editing = !this.editing;
    }

    /**
     * Handles the user's click on the "add row" button.
     */
    async addRowButtonClick() {
        if (this.isInDataInputMode()) {
            this.addDatablockRow();
        } else {
            this.addRegularRow();
        }
    }

    /**
     * Tells the server to add a new row into this table.
     */
    async addRegularRow() {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        let response;

        if (this.isInGlobalAppendMode()) {
            response = await $http.post<TimTable>("/timTable/addUserSpecificRow",
                {docId, parId});
        } else {
            response = await $http.post<TimTable>("/timTable/addRow",
                {docId, parId});
        }

        this.data = response.data;
        this.reInitialize();
    }

    /**
     * Tells the server to add a new datablock-only row into this table.
     */
    async addDatablockRow() {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        const rowId = this.cellDataMatrix.length;
        const response = await $http.post<TimTable>("/timTable/addDatablockRow",
            {docId, parId, rowId});

        this.data = response.data;
        this.reInitialize();
    }

    /**
     * Tells the server to remove a row from this table.
     */
    async removeRow() {
        if (this.viewctrl == null || !this.data.table.rows) {
            return;
        }

        let datablockOnly;
        let rowId;

        if (this.isInDataInputMode()) {
            datablockOnly = true;
            rowId = this.cellDataMatrix.length - 1;
        } else {
            datablockOnly = false;
            rowId = this.data.table.rows.length - 1;
        }

        const docId = this.viewctrl.item.id;
        const parId = this.getOwnParId();

        if (rowId < 1) { return; }

        const response = await $http.post<TimTable>("/timTable/removeRow",
            {docId, parId, rowId, datablockOnly});
        this.data = response.data;
        this.reInitialize();
    }

    /**
     * Handles the user's click on the "add column" button.
     */
    async addColumnButtonClick() {
        if (this.isInDataInputMode()) {
            this.addColumn("/timTable/addDatablockColumn");
        } else {
            this.addColumn("/timTable/addColumn");
        }
    }

    /**
     * Tells the server to add a new column into this table.
     */
    async addColumn(route: string) {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        const response = await $http.post<TimTable>(route,
            {docId, parId});
        this.data = response.data;
        this.reInitialize();
    }

    /**
     * Handles clicks on the "remove column" button.
     */
    async removeColumnButtonClick() {
        if (this.isInDataInputMode()) {
            this.removeDatablockColumn();
        } else {
            this.removeColumn();
        }
    }

    /**
     * Tells the server to remove a datablock column from this table.
     */
    async removeDatablockColumn() {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        const response = await $http.post<TimTable>("/timTable/removeDatablockColumn",
            {docId, parId});
        this.data = response.data;
        this.reInitialize();
    }

    /**
     * Tells the server to remove a column from this table.
     */
    async removeColumn() {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        let colId = this.getColumnCount() - 1;

        if (colId < 1) { return; }

        const response = await $http.post<TimTable>("/timTable/removeColumn",
            {docId, parId, colId});
        this.data = response.data;
        this.reInitialize();
    }

    /**
     * Initializes the cell data matrix, reads the data block and sets its values
     * to the cell data matrix and processes all math.
     * Call this when the whole table's content is refreshed.
     */
    private reInitialize() {
        this.initializeCellDataMatrix();
        this.readDataBlockAndSetValuesToDataCellMatrix();
        ParCompiler.processAllMathDelayed(this.element);

        if (this.currentCell) {
            this.calculateElementPlaces(this.currentCell.row, this.currentCell.col);
        }
    }

    /**
     * Calculates and returns the number of columns in the table.
     */
    private getColumnCount() {
        if (this.viewctrl == null || !this.data.table.rows) {
            return 0;
        }

        let highestCellCount = 0;

        this.data.table.rows.forEach((row) => {
            if (row.row) {
                if (row.row.length > highestCellCount) {
                    highestCellCount = row.row.length;
                }
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
        return this.data.dataInput;
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
    private saveAndCloseSmallEditor() {
        this.saveCurrentCell();
        this.closeSmallEditor();
    }

    /**
     * Checks whether a cell is the currently active cell of the table.
     * The active cell is the cell that is being edited, or if no cell is being edited,
     * the cell that was edited last.
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

        if (this.lastEditedCell) {
            return this.lastEditedCell.row === rowi && this.lastEditedCell.col === coli;
        }

        return false;
    }

    private getColspan(rowi: number, coli: number) {
        const rows = this.data.table.rows;

        if (!rows ||
            rowi >= rows.length ||
            !rows[rowi].row ||
            coli >= rows[rowi].row.length ||
            !rows[rowi].row[coli].colspan) {
            return 1;
        }

        return rows[rowi].row[coli].colspan;
    }

    private getRowspan(rowi: number, coli: number) {
        const rows = this.data.table.rows;

        if (!rows ||
            rowi >= rows.length ||
            !rows[rowi].row ||
            coli >= rows[rowi].row.length ||
            !rows[rowi].row[coli].rowspan) {
            return 1;
        }

        return rows[rowi].row[coli].rowspan;
    }

    /**
     * Returns cell content HTML as trusted through AngularJS's SCE service.
     * Disables AngularJS HTML sanitizing for TimTable cells which breaks some attributes
     * of markdown tables placed inside TimTables (and possibly other things as well).
     * @param rowi Row index
     * @param coli Column index
     */
    private getTrustedCellContentHtml(rowi: number, coli: number) {
        return this.sceService.trustAsHtml(this.cellDataMatrix[rowi][coli].cell);
    }
}

timApp.component("timTable", {
    controller: TimTableController,
    bindings: {
        data: "<",
    },

    require: {
        viewctrl: "?^timView",
    },
    template: `<div ng-mouseenter="$ctrl.mouseInsideTable()"
     ng-mouseleave="$ctrl.mouseOutTable()">
    <div class="timTableContentDiv no-highlight">
    <button class="timButton buttonAddCol" title="Add column" ng-show="$ctrl.isInEditMode()"
            ng-click="$ctrl.addColumnButtonClick()"><span class="glyphicon glyphicon-plus"></span></button>
    <button class="timButton buttonRemoveCol" title="Remove column" ng-show="$ctrl.isInEditMode()"
            ng-click="$ctrl.removeColumnButtonClick()"><span class="glyphicon glyphicon-minus"></span></button>
    <table ng-class="{editable: $ctrl.isInEditMode() && !$ctrl.isInForcedEditMode(), forcedEditable: $ctrl.isInForcedEditMode()}" class="timTableTable"
     ng-style="$ctrl.stylingForTable($ctrl.data.table)" id={{$ctrl.data.table.id}}>
        <col ng-repeat="c in $ctrl.data.table.columns" ng-attr-span="{{c.span}}}" id={{c.id}}
             ng-style="$ctrl.stylingForColumn(c)"/>
        <tr ng-repeat="r in $ctrl.cellDataMatrix" ng-init="rowi = $index"
            ng-style="$ctrl.stylingForRow(rowi)">
                <td ng-class="{'activeCell': $ctrl.isActiveCell(rowi, coli)}"
                 ng-repeat="td in r" ng-init="coli = $index" colspan="$ctrl.getColspan(rowi, coli)" rowspan="$ctrl.getRowspan(rowi, coli)"
                    ng-style="$ctrl.stylingForCell(rowi, coli)" ng-click="$ctrl.cellClicked(td, rowi, coli, $event)">
                    <div ng-bind-html="$ctrl.getTrustedCellContentHtml(rowi, coli)">
                    </div>
                </td>
        </tr>
    </table>
    <button class="timButton buttonAddRow" title="Add row" ng-show="$ctrl.isInEditMode()" ng-click="$ctrl.addRowButtonClick()"><span
            class="glyphicon glyphicon-plus" ng-bind="$ctrl.addRowButtonText"></span></button>
    <button class="timButton buttonRemoveRow" title="Remove row" ng-show="$ctrl.isInEditMode()" ng-click="$ctrl.removeRow()"><span
            class="glyphicon glyphicon-minus"></span></button>            
    </div>
    <input class="editInput" ng-show="$ctrl.isSomeCellBeingEdited()"
                   ng-keydown="$ctrl.keyDownPressedInSmallEditor($event)"
                   ng-keyup="$ctrl.keyUpPressedInSmallEditor($event)" ng-model="$ctrl.editedCellContent">
             <button class="timButton buttonCloseSmallEditor" ng-show="$ctrl.isSomeCellBeingEdited()"
                    ng-click="$ctrl.closeSmallEditor()"
                    class="timButton"><span class="glyphicon glyphicon-remove"></span>
             <button class="timButton buttonAcceptEdit" ng-show="$ctrl.isSomeCellBeingEdited()"
                    ng-click="$ctrl.saveAndCloseSmallEditor()"
                     class="timButton"><span class="glyphicon glyphicon-check"></span>
             <button class="timButton buttonOpenBigEditor" ng-show="$ctrl.isSomeCellBeingEdited()"
                    ng-click="$ctrl.openBigEditor()" class="timButton"><span class="glyphicon glyphicon-pencil"></span>
            </button>
</div>
`,
});
