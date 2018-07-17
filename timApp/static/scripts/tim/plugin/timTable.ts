import {IController, IRootElementService, IScope} from "angular";
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
    [key: string]: string;
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

export interface ICellStyles {
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
}

export interface ICell extends ICellStyles {
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
}

export interface IColumn extends IColumnStyles {
    id?: string;
    span?: number;
    formula?: string;
}

export interface IColumnStyles {
    width?: string;
    backgroundColor?: string;
    border?: string;
    borderTop?: string;
    borderBottom?: string;
    borderLeft?: string;
    borderRight?: string;
}

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
    private static $inject = ["$scope", "$element"];
    public viewctrl?: ViewCtrl;
    public cellDataMatrix: string[][] = [];
    private data!: Binding<TimTable, "<">;
    private editing: boolean = false;
    private editedCellContent: string | undefined;
    private editedCellInitialContent: string | undefined;
    private currentCell?: { row: number, col: number, editorOpen: boolean };
    private mouseInTable?: boolean;
    private bigEditorOpen: boolean = false;

    private addRowButtonText: string = "";

    constructor(private scope: IScope, private element: IRootElementService) {
        this.keyDownPressedTable = this.keyDownPressedTable.bind(this);
    }

    /**
     * Set listener and initializes tabledatablock
     */
    $onInit() {

        this.InitializeCellDataMatrix();
        this.readDataBlockAndSetValuesToDataCellMatrix();

        if (this.viewctrl == null) {
            return;
        } else {
            const parId = getParId(this.element.parents(".par"));
            if (parId == null) {
                return;
            }
            if (this.data.addRowButtonText) {
                this.addRowButtonText = this.data.addRowButtonText;
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
     * Checks whether the table is in edit mode.
     * @returns {boolean} True if the table is in edit mode, otherwise false.
     */
    public isInEditMode() {
        if (!this.editing) {
            return false;
        }
        return this.editing;
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
        this.cellDataMatrix[row][col] = cellHtml;
        ParCompiler.processAllMathDelayed(this.element);
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
            () => { this.bigEditorOpen = false; });
        this.bigEditorOpen = false;
        if (this.currentCell) { this.currentCell.editorOpen = false; }
        if (result.type == "save" && result.text != this.editedCellInitialContent) {
            this.saveCells(result.text, docId, parId, row, col);
            // ctrl.cellDataMatrix[row][col] = result.text
            this.editedCellContent = result.text;
        }
        if (result.type == "cancel") {
            this.currentCell = undefined;
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
        this.openEditor(cell, this.viewctrl.item.id, this.cellDataMatrix[rowi][coli], parId, rowi, coli);
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
    private InitializeCellDataMatrix() {
        this.cellDataMatrix = [];
        if (this.data.table.rows) {
            this.data.table.rows.forEach((item, index) => {
                this.cellDataMatrix[index] = [];
                if (item.row) {
                    item.row.forEach((item2, index2) => {
                        if (item.row) {
                            const itemInRow = item.row[index2];
                            if (isPrimitiveCell(itemInRow)) { this.cellDataMatrix[index][index2] = this.cellToString(itemInRow); } else { this.cellDataMatrix[index][index2] = this.cellToString(itemInRow.cell); }
                        }
                    });
                }
            });
        }
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
     * Returns a cell's content as a string.
     * @param {CellEntity} cell The cell.
     * @returns {string | string}
     */
    private getCellContentString(cell: CellEntity) {
        if (isPrimitiveCell(cell)) { return this.cellToString(cell); }

        return this.cellToString(cell.cell);
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
                    this.setValueToMatrix(address.col, address.row, value.toString());
                }
            }
        }
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
     * Deals with key events
     * @param {KeyboardEvent} ev Pressed key event
     */
    private keyUpPressedInSmallEditor(ev: KeyboardEvent) {
        if (ev.ctrlKey && (ev.keyCode == 40 || ev.keyCode == 39 || ev.keyCode == 38 || ev.keyCode == 37)) {
            this.handleArrowMovement(ev);
        }
    }

    /**
     * Deals with keyevents inside div
     * @param {KeyboardEvent} ev KeyboardEvent
     */
    private keyDownPressedTable(ev: KeyboardEvent) {
        if (this.mouseInTable) {
            if (ev.keyCode == 113) {
                const modal: CellEntity = {
                    cell: "",
                };
                if (this.currentCell != undefined) { this.editorOpen(modal, this.currentCell.row, this.currentCell.col); }
            }
            if (ev.keyCode == 13 || ev.keyCode === 9) { // enter or tab
                const parId = getParId(this.element.parents(".par"));
                if (!this.editing || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) { return; }
                if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
                    const value = this.editedCellContent;
                    if (typeof value == "string" && this.editedCellInitialContent != value) {
                        this.saveCells(value, this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
                    }
                }
                if (ev.keyCode === 13) {
                    if (this.currentCell) {
                        this.openCellNextRowOrColumn(this.currentCell.row + 1, this.currentCell.col);
                    }
                }
                if (ev.keyCode === 9) {
                    if (this.currentCell) {
                        this.openCellNextRowOrColumn(this.currentCell.row, this.currentCell.col + 1);
                    }
                }

            }
            if (ev.keyCode === 27) { // esc
                this.currentCell = undefined;
                this.scope.$apply();
            }

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

        if (ev.keyCode == 40) { // down arrow
            if (this.currentCell) {
                this.openCell(this.currentCell.row + 1, this.currentCell.col);
            }
            return;
        }
        if (ev.keyCode == 39) {
            if (this.currentCell) {
                this.openCell(this.currentCell.row, this.currentCell.col + 1);
            }
            return;
        }
        if (ev.keyCode == 37) {
            if (this.currentCell) {
                this.openCell(this.currentCell.row, this.currentCell.col - 1);
            }
            return;
        }
        if (ev.keyCode == 38) {
            if (this.currentCell) {
                this.openCell(this.currentCell.row - 1, this.currentCell.col);
            }
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
        if (this.data.table.rows) {
            if (rowi >= this.data.table.rows.length) { rowi = 0; } // if bigger then 0
            if (rowi < 0) { rowi = this.data.table.rows.length - 1; }
        }

        if (this.data.table.rows && this.data.table.rows[rowi].row) {
            const rowrow = this.data.table.rows[rowi].row;
            if (rowrow) {
                if (coli >= rowrow.length) { coli = 0; }
                if (coli < 0) { coli = rowrow.length - 1; }
            }
        }
        this.cellClicked(modal, rowi, coli);
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
        if (this.data.table.rows) {
            if (rowi >= this.data.table.rows.length) { rowi = 0; } // if bigger then 0
            if (rowi < 0) { rowi = this.data.table.rows.length - 1; }
        }

        if (this.data.table.rows && this.data.table.rows[rowi].row) {
            const rowrow = this.data.table.rows[rowi].row;
            if (rowrow) {
                if (coli >= rowrow.length) {
                    coli = 0;
                    if (rowi + 1 >= this.data.table.rows.length) { rowi = 0; } else { rowi += 1; }
                }
                if (coli < 0) { coli = rowrow.length - 1; }
            }
        }
        this.cellClicked(modal, rowi, coli);
    }

    /**
     * Sets a value to specific index in cellDataMatrix
     * @param {number} row Row index
     * @param {number} col Column index
     * @param {string} value Stored value
     */
    private setValueToMatrix(row: number, col: number, value: string) {
        try {
            this.cellDataMatrix[col][row] = value;
        } catch (e) {
            console.log("datacellMatrix is not big enough"); // this.updateCellDataMatrix(col, row);
        }
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
        if (!this.editing || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) { return; }

        if (this.currentCell) {
            if (this.currentCell.row == rowi && this.currentCell.col == coli) {
                return;
            }
        }

        await this.getCellData(cell, this.viewctrl.item.id, parId, rowi, coli);
        this.saveCurrentCell();
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
                this.currentCell = undefined;
            }
        }
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
        const tablecell = table.children("tbody").children("tr").eq(rowi).children("td").eq(coli);
        let off;
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
        }
        if (off) {
            this.element.find(".editInput").offset(off);
            await $timeout();
            const edit = this.element.find(".editInput");
            edit.focus();
            const editOffset = edit.offset();
            const editOuterHeight = edit.outerHeight();
            const tableCellOffset = tablecell.offset();
            const editOuterWidth = edit.outerWidth();
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
     * @param {number] coli Table column index
     */
    private stylingForCell(cell: CellEntity, coli: number) {
        const styles = this.stylingForCellOfColumn(coli);

        if (this.getCellContentString(cell) === "") {
            styles["height"] = "2em";
            styles["width"] = "4em";
        }

        if (isPrimitiveCell(cell)) {
            return styles;
        }

        for (const key of Object.keys(cell)) {
            const keyofCell = key as keyof ICell;
            const property: string = styleToHtml[keyofCell];
            if (property === undefined) {
                continue;
            }
            const c = (cell as ICellStyles)[keyofCell as keyof ICellStyles];
            if (!c) {
                continue;
            }
            styles[property] = c;
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

        for (const key of Object.keys(col)) {
            if (!columnCellStyles.has(key)) {
                continue;
            }

            const property: string = styleToHtml[key];
            if (!property) {
                continue;
            }

            styles[property] = col[key];
        }
        return styles;
    }

    /**
     * Sets style attributes for columns
     * @param {IColumn} col The column to be styled
     */
    private stylingForColumn(col: IColumn) {
        const styles: { [index: string]: string } = {};

        for (const key of Object.keys(col)) {
            const property: string = styleToHtml[key];
            if (property === undefined) {
                continue;
            }
            const c = col[key as keyof IColumnStyles];
            if (!c) {
                continue;
            }
            styles[property] = c;
        }
        return styles;
    }

    /**
     * Sets style attributes for rows
     * @param {IRow} row The row to be styled
     */
    private stylingForRow(row: IRow) {
        const styles: { [index: string]: string } = {};

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
     * Tells the server to add a new row into this table.
     */
    async addRow() {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        const response = await $http.post<TimTable>("/timTable/addRow",
            {docId, parId});
        this.data = response.data;
        this.InitializeCellDataMatrix();
        this.readDataBlockAndSetValuesToDataCellMatrix();
    }

    /**
     * Tells the server to add a new column into this table.
     */
    async addColumn() {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        const response = await $http.post<TimTable>("/timTable/addColumn",
            {docId, parId});
        this.data = response.data;
        this.InitializeCellDataMatrix();
        this.readDataBlockAndSetValuesToDataCellMatrix();
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
        this.currentCell = undefined;
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
     <div class="timTableContentDiv">
    <button class="timButton buttonAddCol" title="Add column" ng-show="$ctrl.editing"
            ng-click="$ctrl.addColumn()"><span class="glyphicon glyphicon-plus"></span></button>
    <table ng-class="{editable: $ctrl.editing}" class="timTableTable"
     ng-style="$ctrl.stylingForTable($ctrl.data.table)" id={{$ctrl.data.table.id}}>
        <col ng-repeat="c in $ctrl.data.table.columns" ng-attr-span="{{c.span}}}" id={{c.id}}
             ng-style="$ctrl.stylingForColumn(c)"/>
        <tr ng-repeat="r in $ctrl.data.table.rows" ng-init="rowi = $index" id={{r.id}}
            ng-style="$ctrl.stylingForRow(r)">
                <td ng-repeat="td in r.row" ng-init="coli = $index" colspan="{{td.colspan}}" rowspan="{{td.rowspan}}"
                    id={{td.id}}"
                    ng-style="$ctrl.stylingForCell(td, coli)" ng-click="$ctrl.cellClicked(td, rowi, coli, $event)">
                    <div ng-bind-html="$ctrl.cellDataMatrix[rowi][coli]">
                    </div>
                </td>
        </tr>
    </table>
    <button class="timButton buttonAddRow" title="Add row" ng-show="$ctrl.editing" ng-click="$ctrl.addRow()"><span
            class="glyphicon glyphicon-plus" ng-bind="$ctrl.addRowButtonText"></span></button>
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
