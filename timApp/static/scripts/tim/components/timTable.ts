import {IController, IRootElementService, IScope} from "angular";
import {timApp} from "../app";
import {ViewCtrl} from "../controllers/view/viewctrl";
import {$http, $timeout} from "../ngimport";
import {getParId} from 'tim/controllers/view/parhelpers';
import {openEditorSimple} from "../directives/pareditor";
import {ParCompiler} from "../services/parCompiler";
export const EDITOR_CLASS = "editorArea";
export const EDITOR_CLASS_DOT = "." + EDITOR_CLASS;

const styleToHtml: { [index: string]: string } =
    {
        textAlign: "text-align",
        backgroundColor: "background-color",
        horizontalAlign: "horizontal-align",
        verticalAlign: "vertical-align",
        border: "border",
        borderTop: "border-top",
        borderBottom: "border-bottom",
        borderLeft: "border-left",
        borderRight: "border-right",
        width: "width",
        height: "height",
        color: "color",
        fontSize: "font-size",
        fontFamily: "font-family",
        fontWeight: "font-weight",
        visibility: "visibility"

    };


export interface TimTable {
    table: ITable;
    id?: string;
}

export interface ITable extends ITableStyles {
    rows?: (IRow)[];
    columns?: (IColumn)[];
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
    row?: (CellEntity)[];
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

function isPrimitiveCell (cell : CellEntity): cell is CellType {
    return cell == null || (cell as ICell).cell === undefined;
}


export class TimTableController implements IController {
    private static $inject = ["$scope", "$element"];
    public viewctrl?: ViewCtrl;
    public sc: IScope;
    public cellDataMatrix: string[][];
    public dataCells: DataEntity;
    private srcid: string;  // document id
    private $pars: JQuery;  // paragraph
    private data: TimTable;
    private allcellData: string[];
    private count: number;
    private editing: boolean;
    private DataHashLength: number;
    private editedCellContent: string;
    private currentCell?: { row: number, col: number, editorOpen: boolean };
    private mouseInTable?: boolean;

    constructor(private scope: IScope, private element: IRootElementService) {
        this.keyDownPressedTable = this.keyDownPressedTable.bind(this);
    }

    /**
     * Set listener and initializes tabledatablock
     */
    $onInit() {
        this.count = 0;
        this.InitializeCellDataMatrix();
        this.readDataBlockAndSetValuesToDataCellMatrix();

        if (this.viewctrl == null)
            return;
        else {
            let parId = getParId(this.element.parents(".par"));
            if (parId == null)
                return;
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
        if (!this.editing)
            return false;
        return this.editing;
    }

    /**
     * Set attirbutes value to correct ones when saved cell values
     */
    public editSave() {
        this.editing = false;
        if (this.currentCell) this.currentCell = undefined;
    }

    /**
     * Return true if cell is being edited
     * @param {number} rowi Row index
     * @param {number} coli Col index
     * @returns {{row: number; col: number; editorOpen: boolean} | boolean}
     */
    public isCellBeingEdited(rowi: number, coli: number) {
        return this.currentCell && this.currentCell.col === coli && this.currentCell.row === rowi;
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
     * When editin is cancelled, sets attributes
     */
    public editCancel() {
        this.editing = false;
        if (this.currentCell) this.currentCell = undefined;
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
        if (!(cellContent.startsWith("md:"))) {
            if ((cellContent === this.cellDataMatrix[row][col])) {
                ParCompiler.processAllMathDelayed(this.element);
                return;
            }
        }
        const response = await $http.post<string[]>("/timTable/saveCell", {
            cellContent,
            docId,
            parId,
            row,
            col
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
        let ctrl = this;
        const response = await $http< CellType[]   >({
            url: "/timTable/getCellData",
            method: "GET",
            params: {docId, parId, row, col},
        });

        const data = response.data;
        let value = this.cellToString(data[0]);
        ctrl.editedCellContent = value;
        ctrl.cellDataMatrix[row][col] = value;
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

        let ctrl = this;
        let i = ctrl.editedCellContent;
        // this.cellDataMatrix[rowi][coli] = this.editedCellContent;
        if (ctrl.currentCell) ctrl.currentCell.editorOpen = true;
        const result = await openEditorSimple(docId, ctrl.cellDataMatrix[row][col]);
        if (ctrl.currentCell) ctrl.currentCell.editorOpen = false;
        if (result.type == "save" && result.text != ctrl.editedCellContent) {
            ctrl.saveCells(result.text, docId, parId, row, col);
            ctrl.cellDataMatrix[row][col] = result.text
            ctrl.editedCellContent = result.text;
        }
        if (result.type == "cancel") {
            this.currentCell = undefined;
        }
        if (isPrimitiveCell(cell)) {
        }
        else cell.editorOpen = false;
    }

    /**
     * Opens advanced editor
     * @param {CellEntity} cell Opened cell
     * @param {number} rowi Row index
     * @param {number} coli Column ndex
     */
    private editorOpen(cell: CellEntity, rowi: number, coli: number) {
        if (this.currentCell)
            this.currentCell.editorOpen = true;
        let parId = getParId(this.element.parents(".par"));
        if (parId === undefined || !this.viewctrl) return;
        this.openEditor(cell, this.viewctrl.item.id, this.cellDataMatrix[rowi][coli], parId, rowi, coli)
    }


    /**
     * Opens advanced editor
     */
    private openBigEditor() {

        var modal: CellEntity = {
            cell: this.editedCellContent,
        }
        if (this.currentCell != undefined) this.editorOpen(modal, this.currentCell.row, this.currentCell.col);
    }


    /**
     * Initialize celldatamatrix with the values from yaml and yaml only
     * @constructor
     */
    private InitializeCellDataMatrix() {
        let ctrl = this;
        this.cellDataMatrix = [];
        if (ctrl.data.table.rows)
            ctrl.data.table.rows.forEach((item, index) => {
                ctrl.cellDataMatrix[index] = [];
                if (item.row)
                    item.row.forEach((item2, index2) => {
                        if (item.row) {
                            let itemInRow = item.row[index2];
                            if (isPrimitiveCell(itemInRow)) ctrl.cellDataMatrix[index][index2] = this.cellToString(itemInRow);
                            else this.cellDataMatrix[index][index2] = this.cellToString(itemInRow.cell);
                        }
                    });
            });
    }


    /**
     * Transforms cell to string
     * @param {CellType} cell Changed cell
     * @returns {string}
     */
    private cellToString(cell: CellType){
        if (cell == null) return "";
        return cell.toString();
    }

    /**
     * Reads DataBlock and sets all values to DataCellMatrix
     */
    private readDataBlockAndSetValuesToDataCellMatrix() {
        if (this.data.table.tabledatablock)   // reads tabledatablock and sets all values to datacellmatrix
            for (let item in this.data.table.tabledatablock.cells) {

                const alphaRegExp = new RegExp('([A-Z]*)');
                let alpha = alphaRegExp.exec(item);
                let value = this.data.table.tabledatablock.cells[item];

                if (alpha == null) continue;
                let numberPlace = item.substring(alpha[0].length);

                let address = this.getAddress(alpha[0], numberPlace);
                if (this.checkThatAddIsValid(address))
                    this.setValueToMatrix(address.col, address.row, value.toString());
            }
    }

    /**
     * Coordinates validation
     * @param {{col: number; row: number}} address row and column index
     * @returns {boolean} true if valid
     */
    private checkThatAddIsValid(address: { col: number, row: number }) {
        if (address.col >= 0 && address.row >= 0) return true;
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
            columnIndex += (colValue.charCodeAt(charIndex) - charCodeOfA + 1) * Math.pow(asciiCharCount, reversedCharacterPlaceInString)
            reversedCharacterPlaceInString++;
        }
        columnIndex = columnIndex - 1;
        let rowIndex: number = parseInt(rowValue) - 1;
        return {col: columnIndex, row: rowIndex}
    }


    /**
     * Deals with key events
     * @param {KeyboardEvent} ev Pressed key event
     */
    private keyUpPressedInSmallEditor(ev: KeyboardEvent) {
        if (ev.ctrlKey && (ev.keyCode == 40 || ev.keyCode == 39 || ev.keyCode == 38 || ev.keyCode == 37))
            this.handleArrowMovement(ev);
    }


    /**
     * Deals with keyevents inside div
     * @param {KeyboardEvent} ev KeyboardEvent
     */
    private keyDownPressedTable(ev: KeyboardEvent) {
        if (this.mouseInTable) {
            if (ev.keyCode == 113) {
                var modal: CellEntity = {
                    cell: "",
                }
                if (this.currentCell != undefined) this.editorOpen(modal, this.currentCell.row, this.currentCell.col);
            }
            if (ev.keyCode == 13 || ev.keyCode === 9 || ev.keyCode === 27) { // enter, tab or esc
                let parId = getParId(this.element.parents(".par"));
                if (!this.editing || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) return;
                if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
                    let value = this.editedCellContent;
                    if (typeof value == "string") {
                        this.saveCells(value, this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
                    }
                }
                if (ev.keyCode === 13) {
                    if (this.currentCell)
                        this.openCellNextRowOrColumn(this.currentCell.row + 1, this.currentCell.col);
                }
                if (ev.keyCode === 9) {
                    if (this.currentCell)
                        this.openCellNextRowOrColumn(this.currentCell.row, this.currentCell.col + 1);
                }
                if (ev.keyCode === 27) {
                    this.currentCell = undefined;
                }
            }
        }
    }

    /**
     * Handles arrow movement inside table
     * @param {KeyboardEvent} ev Keyboardevent
     */
    private handleArrowMovement(ev: KeyboardEvent) {
        var modal: CellEntity = {
            cell: "",
        }

        let parId = getParId(this.element.parents(".par"));
        if (!this.editing || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) return;
        if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
            //let value = this.element.find(".editInput").val();
            let value = this.editedCellContent;
            if (typeof value == "string") {
                this.saveCells(value, this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
            }
        }
        if (ev.keyCode == 40) { // down arrow
            if (this.currentCell)
                this.openCell(this.currentCell.row + 1, this.currentCell.col);
            return;
        }
        if (ev.keyCode == 39) {
            if (this.currentCell)
                this.openCell(this.currentCell.row, this.currentCell.col + 1);
            return;
        }
        if (ev.keyCode == 37) {
            if (this.currentCell)
                this.openCell(this.currentCell.row, this.currentCell.col - 1);
            return;
        }
        if (ev.keyCode == 38) {
            if (this.currentCell)
                this.openCell(this.currentCell.row - 1, this.currentCell.col);
        }
    }

    /**
     * Clicks specified cell or hops opposite side of the table
     * @param {number} rowi Row index
     * @param {number} coli Column index
     */
    private openCell(rowi: number, coli: number) {
        var modal: CellEntity = {
            cell: "",
        }
        if (this.data.table.rows) {
            if (rowi >= this.data.table.rows.length) rowi = 0; // if bigger then 0
            if (rowi < 0) rowi = this.data.table.rows.length - 1;
        }

        if (this.data.table.rows && this.data.table.rows[rowi].row) {
            let rowrow = this.data.table.rows[rowi].row;
            if (rowrow) {
                if (coli >= rowrow.length) coli = 0;
                if (coli < 0) coli = rowrow.length - 1;
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
        var modal: CellEntity = {
            cell: "",
        }
        if (this.data.table.rows) {
            if (rowi >= this.data.table.rows.length) rowi = 0; // if bigger then 0
            if (rowi < 0) rowi = this.data.table.rows.length - 1;
        }

        if (this.data.table.rows && this.data.table.rows[rowi].row) {
            let rowrow = this.data.table.rows[rowi].row;
            if (rowrow) {
                if (coli >= rowrow.length) {
                    coli = 0;
                    if (rowi + 1 >= this.data.table.rows.length) rowi = 0;
                    else rowi += 1;
                }
                if (coli < 0) coli = rowrow.length - 1;
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
        }
        catch (e) {
            console.log("datacellMatrix is not big enough");//this.updateCellDataMatrix(col, row);
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
        let parId = getParId(this.element.parents(".par"));
        if (!this.editing || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) return;

        if (this.currentCell)
            if (this.currentCell.row == rowi && this.currentCell.col == coli)  // if same as previous cell
            {
                return;
            }

        this.getCellData(cell, this.viewctrl.item.id, parId, rowi, coli);
        if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
            let value = this.editedCellContent;

            if (typeof value == "string") {
                this.saveCells(value, this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
                this.currentCell = undefined;
            }
        }
        this.currentCell = {row: rowi, col: coli, editorOpen: false};
        this.calculateElementPlaces(rowi, coli, event);
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
         let table = this.element.find(".timTableTable").first();
         let tablecell = table.find('tr').eq(rowi).find('td').eq(coli);
         let off = undefined;
        if (event) {
            let obj = $(event.target);
            if (obj.prop('tagName') !== "TD") {
                obj = obj.parents('td').last();
            }
            off = obj.offset();
            if (!off) return;
        }
        else {
            off = tablecell.offset();
            if (!off) return;
        }
        if(off) {
            this.element.find(".editInput").offset(off);
            await $timeout();
            let edit = this.element.find(".editInput");
            edit.focus();
            let editOffset = edit.offset();
            let editOuterHeight = edit.outerHeight();
            let tablecellOffset = tablecell.offset();
            if (editOffset && editOuterHeight && tablecellOffset)
                this.element.find(".buttonOpenBigEditor").offset({
                    top: editOffset.top + editOuterHeight,
                    left: tablecellOffset.left
                });
        }
    }

    /**
     * Calculates index to abstract form, ex. 1,1 -> A1
     * ex. 3,2 -> C2
     * @param {number} rowi Row index
     * @param {number} coli Column index
     * @returns {string} Letter and number based coordination values
     */
    private calculatePlace(rowi: number, coli: number) {
        const str = "A"; // coli is a number like 2 -> B
        let col: string = String.fromCharCode(str.charCodeAt(0) - 1 + coli); // 65 -1 = 64 + 2 = 66 = B
        return col + rowi;
    }

    /**
     * Add element to DataBlock
     * @param {string} key Attribute name
     * @param {string} value Attribute value
     */
    private addtoDataBlock(key: string, value: string) {
        if (this.data.table.tabledatablock) {
            let modal: CellDataEntity = <CellDataEntity>{
                key: value,
            };
            this.data.table.tabledatablock.cells = modal;
        }
    }

    /**
     * Create new DataBlock with type, but no cells
     */
    private createDataBlock() {
        let modal: DataEntity = <DataEntity>{};
        modal.type = "Relative";
        this.data.table.tabledatablock = modal;
    }


    /**
     * Set styles fo cells
     * @param {CellEntity} cell Styled cell
     */
    private stylingForCell(cell: CellEntity) {

        if (isPrimitiveCell(cell)) {
            return {};
        }
        const styles: { [index: string]: string } = {};
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
     * Set attributes for columns
     * @param {IColumn} col Column that need styling
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
     * Set attributes for rows
     * @param {IRow} row Styled row
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
     * Sets style for table
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
        if (this.currentCell != undefined && typeof(this.currentCell) != "string") {
            this.currentCell = undefined;
        }
        if (!this.editing) this.editSave();
        this.editing = !this.editing;
    }


    /**
     * Tells the server to add a new row into this table.
     */
    async addRow() {
        if (this.viewctrl == null)
            return;

        let parId = this.getOwnParId();
        let docId = this.viewctrl.item.id;
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
        if (this.viewctrl == null)
            return;

        let parId = this.getOwnParId();
        let docId = this.viewctrl.item.id;
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
}


timApp.component("timTable", {
    controller: TimTableController,
    bindings: {
        data: "<",
    },

    require: {
        viewctrl: "?^timView",
    },
    template: `<div ng-class="{editable: $ctrl.editing}" ng-mouseenter="$ctrl.mouseInsideTable()"
     ng-mouseleave="$ctrl.mouseOutTable()">
     <div class="timTableContentDiv">
    <button class="timButton buttonAddRow" title="Add row" ng-show="$ctrl.editing"
            ng-click="$ctrl.addRow()"><span class="glyphicon glyphicon-plus"></span></button>
    <table class="timTableTable" ng-style="$ctrl.stylingForTable($ctrl.data.table)" id={{$ctrl.data.table.id}}>
        <col ng-repeat="c in $ctrl.data.table.columns" ng-attr-span="{{c.span}}}" id={{c.id}}
             ng-style="$ctrl.stylingForColumn(c)"/>
        <tr ng-repeat="r in $ctrl.data.table.rows" ng-init="rowi = $index" id={{r.id}}
            ng-style="$ctrl.stylingForRow(r)">
            <div ng-if="$ctrl.allcellData == undefined">
                <td ng-repeat="td in r.row" ng-init="coli = $index" colspan="{{td.colspan}}" rowspan="{{td.rowspan}}"
                    id={{td.id}}"
                    ng-style="$ctrl.stylingForCell(td)" ng-click="$ctrl.cellClicked(td, rowi, coli, $event)">
                    <div ng-bind-html="$ctrl.cellDataMatrix[rowi][coli]">
                    </div>
                </td>
               </div>
        </tr>
    </table>
    <button class="timButton buttonAddCol" title="Add column" ng-show="$ctrl.editing" ng-click="$ctrl.addColumn()"><span
            class="glyphicon glyphicon-plus"></span></button>
            </div>
    <input class="editInput" ng-show="$ctrl.isSomeCellBeingEdited()"
                   ng-keydown="$ctrl.keyDownPressedInSmallEditor($event)"
                   ng-keyup="$ctrl.keyUpPressedInSmallEditor($event)" ng-model="$ctrl.editedCellContent">
            <button class="timButton buttonOpenBigEditor" ng-show="$ctrl.isSomeCellBeingEdited()"
                    ng-click="$ctrl.openBigEditor()" class="timButton"><span class="glyphicon glyphicon-pencil"></span>
            </button>
</div>
`
});