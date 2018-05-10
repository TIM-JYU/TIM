import {IController, IRootElementService, IScope} from "angular";
import $ from "jquery";
import {timApp} from "../app";
import {ViewCtrl} from "../controllers/view/viewctrl";
import {showMessageDialog} from "../dialog";
import {setSetting} from "../utils";
import {$http} from "../ngimport";
import {getParId} from 'tim/controllers/view/parhelpers';
import {openEditorSimple} from "../directives/pareditor";
import {ParCompiler} from "../services/parCompiler";

//import {IHash} from "../../decls/components/timTable";


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
        fontWeight: "font-weight"

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
}

export interface DataEntity {
    type: "Relative" | "Abstract";
    cells: CellDataEntity;
}


export interface CellDataEntity {
    [key: string]: string;
}


export type CellEntity = ICell | string | number;

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
    cell: string;
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

export interface IHash {
    [details: string]: number;
}


export class TimTableController implements IController {
    private static $inject = ["$scope", "$element"];
    public viewctrl?: ViewCtrl;
    public sc: IScope;
    public cellDataMatrix: string[][];
    public dataCells: DataEntity;
    public DataHash: IHash;
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
        document.addEventListener("keydown", this.keyDownPressedTable);

    }


    $onDestroy() {
        document.removeEventListener("keydown", this.keyDownPressedTable);
    }

    /*
    Update DataHashTable
     */
    UpdateDataHashTable() {

        for (var i = 0; i < 25; i++) {
            this.DataHash[""] = this.DataHashLength + 1;
            this.DataHashLength++;
        }
        //todo: calculate further
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

    /*
    Saving
     */
    public editSave() {
        this.editing = false;
        if (this.currentCell) this.currentCell = undefined;
    }

    public isCellBeingEdited(rowi: number, coli: number) {
        return this.currentCell && this.currentCell.col === coli && this.currentCell.row === rowi;
    }

    public mouseInsideTable() {
        this.mouseInTable = true;
    }


    public mouseOutTable() {
        this.mouseInTable = false;
    }

    /**
     *
     */
    public editCancel() {
        // undo all changes
        this.editing = false;
        if (this.currentCell) this.currentCell = undefined;
    }

    async saveCells(cellContent: string, docId: number, parId: string, row: number, col: number) {
        // tallenna solun sisältö
        /* const response = await $http<{ [cellContent: string]: string; }>({ //Miksi tama toimii vaikka response ei oikeasti ole tuota tyyppia?
             url: "/timTable/saveCell",
             method: "POST",
             params: {cellContent, docId, parId, row, col},
         });*/
        //if (cellContent === this.cellDataMatrix[row][col]) return;
        const response = await $http.post<{ [cellContent: string]: string; }>("/timTable/saveCell", {
            cellContent,
            docId,
            parId,
            row,
            col
        });
        const cellHtml = response.data[0];
        this.cellDataMatrix[row][col] = cellHtml;
        ParCompiler.processAllMathDelayed(this.element);
        // const response = await $http.post<ICell>("/timTable/saveCell", {cellContent, docId, parId, row, col});
    }

    async getCellData(cell: CellEntity, docId: number, parId: string, row: number, col: number) {
        let ctrl = this;
        const response = await $http<{ [cellContent: string]: string; }>({ //Miksi tama toimii vaikka response ei oikeasti ole tuota tyyppia?
            url: "/timTable/getCellData",
            method: "GET",
            params: {docId, parId, row, col},
        });

        const data = response.data;
        ctrl.editedCellContent = data[0]
        //showMessageDialog(this.editedCellContent);
        ctrl.cellDataMatrix[row][col] = data[0];
    }

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
        }
        if (result.type == "cancel") {
            this.currentCell = undefined;
        }
        if (typeof cell === "string" || typeof cell === "number") {
        }
        else cell.editorOpen = false;
    }

    private focus() {
        let ctrl = this;
        return function () {
            if (ctrl.editing === true) {
                showMessageDialog("ko");
            }
        }
    }

    /*
    Opens adv editor
     */
    private editorOpen(cell: CellEntity, rowi: number, coli: number) {
        //this.currentCell = undefined;
        if (this.currentCell)
            this.currentCell.editorOpen = true;
        let parId = getParId(this.element.parents(".par"));
        if (parId === undefined || !this.viewctrl) return;
        this.openEditor(cell, this.viewctrl.item.id, this.cellDataMatrix[rowi][coli], parId, rowi, coli)
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
                            if (typeof itemInRow === "string" || typeof itemInRow === "number") ctrl.cellDataMatrix[index][index2] = String(itemInRow);
                            else this.cellDataMatrix[index][index2] = itemInRow.cell;
                        }
                    });
            });
    }

    /*
    Reads DataBlock and sets all values to DataCellMatrix
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

    /*
    Address validation
     */
    private checkThatAddIsValid(address: { col: number, row: number }) {
        if (address.col >= 0 && address.row >= 0) return true;
    }

    /*
      Get placement, ex. A1 -> 0,0
      C5 -> 2,4
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
     * @param {KeyboardEvent} ev
     * @param {CellEntity} cell
     * @param {number} rowi
     * @param {number} coli
     */
    private keyDownPressed(ev: KeyboardEvent, cell: CellEntity, rowi: number, coli: number) {

        //if (ev. keyCode == 113) this.editorOpen(cell, rowi, coli);
        /* if (ev.keyCode == 13 ||  ev.keyCode === 9) {
             let parId = getParId(this.element.parents(".par"));
             if (!this.editing || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) return;
             if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false

                 this.saveCells(this.cellDataMatrix[this.currentCell.row][this.currentCell.col], this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
                 this.currentCell = undefined;
             }
             if (ev.keyCode === 13) {
                 this.openCell(rowi + 1, coli);
             }
             if (ev.keyCode === 9) {
                 this.openCell(rowi, coli + 1);
             }
         }*/

        if (ev.ctrlKey && (ev.keyCode == 40 || ev.keyCode == 39 || ev.keyCode == 38 || ev.keyCode == 37))
            this.handleArrowMovement(ev, rowi, coli);
    }


    /**
     * Deals with keyevents inside div
     * @param {KeyboardEvent} ev
     */
    private keyDownPressedTable(ev: KeyboardEvent) {
        if (this.mouseInTable) {
            if (ev.keyCode == 113) {
                var modal: CellEntity = {
                    cell: "",
                }
                if (this.currentCell != undefined) this.editorOpen(modal, this.currentCell.row, this.currentCell.col);
            }

            if (ev.keyCode == 13 || ev.keyCode === 9 || ev.keyCode === 27)  { // enter, tab or esc
                let parId = getParId(this.element.parents(".par"));
                if (!this.editing || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) return;
                if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
                    this.saveCells(this.cellDataMatrix[this.currentCell.row][this.currentCell.col], this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
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


    private handleArrowMovement(ev: KeyboardEvent, rowi: number, coli: number) {
        let parId = getParId(this.element.parents(".par"));
        if (!this.editing || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) return;
        if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false

            this.saveCells(this.cellDataMatrix[this.currentCell.row][this.currentCell.col], this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
            this.currentCell = undefined;
        }

        if (ev.keyCode == 40) {
            this.openCell(rowi + 1, coli);
            return;
        }

        if (ev.keyCode == 39) {
            this.openCell(rowi, coli + 1);
            return;
        }

        if (ev.keyCode == 37) {
            this.openCell(rowi, coli - 1);
            return;
        }


        if (ev.keyCode == 38) {
            this.openCell(rowi - 1, coli);
        }
    }

    /*
    Clicks given or hops opposite side of the table
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


    /*
Clicks given or hops opposite side of the table
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

    /*
    Sets a value to specific index in cellDataMatrix
     */
    private setValueToMatrix(row: number, col: number, value: string) {
        try {
            this.cellDataMatrix[col][row] = value;
        }
        catch (e) {
            console.log("datacellMatrix is not big enough");//this.updateCellDataMatrix(col, row);
        }
    }

    private updateCellDataMatrix(col: number, row: number) {
        this.cellDataMatrix = [];
        // if (this.data.table.rows)
        for (var index = 0; index < row; index++) {
            //((item, index) => {
            this.cellDataMatrix[index] = [];
            /*if(this.data.table.rows[index])
             item.row.forEach((item2, index2) => {
                 if (item.row) {
                     let itemInRow = item.row[index2] as ICell;
                     this.cellDataMatrix[index][index2] = itemInRow.cell;
                 }
             });*/
        }
        /*this.cellDataMatrix = [];
    if (this.data.table.rows)
        this.data.table.rows.forEach((item, index) => {
            this.cellDataMatrix[index] = [];
            if (item.row)
                item.row.forEach((item2, index2) => {
                    if (item.row) {
                        let itemInRow = item.row[index2] as ICell;
                        this.cellDataMatrix[index][index2] = itemInRow.cell;
                    }
                });
        });*/


    }

    /*
    Deals with cell clicking
     */
    private cellClicked(cell: CellEntity, rowi: number, coli: number) {
        let parId = getParId(this.element.parents(".par"));
        if (!this.editing || !this.viewctrl || !parId || (this.currentCell && this.currentCell.editorOpen)) return;

        if (this.currentCell)
            if (this.currentCell.row == rowi && this.currentCell.col == coli)  // if same as previous cell
            {
                //cell.editing = true;
                return;
            }

        this.getCellData(cell, this.viewctrl.item.id, parId, rowi, coli);
        //cell.editing = true;
        if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false

            this.saveCells(this.cellDataMatrix[this.currentCell.row][this.currentCell.col], this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
            this.currentCell = undefined;
        }
        this.currentCell = {row: rowi, col: coli, editorOpen: false};
    }

    /*
    Calculates index to abstract form, ex. 1,1 -> A1
    3,2 -> C2
    */
    private calculatePlace(rowi: number, coli: number) {
        const str = "A";
        // col is a number like 2 -> B
        let col: string = String.fromCharCode(str.charCodeAt(0) - 1 + coli); // 65 -1 = 64 + 2 = 66 = B
        return col + rowi;
    }

    /*
    Add element to DataBlock
     */
    private addtoDataBlock(key: string, value: string) {
        if (this.data.table.tabledatablock) {
            let modal: CellDataEntity = <CellDataEntity>{
                key: value,
            };
            this.data.table.tabledatablock.cells = modal;
        }
    }

    /*
    Create new DataBlock with type, but no cells
     */
    private createDataBlock() {
        let modal: DataEntity = <DataEntity>{};
        modal.type = "Relative";
        this.data.table.tabledatablock = modal;
    }

    private stylingForCell(cell: CellEntity) {

        if (typeof cell === "string" || typeof cell === "number") {
            return {};
        }
        const styles: { [index: string]: string } = {};

        // throws errors if cell is null, add null check?

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
     * Ei toimi vielä
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


    public isCellbeingEdited(rowi: string, coli: string) {
        return true;
    }

    /**
     * Gets the contents of a cell from the server
     * @param {number} docId
     * @param {string} parId
     * @param {number} row
     * @param {number} col
     * @returns {Promise<void>}
     */
    /*async getCellData(docId: number, parId: string, row: number, col: number) {
        const response = await $http<{ [cellContent: string]: string; }>({ //Miksi tama toimii vaikka response ei oikeasti ole tuota tyyppia?
            url: "/timTable/getCellData",
            method: "GET",
            params: {docId, parId, row, col},
        }); //TODO: virhekasittely
        const data = response.data;
        this.editedCellContent = data[0]
        //showMessageDialog(this.editedCellContent);
        this.cellDataMatrix[row][col] = data[0];
    }*/
}


timApp.component("timTable", {
    controller: TimTableController,
    bindings: {
        data: "<",
    },

    require: {
        viewctrl: "?^timView",
    },
    template: `<div ng-class="{editable: $ctrl.editing}"  ng-mouseenter="$ctrl.mouseInsideTable()" ng-mouseleave="$ctrl.mouseOutTable()">
    <button style="float: right" class="timButton" title="Add column" ng-show="$ctrl.editing" ng-click="$ctrl.addColumn()"><span class="glyphicon glyphicon-plus"></span></button>
    <table  ng-style="$ctrl.stylingForTable($ctrl.data.table)" id={{$ctrl.data.table.id}}>
    <col ng-repeat="c in $ctrl.data.table.columns" ng-attr-span="{{c.span}}}" id={{c.id}} ng-style="$ctrl.stylingForColumn(c)"/>
    <tr ng-repeat="r in $ctrl.data.table.rows"  ng-init="rowi = $index" id={{r.id}} ng-style="$ctrl.stylingForRow(r)">
      <div ng-if="$ctrl.allcellData == undefined">
        <td ng-repeat="td in r.row" ng-init="coli = $index" colspan="{{td.colspan}}" rowspan="{{td.rowspan}}" id={{td.id}}"
            ng-style="$ctrl.stylingForCell(td)" ng-click="$ctrl.cellClicked(td, rowi, coli)">
            <div ng-if="!$ctrl.isCellBeingEdited(rowi, coli)" ng-bind-html="$ctrl.cellDataMatrix[rowi][coli]">
           </div>
           <input style="position: absolute; color: #000000" ng-if="$ctrl.isCellBeingEdited(rowi, coli)" ng-model="$ctrl.cellDataMatrix[rowi][coli]" ng-keydown="$ctrl.keyDownPressed($event, td, rowi, coli)" focus-me="$ctrl.isCellBeingEdited(rowi, coli)"> 
           <button style="position: relative; top: 30px" ng-if="$ctrl.isCellBeingEdited(rowi, coli)" ng-click="$ctrl.editorOpen(td, rowi, coli)" class="timButton"><span class="glyphicon glyphicon-pencil"></span></button>
        </td>
       </tr>
     <div>  
</table>
<button class="timButton" title="Add row" ng-show="$ctrl.editing" ng-click="$ctrl.addRow()"><span class="glyphicon glyphicon-plus"></span></button>

</div>


<!--
<table style="">
   <col ng-repeat="c in $ctrl.data.columns" style="c.style">  "background-color:{{td.background}};"
    <tr ng-repeat="r in $ctrl.data.rows">terve</tr>
     <tr style="">
        <td colspan="20" style="text-align:center;background-color:yellow;"  ng-click="$ctrl.count = $ctrl.count +1"><h2 id="otsikko">{{$ctrl.count}}</h2></td>
    </tr>
    <tr style="background-color:blue;">
        <td style="" ><span class="math display">[int_a^b f(x) dx]</span></td>
        <td colspan="2" style="" ng-click="$ctrl.editor($event)">{{ans}}</td>
        <td style="border:10px solid red;">
            <div class="figure">
                <img src="/images/108/vesa640.png" alt="vesa"/>
                <p class="caption">vesa</p>
            </div>
        </td>
        <td style="background-color:blue;">Visa</td>
        <td rowspan="2" colspan="2" style="text-align:center;vertical-align:middle;">I'm from a Subversion background
            and, when I had a branch, I knew what I was working on with 'These working files point to this branch'. But
            with Git I'm not sure when I am editing a file in NetBeans or Notepad++, whether it's tied to the master or
            another branch. There's no problem with git in bash, it tells me what I'm doing.
        </td>
        <td><span class="yellow">matti</span></td>
        <td colspan="None" style="">
            <table>
                <thead>
                <tr class="header">
                    <th>Otsikko1</th>
                    <th align="left">Joo</th>
                    <th align="left">Ei</th>
                </tr>
                </thead>
                <tbody>
                <tr class="odd">
                    <td>1.rivi</td>
                    <td align="left">x</td>
                    <td align="left">o</td>
                </tr>
                <tr class="even">
                    <td>2.rivi</td>
                    <td align="left">o</td>
                    <td align="left">x</td>
                </tr>
                </tbody>
            </table>
        </td>
        <td></td>
    </tr>
    <tr style="">
        <td style="border-bottom:1px solid purple;">1-10</td>
        <td style="border-bottom:1px none white;">1-11,5-6</td>
        <td style="">1-11,5-6</td>
        <td style="">1-11,5-6</td>
        <td style="">1-11,5-6</td>
        <td style="">1-11,5-6</td>
        <td style="">1-11,5-6</td>
        <td style="background-color:grey;">1-11,5-6</td>
    </tr>
    <tr style="">
        <td style="">1-10</td>
        <td style="">1-11,5-6</td>
        <td style=""><span class="red">1-11,5-6</span></td>
        <td style="">1-11,5-6</td>
        <td style="">1-11,5-6</td>
        <td style="">1-11,5-6</td>
        <td style="">1-11,5-6</td>
        <td colspan="20" style="">1-11,5-6</td>
    </tr>
</table>-->
`
});



