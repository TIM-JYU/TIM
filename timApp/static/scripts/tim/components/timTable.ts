import {IController, IRootElementService, IScope} from "angular";
import $ from "jquery";
import {timApp} from "../app";
import {ViewCtrl} from "../controllers/view/viewctrl";
import {showMessageDialog} from "../dialog";
import {setSetting} from "../utils";
import {$http} from "../ngimport";
import {getParId} from 'tim/controllers/view/parhelpers';
import {openEditorSimple} from "../directives/pareditor";

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
        fontFamily: "font-family"
    };

export interface TimTable {
    table: ITable;
    id?: string;
}

export interface ITable extends ITableStyles {
    rows?: (IRow)[];
    columns?: (IColumn)[];
    datablock?: DataEntity;
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


export type CellEntity = ICell | string;

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
    fontSize: string;
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
    inputScope? : boolean | undefined;
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
    private srcid: string;  // document id
    private $pars: JQuery;  // paragraph
    private static $inject = ["$scope", "$element"];
    private data: TimTable;
    private allcellData: string[];
    private count: number;
    public viewctrl?: ViewCtrl;
    public sc: IScope;
    private editing: boolean;
    public currentCell: ICell |undefined;
    public cellDataMatrix: string[][];
    public dataCells: DataEntity;
    public DataHash: IHash;
    private DataHashLength: number;
    private editedCellContent: string;


    constructor(private scope: IScope, private element: IRootElementService) {

    }


    $onInit() {
        this.count = 0;
        // this.$pars = $(this.srcid);

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
    }

    /*
    Opens adv editor
     */
    private editorOpen(cell: CellEntity, rowi: number, coli: number) {

        if (typeof cell === "string") return;
        cell.editing = false;
        cell.editorOpen = true;
        let parId = getParId(this.element.parents(".par"));
        /*if(this.viewctrl && parId){
        this.getCellData(cell, this.viewctrl.item.id, parId, rowi, coli);
        }*/
        if (parId != undefined && this.viewctrl) this.openEditor(cell, this.viewctrl.item.id, this.cellDataMatrix[rowi][coli],parId ,rowi, coli)
    }

    /*
    Initialize CellDataMatrix with values from yaml table
     */
    private InitializeCellDataMatrix() {
        this.cellDataMatrix = [];
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
            });
    }

    /*
    Reads DataBlock and sets all values to DataCellMatrix
     */
    private readDataBlockAndSetValuesToDataCellMatrix() {
        if (this.data.table.datablock)   // reads datablock and sets all values to datacellmatrix
            for (let item in this.data.table.datablock.cells) {

                const alphaRegExp = new RegExp('([A-Z]*)');
                let alpha = alphaRegExp.exec(item);
                let value = this.data.table.datablock.cells[item];

                if (alpha == null) continue;
                let numberPlace = item.substring(alpha[0].length);

                let address = this.getAddress(item);
                this.setValueToMatrix(address.col, address.row, value.toString());
            }
    }

    /*
      Get placement, ex. A1 -> 1,1
      C5 -> 3,5
     */
    private getAddress(address: string) {
        const str = "A";
        let col = address.charCodeAt(0) - str.charCodeAt(0);
        let row: number = parseInt(address.substring(1));

        //todo: calculate characters further
        return {col: col, row: row}
    }


    private keyDownPressed(ev : KeyboardEvent, cell: CellEntity, rowi: number, coli: number) {

        if (ev.keyCode == 113) this.editorOpen(cell, rowi, coli);
        if (ev.keyCode == 13) {

            let parId = getParId(this.element.parents(".par"));
            if (!this.editing || typeof cell === "string" || !this.viewctrl || !parId || cell.editorOpen) return;
            if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
                this.currentCell.editing = false;
                this.saveCells(this.cellDataMatrix[this.currentCell.row][this.currentCell.col], this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);

                this.currentCell = undefined;
            }
        }
        if (ev.keyCode == 40) {

            // save current cell
            let parId = getParId(this.element.parents(".par"));
            if (!this.editing || typeof cell === "string" || !this.viewctrl || !parId || cell.editorOpen) return;
            if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
                this.currentCell.editing = false;
                this.saveCells(this.cellDataMatrix[this.currentCell.row][this.currentCell.col], this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
                this.currentCell = undefined;

                //open the new cell

                this.openCell(rowi +1, coli);

            }

        }
    }

    private openCell(rowi: number, coli: number){
        $(this).closest('table').eq(rowi).find('td').eq(coli).click();
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

    /*
    Sets a value to specific index in cellDataMatrix
     */
    private setValueToMatrix(row: number, col: number, value: string) {
        try{
        this.cellDataMatrix[col][row] = value;}
        catch (e)
        {
            this.updateCellDataMatrix(col, row);
        }
    }

    private updateCellDataMatrix(col: number, row:number){
         this.cellDataMatrix = [];
       // if (this.data.table.rows)
            for(var index = 0; index<row; index++) {
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
        if (this.currentCell)
            if (this.currentCell.row == rowi && this.currentCell.col == coli)
                return;

        let parId = getParId(this.element.parents(".par"));
        if (!this.editing || typeof cell === "string" || !this.viewctrl || !parId || cell.editorOpen) return;


        // cell: CellEntity, docId: number, parId: string, row: number, col: number


        this.getCellData(cell, this.viewctrl.item.id, parId, rowi, coli);
        cell.editing = true;
        if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
            this.currentCell.editing = false;
            this.saveCells(this.cellDataMatrix[this.currentCell.row][this.currentCell.col], this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
        }
        this.currentCell = cell;
        this.currentCell.row = rowi;
        this.currentCell.col = coli;
    }

    /*
    Deals with whatever happens when clicked cell
     */
    /*private async cellClicked(cell: CellEntity, rowi: number, coli: number) {
        if (this.currentCell && this.currentCell.editorOpen) return;

        let parId = getParId(this.element.parents(".par"))
        if (typeof cell === "string" || !this.viewctrl || !parId) return; // todo: fixit

        if (!this.editing) return;



        this.getCellData(cell, this.viewctrl.item.id, parId, rowi, coli );
        cell.editing = true;
        // focus on input ->

        if (this.currentCell != undefined && this.currentCell.row != undefined && this.currentCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
            this.currentCell.editing = false;
             this.saveCells(this.cellDataMatrix[this.currentCell.row][this.currentCell.col], this.viewctrl.item.id, parId, this.currentCell.row, this.currentCell.col);
        }
        this.currentCell = cell;
        this.currentCell.row = rowi;
        this.currentCell.col = coli;

       this.focusOnInput();
    }


    /*
    Focuses on input, does not work
     */
    private focusOnInput(){
      // HOW?
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
        if (this.data.table.datablock) {
            let modal: CellDataEntity = <CellDataEntity>{
                key: value,
            };
            this.data.table.datablock.cells = modal;
        }
    }

    /*
    Create new DataBlock with type, but no cells
     */
    private createDataBlock() {
        let modal: DataEntity = <DataEntity>{};
        modal.type = "Relative";
        this.data.table.datablock = modal;
    }


    private stylingForCell(cell: CellEntity) {

        if (typeof cell === "string") {
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
            this.currentCell.editing = false;
        }
        if (!this.editing) this.editSave();
        this.editing = !this.editing;
    }


    /*
    Saving
     */
    public editSave() {
        this.editing = false;
        if (this.currentCell) this.currentCell.editing = false;
    }


    public isCellbeingEdited(rowi: string, coli: string) {
        return true;
    }


    /*
    Cancel
     */
    public editCancel() {
        // undo all changes
        this.editing = false;
        if (this.currentCell) this.currentCell.editing = false;
    }


    async saveCells(cellContent: string, docId: number, parId: string, row: number, col: number) {
        // tallenna solun sisältö
        /* const response = await $http<{ [cellContent: string]: string; }>({ //Miksi tama toimii vaikka response ei oikeasti ole tuota tyyppia?
             url: "/timTable/saveCell",
             method: "POST",
             params: {cellContent, docId, parId, row, col},
         });*/

        const response = await $http.post<{ [cellContent: string]: string; }>("/timTable/saveCell", {cellContent, docId, parId, row, col});
        const cellHtml = response.data[0];
        this.cellDataMatrix[row][col] = cellHtml;

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

/*
            let i = ctrl.editedCellContent;
            // this.cellDataMatrix[rowi][coli] = this.editedCellContent;
            ctrl.currentCell.editorOpen = true;
            const result = await openEditorSimple(docId, i);
            ctrl.currentCell.editorOpen = false;
            if (result.type == "save") {
                ctrl.saveCells(result.text, docId, parId, row, col);
                ctrl.cellDataMatrix[row][col] = result.text
            }
            if (typeof cell === "string") {
            }
            else cell.editorOpen = false;*/

      /*  let i = ctrl.editedCellContent;
        // this.cellDataMatrix[rowi][coli] = this.editedCellContent;
        if (ctrl.currentCell) ctrl.currentCell.editorOpen = true;
        const result = await openEditorSimple(docId, i);
        if (ctrl.currentCell) ctrl.currentCell.editorOpen = false;
        if (result.type == "save") {
            ctrl.saveCells(result.text, docId, parId, row, col);
            ctrl.cellDataMatrix[row][col] = result.text
        }
        if (typeof cell === "string") {
        }
        else cell.editorOpen = false;
        //TODO: virhekasittely*/
    }

    async openEditor(cell: CellEntity, docId: number, value: string, parId: string, row: number, col: number) {

        let ctrl = this;
        let i = ctrl.editedCellContent;
        // this.cellDataMatrix[rowi][coli] = this.editedCellContent;
        if (ctrl.currentCell) ctrl.currentCell.editorOpen = true;
        const result = await openEditorSimple(docId, i);
        if (ctrl.currentCell) ctrl.currentCell.editorOpen = false;
        if (result.type == "save") {
            ctrl.saveCells(result.text, docId, parId, row, col);
            ctrl.cellDataMatrix[row][col] = result.text
        }
        if (result.type == "cancel") {
            this.currentCell = undefined;
        }


        if (typeof cell === "string") {

        }
        else cell.editorOpen = false;
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
    template: `<div ng-class="{editable: $ctrl.editing}""><table ng-style="$ctrl.stylingForTable($ctrl.data.table)">
    <col ng-repeat="c in $ctrl.data.table.columns" ng-attr-span="{{c.span}}}" ng-style="$ctrl.stylingForColumn(c)"/>
    <tr ng-repeat="r in $ctrl.data.table.rows"  ng-init="rowi = $index" ng-style="$ctrl.stylingForRow(r)">
      <div ng-if="$ctrl.allcellData == undefined">
        <td ng-repeat="td in r.row" ng-init="coli = $index" colspan="{{td.colspan}}" rowspan="{{td.rowspan}}"
            ng-style="$ctrl.stylingForCell(td)" ng-click="$ctrl.cellClicked(td, rowi, coli)">
            <div ng-if="td.cell != null && !td.editing" ng-bind-html="$ctrl.cellDataMatrix[rowi][coli]">
           </div>
            <div ng-if="td.cell == null && !td.editing" ng-bind-html="td.cell">  
            </div> 
           <input ng-if="td.editing" ng-model="$ctrl.cellDataMatrix[rowi][coli]" ng-keydown="$ctrl.keyDownPressed($event, td, rowi, coli)" ng-attr-autofocus="{{td.editing}}" > 
           <button ng-if="td.editing" ng-click="$ctrl.editorOpen(td, rowi, coli)" class="glyphicon glyphicon-pencil"></button>
        </td>
       </tr>
     <div>  
</table>
<button class="timButton" ng-show="$ctrl.editing" ng-click="$ctrl.AddRow()">Add row</button>
<button class="timButton" ng-show="$ctrl.editing" ng-click="$ctrl.AddColumn()">Add column</button>

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


