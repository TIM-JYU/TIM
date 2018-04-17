import {IController, IRootElementService, IScope} from "angular";
import $ from "jquery";
import {timApp} from "../app";
import {ViewCtrl} from "../controllers/view/viewctrl";
import {showMessageDialog} from "../dialog";
import {setSetting} from "../utils";
import {$http} from "../ngimport";
import {getParId} from 'tim/controllers/view/parhelpers';

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
    type?: string;
    colspan?: number;
    rowspan?: number;
    id?: string;
    formula?: string;
    row? : number;
    col?: number;
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


class TimTableController implements IController {
    private srcid: string;  // document id
    private $pars: JQuery;  // paragraph
    private static $inject = ["$scope", "$element"];
    private data: TimTable;
    private allcellData: string[];
    private count: number;
    public viewctrl?: ViewCtrl;
    public sc: IScope;
    private editing: boolean;
    private helpCell: ICell;
    public cellDataMatrix: string[][];
    public dataCells: DataEntity;
    public DataHash: IHash;
    private DataHashLength: number;
    private editedCellContent: string;


    constructor(private scope: IScope, private element: IRootElementService) {

    }


    $onInit() {
        this.count = 0;
        this.$pars = $(this.srcid);

        this.InitializeCellDataMatrix();
        this.readDataBlockAndSetValuesToDataCellMatrix();
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
        /*  this.cellDataMatrix = [];
         if (this.data.table.rows) {
             for (let i in this.data.table.rows) {
                 this.cellDataMatrix[i] = []
                 if (this.data.table.rows[i].row)
                     for (let j in this.data.table.rows[i].row!) {
                         var ii = this.data.table.rows[i].row![j] as ICell;
                         this.cellDataMatrix[i][j] = ii.cell;
                     }
             }
         }*/
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


    /*
    Define HashTable that contains final table
    */
    /*private DefineDataHash() {
        this.DataHash = {};
        this.DataHash["A"] = 0; //set
        this.DataHash["B"] = 1; //set
        this.DataHash["C"] = 2; //set
        this.DataHash["D"] = 3; //set
        this.DataHash["E"] = 4; //set
        this.DataHash["F"] = 5; //set
        this.DataHash["G"] = 6; //set
        this.DataHash["H"] = 7; //set
        this.DataHash["I"] = 8; //set
        this.DataHash["J"] = 9; //set
        this.DataHash["K"] = 10; //set
        this.DataHash["L"] = 11; //set
        this.DataHash["M"] = 12; //set
        this.DataHash["N"] = 13; //set
        this.DataHash["O"] = 14; //set
        this.DataHash["P"] = 15; //set
        this.DataHash["Q"] = 16; //set
        this.DataHash["R"] = 17; //set
        this.DataHash["S"] = 18; //set
        this.DataHash["T"] = 19; //set
        this.DataHash["U"] = 20; //set
        this.DataHash["V"] = 21; //set
        this.DataHash["W"] = 22; //set
        this.DataHash["X"] = 23; //set
        this.DataHash["Y"] = 24; //set
        this.DataHash["Z"] = 25; //set

        this.DataHashLength = 25;

        //let value = charPlacement["somestring"]; //get
        //todo: calculate after z
    }*/


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
        this.cellDataMatrix[col][row] = value;
    }


    /*
    Deals with whatever happens when clicked cell
     */
    private cellClicked(cell: CellEntity, rowi: number, coli: number) {

        let parId = getParId(this.element.parents(".par"))
        if (typeof cell === "string" || !this.viewctrl || !parId) return; // todo: fixit

        if (!this.editing) return;

        if (this.helpCell != undefined && typeof(this.helpCell) != "string") {
            this.helpCell.editing = false;
        }

        cell.editing = true;

        if ((!this.data.table.datablock)) {
            this.createDataBlock();  // now it has datablock, but it is not shown in yaml and it has no cell content

            this.getCellData(this.viewctrl.item.id, parId, rowi, coli); //docId ja parId Matin omista testidokuista
            if (this.editedCellContent)
                cell.cell = this.editedCellContent; //TODO: miten saadaan tietää nuo id:t oikeasti?

            let value = cell.cell;
            let placement = this.calculatePlace(rowi, coli);
            this.addtoDataBlock(placement, value);
        }
        else {

            this.getCellData(this.viewctrl.item.id, parId, rowi, coli);
            if (this.editedCellContent){
                cell.cell = this.editedCellContent;
                this.modifyDataBlock(coli, rowi, this.editedCellContent);
            } // modify datablock -> change cellValue if existed
        }

        //cell.editing = true;
        if (this.helpCell != undefined && this.helpCell.row != undefined && this.helpCell.col != undefined) { // if != undefined is missing, then returns some number if true, if the number is 0 then statement is false
            this.saveCells(this.helpCell.cell, this.viewctrl.item.id, parId, this.helpCell.row, this.helpCell.col);
        }
        this.helpCell = cell;
        this.helpCell.row = rowi;
        this.helpCell.col = coli;
    }





    /*
    Modifying datablock: changes the value of cell or creates new one
     */
    private modifyDataBlock(coli: number, rowi: number, value: string) {
        let flag = false;  // helper flag
         const alphaRegExp = new RegExp('([A-Z]*)');
/*
        if (this.data.table.datablock)  // datablock has to exist
            for (let item in this.data.table.datablock.cells) {  // loop through datablock cells

                let alpha = alphaRegExp.exec(item);
                let value = this.data.table.datablock.cells[item];

                if (alpha == null) continue;
                let numberPlace = item.substring(alpha[0].length);

                let address = this.getAddress(item);*/
        this.setValueToMatrix(coli, rowi, value);   // sets value to matrix
        if (this.data.table.datablock)
            for (let item in this.data.table.datablock.cells) { // go trough all cells

                // compare databloc values to coli + rowi

                let alpha = alphaRegExp.exec(item);
                let value = this.data.table.datablock.cells[item];

                if (alpha == null) continue;
                let numberPlace = item.substring(alpha[0].length);
                 let address = this.getAddress(item);

                 if (address.col == coli && address.row == rowi) {
                     this.data.table.datablock!.cells[item] = value;
                 }



                 // datablocks value = itemvalue
                // if (address.col == coli && address.row == rowi) {
                // we have a match, lets change the value
                // this.data.table.datablock.cells[item] = value;  // now datablock has a right value
                //  this.setValueToMatrix(address.col, address.row, value.toString());  // now datacellMatrix has the right value
                flag = true;
            }

        if (!flag) {  // no cells were a match
            //this.calculatePlace()
            this.addtoDataBlock("A" + rowi, value);  // add new cell
        }
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
            // modal.key = "A1";
            // modal[1] = "Kissa";
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
    private stylingForTable() {
        let tab = this.data.table;
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

    /*

     */
    async openEditor() {
        await showMessageDialog("Press OK to get a cat");
        return "cat";

    }


    /*
    
     */
    private editor() {
        if (this.helpCell != undefined && typeof(this.helpCell) != "string") {
            this.helpCell.editing = false;
        }
        if (!this.editing) this.editSave();
        this.editing = !this.editing;
    }


    /*
    Saving
     */
    public editSave() {
        this.editing = false;
        if (this.helpCell) this.helpCell.editing = false;
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
        if (this.helpCell) this.helpCell.editing = false;
    }


    async saveCells(cellContent: string, docId: number, parId: string, row: number, col: number) {
            // tallenna solun sisältö
       /* const response = await $http<{ [cellContent: string]: string; }>({ //Miksi tama toimii vaikka response ei oikeasti ole tuota tyyppia?
            url: "/timTable/saveCell",
            method: "POST",
            params: {cellContent, docId, parId, row, col},
        });*/

       const response = await $http.post<ICell>("/timTable/saveCell", {cellContent, docId, parId, row, col});
    }

    /**
     * Gets the contents of a cell from the server
     * @param {number} docId
     * @param {string} parId
     * @param {number} row
     * @param {number} col
     * @returns {Promise<void>}
     */
    async getCellData(docId: number, parId: string, row: number, col: number) {
        const response = await $http<{ [cellContent: string]: string; }>({ //Miksi tama toimii vaikka response ei oikeasti ole tuota tyyppia?
            url: "/timTable/getCellData",
            method: "GET",
            params: {docId, parId, row, col},
        }); //TODO: virhekasittely
        const data = response.data;
        this.editedCellContent = data[0]
        //showMessageDialog(this.editedCellContent);
        this.cellDataMatrix[row][col] = data[0];
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
    template: `<div ng-class="{editable: $ctrl.editing}""><table>
  <button class="timButton" ng-click="$ctrl.editor()">Edit</button>
   <!--<button ng-app="myApp">Edit</button>-->
    <col ng-repeat="c in $ctrl.data.table.columns" ng-attr-span="{{c.span}}}" ng-style="$ctrl.stylingForColumn(c)"/>
    <tr ng-repeat="r in $ctrl.data.table.rows"  ng-init="rowi = $index" ng-style="$ctrl.stylingForRow(r)">
       <!-- if data does not exists -->
      <div ng-if="$ctrl.allcellData == undefined">
        <td ng-repeat="td in r.row" ng-init="coli = $index" colspan="{{td.colspan}}" rowspan="{{td.rowspan}}"
            ng-style="$ctrl.stylingForCell(td)" ng-click="$ctrl.cellClicked(td, rowi, coli)">
            <div ng-if="td.cell != null && !td.editing" ng-bind-html="$ctrl.cellDataMatrix[rowi][coli]">
            <!--{{td.cell}}-->
           </div>
            <div ng-if="td.cell == null && !td.editing" ng-bind-html="td.cell">
               
            </div> 
            <input ng-if="td.editing" ng-model="$ctrl.cellDataMatrix[rowi][coli]">
           <!-- <input ng-if="td.editing" ng-model="ctrl.cellDataMatrix[rowi][coli]">-->
            <!-- <input ng-if="td.editing" ng-model="td.cell">-->
           <!-- <input ng-if="ctrl.isCellBeingEdited(rowi, coli)" ng-model="ctrl.cellDataMatrix[rowi][coli]"> -->
        </td>
       </tr>
     <div> 
    
    
    
    
    

       <!-- if data exists -->
     <!--   <div ng-if="$ctrl.allcellData != undefined">
         <td ng-repeat="item in $ctrl.allcellData" colspan="{{td.colspan}}" rowspan="{{td.rowspan}}"
            ng-style="$ctrl.stylingForCell(td)" ng-click="$ctrl.cellClicked(td)">
           {{item}}
            <input ng-if="$ctrl.isCellBeingEdited($)" ng-model="td.cell">
        </td>
        </div>  -->
        <!-- if data exists ends  -->
        
     
     
     
     <!--  <div ng-if="$ctrl.allcellData != undefined">
         <td ng-repeat="item in $ctrl.allcellData" colspan="{{td.colspan}}" rowspan="{{td.rowspan}}"
            ng-style="$ctrl.stylingForCell(td)" ng-click="$ctrl.cellClicked(td)">
           {{item}}
            <input ng-if="$ctrl.isCellBeingEdited(rowi, coli)" ng-model="$ctrl.cellDataMatrix[rowi][coli]">
        </td>
        </div>  -->
    
</table>
<!--<p ng-repeat="item in $ctrl.allcellData">{{item}}</p>
<p ng-bind-html="$ctrl.data.table.cellData"></p>-->

<button class="timButton" ng-show="$ctrl.editing" ng-click="$ctrl.editSave()">Save</button>
<button class="timButton" ng-show="$ctrl.editing" ng-click="$ctrl.editCancel()">Cancel</button>
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


