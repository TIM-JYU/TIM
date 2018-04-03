import {IController, IScope} from "angular";
import $ from "jquery";
import {timApp} from "../app";
import {ViewCtrl} from "../controllers/view/viewctrl";
import {showMessageDialog} from "../dialog";


export const EDITOR_CLASS = "editorArea";
export const EDITOR_CLASS_DOT = "." + EDITOR_CLASS;

const styleToHtml: { [index: string]: string } =
    {
        "textAlign": "text-align",
        "backgroundColor": "background-color",
        "horizontalAlign": "horizontal-align",
        "verticalAlign": "vertical-align",
        "border": "border",
        "borderTop": "border-top",
        "borderBottom": "border-bottom",
        "borderLeft": "border-left",
        "borderRight": "border-right",
        "width": "width",
        "height": "height",
        "color": "color",
        "fontSize": "font-size"
    };

export interface TimTable {
    table: ITable;
}

export interface ITable {
    rows?: (RowsEntity)[];
    columns?: (ColumnsEntity)[];
    cellData?: string;
}




export type CellEntity = ICell | string;

export interface RowsEntity {
    row?: (CellEntity)[];
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
}

export interface ICell extends ICellStyles {
    cell: string;
    editing?: boolean;
    type?: string;
    colspan?: number;
    rowspan?: number;
}

export interface ColumnsEntity {
    column?: IColumn[];
}

export interface IColumn extends IColumnStyles {
    id?: string;
    span?: number;
    formula?: string;
}

export interface IColumnStyles {
    width?: string;
    backgroundColor?: string;
}


class TimTableController implements IController {
    private srcid: string;  // document id
    private $pars: JQuery;  // paragraph
    private static $inject = ["$scope"];
    private data: TimTable;
    private allcellData: string[];
    private count: number;
    public viewctrl: ViewCtrl;
    public sc: IScope;
    private editing: boolean;
    private helpCell: CellEntity;

    constructor(private scope: IScope) {

    }

    $onInit() {
        this.count = 0;
        this.$pars = $(this.srcid);

        // read cellData (string)
        var cellp = this.data.table.cellData;
        console.log(cellp);
        // this.data.cellData;
        var cellITems = "";

        // parse cellldata at | mark and put single items in to allcellData table

        this.allcellData = cellITems.split("|");

        // now allcellData should have:
        // 0.     (empty)
        // 1.  Kissa  (items as they are ahown to user)




        // now that they are in a table, they are "easily" read in template


        /** var jj = this.data.
        if(this.data.cellData != undefined)*/




        // if (this.data.cellData != undefined ) showMessageDialog(this.data.cellData);
       // var allCellData = $(this.data.rows);

        //showMessageDialog(allCellData);


    }

    private cellClicked(cell: CellEntity) {

        if (typeof cell === "string") return;
        if (this.editing)  // editing is on
        {
            if(this.helpCell != undefined && typeof(this.helpCell) != "string")
            {
                this.helpCell.editing = false;

            }
            cell.editing = true;
            this.helpCell = cell;
            //cell.cell = "uusi";
            //cell.cell = cell.cell;
        }

    }

    private stylingForCell(cell: CellEntity) {

        if (typeof cell === "string") { return {}; }
        const styles: { [index: string]: string } = {};

        for (const key of Object.keys(cell)) {
            const property: string = styleToHtml[key];
            if (property === undefined) { continue; }
            const c = cell[key as keyof ICellStyles];
            if (!c) { continue; }
            styles[property] = c;
        }
        return styles;
    }

    private stylingForColumn(col: IColumn) {
        const styles: { [index: string]: string } = {};

        for (const key of Object.keys(col)) {
            const property: string = styleToHtml[key];
            if (property === undefined) { continue; }
            const c = col[key as keyof IColumnStyles];
            if (!c) { continue; }
            styles[property] = c;
        }
        return styles;
    }

    async openEditor() {
        await showMessageDialog("Press OK to get a cat");
        return "cat";

    }



    private editor() {

         if(this.helpCell != undefined && typeof(this.helpCell) != "string")
            {
                this.helpCell.editing = false;
            }
        if (!this.editing) this.editSave();
        this.editing = !this.editing;



    }

    public editSave(){

        this.editing = false;

      // mofdify data storage
      // send data to server



    }

    public editCancel(){
        // undo all changes
        this.editing = false;
    }
}

// - yaml-taulukko oliotaulukoksi (kaikki solut olioita) - ei tarvitse, jos solun editointitilaa ei tallenneta soluun itseensä (voisi olla indeksien perusteella)
// - kun editoidaan solua, katsotaan onko cellDatassa vastaavaa ja jos ei, kopioidaan solun sisältö sinne
// - taulukon templatessa on solun kohdalla kutsu tyyliin $ctrl.getCellContent(td)
// - tallennettaessa lähetetään cellData palvelimelle johonkin reittiin

timApp.component("timTable", {
    controller: TimTableController,
    bindings: {
        data: "<",
    },
    template: `<div ng-class="{editable: $ctrl.editing}""><table>
  <button ng-click="$ctrl.editor()">Edit</button>
   <!--<button ng-app="myApp">Edit</button>-->
    <col ng-repeat="c in $ctrl.finalTable.columns" ng-attr-span="{{c.span}}}" ng-style="$ctrl.stylingForColumn(c)"/>
    <tr ng-repeat="r in $ctrl.data.table.rows"  style="background-color:{{r.backgroundColor}}">
       <!-- if data exists -->
     <!--   <div ng-if="$ctrl.allcellData != undefined">
         <td ng-repeat="item in $ctrl.allcellData" colspan="{{td.colspan}}" rowspan="{{td.rowspan}}"
            ng-style="$ctrl.stylingForCell(td)" ng-click="$ctrl.cellClicked(td)">
           {{item}}
            <input ng-if="$ctrl.isCellBeingEdited($)" ng-model="td.cell">
        </td>
        </div>  -->
        <!-- if data exists ends  -->
        
       <!-- <div ng-if="$ctrl.allcellData != undefined">
         <td ng-repeat="item in $ctrl.allcellData" colspan="{{td.colspan}}" rowspan="{{td.rowspan}}"
            ng-style="$ctrl.stylingForCell(td)" ng-click="$ctrl.cellClicked(td)">
           {{item}}
            <input ng-if="$ctrl.isCellBeingEdited(rowi, coli)" ng-model="$ctrl.cellDataMatrix[rowi][coli]">
        </td>
        </div>  -->
        
        
        
        <!-- if data does not exists -->
        <div ng-if="$ctrl.allcellData == undefined">
        <td ng-repeat="td in r.row" colspan="{{td.colspan}}" rowspan="{{td.rowspan}}"
            ng-style="$ctrl.stylingForCell(td)" ng-click="$ctrl.cellClicked(td)">
            <div ng-if="td.cell != null && !td.editing" ng-bind-html="td.cell">
            <!--{{td.cell}}-->
            </div>
            <div ng-if="td.cell == null && !td.editing" ng-bind-html="td">
               
            </div> 
            <input ng-if="td.editing" ng-model="td.cell">
        </td>
       </tr>
     <div>
     
     <!-- if data does not exists ENDS -->
    
</table>
<!--<p ng-repeat="item in $ctrl.allcellData">{{item}}</p>
<p ng-bind-html="$ctrl.data.table.cellData"></p>-->

<button ng-show="$ctrl.editing" ng-click="$ctrl.editSave()">Save</button>
<button ng-show="$ctrl.editing" ng-click="$ctrl.editCancel()">Cancel</button>
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


