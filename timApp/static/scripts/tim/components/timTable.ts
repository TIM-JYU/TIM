import {IController, IScope} from "angular";
import $ from "jquery";
import {timApp} from "../app";
import {ViewCtrl} from "../controllers/view/viewctrl";
import {showMessageDialog} from "../dialog";


export const EDITOR_CLASS = "editorArea";
export const EDITOR_CLASS_DOT = "." + EDITOR_CLASS;


export interface TimTable {
    table: ITable;
}

export interface ITable {
    rows?: (RowsEntity)[];
    columns?: (ColumnsEntity)[];
}

export type CellEntity = ICell | string;

export interface RowsEntity {
    backgroundColor?: string;
    row?: ( CellEntity)[];
}

export interface ICell {
    editing?: boolean;
    cell: string;
    type?: string;
    colspan?: number;
    horizontalalign?: string;
    border?: string;
    borderTop?: string;
    borderBottom?: string;
    borderLeft?: string;
    borderRight?: string;
    backgroundColor?: string;
    textalign?: string;
    rowspan?: number;
    verticalAlign?: string;
}

export interface ColumnsEntity {
    style?: string;
    width?: string;
    backgroundColor?: string;
    formula?: string;
    column?: Column;
}

export interface Column {
    width: number;
    name: string;
}


class TimTableController implements IController {
    private srcid: string;  // document id
    private $pars: JQuery;  // paragraph???
    private static $inject = ["$scope"];
    private data: ITable;
    private count: number;
    public viewctrl: ViewCtrl;
    public sc: IScope;
    private editing: boolean;

    constructor(private scope: IScope) {

    }

    $onInit() {
        this.count = 0;
        this.$pars = $(this.srcid);
    }

    private cellClicked(cell: CellEntity) {
        if (typeof cell === "string") return;
        if (this.editing)
            cell.editing = true;
        //
        cell.cell = "uusi";
    }

    private stylingForCell(cell: CellEntity) {

        if (typeof cell == "string") return {};
        let styles: { [index: string]: string } = {};


        if (cell.backgroundColor) styles["background-color"] = cell.backgroundColor;
        return styles;
    }

    async openEditor() {
        await showMessageDialog("Press OK to get a cat");
        return "cat";
    }

    private editor() {


        this.editing = !this.editing;

    }
}


timApp.component("timTable", {
    controller: TimTableController,
    bindings: {
        data: "<",
    },
    template: `<div ng-class="{editable: $ctrl.editing}""><table>
  <button ng-click="$ctrl.editor()">Edit</button>
   <!--<button ng-app="myApp">Edit</button>-->
    <tr ng-repeat="r in $ctrl.data.table.rows"  style="background-color:{{r.backgroundColor}}">
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
</table>

<button ng-show="$ctrl.editing">Save</button>
<button ng-show="$ctrl.editing">Cancel</button>
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


