import {IRootElementService, IScope} from "angular";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {TimTableController} from "./timTable";

export interface ITimTableToolbarCallbacks {
    setCell: (value: object) => void;
    addToTemplates: () => void;
    addColumn: (offset: number) => void;
    addRow: (offset: number) => void;
    removeColumn: () => void;
    removeRow: () => void;
}

export interface ITimTableEditorToolbarParams {
    callbacks: ITimTableToolbarCallbacks;
    activeTable: TimTableController;
}

let instance: TimTableEditorToolbarController | undefined;

export class TimTableEditorToolbarController extends DialogController<{params: ITimTableEditorToolbarParams},
    {}> {
    static component = "timTableEditorToolbar";
    static $inject = ["$element", "$scope"] as const;
    private colorOpts = {
        format: "hex",
        inputClass: "form-control input-xs",
        placeholder: "yellow",
        round: false,
    };

    readonly DEFAULT_CELL_BGCOLOR = "FFFF00"; // "yellow"; // "#EEEEEE";

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
        instance = this;
    }

    $onInit() {
        super.$onInit();
        this.callbacks = this.resolve.params.callbacks;
        this.activeTable = this.resolve.params.activeTable;
        this.hid = this.activeTable.data.hid;
        this.draggable.setCloseFn(undefined); // Hides the close button
        // if (this.activeTable.data.lockCellCount) {
        //     this.lockCellCount = this.activeTable.data.lockCellCount;
        // }
    }

    /**
     * Checks for changes in the cell background color selector.
     */
    $doCheck() {

    }

    public callbacks!: ITimTableToolbarCallbacks; // $onInit
    public activeTable?: TimTableController;
    private visible: boolean = true;
    private hid?: any;
    // private lockCellCount: boolean = false;

    private previousBackgroundColor: string = this.DEFAULT_CELL_BGCOLOR;
    private cellBackgroundColor: string = this.DEFAULT_CELL_BGCOLOR;

    public getTitle() {
        return "Edit table";
    }

    dismiss() {
        this.hide();
    }

    /**
     * Hides the toolbar and removes the instance.
     */
    public hide() {
        this.close("");
        this.visible = false;
        this.scope.$apply();
        instance = undefined;
    }

    public hideIfActiveTable(table: object) {
        if (table == this.activeTable) {
            this.hide();
        }
    }

    /**
     * Shows the toolbar.
     * @param callbacks Callbacks for communicating with the table.
     * @param activeTable The object that requested the toolbar to open.
     */
    public show(callbacks: ITimTableToolbarCallbacks, activeTable: TimTableController) {
        this.visible = true;
        this.activeTable = activeTable;
        this.hid = this.activeTable.data.hid;
        this.callbacks = callbacks;
    }

    /**
     * Sets the color of the toolbar's color picker object.
     * @param color The color.
     */
    public setColorPickerColor(color: string) {
        this.cellBackgroundColor = color;
    }

    /**
     * Sets the text-align value of a cell.
     */
    private setTextAlign(value: string) {
       //  this.callbacks.setTextAlign(value);
        this.callbacks.setCell({textAlign: value});
    }

    /**
     * Sets the cell by template
     */
    private setCell(value: object) {
        this.callbacks.setCell(value);
    }

    /**
     * Clears cell format
     */
    private clearFormat() {
        this.callbacks.setCell({CLEAR: "ALL"});
    }

    private pinSelected() {
        const style: any = {};
        if ( !this.activeTable ) { return style; }
        if ( this.activeTable.shiftDown ) {
            style.background = "black";
        }
        return style;
    }

    private changePin() {
        const t: any = this.activeTable;
        if ( !t ) { return; }
        t.shiftDown = !t.shiftDown;
        t.startCell = t.activeCell;
    }

    /**
     * Add current cell to templates
     */
    private addToTemplates() {
        this.callbacks.addToTemplates();
    }

    /**
     * Gets the cell text for toolbar
     */
    private getCellForToolbar(value: any) {
        let v = value.text;
        if ( !v ) { v = value.cell; }
        if ( !v ) { return "\u2003"; } // &#8195  em space &emsp;
        // v = v.replace('$', '');
        return v.substr(0, 5);
    }

    private eventApi = {
        onClose: (api: any, color: string, $event: any) => {
            TimTableEditorToolbarController.onColorPickerClose(color);
        },
    };

    /**
     * Updates the color of a cell when the color picker is closed.
     * @param color The color.
     */
    private static onColorPickerClose(color: string) {
        if (instance) {
            instance.previousBackgroundColor = instance.cellBackgroundColor;
            instance.callbacks.setCell({backgroundColor: "#" + color});
        }
    }

    private getStyle() {
        return {"background-color": "#" + this.previousBackgroundColor};
    }

    private applyBackgroundColor() {
        this.callbacks.setCell({backgroundColor: "#" + this.previousBackgroundColor});
    }

    private addColumn(offset: number) {
        this.callbacks.addColumn(offset);
    }

    private addRow(offset: number) {
        this.callbacks.addRow(offset);
    }

    private removeColumn() {
        this.callbacks.removeColumn();
    }

    private removeRow() {
        this.callbacks.removeRow();
    }
}

export function isToolbarEnabled() {
    return true;
}

export function openTableEditorToolbar(p: ITimTableEditorToolbarParams) {
    if (instance) {
        instance.show(p.callbacks, p.activeTable);
    } else {
        showDialog(
            TimTableEditorToolbarController,
            {params: () => p},
            {
                forceMaximized: false,
                showMinimizeButton: false,
            });
    }
}

export function hideToolbar(closingTable: object) {
    if (instance) {
        // instance.hideIfActiveTable(closingTable);
        instance.hide();
    }
}

registerDialogComponent(TimTableEditorToolbarController,
    {
        template: `
<tim-dialog class="overflow-visible">
    <dialog-body>
        <div class="row" >
            <div class="col-xs-12" style="top: -0.8em;">
                <div class="btn-group" role="menuitem" uib-dropdown ng-hide="$ctrl.hid.editMenu">
                    <button class="timButton btn-xs" uib-dropdown-toggle>Edit <span class="caret"></span></button>
                    <ul class="dropdown-menu" uib-dropdown-menu>
                        <li role="menuitem" ng-click="$ctrl.removeRow()"><a>Remove row</a></li>
                        <li role="menuitem" ng-click="$ctrl.removeColumn()"><a>Remove column</a></li>
                    </ul>
                </div>
                <div class="btn-group" role="menuitem" uib-dropdown ng-hide="$ctrl.hid.insertMenu">
                    <button class="timButton btn-xs" uib-dropdown-toggle>Insert <span class="caret"></span></button>
                    <ul class="dropdown-menu" uib-dropdown-menu>
                        <li role="menuitem" ng-click="$ctrl.addRow(0)"><a>Row above</a></li>
                        <li role="menuitem" ng-click="$ctrl.addRow(1)"><a>Row below</a></li>
                        <li role="menuitem" ng-click="$ctrl.addColumn(0)"><a>Column to the left</a></li>
                        <li role="menuitem" ng-click="$ctrl.addColumn(1)"><a>Column to the right</a></li>
                    </ul>
                </div>
            </div>
        </div>
        <div class="row">
            <div class="col-xs-12" id="timTableToolbarRow">
                <color-picker ng-model="$ctrl.cellBackgroundColor"
                              ng-hide="$ctrl.hid.colorPicker"
                              event-api="$ctrl.eventApi"
                              options="$ctrl.colorOpts"
                              style="top: -0.01em;position: relative;">
                </color-picker>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hid.colorPicker"
                        ng-click="$ctrl.applyBackgroundColor()">Apply color
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hid.alignLeft"
                        title="Align left"
                        ng-click="$ctrl.setTextAlign('left')">
                    <i class="glyphicon glyphicon-align-left"></i>
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hid.alignCenter"
                        title="Align center"
                        ng-click="$ctrl.setTextAlign('center')">
                    <i class="glyphicon glyphicon-align-center"></i>
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hid.alignRight"
                        title="Align right"
                        ng-click="$ctrl.setTextAlign('right')">
                    <i class="glyphicon glyphicon-align-right"></i>
                </button>
                <button class="timButton btn-xs" ng-repeat="r in $ctrl.activeTable.data.toolbarTemplates" ng-init="rowi = $index"
                     ng-style="r" style="color:black;background-color:white" ng-click="$ctrl.setCell(r)" ng-bind-html="$ctrl.getCellForToolbar(r)">
                     <!--{{$ctrl.getCellForToolbar(r)}}-->
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hid.addToTemplates"
                        title="Add current cell to templates"
                        ng-click="$ctrl.addToTemplates()">
                    <i class="glyphicon glyphicon-star-empty"></i>
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hid.clearFormat"
                        title="Clear format"
                        ng-click="$ctrl.clearFormat()">
                    <i class="glyphicon glyphicon-trash"></i>
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hid.changePin"
                        title="Pin area corner"
                        ng-style="$ctrl.pinSelected()"
                        ng-click="$ctrl.changePin()">
                    <i class="glyphicon glyphicon-pushpin"></i>
                </button>

            </div>
        </div>
    </dialog-body>
</tim-dialog>
`,
    });
