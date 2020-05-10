import {IScope} from "angular";
import {DialogController} from "tim/ui/dialogController";
import {registerDialogComponent, showDialog} from "../ui/dialog";
import {HideValues, IToolbarTemplate, TimTableComponent} from "./timTable";

export interface ITimTableToolbarCallbacks {
    setCell: (value: IToolbarTemplate) => void;
    addToTemplates: () => void;
    addColumn: (offset: number) => void;
    addRow: (offset: number) => void;
    removeColumn: () => void;
    removeRow: () => void;
    closeEditor: () => void;
    isEdit: () => boolean;
}

export interface ITimTableEditorToolbarParams {
    callbacks: ITimTableToolbarCallbacks;
    activeTable: TimTableComponent;
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

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);
        instance = this;
    }

    $onInit() {
        super.$onInit();
        this.callbacks = this.resolve.params.callbacks;
        this.activeTable = this.resolve.params.activeTable;
        this.hide = this.activeTable.data.hide;
        // this.draggable.setCloseFn(undefined); // Hides the close button
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
    public activeTable?: TimTableComponent;
    private visible: boolean = true;
    private hide?: HideValues;
    // private lockCellCount: boolean = false;

    private previousBackgroundColor: string = this.DEFAULT_CELL_BGCOLOR;
    private cellBackgroundColor: string = this.DEFAULT_CELL_BGCOLOR;

    public getTitle() {
        return "Edit table";
    }

    dismiss() {
        this.hideThis();
    }

    /**
     * Hides the toolbar and removes the instance.
     */
    public hideThis() {
        this.close("");
        this.visible = false;
        // this.scope.$apply();
        instance = undefined;
        this.callbacks.closeEditor();
    }

    // noinspection JSUnusedGlobalSymbols
    public hideIfActiveTable(table: object) {
        if (table == this.activeTable) {
            this.hideThis();
        }
    }

    /**
     * Shows the toolbar.
     * @param callbacks Callbacks for communicating with the table.
     * @param activeTable The object that requested the toolbar to open.
     */
    public show(callbacks: ITimTableToolbarCallbacks, activeTable: TimTableComponent) {
        this.visible = true;
        this.activeTable = activeTable;
        this.hide = this.activeTable.data.hide;
        this.callbacks = callbacks;
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Sets the color of the toolbar's color picker object.
     * @param color The color.
     */
    public setColorPickerColor(color: string) {
        this.cellBackgroundColor = color;
    }

    // noinspection JSUnusedLocalSymbols
    /**
     * Sets the text-align value of a cell.
     */
    private setTextAlign(value: string) {
       //  this.callbacks.setTextAlign(value);
        this.callbacks.setCell({ style: {textAlign: value}});
    }

    // noinspection JSUnusedLocalSymbols
    /**
     * Sets the cell by template
     */
    public setCell(value: IToolbarTemplate) { // } Record<string, string>) {
        this.callbacks.setCell(value);
    }

    // noinspection JSUnusedLocalSymbols
    /**
     * Clears cell format
     */
    private clearFormat() {
        this.callbacks.setCell({ style: {CLEAR: "ALL"}});
    }

    // noinspection JSUnusedLocalSymbols
    private pinSelected() {
        const style: Record<string, string> = {};
        if (!this.activeTable) { return style; }
        if (this.activeTable.shiftDown) {
            style.background = "black";
        }
        return style;
    }

    // noinspection JSUnusedLocalSymbols
    private changePin() {
        const t = this.activeTable;
        if (!t) { return; }
        t.shiftDown = !t.shiftDown;
        t.startCell = t.activeCell;
    }

    // noinspection JSUnusedLocalSymbols
    /**
     * Add current cell to templates
     */
    private addToTemplates() {
        this.callbacks.addToTemplates();
    }

    // noinspection JSUnusedLocalSymbols,JSMethodCanBeStatic
    /**
     * Gets the cell text for toolbar
     */
    private getCellForToolbar(value: IToolbarTemplate) {
        let v = value.text;
        if (v === "notext") { return undefined; }
        if (!v) { v = value.cell; }
        if (!v) { v = value.toggleCell; }
        if (!v) { return "\u2003"; } // &#8195  em space &emsp;
        v = "" + v;
        // v = v.replace('$', '');
        return v.substr(0, Math.min(v.length, 5));
    }

    // noinspection JSUnusedLocalSymbols,JSMethodCanBeStatic
    /**
     * Gets the cell text for toolbar
     */
    private getTitleForToolbar(value: IToolbarTemplate) {
        const v = value.title;
        if (!v) { return null; } // &#8195  em space &emsp;
        return v;
    }

    // noinspection JSUnusedLocalSymbols,JSMethodCanBeStatic
    /**
     * Gets the cell class for toolbar
     */
    private getCellClassForToolbar(value: IToolbarTemplate): string {
        let cls = "toolbarButton";
        if (value.buttonClass) {
            cls = " " + value.buttonClass;
        } else if (value.style && value.style.class) {
            cls += " " + value.style.class;
        } else if (value.toggleStyle && value.toggleStyle.class) {
            cls += " " + value.toggleStyle.class;
        }
        return cls;
    }

    // noinspection JSUnusedLocalSymbols,JSMethodCanBeStatic
    /**
     * Gets the cell inner class for toolbar
     */
    private getIClassForToolbar(value: IToolbarTemplate): string | undefined {
        if (value.iClass) {
            if (value.iClass.includes("glyphicon")) {
                return "glyphicon " + value.iClass;
            }
            return value.iClass;
        }
        return undefined;
    }

    // noinspection JSUnusedLocalSymbols,JSMethodCanBeStatic
    /**
     * Gets the cell style for toolbar
     */
    private getCellStyleForToolbar(value: IToolbarTemplate) {
        let ret = value.style;
        if (!ret) { ret = value.toggleStyle; }
        if (value.buttonStyle) {
            // @ts-ignore
            ret = value.buttonStyle;
        }
        if (!ret) { return ret; }
        let sret = JSON.stringify(ret);
        sret = sret.replace(/"/g, "").replace("{", "").replace("}", "").replace(/:/g, ": ");
        return sret;
    }

    // noinspection JSUnusedLocalSymbols
    private eventApi = {
        onClose: (api: unknown, color: string, $event: unknown) => {
            TimTableEditorToolbarController.onColorPickerClose(color);
        },
    };

    // noinspection JSUnusedLocalSymbols
    /**
     * Updates the color of a cell when the color picker is closed.
     * @param color The color.
     */
    private static onColorPickerClose(color: string) {
        if (instance) {
            instance.previousBackgroundColor = instance.cellBackgroundColor;
            instance.callbacks.setCell({ style: {backgroundColor: "#" + color}});
        }
    }

    // noinspection JSUnusedLocalSymbols
    private getStyle() {
        return {"background-color": "#" + this.previousBackgroundColor};
    }

    // noinspection JSUnusedLocalSymbols
    private applyBackgroundColor() {
        this.callbacks.setCell({ style: {backgroundColor: "#" + this.previousBackgroundColor}});
    }

    // noinspection JSUnusedLocalSymbols
    private addColumn(offset: number) {
        this.callbacks.addColumn(offset);
    }

    // noinspection JSUnusedLocalSymbols
    private addRow(offset: number) {
        this.callbacks.addRow(offset);
    }

    // noinspection JSUnusedLocalSymbols
    private removeColumn() {
        this.callbacks.removeColumn();
    }

    // noinspection JSUnusedLocalSymbols
    private removeRow() {
        this.callbacks.removeRow();
    }

    private borderRect() {
        const brect = {
            // eslint-disable-next-line quote-props
            rect: {
                b: {style: {class: "bb"}},
                c1b: {style: {class: "bb bl br"}},
                c1c: {style: {class: "bl br"}},
                c1t: {style: {class: "bt bl br"}},
                l: {style: {class: "bl"}},
                lb: {style: {class: "bb bl"}},
                lt: {style: {class: "bt bl"}},
                one: {style: {class: "bt bl bb br"}},
                r: {style: {class: "br"}},
                r1c: {style: {class: "bt bb"}},
                r1l: {style: {class: "bt bl bb"}},
                r1r: {style: {class: "bt br bb"}},
                rb: {style: {class: "bb br"}},
                rt: {style: {class: "bt br"}},
                t: {style: {class: "bt"}},
            }};
        this.setCell(brect);
    }

    private removeBorders() {
        const removeBrd = {
          removeStyle: {
              class: ["bt", "br", "bl", "bb"],
          },
        };
        this.setCell(removeBrd);
    }

    public applyTemplate(templ: IToolbarTemplate) {
        if (templ.notInEdit && this.callbacks.isEdit()) {
            return;
        }
        if (templ.commands) {
            for (const cmd of templ.commands) {
                const c = cmd.split(" ");
                const cmds = c[0];
                const param = c[1] ?? "";
                if (cmds === "clearFormat") { this.clearFormat(); }
                if (cmds === "applyBackgroundColor") { this.applyBackgroundColor(); }
                if (cmds === "setTextAlign") { this.setTextAlign(param); }
                if (cmds === "addToTemplates") { this.addToTemplates(); }
                if (cmds === "borderRect") { this.borderRect(); }
                if (cmds === "removeBorders") { this.removeBorders(); }
            }
        }
        this.setCell(templ);
    }
}

export function isToolbarEnabled() {
    return true;
}

export function handleToolbarKey(ev: KeyboardEvent, toolBarTemplates: IToolbarTemplate[] | undefined) {
    if (!instance || !toolBarTemplates) { return true; }
    for (const templ of toolBarTemplates) {
        if (!templ || !templ.shortcut) { continue; }
        const k = templ.shortcut.split("+");
        let key = k[0];
        let mod = "";
        if (k.length >= 2) {
            mod = k[0];
            key = k[1];
        }
        if (templ.chars) { // regexp for keys to use
            if (ev.key.match("^" + templ.chars + "$")) {
                templ.cell = ev.key;
                instance.applyTemplate(templ);
                return true;
            }
            continue;
        }
        const evmod = (ev.altKey ? "a" : "") + (ev.ctrlKey ? "c" : "") + (ev.shiftKey ? "s" : "");
        if (mod != evmod) { continue; }
        if (ev.key === key) {
            instance.applyTemplate(templ);
            return true;
        }
    }
    return false;
}

export function isToolbarOpen() {
    return !!instance;
}

export function openTableEditorToolbar(p: ITimTableEditorToolbarParams) {
    if (instance && !instance.closed) {
        instance.show(p.callbacks, p.activeTable);
    } else {
        showDialog(
            TimTableEditorToolbarController,
            {params: () => p},
            {
                forceMaximized: false,
                showMinimizeButton: false,
                // absolute: true,
            });
    }
}

// noinspection JSUnusedLocalSymbols
export function hideToolbar(closingTable: object) {
    if (instance) {
        // instance.hideIfActiveTable(closingTable);
        instance.hideThis();
    }
}

registerDialogComponent(TimTableEditorToolbarController,
    {
        template: `
<tim-dialog class="overflow-visible">
    <dialog-body>
        <div class="row" >
            <div class="col-xs-12" style="top: -0.8em;">
                <div class="btn-group" role="menuitem" uib-dropdown ng-hide="$ctrl.hide.editMenu">
                    <button class="timButton btn-xs" uib-dropdown-toggle>Edit <span class="caret"></span></button>
                    <ul class="dropdown-menu" uib-dropdown-menu>
                        <li role="menuitem" ng-click="$ctrl.removeRow()"><a>Remove row</a></li>
                        <li role="menuitem" ng-click="$ctrl.removeColumn()"><a>Remove column</a></li>
                    </ul>
                </div>
                <div class="btn-group" role="menuitem" uib-dropdown ng-hide="$ctrl.hide.insertMenu">
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
        <div class="row timTableToolbarRow">
            <div class="col-xs-12" id="timTableToolbarRow">
                <color-picker ng-model="$ctrl.cellBackgroundColor"
                              ng-hide="$ctrl.hide.colorPicker"
                              event-api="$ctrl.eventApi"
                              options="$ctrl.colorOpts"
                              style="top: -0.01em;position: relative;">
                </color-picker>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hide.colorPicker"
                        ng-click="$ctrl.applyBackgroundColor()">Apply color
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hide.alignLeft"
                        title="Align left"
                        ng-click="$ctrl.setTextAlign('left')">
                    <i class="glyphicon glyphicon-align-left"></i>
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hide.alignCenter"
                        title="Align center"
                        ng-click="$ctrl.setTextAlign('center')">
                    <i class="glyphicon glyphicon-align-center"></i>
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hide.alignRight"
                        title="Align right"
                        ng-click="$ctrl.setTextAlign('right')">
                    <i class="glyphicon glyphicon-align-right"></i>
                </button>
                <button class="btn-xs" ng-repeat="r in $ctrl.activeTable.data.toolbarTemplates" ng-init="rowi = $index"
                     ng-hide="r.hide"
                     ng-class="$ctrl.getCellClassForToolbar(r)"
                     ng-style="$ctrl.getCellStyleForToolbar(r)" ng-click="$ctrl.applyTemplate(r)" 
                     title="{{$ctrl.getTitleForToolbar(r)}}">{{$ctrl.getCellForToolbar(r)}}
                     <i ng-class="$ctrl.getIClassForToolbar(r)"></i>
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hide.addToTemplates"
                        title="Add current cell(s) to templates"
                        ng-click="$ctrl.addToTemplates()">
                    <i class="glyphicon glyphicon-star-empty"></i>
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hide.clearFormat"
                        title="Clear format"
                        ng-click="$ctrl.clearFormat()">
                    <i class="glyphicon glyphicon-trash"></i>
                </button>
                <button class="timButton btn-xs"
                        ng-hide="$ctrl.hide.changePin"
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
