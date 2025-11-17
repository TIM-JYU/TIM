import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import type {
    HideValues,
    IToolbarTemplate,
    TimTableComponent,
} from "tim/plugin/timTable/tim-table.component";
import {setToolbarInstance} from "tim/plugin/timTable/toolbarUtils";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {CommonModule} from "@angular/common";

export interface ITimTableToolbarCallbacks {
    setCell: (value: IToolbarTemplate) => void;
    getCell: () => string;
    addToTemplates: () => void;
    addColumn: (offset: number) => void;
    addRow: (offset: number) => void;
    removeColumn: () => void;
    removeRow: () => void;
    closeEditor: (save: boolean) => void;
    isEdit: () => boolean;
    getColumn: () => number;
    getColumnName: () => string;
    setToColumnByIndex: (col: number) => number;
    setToColumnByName: (name: string) => number;
    fillTableCSV: (s: string) => void;
    filterCallback: (cmd: string, value?: string) => void;
    editCallback: (cmd: string) => void;
}

export interface ITimTableEditorToolbarParams {
    callbacks: ITimTableToolbarCallbacks;
    activeTable: TimTableComponent;
}

const DEFAULT_CELL_BGCOLOR = "#FFFF00";

@Component({
    selector: "tim-table-editor-toolbar-dialog",
    template: `
        <tim-dialog-frame class="overflow-visible">
            <ng-container header>
                {{getTitle()}}
            </ng-container>
            <ng-container body>
                <div class="row">
                    <div class="col-xs-12" style="top: -0.8em;">
                        <div class="btn-group" role="menuitem" dropdown *ngIf="canRemoveCells">
                            <button class="timButton btn-xs dropdown-toggle" dropdownToggle>Edit <span class="caret"></span></button>
                            <ul class="dropdown-menu" *dropdownMenu>
                                <li role="menuitem" (click)="edit('selectAll')"><a>Select all</a></li>
                                <li role="menuitem" (click)="edit('copy')"><a>Copy selection</a></li>
                                <li role="menuitem" (click)="edit('paste')"><a>Paste</a></li>
                                <li role="menuitem" (click)="edit('undo')"><a>Undo</a></li>
                                <li role="menuitem" (click)="removeRow()"><a>Remove row</a></li>
                                <li role="menuitem" (click)="removeColumn()"><a>Remove column</a></li>
                            </ul>
                        </div>
                        &ngsp;
                        <div class="btn-group" role="menuitem" dropdown *ngIf="canInsertCells">
                            <button class="timButton btn-xs dropdown-toggle" dropdownToggle>Insert <span class="caret"></span></button>
                            <ul class="dropdown-menu" *dropdownMenu>
                                <li role="menuitem" (click)="addRow(0)"><a>Row above</a></li>
                                <li role="menuitem" (click)="addRow(1)"><a>Row below</a></li>
                                <li role="menuitem" (click)="addColumn(0)"><a>Column to the left</a></li>
                                <li role="menuitem" (click)="addColumn(1)"><a>Column to the right</a></li>
                            </ul>
                        </div>
                        &ngsp;
                        <div class="btn-group" role="menuitem" dropdown *ngIf="canFilters">
                            <button class="timButton btn-xs dropdown-toggle" dropdownToggle>Filter <span class="caret"></span></button>
                            <ul class="dropdown-menu" *dropdownMenu>
                                <li role="menuitem" (click)="filter('copy')"><a>Copy filters</a></li>
                                <li role="menuitem" (click)="filter('paste')"><a>Paste filters</a></li>
                            </ul>
                        </div>
                    </div>
                </div>
                <div class="row timTableToolbarRow">
                    <div class="col-xs-12 form-inline">
                        <input [(ngModel)]="cellBackgroundColor"
                               (change)="onColorChange(asInput($event.target).value)"
                               type="color"
                               *ngIf="!hide?.colorPicker"
                               class="form-control input-xs"
                               style="width: 70px;">
                        &ngsp;
                        <button class="timButton btn-xs"
                                *ngIf="!hide?.colorPicker"
                                (click)="applyBackgroundColor()">Apply color
                        </button>
                        &ngsp;
                        <button class="timButton btn-xs"
                                *ngIf="!hide?.alignLeft"
                                title="Align left"
                                (click)="setTextAlign('left')">
                            <i class="glyphicon glyphicon-align-left"></i>
                        </button>
                        &ngsp;
                        <button class="timButton btn-xs"
                                *ngIf="!hide?.alignCenter"
                                title="Align center"
                                (click)="setTextAlign('center')">
                            <i class="glyphicon glyphicon-align-center"></i>
                        </button>
                        &ngsp;
                        <button class="timButton btn-xs"
                                *ngIf="!hide?.alignRight"
                                title="Align right"
                                (click)="setTextAlign('right')">
                            <i class="glyphicon glyphicon-align-right"></i>
                        </button>
                        &ngsp;
                        <button class="timButton btn-xs"
                                *ngIf="!hide?.changeTable"
                                title="Change cell to table area"
                                (click)="changeCellToTableArea()">
                            <i class="glyphicon glyphicon-th"></i>
                        </button>
                        &ngsp;
                        <ng-container *ngFor="let r of activeTable?.data?.toolbarTemplates">
                            <button class="btn-xs"
                                    *ngIf="!r.hide"
                                    [ngClass]="getCellClassForToolbar(r)"
                                    [ngStyle]="getCellStyleForToolbar(r)"
                                    (click)="applyTemplate(r)"
                                    [title]="getTitleForToolbar(r)">
                                {{getCellForToolbar(r)}}
                                <i [ngClass]="getIClassForToolbar(r)"></i>
                            </button>
                            &ngsp;
                        </ng-container>
                        <button class="timButton btn-xs"
                                *ngIf="!hide?.addToTemplates"
                                title="Add current cell(s) to templates"
                                (click)="addToTemplates()">
                            <i class="glyphicon glyphicon-star-empty"></i>
                        </button>
                        &ngsp;
                        <button class="timButton btn-xs"
                                *ngIf="!hide?.clearFormat"
                                title="Clear format"
                                (click)="clearFormat()">
                            <i class="glyphicon glyphicon-trash"></i>
                        </button>
                        &ngsp;
                        <button *ngIf="!hide?.changePin"
                                class="timButton btn-xs"
                                [class.btn-toggled]="pinToggled()"
                                title="Pin area corner"
                                (click)="changePin()">
                            <i class="glyphicon glyphicon-pushpin"></i>
                        </button>
                    </div>
                </div>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class TimTableEditorToolbarDialogComponent extends AngularDialogComponent<
    ITimTableEditorToolbarParams,
    void
> {
    protected dialogName = "TableEditorToolbar";
    canInsertCells = false;
    canRemoveCells = false;
    canFilters: boolean = true;

    ngOnInit() {
        setToolbarInstance(this);
        this.callbacks = this.data.callbacks;
        this.activeTable = this.data.activeTable;
        this.hide = this.activeTable.data.hide;

        // TODO: Right now deleting specific rows/columns is not supported for tasks
        this.canInsertCells = !this.activeTable.task && !this.hide?.insertMenu;
        this.canRemoveCells = !this.activeTable.task && !this.hide?.editMenu;
    }

    public callbacks!: ITimTableToolbarCallbacks; // ngOnInit
    public activeTable?: TimTableComponent;
    hide?: HideValues;

    private previousBackgroundColor = DEFAULT_CELL_BGCOLOR;
    cellBackgroundColor = DEFAULT_CELL_BGCOLOR;

    public getTitle() {
        return "Edit table";
    }

    dismiss() {
        this.hideThis();
    }

    /**
     * Hides the toolbar and removes the instance.
     */
    public hideThis(keepEditorOpen?: boolean) {
        this.close();
        setToolbarInstance(undefined);
        if (!keepEditorOpen) {
            this.callbacks.closeEditor(false);
        }
    }

    public hideIfActiveTable(table: TimTableComponent) {
        if (table == this.activeTable) {
            this.hideThis();
        }
    }

    asInput(e: EventTarget | null) {
        return e as HTMLInputElement;
    }

    /**
     * Shows the toolbar.
     * @param callbacks Callbacks for communicating with the table.
     * @param activeTable The object that requested the toolbar to open.
     */
    public show(
        callbacks: ITimTableToolbarCallbacks,
        activeTable: TimTableComponent
    ) {
        this.activeTable = activeTable;
        this.hide = this.activeTable.data.hide;
        this.callbacks = callbacks;
    }

    /**
     * Sets the text-align value of a cell.
     */
    setTextAlign(value: string) {
        this.callbacks.setCell({style: {textAlign: value}});
    }

    changeCellToTableArea() {
        const val = this.callbacks.getCell().trimEnd();
        this.callbacks.fillTableCSV(val);
    }

    /**
     * Sets the cell by template
     */
    public setCell(value: IToolbarTemplate) {
        this.callbacks.setCell(value);
    }

    /**
     * Clears cell format
     */
    clearFormat() {
        this.callbacks.setCell({style: {CLEAR: "ALL"}});
    }

    pinToggled(): boolean {
        return this.activeTable?.shiftDown ?? false;
    }

    changePin() {
        const t = this.activeTable;
        if (!t) {
            return;
        }
        t.shiftDown = !t.shiftDown;
        t.startCell = t.activeCell;
    }

    /**
     * Add current cell to templates
     */
    addToTemplates() {
        this.callbacks.addToTemplates();
    }

    /**
     * Gets the cell text for toolbar
     */
    getCellForToolbar(value: IToolbarTemplate) {
        let v = value.text;
        if (v === "notext") {
            return undefined;
        }
        if (!v) {
            v = value.cell;
        }
        if (!v) {
            v = value.toggleCell;
        }
        if (!v) {
            return "\u2003";
        } // &#8195  em space &emsp;
        v = "" + v;
        // v = v.replace('$', '');
        if (v === value.text && !value.favorite) {
            return v;
        }
        if (v.length <= 5) {
            return v;
        }
        return v.substr(0, Math.min(v.length, 5)) + "..";
    }

    /**
     * Gets the cell text for toolbar
     */
    getTitleForToolbar(value: IToolbarTemplate) {
        let v = value.title;
        if (!v) {
            v = value.cell;
        } // &#8195  em space &emsp;
        if (!v) {
            v = value.text;
        } // &#8195  em space &emsp;
        if (!v) {
            return null;
        } // &#8195  em space &emsp;
        return v;
    }

    /**
     * Gets the cell class for toolbar
     */
    getCellClassForToolbar(value: IToolbarTemplate): string {
        let cls = "toolbarButton";
        if (value.buttonClass) {
            cls = " " + value.buttonClass;
        } else if (value.style?.class) {
            cls += " " + value.style.class;
        } else if (value.toggleStyle?.class) {
            cls += " " + value.toggleStyle.class;
        }
        return cls;
    }

    /**
     * Gets the cell inner class for toolbar
     */
    getIClassForToolbar(value: IToolbarTemplate): string {
        if (value.iClass) {
            if (value.iClass.includes("glyphicon")) {
                return "glyphicon " + value.iClass;
            }
            return value.iClass;
        }
        return "";
    }

    /**
     * Gets the cell style for toolbar
     */
    getCellStyleForToolbar(value: IToolbarTemplate) {
        let ret = value.style;
        if (!ret) {
            ret = value.toggleStyle;
        }
        if (value.buttonStyle) {
            ret = value.buttonStyle;
        }
        return ret ?? null;
    }

    /**
     * Updates the color of a cell when the color picker is closed.
     * @param color The color string as a hex value. It starts with "#".
     */
    onColorChange(color: string) {
        this.previousBackgroundColor = this.cellBackgroundColor;
        this.callbacks.setCell({style: {backgroundColor: color}});
    }

    // noinspection JSUnusedLocalSymbols
    private getStyle() {
        return {"background-color": "#" + this.previousBackgroundColor};
    }

    applyBackgroundColor() {
        this.callbacks.setCell({
            style: {backgroundColor: "#" + this.previousBackgroundColor},
        });
    }

    addColumn(offset: number) {
        this.callbacks.addColumn(offset);
    }

    addRow(offset: number) {
        this.callbacks.addRow(offset);
    }

    removeColumn() {
        this.callbacks.removeColumn();
    }

    removeRow() {
        this.callbacks.removeRow();
    }

    edit(cmd: string) {
        this.callbacks.editCallback(cmd);
    }

    filter(cmd: string) {
        this.callbacks.filterCallback(cmd);
    }

    private borderRect() {
        const brect = {
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
            },
        };
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
        if (templ.column) {
            const col = this.callbacks.getColumn();
            if (!templ.column.includes(col)) {
                return;
            }
        }
        if (templ.columnName) {
            const colName = this.callbacks.getColumnName();
            if (!templ.columnName.includes(colName)) {
                return;
            }
        }
        if (templ.toColName) {
            if (this.callbacks.setToColumnByName(templ.toColName) < 0) {
                return;
            }
        }
        if (templ.toColIndex) {
            if (this.callbacks.setToColumnByIndex(templ.toColIndex) < 0) {
                return;
            }
        }
        if (templ.filters) {
            this.callbacks.filterCallback(
                "filters",
                JSON.stringify(templ.filters)
            );
            return;
        }
        if (templ.commands) {
            for (const cmd of templ.commands) {
                const c = cmd.split(" ");
                const cmds = c[0];
                const param = c[1] ?? "";
                if (cmds === "clearFormat") {
                    this.clearFormat();
                }
                if (cmds === "applyBackgroundColor") {
                    this.applyBackgroundColor();
                }
                if (cmds === "setTextAlign") {
                    this.setTextAlign(param);
                }
                if (cmds === "addToTemplates") {
                    this.addToTemplates();
                }
                if (cmds === "borderRect") {
                    this.borderRect();
                }
                if (cmds === "removeBorders") {
                    this.removeBorders();
                }
            }
        }

        this.setCell(templ);
        if (templ.closeEdit) {
            this.callbacks.closeEditor(true);
        }
    }
}

@NgModule({
    declarations: [TimTableEditorToolbarDialogComponent],
    imports: [
        CommonModule,
        FormsModule,
        TimUtilityModule,
        DialogModule,
        BsDropdownModule.forRoot(),
    ],
})
export class TimTableEditorToolbarDialogModule {}
