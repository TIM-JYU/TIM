/**
 * Implementation for TimTable
 *
 * The table can be sorted by clicking the header row.
 * The sort order is kept in permutation table permTable and the real data is
 * not sorted, only shown using the permutation table.
 *
 * That means that there are three types of coordinates:
 * - spreadsheet-like coordinates, for example D7
 * - cell indices by original indices of cells
 * - screen indices for the cell
 *
 * So one must be careful about what coordinates to use and when.
 * Especially for areas there is type SelectedCells that keeps track from both.
 * At the moment it is supposed that selection must always be a rectangular area and
 * columns are not sorted. If one adds the possibility to change the order of columns,
 * then the behavior of SelectedCell interface must be changed.
 *
 * To change from original coord to screencoord, there is permTableToScreen.
 * Mostly the screencoordinates starts in code by s like sy, srow and so on.
 */
// TODO: Static headers and filter rows so they do not scroll
// TODO: Toolbar visible only when hit an editable field (not locked)
// TODO: Toolbar shall not steal focus when created first time
// TODO: save filter and sort conditions
// TODO: Show sort icons weakly, so old icons with gray
// TODO: set styles also by list of cells like value
// TODO: Global favorites to toolbar
// TODO: Save favorites
// TODO: TableForm does not support md:
// TODO: Use Angular's HTTP service instead of AngularJS $http

import * as t from "io-ts";
import type {
    AfterViewInit,
    ApplicationRef,
    DoBootstrap,
    DoCheck,
    OnDestroy,
    OnInit,
} from "@angular/core";
import {
    ChangeDetectionStrategy,
    ChangeDetectorRef,
    Component,
    ElementRef,
    HostListener,
    Input,
    NgModule,
    NgZone,
    QueryList,
    ViewChild,
    ViewChildren,
} from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import type {Subscription} from "rxjs";
import {openEditorSimple} from "tim/editor/pareditorOpen";
import angular from "angular";
import {PurifyModule} from "tim/util/purify.module";
import {DataViewModule} from "tim/plugin/dataview/data-view.module";
import type {
    DataModelProvider,
    VirtualScrollingOptions,
} from "tim/plugin/dataview/data-view.component";
import {DataViewComponent} from "tim/plugin/dataview/data-view.component";
import type {IGenericPluginMarkup} from "tim/plugin/attributes";
import {nullable, withDefault} from "tim/plugin/attributes";
import {showConfirm} from "tim/ui/showConfirmDialog";
import type {
    ICtrlWithMenuFunctionEntry,
    IMenuFunctionEntry,
} from "tim/document/viewutils";
import type {OnClickArg} from "tim/document/eventhandlers";
import {onClick} from "tim/document/eventhandlers";
import type {
    ISetAnswerResult,
    ITimComponent,
    ViewCtrl,
} from "tim/document/viewctrl";
import {ChangeType, FormModeOption} from "tim/document/viewctrl";
import {ParCompiler} from "tim/editor/parCompiler";
import {
    getKeyCode,
    isArrowKey,
    isKeyCode,
    KEY_DOWN,
    KEY_ENTER,
    KEY_ESC,
    KEY_F2,
    KEY_LEFT,
    KEY_RIGHT,
    KEY_TAB,
    KEY_UP,
} from "tim/util/keycodes";
import {$http} from "tim/util/ngimport";
import {
    clone,
    copyToClipboard,
    defaultErrorMessage,
    defaultTimeout,
    maxContentOrFitContent,
    scrollToViewInsideParent,
    StringOrNumber,
    timeout,
    to,
} from "tim/util/utils";
import {TaskId} from "tim/plugin/taskid";
import {PluginMeta} from "tim/plugin/util";
import type {PluginJson} from "tim/plugin/angular-plugin-base.directive";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {computeHiddenRowsFromFilters} from "tim/plugin/timTable/filtering";
import {
    handleToolbarKey,
    hideToolbar,
    isToolbarEnabled,
    isToolbarOpen,
    showTableEditorToolbar,
} from "tim/plugin/timTable/toolbarUtils";
import {createParContext} from "tim/document/structure/create";
import {CommonModule} from "@angular/common";

const timDateRegex = /^\d{4}-\d{2}-\d{2}[ T]?\d{2}:\d{2}(:\d{2})?$/;

function replaceAll(s: string, s1: string, s2: string): string {
    const re = new RegExp(s1, "g");
    return s.replace(re, s2);
}

const sortLang: string = "fi";

const styleToHtml: Record<string, string> = {
    backgroundColor: "background-color",
    border: "border",
    borderBottom: "border-bottom",
    borderLeft: "border-left",
    borderRight: "border-right",
    borderTop: "border-top",
    color: "color",
    colspan: "colspan",
    fontFamily: "font-family",
    fontSize: "font-size",
    fontWeight: "font-weight",
    height: "height",
    horizontalAlign: "horizontal-align",
    maxWidth: "max-width",
    minWidth: "min-width",
    rowspan: "rowspan",
    textAlign: "text-align",
    verticalAlign: "vertical-align",
    visibility: "visibility",
    whiteSpace: "white-space",
    width: "width",
};

export interface CellResponse {
    row: number;
    col: number;
    cellHtml: string;
}

export type CellToSave = {
    row: number;
    col: number;
    c: string;
    key?: undefined;
};

export type CellAttrToSave = {
    row: number;
    col: number;
    c: string;
    key: string;
};

export interface IRectLimits {
    minx: number;
    maxx: number;
    miny: number;
    maxy: number;
}

export interface HideValues {
    edit?: boolean;
    insertMenu?: boolean;
    editMenu?: boolean;
    toolbar?: boolean;
    editorButtons?: boolean;
    editorPosition?: boolean;
    select?: boolean;
    addRow?: boolean;
    delRow?: boolean;
    addCol?: boolean;
    delCol?: boolean;
    needFirstClick?: boolean;
    sort?: boolean;
    colorPicker?: boolean;
    alignLeft?: boolean;
    alignCenter?: boolean;
    alignRight?: boolean;
    changeTable?: boolean;
    addToTemplates?: boolean;
    clearFormat?: boolean;
    changePin?: boolean;
}

export interface IToolbarTemplate {
    cell?: string;
    text?: string;
    class?: string;
    shortcut?: string;
    chars?: string;
    removeStyle?: Record<string, string | string[]>;
    toggleStyle?: Record<string, string>;
    toggleCell?: string;
    buttonClass?: string;
    iClass?: string; // class for inner part, mostly like glyphicon-star-empty
    style?: Record<string, string>;
    buttonStyle?: Record<string, string>;
    rect?: Record<string, IToolbarTemplate>;
    area?: (IToolbarTemplate | undefined)[][];
    title?: string;
    onlyEmpty?: boolean;
    toggle?: boolean;
    delta?: Record<string, number>;
    hide?: boolean;
    commands?: string[];
    notInEdit?: boolean;
    favorite?: boolean;
    closeEdit?: boolean; // if true, close small editor after done
    column?: number[];
    columnName?: string[];
    toColIndex?: number;
    toColName?: string;
}

export interface TimTable extends IGenericPluginMarkup {
    table: ITable;
    id?: string;
    headers?: string[];
    saveAttrs?: string[]; // what attributes to save when jsrunner sends data
    headersStyle?: Record<string, string>;
    addRowButtonText?: string;
    forcedEditMode?: boolean;
    globalAppendMode?: boolean;
    dataInput?: boolean;
    task?: boolean;
    taskCanModifyTable?: boolean;
    taskBorders?: boolean;
    userdata?: DataEntity;
    editorBottom?: boolean;
    editorButtonsBottom?: boolean;
    editorButtonsRight?: boolean;
    toolbarTemplates?: IToolbarTemplate[];
    hide?: HideValues;
    hideSaveButton?: boolean;
    // hiddenRows?: IRow[];
    hiddenRows?: number[];
    hiddenColumns?: number[];
    locked?: boolean;
    lockedCells?: string[];
    lockedColumns?: string[];
    // cellsTosave may include un-rect area (not at the moment but maybe in future
    saveCallBack?: (cellsTosave: CellToSave[]) => void;
    saveStyleCallBack?: (cellsTosave: CellAttrToSave[]) => void;
    cbCallBack?: (cbs: boolean[], n: number, index: number) => void;
    maxWidth?: string; // Possibly obsolete if cell/column layout can be given in data.table.columns
    minWidth?: string;
    singleLine?: boolean;
    filterRow?: boolean;
    cbColumn?: boolean;
    nrColumn?: boolean | number;
    charRow?: boolean | number;
    saveUserDataHeader?: boolean;
    maxRows?: string;
    maxCols?: string;
    button?: string;
    autosave?: boolean;
    disableUnchanged?: boolean;
    // TODO: need self-explanatory name for this attribute
    //  could also use hideBrowser?
    nonUserSpecific?: boolean; // Whether (task-mode) table should react to user changes
    // lockCellCount?: boolean;
    header?: string;
    footer?: string;
    stem?: string;
    disableSelect?: boolean;
    savedText?: string;
    connectionErrorMessage?: string;
    tag?: string;
    undo?: {
        button?: string;
        title?: string;
        confirmation?: string;
        confirmationTitle?: string;
    };
    dataView?: DataViewSettings | null;
    isPreview: boolean;
}

export const DataViewVirtualScrollingSettingsType = t.type({
    enabled: withDefault(t.boolean, true),
    verticalOverflow: withDefault(t.number, 1),
    horizontalOverflow: withDefault(t.number, 999), // TODO: Reduce when horizontal scrolling works better
});

export const DataViewSettingsType = t.type({
    virtual: nullable(DataViewVirtualScrollingSettingsType),
    rowHeight: withDefault(t.number, 30),
    columnWidths: withDefault(t.record(t.string, t.number), {}),
    // We use custom table width for DataView because its behaviour differs from TimTable
    // For example, max-content works for both Chrome and Firefox to do fullwidth
    tableWidth: withDefault(t.string, "max-content"),
    fixedColumns: withDefault(t.number, 0),
    reportSlowLoad: withDefault(t.boolean, true),
});

export interface DataViewSettings
    extends t.TypeOf<typeof DataViewSettingsType> {
    // Currently PyCharm isn't able to analyze types generated via io-ts inside Angular templates,
    // which is why these two are defined twice
    tableWidth: string;
    fixedColumns: number;
}

interface Rng {
    range?: number[] | number | string;
    validrange?: readonly number[];
    def?: Record<string, string>;
}

export interface ITable {
    // extends ITableStyles
    countRow?: number;
    countCol?: number;
    defrows?: Record<string, string>;
    defcols?: Record<string, string>;
    defcells?: Record<string, string>;
    defcolsrange?: Rng[];
    defrowsrange?: Rng[];
    defcellsrange?: Rng[];
    rows?: IRow[];
    columns?: IColumn[];
    tabledatablock?: DataEntity;

    [key: string]: unknown;
}

export interface DataEntity {
    type: "Relative" | "Abstract";
    cells: ICellDataEntity;
}

export interface ICellIndex {
    x: number;
    y: number;
}

export interface ISelectedCells {
    cells: ICellIndex[]; // List of original cell indices inside selected area
    srows: boolean[]; // table for screen indices selected
    scol1: number; // screen index for first selected column
    scol2: number; // screen index for last selected column
    srow1: number; // screen index for first selected row
    srow2: number; // screen index for last selected row
}

export type ICellDataEntity = Record<string, CellEntity>;

const CellTypeR = t.union([t.string, t.number, t.boolean, t.null]);
export type CellType = t.TypeOf<typeof CellTypeR>;
export type CellEntity = ICell | CellType;

export interface IRow {
    // extends IRowStyles
    row?: CellEntity[];

    [key: string]: unknown;
}

export type IColumn = Record<string, unknown>;

export interface ICellCoord {
    row: number;
    col: number;
}

export interface ICell {
    // extends ICellStyles
    cell: CellType;
    class?: string;
    editorOpen?: boolean;
    colspan?: number;
    rowspan?: number;
    underSpanOf?: ICellCoord;
    renderIndexX?: number;
    renderIndexY?: number; // TODO: Is this useless?
    styleCache?: Record<string, string>; // cache of computed styles for this cell

    [key: string]: unknown;
}

/**
 * Styles
 */

const tableStyles: Set<string> = new Set<string>([
    "backgroundColor",
    "border",
    "borderTop",
    "borderBottom",
    "borderLeft",
    "borderRight",
    "verticalAlign",
    "textAlign",
    "color",
    "fontFamily",
    "fontSize",
    "visibility",
    "width",
]);

const rowStyles: Set<string> = new Set<string>([
    "backgroundColor",
    "border",
    "borderTop",
    "borderBottom",
    "borderLeft",
    "borderRight",
    "verticalAlign",
    "textAlign",
    "color",
    "fontFamily",
    "fontSize",
    "fontWeight",
    "height",
]);

const cellStyles: Set<string> = new Set<string>([
    "verticalAlign",
    "fontSize",
    "border",
    "borderTop",
    "borderBottom",
    "borderLeft",
    "borderRight",
    "backgroundColor",
    "textAlign",
    "fontFamily",
    "color",
    "fontWeight",
    "width",
    "height",
    "colspan",
    "rowspan",
]);

const columnStyles: Set<string> = new Set<string>([
    "width",
    "backgroundColor",
    "border",
    "borderTop",
    "borderBottom",
    "borderLeft",
    "borderRight",
]);

const columnCellStyles: Set<string> = new Set<string>([
    "fontSize",
    "verticalAlign",
    "textAlign",
    "fontFamily",
    "color",
    "fontWeight",
    "whiteSpace",
]);

enum Direction {
    Up = 1,
    Down = 2,
    Left = 4,
    Right = 8,
}

const UP_OR_DOWN = [Direction.Up, Direction.Down];
const LEFT_OR_RIGHT = [Direction.Left, Direction.Right];

export function isPrimitiveCell(cell: CellEntity): cell is CellType {
    // return cell == null || (cell as ICell).cell === undefined;
    return cell == null || typeof cell !== "object";
}

/**
 * Transforms column index to letter.
 * @param colIndex ex. 2
 * @return column index as letter
 */
export function colnumToLetters(colIndex: number): string {
    const ASCII_OF_A = 65;
    const ASCII_CHAR_COUNT = 26;
    const lastChar = String.fromCharCode(
        ASCII_OF_A + (colIndex % ASCII_CHAR_COUNT)
    );
    const remainder = Math.floor(colIndex / ASCII_CHAR_COUNT);

    if (remainder == 0) {
        return lastChar;
    } else if (remainder <= ASCII_CHAR_COUNT) {
        return String.fromCharCode(ASCII_OF_A + remainder - 1) + lastChar;
    }
    // recursive call to figure out the rest of the letters
    return colnumToLetters(remainder - 1) + lastChar;
}

interface ICurrentCell {
    row: number;
    col: number;
    editorOpen: boolean;
    editedCellContent: string;
    readonly editedCellInitialContent: string;
}

const emptyStyle = {} as const;

enum ChangeDetectionHint {
    DoNotTrigger,
    NeedToTrigger,
}

export enum ClearSort {
    Yes,
    No,
}

/**
 * TimTable Angular component.
 *
 * NOTES:
 * - In the event handlers of template, use naming "handle<EventName><ElementName>". Never call event handlers in
 *   controller methods.
 * - Call this.c() only at the end of event handlers.
 */
@Component({
    selector: "tim-table",
    changeDetection: ChangeDetectionStrategy.OnPush,
    template: `
        <div #timTableRunDiv [ngClass]="{csRunDiv: taskBorders, 'no-overflow': dataView}"
             [class.cs-has-header]="data.header"
             class="timTableRunDiv no-popup-menu"
             [ngStyle]="dataView ? {} : {
                'max-height': maxRows,
                'width': maxCols
             }"
        >
            <h4 *ngIf="data.header" [innerHtml]="data.header | purify"></h4>
            <p *ngIf="data.stem" class="stem" [innerHtml]="data.stem | purify"></p>
            <div class="timTableContentDiv no-highlight"
                 [ngClass]="{disableSelect: disableSelect}">
                <div class="buttonsCol">
                    <button class="timButton" title="Remove column"
                            [disabled]="isPreview()"
                            *ngIf="delColEnabled()"
                            (click)="handleClickRemoveColumn()"><span class="glyphicon glyphicon-minus"></span></button>
                    <button class="timButton" title="Add column" *ngIf="addColEnabled()"
                            [disabled]="isPreview()"
                            (click)="handleClickAddColumn()"><span class="glyphicon glyphicon-plus"></span></button>
                </div>
                <ng-container *ngIf="dataView; else tableView">
                    <tim-data-view [virtualScrolling]="dataViewVScrolling"
                                   [modelProvider]="this"
                                   [tableClass]="{editable: isInEditMode() && !isInForcedEditMode(),
                                                forcedEditable: isInForcedEditMode(),
                                                timTableTable: true}"
                                   [tableStyle]="stylingForTable(data.table)"
                                   [id]="data.table.id"
                                   [columnIdStart]="nrColStart"
                                   [tableMaxHeight]="maxRows"
                                   [tableMaxWidth]="dataView.tableWidth"
                                   [fixedColumnCount]="dataView.fixedColumns"
                                   [headerStyle]="headersStyle"
                                   [reportSlowLoad]="dataView.reportSlowLoad"
                                   #dataViewComponent>
                        <ng-container *ngTemplateOutlet="inlineEditorTemplate"></ng-container>
                    </tim-data-view>
                </ng-container>
                <ng-template #tableView>
                    <table #tableElem
                           [ngClass]="{editable: isInEditMode() && !isInForcedEditMode(),
                                                  forcedEditable: isInForcedEditMode()}"
                           class="timTableTable"
                           [ngStyle]="stylingForTable(data.table)" [id]="data.table.id">
                        <col class="nrcolumn" *ngIf="data.nrColumn"/>
                        <col class="cbColumn" *ngIf="data.cbColumn"/>
                        <col *ngFor="let c of columns; let i = index" [span]="c.span" [id]="c.id"
                             [ngStyle]="stylingForColumn(c, i)"/>
                        <thead>
                        <tr *ngIf="data.charRow"> <!--Char coordinate row -->
                            <td class="nrcolumn charRow" *ngIf="data.nrColumn"></td>
                            <td class="cbColumn charRow" *ngIf="data.cbColumn"></td>
                            <td class="charRow" [hidden]="!showColumn(coli)"
                                *ngFor="let c of cellDataMatrix[0]; let coli = index" [attr.span]="c.span">
                                <span [innerText]="coliToLetters(coli)"></span>
                            </td>
                        </tr>
                        <tr *ngIf="data.headers"> <!-- Header row -->
                            <td class="nrcolumn totalnr" *ngIf="data.nrColumn"
                                (click)="handleClickClearFilters()"
                                title="Click to show all"
                            >{{totalRows}}</td>
                            <td *ngIf="data.cbColumn"><input type="checkbox" [(ngModel)]="cbAllFilter"
                                                             (ngModelChange)="handleChangeCheckbox(-1)"
                                                             title="Check for all visible rows">
                            </td>
                            <td class="headers"
                                *ngFor="let c of data.headers; let coli = index"
                                [hidden]="!showColumn(coli)"
                                (click)="handleClickHeader(coli)"
                                title="Click to sort"
                                [ngStyle]="headersStyle">{{c}}<span
                                    [ngStyle]="sortSymbolStyle[coli]">{{sortSymbol[coli]}}</span>
                            </td>
                        </tr>
                        </thead>
                        <tbody>
                        <tr *ngIf="filterRow"> <!-- Filter row -->
                            <td class="nrcolumn totalnr" *ngIf="data.nrColumn"><span
                                    *ngIf="hiddenRowCount">{{visibleRowCount}}</span></td>
                            <td *ngIf="data.cbColumn"><input type="checkbox" [(ngModel)]="cbFilter"
                                                             (ngModelChange)="handleChangeFilter()"
                                                             title="Check to show only checked rows"></td>

                            <td [hidden]="!showColumn(coli)"
                                *ngFor="let c of cellDataMatrix[0]; let coli = index" [attr.span]="c.span">
                                <div class="filterdiv">
                                    <input type="text" (ngModelChange)="handleChangeFilter()"
                                           [(ngModel)]="filters[coli]"
                                           title="Write filter condition">
                                </div>
                            </td>
                        </tr> <!-- Now the matrix -->
                        <tr *ngFor="let rowi of permTable; let i = index"
                            [style]="stylingForRow(rowi)"
                            [class]="classForRow(rowi)"
                            [hidden]="!showRow(rowi)"
                        >
                            <td class="nrcolumn" *ngIf="data.nrColumn">{{i + nrColStart}}</td>
                            <td class="cbColumn" *ngIf="data.cbColumn">
                                <input type="checkbox" [(ngModel)]="cbs[rowi]"
                                       (ngModelChange)="handleChangeCheckbox(rowi)">
                            </td>
                            <ng-container *ngFor="let td of cellDataMatrix[rowi]; let coli = index">
                                <td *ngIf="!td.underSpanOf"
                                    [hidden]="!showColumn(coli)"
                                    [class]="classForCell(rowi, coli)"
                                    [attr.colspan]="td.colspan"
                                    [attr.rowspan]="td.rowspan"
                                    [style]="stylingForCell(rowi, coli)"
                                    (click)="handleClickCell(rowi, coli, $event)"
                                    [innerHtml]="td.cell | purify">
                                    <!--                                <div [innerHtml]="td.cell"></div>-->
                                </td>
                            </ng-container> <!-- one cell -->
                        </tr> <!-- the matrix -->
                        </tbody>
                    </table>
                </ng-template>
                <div class="buttonsRow">
                    <button class="timButton" title="Remove row" *ngIf="delRowEnabled()"
                            [disabled]="isPreview()"
                            (click)="handleClickRemoveRow()"><span
                            class="glyphicon glyphicon-minus"></span></button>
                    <button class="timButton" title="Add row" *ngIf="addRowEnabled()"
                            [disabled]="isPreview()"
                            (click)="handleClickAddRow()"><span
                            class="glyphicon glyphicon-plus" [innerText]="addRowButtonText"></span></button>
                </div>
                <ng-container *ngIf="!dataView">
                    <ng-container *ngTemplateOutlet="inlineEditorTemplate"></ng-container>
                </ng-container>
                <ng-template #inlineEditorTemplate>
                    <div #inlineEditor class="timTableEditor inlineEditorDiv no-highlight" *ngIf="currentCell">
                        <input class="editInput" #editInput autocomplete="off"
                               (blur)="smallEditorLostFocus($event)"
                           (keyup)="handleKeyUpSmallEditor($event)"
                           [(ngModel)]="currentCell.editedCellContent">
                    <span #inlineEditorButtons
                          class="inlineEditorButtons"
                          style="position: absolute; width: max-content"
                          [hidden]="hide.editorButtons">
                    <span [hidden]="hide.editorPosition" [innerHtml]="editorPosition"
                          (click)="handleClickEditorPosition()" style="background: yellow;"></span>
                    <button
                            #buttonOpenBigEditor class="timButton buttonOpenBigEditor"
                            (click)="handleClickOpenBigEditor()"><span class="glyphicon glyphicon-pencil"></span>
                    </button>
                    <button class="timButton buttonCloseSmallEditor"
                            (click)="handleClickCancelSmallEditor()"><tim-close-button></tim-close-button>
                    </button>
                    <button class="timButton buttonAcceptEdit"
                            (click)="handleClickAcceptSmallEditor()"><span class="glyphicon glyphicon-ok"></span>
                    </button>
                    </span>
                </div>
                    </ng-template>
            </div>
            <div class="csRunMenuArea" *ngIf="task && !data.hideSaveButton && !isLocked()">
                <p class="csRunMenu">
                    <button class="timButton" [disabled]="disableUnchanged && !edited" *ngIf="(task && hasButton) || saveFailed"
                            (click)="handleClickSave()">{{button}}</button>
                    &nbsp;
                    <a href="" *ngIf="undoButton && isUnSaved()" [title]="undoTitle"
                       (click)="tryResetChanges($event)">{{undoButton}}</a>
                    <span [hidden]="!result">{{result}}</span>
                </p>
            </div>
            <p class="plgfooter" *ngIf="data.footer" [innerHtml]="data.footer | purify"></p>
            <div *ngIf="connectionErrorMessage" class="error" style="font-size: 12px"
                 [innerHtml]="connectionErrorMessage"></div>
        </div>
        <div class="timTableError">
            <pre class="error" *ngIf="error" [innerText]="error"></pre>
        </div>
    `,
    styleUrls: ["./tim-table.component.scss", "./table-common.scss"],
})
export class TimTableComponent
    implements
        ITimComponent,
        OnInit,
        OnDestroy,
        DoCheck,
        AfterViewInit,
        DataModelProvider,
        ICtrlWithMenuFunctionEntry,
        PluginJson
{
    error: string = "";
    public viewctrl?: ViewCtrl;
    public cellDataMatrix: ICell[][] = []; // this has all table data as original indices (sort does not affect)
    private prevCellDataMatrix: ICell[][] = [];
    public columns: IColumn[] = [];
    @Input() public data!: TimTable;
    @Input() public json!: string;
    private prevData!: TimTable;
    private editRight = false;
    private userdata?: DataEntity = undefined;
    private prevUserdata?: DataEntity = undefined;
    private editing = false;
    private forcedEditMode = false;
    task = false;
    @Input() taskid?: TaskId;
    private isRunning = false;
    public taskBorders = false;
    currentCell?: ICurrentCell; // the cell currently being edited
    public activeCell?: ICellCoord;
    public startCell?: ICellCoord;
    private selectedCells: ISelectedCells = {
        cells: [],
        srows: [],
        scol1: 0,
        scol2: 0,
        srow1: 0,
        srow2: 0,
    };
    public shiftDown = false;
    public cbAllFilter = false;
    public cbs: boolean[] = [];
    public filters: (string | undefined)[] = [];
    public sortDir: number[] = [];
    public sortSymbol: string[] = [];
    public sortSymbolStyle: Record<string, string>[] = [];
    public sortSymbolStyles: Record<string, string>[] = [
        {fontSize: "xx-small"},
        {fontSize: "smaller"},
        {fontSize: "inherit"},
    ];
    public emptyRing: number[] = [-1, -1, -1]; // keep this and sortSymbolStyles equal length
    public sortSymbols: string[] = [" ▼", "", " ▲"];
    public sortRing: number[] = [];
    public sortDirRing: number[] = [];
    // ["🢓", "", "🢑"];  // this small does not work in iPad/Android
    private currentHiddenRows = new Set<number>();
    private rowDelta = 0;
    private colDelta = 0;
    public nrColStart = 1; // what number we start column numbers if it is numbered
    private intRowStart = 1;
    private intRow = false;
    cbFilter = false;
    filterRow = false;
    maxRows = "2000em";
    maxCols = maxContentOrFitContent();
    permTable: number[] = [];
    private permTableToScreen: number[] = []; // inverse perm table to get screencoordinate for row
    edited = false;
    @ViewChild("editInput") private editInput?: ElementRef<HTMLInputElement>;
    @ViewChild("inlineEditor") private editorDiv!: ElementRef<HTMLDivElement>;
    @ViewChild("inlineEditorButtons")
    private editorButtons!: ElementRef<HTMLDivElement>;
    @ViewChild("tableElem") private tableElem!: ElementRef<HTMLTableElement>;
    @ViewChild("timTableRunDiv")
    private timTableRunDiv!: ElementRef<HTMLDivElement>;
    @ViewChild("buttonOpenBigEditor")
    private buttonOpenBigEditor!: ElementRef<HTMLButtonElement>;
    @ViewChild("dataViewComponent") dataViewComponent?: DataViewComponent;
    @ViewChildren("editInput") private editInputs?: QueryList<
        ElementRef<HTMLInputElement>
    >;
    dataView?: DataViewSettings | null;
    private editInputStyles: string = "";
    private editInputClass: string = "";
    headersStyle: Record<string, string> | null = null;
    button: string = "Tallenna";
    hasButton: boolean = true;
    private noNeedFirstClick = false;
    hide: HideValues = {editorPosition: true};
    disableSelect: boolean = true;
    result?: string;
    connectionErrorMessage?: string;
    saveFailed = false;

    /**
     * Stores the last direction that the user moved towards with arrow keys
     * or Enter / Tab. Used for saving and retrieving the "coordinate" of a cell that is
     * embedded in another cell through colspan / rowspan so it can be used in navigation.
     */
    private lastDirection?: {direction: Direction; coord: number};
    private mouseInTable?: boolean;

    addRowButtonText: string = "";
    editorPosition: string = "";
    private pluginMeta: PluginMeta;
    private inputSub?: Subscription;
    private customDomUpdateInProgress?: boolean;
    private rowStyleCache = new Map<number, Record<string, string>>();
    private rowClassCache = new Map<number, string>();
    private shouldSelectInputText = false;
    private columnResolveState: boolean[] = [];
    // Number of rows in the table in its original state (i.e. no user state is applied)
    private initialRowCount: number = 0;
    // Number of rows in the table in its original state (i.e. no user state is applied)
    private initialColCount: number = 0;

    constructor(
        private el: ElementRef,
        public cdr: ChangeDetectorRef,
        private zone: NgZone
    ) {
        this.pluginMeta = new PluginMeta($(el.nativeElement));
        // if ( !this.data.hide ) this.data.hide = {};
    }

    get dataViewVScrolling() {
        const opts: Partial<VirtualScrollingOptions> = {};
        if (this.dataView?.virtual) {
            opts.enabled = this.dataView.virtual.enabled;
            opts.viewOverflow = {
                vertical: this.dataView.virtual.verticalOverflow,
                horizontal: this.dataView.virtual.horizontalOverflow,
            };
        }
        return opts;
    }

    get element(): JQuery<HTMLElement> {
        return $(this.el.nativeElement);
    }

    get markup() {
        return this.data;
    }

    get disableUnchanged() {
        return this.data.disableUnchanged;
    }

    get undoButton() {
        return this.data.undo?.button;
    }

    get undoTitle() {
        return this.data.undo?.title;
    }

    get undoConfirmationTitle() {
        return this.data.undo?.confirmationTitle;
    }

    get undoConfirmation() {
        return this.data.undo?.confirmation;
    }

    private getEditInputElement() {
        return this.editInput?.nativeElement;
    }

    /**
     * Force change detection.
     */
    c() {
        this.cdr.detectChanges();
    }

    /**
     * Set listener and initializes tabledatablock
     */
    async ngOnInit() {
        if (this.json) {
            this.data = JSON.parse(this.json);
        }

        if (this.data.hide) {
            this.hide = {...this.hide, ...this.data.hide};
        }

        this.dataView = this.data.dataView;

        if (typeof this.data.nrColumn === "number") {
            // for backward compatibility
            this.nrColStart = this.data.nrColumn;
            this.data.nrColumn = true; // nrColumn in this code is supposed to be boolean
        }
        if (typeof this.data.charRow === "number") {
            // for backward compatibility
            this.intRowStart = this.data.charRow;
            this.intRow = true;
            this.data.charRow = true; // charRow in this code is supposed to be boolean
        }

        this.disableSelect = this.data.disableSelect ?? false;
        this.noNeedFirstClick = this.hide.needFirstClick ?? false;

        this.hasButton = this.data.button !== "" && this.data.button !== null;
        this.button = this.data.button ?? this.button;

        if (this.data.maxRows) {
            this.maxRows = this.data.maxRows;
        }
        if (this.data.maxCols) {
            this.maxCols = this.data.maxCols;
        }

        this.headersStyle = this.data.headersStyle ?? null;

        if (!this.headersStyle) {
            this.headersStyle = {
                // backgroundColor: "lightgray",
                // "font-weight": "bold",
            };
        }
        if (this.data.singleLine) {
            const hs = this.headersStyle;
            hs["white-space"] = "nowrap";
        }
        this.userdata = this.data.userdata;
        this.reInitialize();

        this.filterRow = this.data.filterRow ?? false;
        if (this.cellDataMatrix.length <= 2) {
            this.filterRow = false;
        }
        this.colDelta = 0;
        this.rowDelta = 0;

        if (this.data.nrColumn) {
            this.colDelta += 1;
        }
        if (this.data.cbColumn) {
            this.colDelta += 1;
        }
        // if ( this.data.headers ) { this.rowDelta += 1; }
        if (this.filterRow) {
            this.rowDelta += 1;
        }

        let tb;
        if (this.data.taskBorders) {
            tb = true;
        } else if (this.data.taskBorders == false) {
            tb = false;
        } else {
            tb = this.data.task == true;
        }
        this.taskBorders = tb;
        this.viewctrl = vctrlInstance;
        if (this.viewctrl == null) {
        } else {
            this.editRight = this.viewctrl.item.rights.editable;

            if (this.data.task) {
                this.task = true;
            }

            // if(this.data.lockCellCount){
            //     this.lockCellCount = this.data.lockCellCount;
            // }
            if (this.data.addRowButtonText) {
                this.addRowButtonText = " " + this.data.addRowButtonText;
            }
            if (this.data.forcedEditMode) {
                this.forcedEditMode =
                    this.data.forcedEditMode && (this.editRight || this.task);
                this.editing = this.forcedEditMode;
            }

            if (this.data.task) {
                this.task = true;
                this.forcedEditMode = true;
                if (!this.isPreview()) {
                    this.viewctrl.addTimComponent(this);
                }
            }

            // Ensure we're not in the middle of document update
            await this.viewctrl.documentUpdate;
            // Also wait for the element to be attached to the DOM
            await timeout();
            const par = this.getPar();
            if (par) {
                this.viewctrl.addTable(this, par);
            }
        }
        this.currentHiddenRows = new Set(this.data.hiddenRows);
        onClick("body", ($this, e) => {
            this.onClick(e);
        });

        this.prevCellDataMatrix = clone(this.cellDataMatrix);
        this.prevUserdata = clone(this.userdata);
        this.prevData = clone(this.data);

        // For performance, we detach the automatic change detector for this component and call it manually.
        this.cdr.detach();

        if (this.viewctrl) {
            await this.viewctrl.documentUpdate;
            this.viewctrl.addParMenuEntry(this, this.getPar()!);
        }
    }

    ngAfterViewInit() {
        this.inputSub = this.editInputs?.changes.subscribe(
            (val: QueryList<ElementRef<HTMLInputElement>>) => {
                if (val.length > 0) {
                    this.updateSmallEditorPosition();
                }
            }
        );
        if (this.task && !this.getTaskId()) {
            this.error = "Task-mode on but TaskId is missing!";
            this.c();
        }
    }

    /**
     * Executed after Angular has finished its own DOM updates for this component,
     * so here we can perform our own DOM updates. We must call runOutsideAngular because otherwise
     * ngAfterViewChecked would be called endlessly.
     */
    ngAfterViewChecked() {
        this.doCustomDomUpdates();
    }

    private doCustomDomUpdates() {
        if (this.customDomUpdateInProgress) {
            return;
        }
        this.customDomUpdateInProgress = true;
        void this.zone.runOutsideAngular(async () => {
            // Update position before focusing. Otherwise, focus causes a scroll to the old position which can be far
            // away from the new position.
            this.updateSmallEditorPosition();
            if (this.shouldSelectInputText) {
                this.focusSmallEditor();
                this.shouldSelectInputText = false;
            }
            await ParCompiler.processAllMath(this.element);
            this.customDomUpdateInProgress = false;
        });
    }

    get totalRows() {
        return this.cellDataMatrix.length;
    }

    get totalCols() {
        return this.cellDataMatrix[0].length;
    }

    get hiddenRowCount() {
        return this.currentHiddenRows.size;
    }

    get visibleRowCount() {
        return this.totalRows - this.hiddenRowCount;
    }

    ngDoCheck() {}

    /**
     * Removes listener and cleans up
     */
    ngOnDestroy() {
        if (this.data.task && !this.isPreview()) {
            this.viewctrl?.removeTimComponent(this);
        }
        this.inputSub?.unsubscribe();
        this.removeEventListeners();
        // document.removeEventListener("click", this.onClick);
    }

    private eventListenersActive = false;

    private removeEventListeners() {
        if (!this.eventListenersActive) {
            return;
        }
        this.eventListenersActive = false;
        document.removeEventListener("keyup", this.keyUpTable);
        document.removeEventListener("keydown", this.keyDownTable);
        document.removeEventListener("keypress", this.keyPressTable);
        // document.removeEventListener("click", this.onClick);
    }

    private addEventListeners() {
        if (this.eventListenersActive) {
            return;
        }
        this.eventListenersActive = true;
        document.addEventListener("keyup", this.keyUpTable);
        document.addEventListener("keydown", this.keyDownTable);
        document.addEventListener("keypress", this.keyPressTable);
        // document.addEventListener("click", this.onClick);
    }

    private removeListenersAndCloseEditor() {
        this.removeEventListeners();
        this.closeSmallEditor();
    }

    private hideToolbar() {
        this.removeListenersAndCloseEditor();
        hideToolbar(this);
    }

    private openToolbar() {
        // return;
        this.addEventListeners();
        if (this.isInEditMode() && isToolbarEnabled() && !this.hide.toolbar) {
            showTableEditorToolbar({
                callbacks: {
                    setCell: (val) => this.handleToolbarSetCell(val),
                    getCell: () => this.getCurrentCellAsString(),
                    addToTemplates: () => this.handleToolbarAddToTemplates(),
                    addColumn: (offset) => this.handleToolbarAddColumn(offset),
                    addRow: (offset) => this.handleToolbarAddRow(offset),
                    removeColumn: () => this.handleToolbarRemoveColumn(),
                    removeRow: () => this.handleToolbarRemoveRow(),
                    closeEditor: (save: boolean) =>
                        this.saveCloseSmallEditor(save),
                    isEdit: () => this.isEdit(),
                    getColumn: () => this.getColumn(),
                    getColumnName: () => this.getColumnName(),
                    setToColumnByIndex: (col: number) =>
                        this.setToColumnByIndex(col),
                    setToColumnByName: (name: string) =>
                        this.setToColumnByName(name),
                },
                activeTable: this,
            });
        }
    }

    private isEdit(): boolean {
        return !!this.currentCell;
    }

    private async onClick(e: OnClickArg) {
        if (this.mouseInTable) {
            if (
                this.isInEditMode() &&
                isToolbarEnabled() &&
                !this.hide.toolbar
            ) {
                // this.openToolbar();
            } else {
                // Hide the toolbar if we're not in edit mode
                if (!this.isSomeCellBeingEdited()) {
                    this.hideToolbar();
                }
            }
        } else {
            if (!this.eventListenersActive) {
                return;
            } // No need to look anything when already inactive
            if (this.currentCell?.editorOpen) {
                return;
            }

            const target = e.target;

            if (target) {
                // Do not hide the toolbar if the user clicks on it
                if ($(target).parents(".modal-dialog").length > 0) {
                    return;
                }

                if ($(target).parents(".timTableEditor").length > 0) {
                    return;
                }
                this.activeCell = undefined;
                await this.saveCurrentCell();
                this.c();

                // Do not hide the toolbar if the user clicks on another TimTable
                if ($(target).parents(".timTableTable").length > 0) {
                    this.removeListenersAndCloseEditor();
                    return;
                }
            }

            this.hideToolbar();
        }
    }

    /**
     * Checks whether the table is set to be always in edit mode
     * (assuming the user has edit rights).
     * @returns {boolean} True if the table is always in edit mode, otherwise false.
     */
    public isInForcedEditMode() {
        return this.forcedEditMode;
    }

    public isLocked() {
        return this.data.locked;
    }

    public coliToLetters(coli: number) {
        if (this.intRow) {
            return this.intRowStart + coli;
        }
        return colnumToLetters(coli);
    }

    /**
     * Changes all visible check boxes on this column to same value as header row cb
     * if header row cb clicked. Otherwise just update hidden rows if cbFilter is on.
     * @param rowi index of checkbox changed
     */
    async handleChangeCheckbox(rowi: number) {
        if (rowi == -1) {
            const b = this.cbAllFilter;
            for (let i = 0; i < this.cellDataMatrix.length; i++) {
                if (this.currentHiddenRows.has(i)) {
                    continue;
                }
                this.cbs[i] = b;
            }
            this.dataViewComponent?.updateAllSelected();
        }
        if (this.cbFilter) {
            await this.updateFilter();
            this.dataViewComponent?.updateVisible();
        } else {
            this.countCBs(rowi);
        }
        this.c();
    }

    private countCBs(rowi: number) {
        let n = 0;
        for (let i = 0; i < this.cellDataMatrix.length; i++) {
            if (this.currentHiddenRows.has(i)) {
                continue;
            }
            if (this.cbs[i]) {
                n++;
            }
        }
        if (this.data.cbCallBack) {
            this.data.cbCallBack(this.cbs, n, rowi);
        }
    }

    public static rowToList(row: ICell[]) {
        const rlist: string[] = [];
        for (const c of row) {
            rlist.push(c ? (c.cell ? c.cell.toString() : "") : "");
        }
        return rlist;
    }

    /**
     * Make smaller matrix by cutting only needed columns
     * @param rows list of rows to use
     * @param cols col indices to take
     */
    public static makeSmallerMatrix(rows: string[][], cols: number[]) {
        const result = [];
        for (const r of rows) {
            const newrow = [];
            for (const i of cols) {
                newrow.push(r[i]);
            }
            result.push(newrow);
        }
        return result;
    }

    public getCheckedRows(startFrom: number, visible: boolean) {
        const crows = [];
        for (let i = startFrom; i < this.cellDataMatrix.length; i++) {
            if (this.currentHiddenRows.has(i) && visible) {
                continue;
            }
            if (this.cbs[i]) {
                crows.push(TimTableComponent.rowToList(this.cellDataMatrix[i]));
            }
        }
        return crows;
    }

    async handleChangeFilter() {
        await this.updateFilter();
        this.c();
    }

    /**
     * Adds rows to this.hiddenRows if their row values matches the given filters
     * TODO: add also < and > compare
     */
    async updateFilter() {
        await this.saveAndCloseSmallEditor();
        this.disableStartCell();
        // TODO check if better way to save than just making saveAndCloseSmallEditor public and calling it
        // this.saveAndCloseSmallEditor();
        const hidden = computeHiddenRowsFromFilters(
            this.cellDataMatrix,
            (i) => this.cbs[i],
            this.filters,
            this.cbFilter,
            (c: ICell[], colIndex) => c[colIndex].cell
        );
        this.currentHiddenRows = new Set([
            ...(this.data.hiddenRows ?? []),
            ...hidden,
        ]);
        this.countCBs(-1);
        this.dataViewComponent?.updateVisible();
    }

    sortByColumn(ai: number, bi: number, col: number, dir: number): number {
        // TODO: numeric sort also
        const a = this.cellDataMatrix[ai];
        const b = this.cellDataMatrix[bi];
        const ca = a[col];
        const cb = b[col];
        const cca = ca.cell;
        const ccb = cb.cell;
        const va = "" + cca;
        const vb = "" + ccb;

        if (timDateRegex.test(va) && timDateRegex.test(vb)) {
            return va.localeCompare(vb, sortLang) * dir;
        }

        // changes:  1,20 -> 1.20,  12:03 -> 12.03 TODO: 12:31:15 not handled correctly
        const na = parseFloat(va.replace(",", ".").replace(":", "."));
        const nb = parseFloat(vb.replace(",", ".").replace(":", "."));
        if (isNaN(na) || isNaN(nb)) {
            return va.localeCompare(vb, sortLang) * dir;
        }
        let ret = 0;
        if (na > nb) {
            ret = 1;
        } else if (na < nb) {
            ret = -1;
        }
        return ret * dir;
    }

    clearSortOrder() {
        this.permTable = [];
        for (let i = 0; i < this.cellDataMatrix.length; i++) {
            this.permTable[i] = i;
            this.permTableToScreen[i] = i;
        }
        this.sortDir = [];
        this.sortSymbol = [];
        this.sortSymbolStyle = [];
        this.sortRing = this.emptyRing.slice();
        this.sortDirRing = this.emptyRing.slice();
        this.dataViewComponent?.updateRowSortOrder(this.permTable);
    }

    private lastSortCol = this.emptyRing.slice();
    private lastSortDir = this.emptyRing.slice();

    repeatLastSort() {
        const cols = this.lastSortCol.slice();
        const dirs = this.lastSortDir.slice();
        for (let i = 0; i < cols.length; i++) {
            this.doSort(cols[i], dirs[i]);
        }
        this.c();
    }

    copyArray(a: number[], b: number[]) {
        for (let i = 0; i < a.length; i++) {
            b[i] = a[i];
        }
    }

    async handleClickHeader(col: number) {
        await this.saveAndCloseSmallEditor();
        if (this.hide.sort) {
            return;
        }
        let dir = this.sortDir[col];
        if (!dir) {
            dir = -1;
        }
        dir = -dir;
        this.doSort(col, dir);
        this.c();
    }

    doSort(col: number, dir: number) {
        if (col < 0) {
            return;
        }
        this.sortDir[col] = dir;

        const nl = this.sortRing.length - 1;

        if (this.sortRing[nl] != col) {
            // push old symbols  left and drop leftmost away
            const coli = this.sortRing.indexOf(col);
            if (coli < 0) {
                // drop lefmost away
                const last = this.sortRing.shift() ?? -1;
                this.sortDirRing.shift();
                if (last >= 0) {
                    this.sortDir[last] = 0;
                    this.sortSymbol[last] = "";
                    this.sortSymbolStyle[last] = {};
                }
            } else {
                this.sortRing.splice(coli, 1);
                this.sortDirRing.splice(coli, 1);
            }
            this.sortRing.push(col);
            this.sortDirRing.push(dir);
            for (let i = 0; i < this.sortRing.length; i++) {
                const ic = this.sortRing[i];
                this.sortSymbolStyle[ic] = this.sortSymbolStyles[i];
            }
        } else {
            this.sortDirRing[nl] = dir;
        }
        this.sortSymbolStyle[col] = this.sortSymbolStyles[nl];
        this.sortSymbol[col] = this.sortSymbols[dir + 1];

        // this.rowKeys.sort((a, b) => this.sortByRealName(a, b));
        this.permTable.sort((a, b) => this.sortByColumn(a, b, col, dir));
        for (let i = 0; i < this.permTable.length; i++) {
            this.permTableToScreen[this.permTable[i]] = i;
        }
        this.disableStartCell();
        this.dataViewComponent?.updateRowSortOrder(this.permTable);

        // Keep track from last sort col and dir
        this.copyArray(this.sortRing, this.lastSortCol);
        this.copyArray(this.sortDirRing, this.lastSortDir);
    }

    /**
     * Checks whether the table is in edit mode.
     * @returns {boolean} True if the table is in edit mode, otherwise false.
     */
    public isInEditMode() {
        return (this.editing || this.forcedEditMode) && !this.data.locked;
    }

    getMenuEntry(): IMenuFunctionEntry {
        return {
            func: () => this.toggleEditMode(),
            desc: this.editing ? "Close table editor" : "Edit table",
            show: !this.isInForcedEditMode() && !this.isLocked(),
        };
    }

    public addRowEnabled() {
        if (this.hide.addRow) {
            return false;
        }
        if (this.task) {
            return this.data.taskCanModifyTable;
        }
        return this.editRight && this.isInEditMode();
    }

    public delRowEnabled() {
        if (this.hide.delRow) {
            return false;
        }
        if (this.task) {
            return (
                this.data.taskCanModifyTable &&
                this.totalRows > this.initialRowCount
            );
        }
        return !this.task && this.editRight && this.isInEditMode();
    }

    public addColEnabled() {
        if (this.hide.addCol) {
            return false;
        }
        if (this.task) {
            return this.data.taskCanModifyTable;
        }
        return this.editRight && this.isInEditMode();
    }

    public delColEnabled() {
        if (this.hide.delCol) {
            return false;
        }
        if (this.task) {
            return (
                this.data.taskCanModifyTable &&
                this.totalCols > this.initialColCount
            );
        }
        return this.editRight && this.isInEditMode();
    }

    /**
     * Set attributes value to correct ones when saved cell values
     */
    public editSave() {
        this.editing = false;
        if (this.currentCell) {
            this.currentCell = undefined;
        }
    }

    /**
     * Returns true if the simple cell content editor is open.
     */
    public isSomeCellBeingEdited() {
        return this.currentCell;
    }

    @HostListener("mouseenter")
    public mouseInsideTable() {
        this.mouseInTable = true;
    }

    @HostListener("mouseleave")
    public mouseOutTable() {
        this.mouseInTable = false;
    }

    async handleClickSave() {
        await this.sendDataBlock();
        this.c();
    }

    public async sendDataBlock() {
        if (this.isRunning) {
            return;
        }
        await this.saveAndCloseSmallEditor();
        await this.sendDataBlockAsync();
    }

    async sendDataBlockAsync() {
        if (!this.task) {
            return;
        }
        if (!this.getTaskId()) {
            this.error = "Task-mode on but TaskId is missing!";
            return;
        }
        this.connectionErrorMessage = undefined;
        this.error = "";
        this.isRunning = true;
        const url = this.pluginMeta.getAnswerUrl();
        // noinspection UnnecessaryLocalVariableJS
        const params1 = {
            input: {
                answers: {
                    userdata: this.userdata,
                    // headers: this.data.headers ? this.data.headers : undefined,
                },
            },
        };
        let params = params1;

        if (this.data.saveUserDataHeader) {
            // noinspection UnnecessaryLocalVariableJS
            const params2 = {
                input: {
                    answers: {
                        userdata: this.userdata,
                        headers: this.data.headers
                            ? this.data.headers
                            : undefined,
                    },
                },
            };
            params = params2;
        }
        this.error = "";
        if (this.data.saveAttrs) {
            for (const key of this.data.saveAttrs) {
                // @ts-expect-error
                const value = this.data[key];
                // @ts-expect-error
                params.input.answers[key] = value;
            }
        }

        const r = await to(
            $http.put<{
                web: {
                    result: string;
                    error?: string;
                };
            }>(url, params, {timeout: defaultTimeout})
        );
        if (r.ok) {
            this.edited = false;
            this.saveFailed = false;
            this.updateListeners();
            this.prevCellDataMatrix = clone(this.cellDataMatrix);
            this.prevUserdata = clone(this.userdata);
            this.prevData = clone(this.data);
            let result = r.result.data.web.result;
            const savedText = this.data.savedText;
            if (result == "Saved" && savedText) {
                result = savedText;
            }
            this.result = result;
            this.error = r.result.data.web.error ?? "";
            // this.result = r.result.data.web.result;
        } else {
            this.saveFailed = true;
            this.connectionErrorMessage =
                r.result.data?.error ??
                this.data.connectionErrorMessage ??
                defaultErrorMessage;
        }
        this.isRunning = false;
        return r;
    }

    /**
     * Sets edited flag to false
     * Used to keep track of unsaved state when table is used as component
     * in another plugin
     */
    public confirmSaved() {
        this.edited = false;
        this.updateListeners();
    }

    /*
     * Set attribute to user object.  If key == CLEAR, remove attribute in value
     * IF key == CLEAR and value = ALL, clear all attributes
     */
    private setUserAttribute = (c: CellAttrToSave): void => {
        const key = c.key;
        const row = c.row;
        const col = c.col;
        const value = c.c;

        // Force style cache refresh for this cell.
        this.cellDataMatrix[row][col].styleCache = undefined;

        if (c.key != "CLEAR") {
            this.cellDataMatrix[row][col][key] = value;
        }
        if (!this.userdata) {
            return;
        }
        const coordinate = colnumToLetters(col) + "" + (row + 1);
        if (!this.userdata.cells[coordinate]) {
            if (key == "CLEAR") {
                return;
            } // nothing to do
            const data: CellEntity = {cell: null};
            data[key] = value;
            this.userdata.cells[coordinate] = data;
            return;
        }
        const cellValue = this.userdata.cells[coordinate];
        if (isPrimitiveCell(cellValue)) {
            if (key == "CLEAR") {
                return;
            } // nothing to do
            const data: CellEntity = {cell: this.cellToString(cellValue)};
            data[key] = value;
            this.userdata.cells[coordinate] = data;
            return;
        }
        if (key != "CLEAR") {
            cellValue[key] = value;
            return;
        }
        if (value == "ALL") {
            for (const k of Object.keys(cellValue)) {
                if (k == "cell") {
                    continue;
                }
                delete this.cellDataMatrix[row][col][k];
            }
            this.userdata.cells[coordinate] = cellValue.cell;
            return;
        }

        delete cellValue[value];
        delete this.cellDataMatrix[row][col][value];
    };

    setUserContent(row: number, col: number, content: string) {
        this.cellDataMatrix[row][col].cell = content;
        if (!this.userdata) {
            return;
        }
        const coordinate = colnumToLetters(col) + "" + (row + 1);
        if (!this.userdata.cells[coordinate]) {
            this.userdata.cells[coordinate] = content;
            return;
        }
        const cellValue = this.userdata.cells[coordinate];
        if (isPrimitiveCell(cellValue)) {
            this.userdata.cells[coordinate] = content;
            return;
        }
        cellValue.cell = content;
    }

    disableStartCell() {
        this.startCell = undefined;
        this.shiftDown = false;
        this.selectedCells.srows = [];
    }

    getSelectedCells(row: number, col: number): ISelectedCells {
        const ret: ICellIndex[] = [];
        const srows: boolean[] = [];

        const scol = col;
        const srow = this.permTableToScreen[row];
        if (scol == undefined || srow < 0) {
            return {
                cells: ret,
                srows: [],
                scol1: 0,
                scol2: 0,
                srow1: 0,
                srow2: 0,
            };
        }

        let sx1 = scol;
        let sx2 = scol;
        let sy1 = srow;
        let sy2 = srow;
        let ymin = 1000000;
        let ymax = 0;
        if (this.startCell) {
            const sscol = this.startCell.col; // start screen...
            const ssrow = this.permTableToScreen[this.startCell.row];
            sx1 = Math.min(scol, sscol);
            sx2 = Math.max(scol, sscol);
            sy1 = Math.min(srow, ssrow);
            sy2 = Math.max(srow, ssrow);
        }

        for (let sy = sy1; sy <= sy2; sy++) {
            const y = this.permTable[sy];
            if (this.currentHiddenRows.has(y)) {
                continue;
            }
            for (let sx = sx1; sx <= sx2; sx++) {
                if (this.data.hiddenColumns?.includes(sx)) {
                    continue;
                }
                srows[sy] = true;
                ret.push({x: sx, y: y});
                ymin = Math.min(y, ymin);
                ymax = Math.max(y, ymax);
            }
        }
        return {
            cells: ret,
            srows: srows,
            scol1: sx1,
            scol2: sx2,
            srow1: ymin,
            srow2: ymax,
        };
    }

    /**
     * Saves cell content
     * @param {string} cellContent Saved value
     * @param {number} docId  Document id
     * @param {string} parId Paragraph id
     * @param {number} row  Row index
     * @param {number} col Column index
     * @param {ISelectedCells} selectedCells cells to fill with that content
     */
    async saveCells(
        cellContent: string,
        docId: number,
        parId: string,
        row: number,
        col: number,
        selectedCells: ISelectedCells
    ) {
        if (
            row >= this.cellDataMatrix.length ||
            col >= this.cellDataMatrix[row].length
        ) {
            return;
        }
        const cellsToSave: CellToSave[] = [];
        if (this.task) {
            // const cells = this.getSelectedCells(row, col);
            for (const c of selectedCells.cells) {
                cellsToSave.push({c: cellContent, row: c.y, col: c.x});
                this.setUserContent(c.y, c.x, cellContent);
            }
            if (this.data.saveCallBack) {
                this.data.saveCallBack(cellsToSave);
            }
            this.edited = true;
            this.updateListeners();
            this.result = "";
            if (this.data.autosave) {
                await this.sendDataBlockAsync();
            }
            this.dataViewComponent?.updateCellsContents(
                selectedCells.cells.map((c) => ({row: c.y, col: c.x}))
            );
            return;
        }

        for (const c of selectedCells.cells) {
            cellsToSave.push({c: cellContent, row: c.y, col: c.x});
        }
        const response = await to(
            $http.post<CellResponse[]>("/timTable/saveMultiCell", {
                docId,
                parId,
                cellsToSave: cellsToSave,
            })
        );
        if (!response.ok) {
            return;
        }
        const cellHtmls = response.result.data;
        for (const c of cellHtmls) {
            this.cellDataMatrix[c.row][c.col].cell = c.cellHtml;
        }
        this.dataViewComponent?.updateCellsContents(
            cellHtmls.map((c) => ({row: c.row, col: c.col}))
        );
        this.c();
    }

    /**
     * Get cell data
     * @param {number} docId Document id
     * @param {string} parId Paragraph id
     * @param {number} row Row index
     * @param {number} col Column index
     * @returns {Promise<string>}
     */
    async getCellData(docId: number, parId: string, row: number, col: number) {
        const response = await to(
            $http<CellType[]>({
                url: "/timTable/getCellData",
                method: "GET",
                params: {docId, parId, row, col},
            })
        );
        if (!response.ok) {
            throw Error("getCellData failed");
        }
        const data = response.result.data;
        return this.cellToString(data[0]);
    }

    /**
     * Opens editor
     * @param {CellEntity} cell
     * @param {number} docId Document id
     * @param {string} value Value that editor will show
     * @param {string} parId Paragraph id
     * @param curr The current cell info
     */
    async openBigEditorAsync(
        cell: CellEntity,
        docId: number,
        value: string,
        parId: string,
        curr: ICurrentCell
    ) {
        curr.editorOpen = true;
        const result = await openEditorSimple(
            docId,
            curr.editedCellContent,
            "Edit table cell",
            "timTableCell"
        );
        curr.editorOpen = false;
        if (
            result.type == "save" &&
            result.text != curr.editedCellInitialContent
        ) {
            await this.saveCells(
                result.text,
                docId,
                parId,
                curr.row,
                curr.col,
                this.selectedCells
            );
            // ctrl.cellDataMatrix[row][col] = result.text
            curr.editedCellContent = result.text;
            this.closeSmallEditor();
        }
        if (result.type == "cancel") {
            this.closeSmallEditor();
        }
        if (isPrimitiveCell(cell)) {
        } else {
            cell.editorOpen = false;
        }
    }

    /**
     * Opens advanced editor
     * @param {CellEntity} cell Opened cell
     * @param curr Current cell info.
     */
    private async openBigEditorNow(cell: CellEntity, curr: ICurrentCell) {
        const parId = this.getOwnParId();
        if (parId === undefined || !this.viewctrl) {
            return;
        }
        await this.openBigEditorAsync(
            cell,
            this.viewctrl.item.id,
            this.getCellContentString(curr.row, curr.col),
            parId,
            curr
        );
    }

    /**
     * Opens advanced editor
     */
    async handleClickOpenBigEditor() {
        if (!this.currentCell || this.currentCell.editorOpen) {
            return;
        }

        const modal: CellEntity = {
            cell: this.currentCell.editedCellContent,
        };
        await this.openBigEditorNow(modal, this.currentCell);
        this.c();
    }

    /**
     * Check if defcols that columns array has as many items than celldatamatrix
     */
    private ensureColumns() {
        if (!this.data.table) {
            return;
        }
        if (!this.data.table.defcols && !this.data.table.columns) {
            return;
        }
        if (this.data.table.defcols) {
            const n = this.cellDataMatrix[0].length;
            if (!this.data.table.columns) {
                this.data.table.columns = [];
            }
            for (let i = this.data.table.columns.length; i < n; i++) {
                this.data.table.columns.push({});
            }
        }
        this.columns = Object.assign([], this.data.table.columns);
    }

    /**
     * Initialize celldatamatrix with the values from yaml and yaml only
     */
    private initializeCellDataMatrix(clearSort: ClearSort = ClearSort.Yes) {
        if (!this.data.table) {
            this.data.table = {};
        }
        if (!this.data.table.rows) {
            this.data.table.rows = [];
        }

        let ncols = this.data.table.countCol ?? 0;
        const nrows = Math.max(
            this.data.table.rows.length,
            this.data.table.countRow ?? 0
        );

        for (const row of this.data.table.rows) {
            if (row.row) {
                ncols = Math.max(row.row.length, ncols);
            }
        }
        this.cellDataMatrix.splice(nrows); // truncate matrix in case the number of rows has decreased
        for (let iy = 0; iy < nrows; iy++) {
            if (
                !this.cellDataMatrix[iy] ||
                this.cellDataMatrix[iy].length != ncols
            ) {
                this.cellDataMatrix[iy] = [];
                for (let ix = 0; ix < ncols; ix++) {
                    this.cellDataMatrix[iy][ix] = this.createDummyCell(iy, ix);
                }
            }
            const row = this.data.table.rows[iy];
            if (!row || !row.row) {
                continue;
            }
            for (let ix = 0; ix < row.row.length; ix++) {
                const itemInRow = row.row[ix];
                const cell = this.cellDataMatrix[iy][ix];
                cell.styleCache = undefined;
                for (const k of Object.keys(cell)) {
                    if (k == "cell") {
                        continue;
                    }
                    delete cell[k];
                }
                this.applyCellEntityAttributesToICell(itemInRow, cell);
            }
        }
        this.ensureColumns();
        if (clearSort == ClearSort.Yes) {
            this.clearSortOrder();
        }
    }

    /**
     * Applies a cell entity's possible attributes to an ICell instance.
     * @param {CellEntity} sourceCell The source CellEntity from which the attributes are taken.
     * @param {ICell} targetCell The ICell instance to which the attributes are applied to.
     */
    private applyCellEntityAttributesToICell(
        sourceCell: CellEntity,
        targetCell: ICell
    ) {
        targetCell.styleCache = undefined;
        if (isPrimitiveCell(sourceCell)) {
            targetCell.cell = this.cellToString(sourceCell);
            return;
        }

        for (const [key, value] of Object.entries(sourceCell)) {
            if (value != null) {
                targetCell[key] = value;
            }
        }
        if (targetCell.cell == null) {
            targetCell.cell = "";
        }
    }

    // noinspection JSMethodCanBeStatic
    /**
     * Transforms cell to string
     * @param {CellType} cell Changed cell
     * @returns {string}
     */
    private cellToString(cell: CellType) {
        if (cell == null) {
            return "";
        }
        return cell.toString();
    }

    /**
     * Returns a cell's content from the datablock.
     * @param {number} rowi: Table row index
     * @param {number} coli: Table column index
     * @returns {string | string}
     */
    private getCellContentString(rowi: number, coli: number) {
        return this.cellToString(this.cellDataMatrix[rowi][coli].cell);
    }

    /**
     * Combines datablock data
     * @param {ICellDataEntity} cells: cells part of tabledatablock
     */
    public processDataBlock(cells: ICellDataEntity) {
        const alphaRegExp = /([A-Z]*)/;
        for (const [item, value] of Object.entries(cells)) {
            const alpha = alphaRegExp.exec(item);

            if (alpha == null) {
                continue;
            }
            const numberPlace = item.substring(alpha[0].length);

            const address = TimTableComponent.getAddress(alpha[0], numberPlace);
            if (this.checkThatAddIsValid(address)) {
                this.setValueToMatrix(address.row, address.col, value);
            }
        }
    }

    /**
     * Combines datablock data with YAML table data.
     * Also processes rowspan and colspan and sets the table up for rendering.
     */
    private processDataBlockAndSpanInfo() {
        if (this.data.table.tabledatablock) {
            // reads tabledatablock and sets all values to datacellmatrix
            this.processDataBlock(this.data.table.tabledatablock.cells);
        }

        // Process cell col/rowspan and figure out which cells should be rendered as part of another cell
        // (or, in terms of HTML, should not be rendered at all)

        for (let y = 0; y < this.cellDataMatrix.length; y++) {
            const row = this.cellDataMatrix[y];

            if (!row) {
                continue;
            }

            let renderIndexX = 0;

            for (let x = 0; x < row.length; x++) {
                const cell = row[x];
                if (cell.underSpanOf) {
                    continue;
                }
                if (cell.cell == "None") {
                    cell.cell = "";
                }
                cell.renderIndexX = renderIndexX;
                cell.renderIndexY = y;
                renderIndexX++;

                const colspan = cell.colspan ? cell.colspan : 1;
                const rowspan = cell.rowspan ? cell.rowspan : 1;

                if (colspan === 1 && rowspan === 1) {
                    continue;
                } // might enhance performance?

                for (let spanCellY = 0; spanCellY < rowspan; spanCellY++) {
                    if (y + spanCellY >= this.cellDataMatrix.length) {
                        break;
                    }

                    const spanRow = this.cellDataMatrix[y + spanCellY];

                    for (let spanCellX = 0; spanCellX < colspan; spanCellX++) {
                        if (spanCellY == 0 && spanCellX == 0) {
                            continue;
                        }
                        if (x + spanCellX >= spanRow.length) {
                            break;
                        }

                        const spanCell = spanRow[x + spanCellX];
                        if (spanCell.underSpanOf) {
                            break;
                        }

                        spanCell.underSpanOf = {row: y, col: x};
                    }
                }
            }
        }
    }

    // noinspection JSMethodCanBeStatic
    /**
     * Coordinates validation
     * @param {{col: number, row: number}} address row and column index
     * @returns {boolean} true if valid
     */
    checkThatAddIsValid(address: ICellCoord): boolean {
        return address.col >= 0 && address.row >= 0;
    }

    /**
     * Get placement, ex. A1 -> 0,0
     * ex. C5 -> 2,4
     * @param {string} colValue Column value, ex. 'A'
     * @param {string} rowValue  Row value, ex. '1'
     * @returns {{col: number, row: number}} Coordinates as index numbers
     */
    static getAddress(colValue: string, rowValue: string) {
        const charCodeOfA = "A".charCodeAt(0);
        const asciiCharCount = 26;
        let reversedCharacterPlaceInString = 0;
        let columnIndex = 0;
        for (let charIndex = colValue.length - 1; charIndex >= 0; charIndex--) {
            columnIndex +=
                (colValue.charCodeAt(charIndex) - charCodeOfA + 1) *
                Math.pow(asciiCharCount, reversedCharacterPlaceInString);
            reversedCharacterPlaceInString++;
        }
        columnIndex = columnIndex - 1;
        const rowIndex = parseInt(rowValue, 10) - 1;
        return {col: columnIndex, row: rowIndex};
    }

    /**
     * Sets a value to specific index in cellDataMatrix
     * @param {number} row Row index
     * @param {number} col Column index
     * @param {string} value Stored value
     */
    private setValueToMatrix(row: number, col: number, value: CellEntity) {
        if (row >= this.cellDataMatrix.length) {
            this.resizeCellDataMatrixHeight(row + 1);
        }
        if (col >= this.cellDataMatrix[row].length) {
            this.resizeRowWidth(row, col + 1);
        }

        this.applyCellEntityAttributesToICell(
            value,
            this.cellDataMatrix[row][col]
        );
    }

    /**
     * Increases the height of the cell data matrix to the specified number.
     * @param {number} length The new height of the cell data matrix.
     */
    private resizeCellDataMatrixHeight(length: number) {
        for (let i = this.cellDataMatrix.length; i < length; i++) {
            this.cellDataMatrix[i] = [];
            if (i < 1) {
                continue;
            }
            for (let j = 0; j < this.cellDataMatrix[i - 1].length; j++) {
                this.cellDataMatrix[i][j] = this.createDummyCell(i, j);
            }
        }
    }

    /**
     * Increases the width of a row in the cell data matrix.
     * @param {number} rowIndex The index of the row to expand.
     * @param {number} width The new width of the row.
     */
    private resizeRowWidth(rowIndex: number, width: number) {
        for (const row of this.cellDataMatrix) {
            for (let i = row.length; i < width; i++) {
                row[i] = this.createDummyCell(rowIndex, i);
            }
        }
        this.ensureColumns();
    }

    // noinspection JSMethodCanBeStatic
    /**
     * Creates and returns an "empty" ICell with no content.
     * @returns {{cell: string}}
     */
    private createDummyCell(r: number, c: number): ICell {
        const cell: ICell = {cell: ""};
        cell.renderIndexY = r;
        cell.renderIndexX = c;
        return cell;
    }

    /**
     * Deals with key events
     * @param {KeyboardEvent} ev Pressed key event
     */
    async handleKeyUpSmallEditor(ev: KeyboardEvent) {
        // Arrow keys
        let ch: ChangeDetectionHint;
        if (ev.ctrlKey && isArrowKey(ev)) {
            ch = await this.handleArrowMovement(ev);
        } else {
            ch = await this.doKeyUpTable(ev);
        }
        if (ch == ChangeDetectionHint.NeedToTrigger) {
            this.c();
        }
    }

    async smallEditorLostFocus(_ev: unknown) {
        if (this.hide.editorButtons) {
            // Autosave when no editorButtons
            await this.saveCurrentCell();
            // this.closeSmallEditor();
            // TODO: can not use this, because then save is done also for Cancel, BigEditor, Toolbar buttons and so on
        }
        // hideToolbar(this);
    }

    handleClickEditorPosition() {
        copyToClipboard(this.editorPosition);
    }

    handleClickCancelSmallEditor() {
        if (this.currentCell?.editorOpen) {
            return;
        }
        this.closeSmallEditor();
        this.c();
    }

    async handleClickAcceptSmallEditor() {
        if (this.currentCell?.editorOpen) {
            return;
        }
        await this.saveAndCloseSmallEditor();
        this.c();
    }

    private keyDownTable = (ev: KeyboardEvent) => {
        this.shiftDown = ev.shiftKey;

        // if (!this.mouseInTable) return;
        // TODO: Check properly if table has focus when preventing default tab behavior

        // Prevents unwanted scrolling in Firefox.
        if (ev.ctrlKey && isArrowKey(ev)) {
            ev.preventDefault();
        }

        if (ev.ctrlKey && ev.key === "s") {
            this.saveAndCloseSmallEditor();
            this.save();
            ev.preventDefault();
            this.c();
            return; //  ChangeDetectionHint.NeedToTrigger;
        }

        if (!this.isSomeCellBeingEdited()) {
            return;
        }
        if (isKeyCode(ev, KEY_TAB)) {
            ev.preventDefault();
        }
    };

    private keyPressTable = (ev: KeyboardEvent) => {
        // if (!this.mouseInTable) return;
        if (isKeyCode(ev, KEY_TAB)) {
            ev.preventDefault();
        }
    };

    /**
     * Deals with key events inside the table.
     * @param {KeyboardEvent} ev KeyboardEvent
     */
    private keyUpTable = async (ev: KeyboardEvent) => {
        // this.shiftDown = ev.shiftKey;
        this.shiftDown = ev.shiftKey;

        if (this.isSomeCellBeingEdited()) {
            return;
        } // if active, then the smalleditor has listener
        // if (this.mouseInTable) { }
        if (await this.doKeyUpTable(ev)) {
            this.c();
        }
    };

    /**
     * Handles key up event.
     * @param ev
     */
    private async doKeyUpTable(
        ev: KeyboardEvent
    ): Promise<ChangeDetectionHint> {
        if (isKeyCode(ev, KEY_F2)) {
            // TODO: change all other keys like this to avoid deprecated warnings
            if (this.hide.edit) {
                return ChangeDetectionHint.DoNotTrigger;
            }
            const modal: CellEntity = {
                cell: "",
            };
            if (this.currentCell != undefined && !this.currentCell.editorOpen) {
                await this.openBigEditorNow(modal, this.currentCell);
                return ChangeDetectionHint.NeedToTrigger;
            }

            // If no cell is being edited, open the active cell for editing.
            if (this.activeCell != undefined) {
                await this.openCell(this.activeCell.row, this.activeCell.col);
                return ChangeDetectionHint.NeedToTrigger;
            }
        } else if (isKeyCode(ev, KEY_ENTER)) {
            if (!this.isInEditMode() || !this.viewctrl) {
                return ChangeDetectionHint.DoNotTrigger;
            }
            ev.preventDefault();

            if (ev.shiftKey) {
                await this.doCellMovement(Direction.Up);
                this.disableStartCell();
                return ChangeDetectionHint.NeedToTrigger;
            }

            const parId = this.getOwnParId();
            if (parId && this.currentCell !== undefined) {
                await this.doCellMovement(Direction.Down);
                return ChangeDetectionHint.NeedToTrigger;
            }

            if (this.activeCell) {
                await this.openCell(this.activeCell.row, this.activeCell.col);
                return ChangeDetectionHint.NeedToTrigger;
            }
        } else if (isKeyCode(ev, KEY_TAB)) {
            ev.preventDefault();
            if (this.currentCell != undefined) {
                if (ev.shiftKey) {
                    await this.doCellMovement(Direction.Left);
                    this.disableStartCell();
                } else {
                    await this.doCellMovement(Direction.Right);
                }
                return ChangeDetectionHint.NeedToTrigger;
            }
        } else if (isKeyCode(ev, KEY_ESC)) {
            ev.preventDefault();
            if (this.currentCell) {
                this.currentCell = undefined;
                return ChangeDetectionHint.NeedToTrigger;
            }
        } else if (handleToolbarKey(ev, this.data.toolbarTemplates)) {
            ev.preventDefault();
            return ChangeDetectionHint.NeedToTrigger;
        } else if (
            !this.currentCell &&
            (ev.ctrlKey || ev.altKey) &&
            isArrowKey(ev)
        ) {
            if (await this.handleArrowMovement(ev)) {
                ev.preventDefault();
                return ChangeDetectionHint.NeedToTrigger;
            }
        }
        return ChangeDetectionHint.DoNotTrigger;
    }

    /**
     * Handles arrow movement inside table.
     * @param {KeyboardEvent} ev Keyboardevent
     */
    private async handleArrowMovement(
        ev: KeyboardEvent
    ): Promise<ChangeDetectionHint> {
        const parId = this.getOwnParId();
        if (
            !(this.editing || this.task) ||
            !this.viewctrl ||
            !parId ||
            this.currentCell?.editorOpen
        ) {
            return ChangeDetectionHint.DoNotTrigger;
        }

        const keyCode = getKeyCode(ev);
        if (keyCode === KEY_DOWN) {
            ev.preventDefault();
            return this.doCellMovement(Direction.Down, true);
        } else if (keyCode === KEY_RIGHT) {
            ev.preventDefault();
            return this.doCellMovement(Direction.Right, true);
        } else if (keyCode === KEY_LEFT) {
            ev.preventDefault();
            return this.doCellMovement(Direction.Left, true);
        } else if (keyCode === KEY_UP) {
            ev.preventDefault();
            return this.doCellMovement(Direction.Up, true);
        }
        return ChangeDetectionHint.DoNotTrigger;
    }

    private async doCellMovementN(
        n: number,
        direction: Direction,
        needLastDir?: boolean
    ) {
        for (let i = 0; i < n; i++) {
            await this.doCellMovement(direction, needLastDir, true);
        }
    }

    /**
     * Switches the edit mode to another cell relative to either the current
     * or last edited cell.
     * @param direction The direction that the cell edit mode should move to.
     * @param needLastDir Whether to read x/y from previous direction
     * @param forceOne Prevent selection even Shift is down
     */
    private async doCellMovement(
        direction: Direction,
        needLastDir?: boolean,
        forceOne: boolean = false
    ): Promise<ChangeDetectionHint> {
        if (!this.activeCell) {
            return ChangeDetectionHint.DoNotTrigger;
        }
        let x = this.activeCell.col;
        let y = this.activeCell.row;
        if (this.lastDirection && needLastDir) {
            if (UP_OR_DOWN.includes(this.lastDirection.direction)) {
                if (UP_OR_DOWN.includes(direction)) {
                    x = this.lastDirection.coord;
                }
            } else {
                if (LEFT_OR_RIGHT.includes(direction)) {
                    y = this.lastDirection.coord;
                }
            }
        }
        let nextCell = this.getNextCell(x, y, direction);
        let prevX = x;
        let prevY = y;
        let i = 0;
        const maxIters =
            this.cellDataMatrix.length * this.cellDataMatrix[0].length;
        // Iterate towards direction until next non-locked cell in a non-hidden row or column is found
        // or until iterator arrives at the same cell (or iteration gets stuck)
        while (nextCell && i < maxIters) {
            i++;
            if (i > maxIters) {
                this.error = "Error finding next cell";
                return ChangeDetectionHint.NeedToTrigger;
            }
            if (nextCell.row == y && nextCell.col == x) {
                break;
            }
            // When going right: if returned col was not to the right, then go to next row and restart from col 0.
            // Apply similar logic to other directions.
            if (
                direction == Direction.Right &&
                nextCell &&
                nextCell.col <= prevX
            ) {
                prevY = this.constrainRowIndex(prevY + 1);
                prevX = 0;
                nextCell = {row: prevY, col: prevX};
            }
            if (
                direction == Direction.Left &&
                nextCell &&
                nextCell.col >= prevX
            ) {
                prevY = this.constrainRowIndex(prevY - 1);
                prevX = this.cellDataMatrix[prevY].length - 1;
                nextCell = {row: prevY, col: prevX};
            }
            if (
                direction == Direction.Down &&
                nextCell &&
                nextCell.row <= prevY
            ) {
                prevY = 0;
                prevX = this.constrainColumnIndex(prevY, prevX + 1);
                nextCell = {row: prevY, col: prevX};
            }
            if (
                direction == Direction.Up &&
                nextCell &&
                nextCell.row >= prevY
            ) {
                prevY = this.cellDataMatrix.length - 1;
                prevX = this.constrainColumnIndex(prevY, prevX - 1);
                nextCell = {row: prevY, col: prevX};
            }

            // Stop iterating if cell is not in hiddenRows/hiddenColumns and is not locked.
            if (
                !this.currentHiddenRows.has(nextCell.row) &&
                !this.data.hiddenColumns?.includes(nextCell.col) &&
                !this.data.lockedCells?.includes(
                    colnumToLetters(nextCell.col) + (nextCell.row + 1)
                ) &&
                !this.data.lockedColumns?.includes(
                    colnumToLetters(nextCell.col)
                )
            ) {
                break;
            }
            nextCell = this.getNextCell(nextCell.col, nextCell.row, direction);
        }

        if (!nextCell) {
            return ChangeDetectionHint.DoNotTrigger;
        }

        if (this.currentCell) {
            await this.openCell(nextCell.row, nextCell.col, forceOne);
            return ChangeDetectionHint.NeedToTrigger;
        }

        this.setActiveCell(nextCell.row, nextCell.col, forceOne);
        return ChangeDetectionHint.NeedToTrigger;
    }

    /**
     * Gets the next cell in a given direction from a cell.
     * Takes rowspan and colspan into account.
     * @param x The original X coordinate (column index) of the source cell.
     * @param y The original Y coordinate (original row index) of the source cell.
     * @param direction The direction.
     */
    private getNextCell(
        x: number,
        y: number,
        direction: Direction
    ): ICellCoord | null {
        let sourceCell = this.cellDataMatrix[y][x];
        while (sourceCell.underSpanOf) {
            sourceCell =
                this.cellDataMatrix[sourceCell.underSpanOf.row][
                    sourceCell.underSpanOf.col
                ];
        }

        let nextRow;
        let nextColumn;
        let cell;
        const sy = this.permTableToScreen[y]; // index in screen
        switch (direction) {
            case Direction.Up:
                nextRow = this.constrainRowIndex(sy - 1);
                nextColumn = this.constrainColumnIndex(nextRow, x);
                nextRow = this.permTable[nextRow];
                this.lastDirection = {direction: direction, coord: nextColumn};
                break;
            case Direction.Left:
                nextRow = this.constrainRowIndex(sy);
                nextColumn = this.constrainColumnIndex(nextRow, x - 1);
                nextRow = this.permTable[nextRow];
                this.lastDirection = {direction: direction, coord: nextRow};
                break;
            case Direction.Down:
                const sourceRowspan = sourceCell.rowspan
                    ? sourceCell.rowspan
                    : 1;
                nextRow = this.constrainRowIndex(sy + sourceRowspan);
                nextColumn = this.constrainColumnIndex(nextRow, x);
                nextRow = this.permTable[nextRow];
                this.lastDirection = {direction: direction, coord: nextColumn};
                break;
            case Direction.Right:
                const sourceColspan = sourceCell.colspan
                    ? sourceCell.colspan
                    : 1;
                nextRow = this.constrainRowIndex(sy);
                nextColumn = this.constrainColumnIndex(
                    nextRow,
                    x + sourceColspan
                );
                nextRow = this.permTable[nextRow];
                this.lastDirection = {direction: direction, coord: nextRow};
                break;
            default:
                return null;
        }

        cell = this.cellDataMatrix[nextRow][nextColumn];
        while (cell.underSpanOf) {
            nextRow = cell.underSpanOf.row;
            nextColumn = cell.underSpanOf.col;
            cell = this.cellDataMatrix[nextRow][nextColumn];
        }

        return {row: nextRow, col: nextColumn};
    }

    private constrainRowIndex(rowIndex: number) {
        if (rowIndex >= this.cellDataMatrix.length) {
            return 0;
        }
        if (rowIndex < 0) {
            return this.cellDataMatrix.length - 1;
        }
        return rowIndex;
    }

    private constrainColumnIndex(rowIndex: number, columnIndex: number) {
        const row = this.cellDataMatrix[rowIndex];
        if (columnIndex >= row.length) {
            return 0;
        }
        if (columnIndex < 0) {
            return row.length - 1;
        }

        return columnIndex;
    }

    private setActiveCell(
        rowi: number,
        coli: number,
        forceOne: boolean = false
    ) {
        this.clearSmallEditorStyles();

        let cell = this.cellDataMatrix[rowi][coli];
        while (cell.underSpanOf) {
            rowi = cell.underSpanOf.row;
            coli = cell.underSpanOf.col;
            cell = this.cellDataMatrix[rowi][coli];
        }
        this.activeCell = {row: rowi, col: coli};
        if (!this.shiftDown || forceOne) {
            this.startCell = {row: rowi, col: coli};
        }
        this.selectedCells = this.getSelectedCells(rowi, coli);
        this.openToolbar();

        if (
            cell.renderIndexX === undefined ||
            cell.renderIndexY === undefined
        ) {
            return; // we should never be able to get here
        }
        if (this.dataViewComponent) {
            this.dataViewComponent.markCellsSelected(this.selectedCells.cells);
        } else {
            const sr = this.permTableToScreen[rowi];
            const table = $(this.tableElem.nativeElement);
            // const ry = this.permTable[cell.renderIndexY];
            const tablecell = table
                .children("tbody")
                .last()
                .children("tr")
                .eq(sr + this.rowDelta)
                .children("td")
                .eq(cell.renderIndexX + this.colDelta);

            const parent = $(this.timTableRunDiv.nativeElement);
            // tablecell[0].scrollIntoView(false);
            const h = tablecell.height();
            const w = tablecell.width();
            if (h != null && w != null) {
                scrollToViewInsideParent(
                    tablecell[0],
                    parent[0],
                    w,
                    3 * h,
                    w,
                    h
                );
            }
        }
        this.updateSmallEditorPosition(); // TODO: vesa added here, because somtiems it did not update the pos
    }

    /**
     * Clicks specified cell or hops opposite side of the table
     * @param {number} rowi Row index
     * @param {number} coli Column index
     * @param {boolean} forceOne should it force selection just one cell
     */
    private async openCell(
        rowi: number,
        coli: number,
        forceOne: boolean = false
    ) {
        /*
        const modal: CellEntity = {
            cell: "",
        };
        */
        rowi = this.constrainRowIndex(rowi);
        coli = this.constrainColumnIndex(rowi, coli);

        await this.openCellForEditing(rowi, coli, undefined, forceOne);
    }

    /**
     * Deals with cell clicking
     * @param {number} rowi Row index
     * @param {number} coli Column index
     * @param {MouseEvent} event If mouse was clikced
     */
    async handleClickCell(rowi: number, coli: number, event?: MouseEvent) {
        await this.openCellForEditing(rowi, coli, event);
        this.lastDirection = undefined;
        this.c();
    }

    /**
     * Opens a cell for editing.
     * @param rowi The row index.
     * @param coli The column index.
     * @param event The mouse event, if the cell was clicked.
     * @param forceOne should selection be forced to just one cell
     */
    private async openCellForEditing(
        rowi: number,
        coli: number,
        event?: MouseEvent,
        forceOne: boolean = false
    ) {
        if (this.isPreview()) {
            return;
        }

        const parId = this.getOwnParId();

        if (
            !this.isInEditMode() ||
            !this.viewctrl ||
            !parId ||
            this.currentCell?.editorOpen
        ) {
            return;
        }

        if (this.currentCell) {
            if (
                this.currentCell.row === rowi &&
                this.currentCell.col === coli &&
                this.currentCell.editorOpen
            ) {
                return;
            }
        }

        const cellCol = colnumToLetters(coli);
        const cellCoordinate = cellCol + (rowi + 1);
        this.editorPosition = cellCoordinate;
        if (
            this.data.lockedCells?.includes(cellCoordinate) ||
            this.data.lockedColumns?.includes(cellCol)
        ) {
            return;
        }

        const activeCell = this.activeCell;
        if (this.hide.edit) {
            // if hide-attr contains edit, then no edit
            this.setActiveCell(rowi, coli);
            return;
        }
        const isCurrentCell = !!this.currentCell;
        if (
            isCurrentCell ||
            this.noNeedFirstClick ||
            (activeCell && activeCell.row === rowi && activeCell.col === coli)
        ) {
            if (this.currentCell) {
                const newvalue = this.currentCell.editedCellContent;
                if (this.currentCell.editedCellInitialContent != newvalue) {
                    await this.saveCurrentCell();
                }
            }
            let value;
            if (!this.task) {
                value = await this.getCellData(
                    this.viewctrl.item.id,
                    parId,
                    rowi,
                    coli
                );
            } else {
                value = this.getCellContentString(rowi, coli);
            }
            if (true || isToolbarOpen()) {
                // TODO: why toolbar must be open?
                this.currentCell = {
                    row: rowi,
                    col: coli,
                    editorOpen: false,
                    editedCellContent: value,
                    editedCellInitialContent: value,
                };
                this.getPar()?.addClass("live-update-pause");
            }
            // XXXX
            // Workaround: For some reason, if initial value is empty, the model is not always reflected in DOM.
            this.shouldSelectInputText = true;
            if (this.editInput) {
                this.editInput.nativeElement.value = value;
            }
        }
        try {
            this.setActiveCell(rowi, coli, forceOne);
        } catch {
            // TODO: why here, see DataViewComponent.getDataCell  get negative rows???
        }
    }

    /**
     * Saves the possible currently edited cell.
     */
    private async saveCurrentCell() {
        const parId = this.getOwnParId();

        if (this.viewctrl && parId && this.currentCell != undefined) {
            const value = this.currentCell.editedCellContent;

            if (this.currentCell.editedCellInitialContent != value) {
                await this.saveCells(
                    value,
                    this.viewctrl.item.id,
                    parId,
                    this.currentCell.row,
                    this.currentCell.col,
                    this.selectedCells
                );
                return true;
            }
        }
        return false;
    }

    /*
    private async saveToCell(cell: ICellCoord | undefined, value: string, selectedCells: ISelectedCells) {
        if (!this.viewctrl || !cell) {
            return;
        }
        const parId = this.getOwnParId();
        if (!parId) {
            return;
        }

        const docId = this.viewctrl.item.id;
        const rowId = cell.row;
        const colId = cell.col;

        await this.saveCells(value, docId, parId, rowId, colId, selectedCells);
        return true;
    }
    */

    /*
    private async saveToCurrentCell(value: string) {
        return this.saveToCell(this.activeCell, value, this.selectedCells);
    }
    */

    /**
     * Updates position of the small cell editor (if it is open).
     */
    private updateSmallEditorPosition() {
        const editInputElement = this.getEditInputElement();
        if (!this.currentCell || !editInputElement) {
            return;
        }
        if (this.dataViewComponent) {
            this.dataViewComponent.updateEditorPosition(
                this.currentCell.row,
                this.currentCell.col
            );
            return;
        }
        let rowi = this.currentCell.row;
        let coli = this.currentCell.col;
        const sr = this.permTableToScreen[rowi];
        const table = $(this.tableElem.nativeElement);
        if (rowi >= this.cellDataMatrix.length) {
            rowi--;
            if (rowi < 0) {
                return;
            }
        }
        if (coli >= this.cellDataMatrix[rowi].length) {
            coli--;
            if (coli < 0) {
                return;
            }
        }
        const cell = this.cellDataMatrix[rowi][coli];
        if (
            cell.renderIndexX === undefined ||
            cell.renderIndexY === undefined
        ) {
            return; // we should never be able to get here
        }
        // const ry = this.permTable[cell.renderIndexY];
        const tablecell = table
            .children("tbody")
            .last()
            .children("tr")
            .eq(sr + this.rowDelta)
            .children("td")
            .eq(cell.renderIndexX + this.colDelta);
        // const tableCellOffset = tablecell.offset(); // this gave a wrong result?
        const tableCellOffset = table
            .children("tbody")
            .last()
            .children("tr")
            .eq(sr + this.rowDelta)
            .children("td")
            .eq(cell.renderIndexX + this.colDelta)
            .offset();

        if (!tableCellOffset) {
            return;
        }
        if (!table) {
            return;
        }

        const inlineEditorDiv = $(this.editorDiv.nativeElement);
        inlineEditorDiv.height(1);
        inlineEditorDiv[0].style.position = "relative";
        const toff = table.offset()!;
        inlineEditorDiv.offset({
            left: toff.left,
            top: toff.top + table.height()!,
        });
        const editinp = $(editInputElement);

        if (this.data.editorBottom) {
            return;
        }
        editinp.offset(tableCellOffset);

        const editOffset = editinp.offset();
        const tableCellWidth = tablecell.innerWidth();
        const minEditWidth = 20;

        let editOuterWidth;
        if (tableCellWidth) {
            editOuterWidth = Math.max(minEditWidth, tableCellWidth);
        } else {
            editOuterWidth = minEditWidth;
        }

        editinp.width(editOuterWidth);
        editinp.height(tablecell.innerHeight()! - 2);

        const inlineEditorButtons = $(this.editorButtons.nativeElement);
        if (this.data.editorButtonsBottom) {
            inlineEditorButtons.offset({
                left: toff.left,
                top: toff.top + table.height()! + 5,
            });
            return;
        }
        if (this.data.editorButtonsRight) {
            inlineEditorButtons.offset({
                left: tableCellOffset.left + editOuterWidth + 5,
                top: tableCellOffset.top + 5,
            });
            return;
        }
        const editOuterHeight = editinp.outerHeight();
        const buttonOpenBigEditor = this.buttonOpenBigEditor.nativeElement;
        const h1 = buttonOpenBigEditor.offsetHeight;
        const h = tablecell.outerHeight() ?? 20;
        if (
            editOffset &&
            editOuterHeight &&
            tableCellOffset &&
            editOuterWidth
        ) {
            const mul = sr == 0 ? 1 : 2;
            inlineEditorButtons.offset({
                left: tableCellOffset.left,
                // top: (cell2y ? cell2y : editOffset.top) - h - 5,
                top: editOffset.top - mul * h + (h - h1), //  - 5,
            });
        }
    }

    private focusSmallEditor() {
        const editInputElement = this.getEditInputElement();
        if (!editInputElement) {
            return;
        }
        editInputElement.focus();
        if (!this.hide.select) {
            editInputElement.setSelectionRange(
                0,
                editInputElement.value.length
            );
        }
    }

    classForCell(rowi: number, coli: number) {
        let cls = this.cellDataMatrix[rowi][coli].class;
        if (!cls) {
            cls = "";
        }
        cls += this.isActiveCell(rowi, coli) ? " activeCell" : "";
        return cls;
        //                                [class.activeCell]="isActiveCell(rowi, coli)"
    }

    /**
     * Sets style attributes for cells
     * @param {number} rowi Table row index
     * @param {number} coli Table column index
     */
    stylingForCell(rowi: number, coli: number) {
        const sc = this.cellDataMatrix[rowi][coli].styleCache;
        if (sc !== undefined) {
            return sc;
        }
        const styles = this.stylingForCellOfColumn(coli);

        if (this.getCellContentString(rowi, coli) === "") {
            styles.height = "2em";
            styles.width = "1.5em";
        }

        const def = this.data.table.defcells;
        if (def) {
            this.cellDataMatrix[rowi][coli].class = this.applyStyle(
                styles,
                def,
                cellStyles
            );
        }

        const defrange = this.data.table.defcellsrange;
        if (defrange) {
            const rown = this.cellDataMatrix.length;
            const coln = this.cellDataMatrix[0].length;
            for (const dr of defrange) {
                const r = dr.validrange ?? this.checkRange(dr.range);
                dr.validrange = r;
                if (this.checkIndex2(r, rown, coln, rowi, coli)) {
                    this.applyStyle(styles, dr.def, columnStyles);
                }
            }
        }

        const cell = this.cellDataMatrix[rowi][coli];

        this.cellDataMatrix[rowi][coli].class = this.applyStyle(
            styles,
            cell,
            cellStyles
        );

        if (this.data.maxWidth) {
            styles["max-width"] = this.data.maxWidth;
            styles.overflow = "hidden";
        }
        if (this.data.minWidth) {
            styles["min-width"] = this.data.minWidth;
        }
        if (this.data.singleLine) {
            styles["white-space"] = "nowrap";
        }
        cell.styleCache = styles;
        return styles;
    }

    /**
     * Parses cell style attributes for a column
     * @param {number} coli The index of the column
     */
    private stylingForCellOfColumn(coli: number) {
        const styles: Record<string, string> = {};
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

        this.applyStyle(styles, col, columnCellStyles);
        return styles;
    }

    /**
     * Makex r[i] to index if possible, otherwise return def
     * @param r ange to check
     * @param i index to take from r
     * @param n max value
     * @param def in case no item
     */
    private static toIndex(
        r: readonly number[],
        i: number,
        n: number,
        def: number
    ) {
        if (r.length <= i) {
            return def;
        }
        let idx = r[i];
        if (idx < 0) {
            idx = n + idx;
        }
        if (idx < 0) {
            idx = 0;
        }
        if (idx >= n) {
            idx = n - 1;
        }
        return idx;
    }

    // noinspection JSMethodCanBeStatic
    /**
     * Checks if dr.range is valid range.  If it is string, change it to array.
     * To prevent another check, mark it checked.
     * @param dr default range to check
     */
    private checkRange(
        dr: readonly number[] | number | string | undefined
    ): readonly number[] | undefined {
        const r = dr;
        if (!r) {
            return;
        }
        if (typeof r === "number") {
            return [r];
        }
        if (typeof r !== "string") {
            return r;
        }

        const json = "[" + r.replace("[", "").replace("]", "") + "]";
        try {
            const parsed = JSON.parse(json);
            if (t.array(t.number).is(parsed)) {
                return parsed;
            } else {
                return [];
            }
        } catch (e) {
            return [];
        }
    }

    // noinspection JSMethodCanBeStatic
    /**
     * Check if index is between r[0]-r[1] where negative means i steps backward
     * @param r range to check, may be like [1,-1]
     * @param rown max_value
     * @param coln max_value
     * @param rowi index to check
     * @param coli index to check
     */
    private checkIndex2(
        r: readonly number[] | undefined,
        rown: number,
        coln: number,
        rowi: number,
        coli: number
    ): boolean {
        if (!r) {
            return false;
        }
        if (r.length == 0) {
            return false;
        }
        const ir1 = TimTableComponent.toIndex(r, 0, rown, 0);
        if (rowi < ir1) {
            return false;
        }
        const ic1 = TimTableComponent.toIndex(r, 1, coln, 0);
        if (coli < ic1) {
            return false;
        }
        const ir2 = TimTableComponent.toIndex(r, 2, rown, ir1);
        if (ir2 < rowi) {
            return false;
        }
        const ic2 = TimTableComponent.toIndex(r, 3, coln, ic1);
        return ic2 >= coli;
    }

    // noinspection JSMethodCanBeStatic
    /**
     * Check if index is between r[0]-r[1] where negative means i steps backward
     * @param r range to check, may be like [1,-1]
     * @param n max_value
     * @param index index to check
     */
    private checkIndex(
        r: readonly number[] | undefined,
        n: number,
        index: number
    ): boolean {
        if (!r) {
            return false;
        }
        if (r.length == 0) {
            return false;
        }
        const i1 = TimTableComponent.toIndex(r, 0, n, 0);
        if (index < i1) {
            return false;
        }
        const i2 = TimTableComponent.toIndex(r, 1, n, i1);
        return i2 >= index;
    }

    /**
     * Sets style attributes for columns
     * @param {IColumn} col The column to be styled
     * @param index col index
     */
    stylingForColumn(col: IColumn, index: number) {
        /*
        if (this.data.nrColumn) {
            index--;
            if (index < 0) { return; }
        }
        */

        const styles: Record<string, string> = {};

        const def = this.data.table.defcols;
        if (def) {
            this.applyStyle(styles, def, columnStyles);
        }

        const defrange = this.data.table.defcolsrange;
        if (defrange) {
            const n = this.cellDataMatrix[0].length;
            for (const dr of defrange) {
                const r = dr.validrange ?? this.checkRange(dr.range);
                dr.validrange = r;
                if (this.checkIndex(r, n, index)) {
                    this.applyStyle(styles, dr.def, columnStyles);
                }
            }
        }

        this.applyStyle(styles, col, columnStyles);
        return styles;
    }

    classForRow(rowi: number) {
        return this.rowClassCache.get(rowi);
    }

    /**
     * Sets style attributes for rows
     * @param {IRow} rowi The row to be styled
     */
    stylingForRow(rowi: number) {
        if (!this.data.table) {
            return emptyStyle;
        }
        const cached = this.rowStyleCache.get(rowi);
        if (cached) {
            return cached;
        }
        const styles: Record<string, string> = {};

        const def = this.data.table.defrows;
        if (def) {
            this.applyStyle(styles, def, rowStyles);
        }
        const defrange = this.data.table.defrowsrange;
        if (defrange) {
            // todo: do all this on init
            const n = this.cellDataMatrix.length;
            for (const dr of defrange) {
                const r = dr.validrange ?? this.checkRange(dr.range);
                dr.validrange = r;
                if (this.checkIndex(r, n, rowi)) {
                    this.applyStyle(styles, dr.def, rowStyles);
                }
            }
        }

        if (!this.data.table.rows || rowi >= this.data.table.rows.length) {
            return styles;
        }

        const row = this.data.table.rows[rowi];
        const cls = this.applyStyle(styles, row, rowStyles);
        this.rowStyleCache.set(rowi, styles);
        this.rowClassCache.set(rowi, cls);
        return styles;
    }

    /**
     * Sets style attributes for the whole table
     * @returns {{[p: string]: string}}
     */
    stylingForTable(tab: ITable) {
        const styles: Record<string, string> = {};
        this.applyStyle(styles, tab, tableStyles);
        return styles;
    }

    // noinspection JSMethodCanBeStatic
    /**
     * Generic function for setting style attributes.
     * Verifies that given style attributes are valid and applies them.
     * Non-valid style attributes are not applied.
     * @param styles The dictionary that will contain the final object styles
     * @param object The object that contains the user-given style attributes
     * @param validAttrs A set that contains the accepted style attributes
     * @return possible class
     */
    private applyStyle(
        styles: Record<string, string | number>,
        object: Record<string, unknown> | undefined,
        validAttrs: Set<string>
    ): string {
        if (!object) {
            return "";
        }
        let cls: string = "";
        for (const [key, value] of Object.entries(object)) {
            // At least fontSize needs to be a number, so we accept numbers too.
            if (key === "class") {
                cls = String(value);
                continue;
            }
            if (!validAttrs.has(key) || !StringOrNumber.is(value)) {
                continue;
            }

            const property = styleToHtml[key];
            if (!property) {
                continue;
            }

            styles[property] = value;
        }
        return cls;
    }

    /**
     * Toggles the table's edit mode on or off.
     */
    public async toggleEditMode() {
        await this.saveCurrentCell();
        this.currentCell = undefined;
        if (!this.editing) {
            this.editSave();
        }
        this.editing = !this.editing;
        this.c();
    }

    /**
     * Tells the server to add a new row into this table.
     */
    async addRow(rowId: number) {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        if (rowId == -1) {
            rowId = this.cellDataMatrix.length;
        }

        if (this.currentCell && rowId <= this.currentCell?.row) {
            this.closeSmallEditor();
        }

        if (this.task) {
            this.initUserData(this.userdata);
            const colCount = this.totalCols;
            // TODO: Move all previous rows if rowId is not the new row. We'd need to take into account locked rows.
            for (let col = 0; col < colCount; col++) {
                const coords = colnumToLetters(col) + (rowId + 1);
                this.userdata.cells[coords] = "";
            }
        } else if (this.isInGlobalAppendMode()) {
            const response = await to(
                $http.post<TimTable>("/timTable/addUserSpecificRow", {
                    docId,
                    parId,
                })
            );
            if (!response.ok) {
                return;
            }
            this.data = response.result.data;
        } else {
            const route = this.isInDataInputMode()
                ? "/timTable/addDatablockRow"
                : "/timTable/addRow";
            const response = await to(
                $http.post<TimTable>(route, {docId, parId, rowId})
            );
            if (!response.ok) {
                return;
            }
            this.data = response.result.data;
        }
        this.reInitialize();
    }

    /**
     * Tells the server to remove a row from this table.
     */
    async removeRow(rowId: number) {
        if (this.viewctrl == null || !this.data.table.rows) {
            return;
        }

        if (this.currentCell && rowId <= this.currentCell?.row) {
            this.closeSmallEditor();
        }

        const datablockOnly = this.isInDataInputMode() || this.task;

        if (rowId == -1) {
            if (datablockOnly) {
                rowId = this.cellDataMatrix.length - 1;
            } else {
                rowId = this.data.table.rows.length - 1;
            }
        }

        const docId = this.viewctrl.item.id;
        const parId = this.getOwnParId();

        if (rowId < 0 || this.cellDataMatrix.length < 2) {
            return;
        }

        if (this.task && this.userdata) {
            const colCount = this.totalCols;
            for (let col = 0; col < colCount; col++) {
                const coords = colnumToLetters(col) + (rowId + 1);
                delete this.userdata.cells[coords];
            }
        } else {
            const response = await to(
                $http.post<TimTable>("/timTable/removeRow", {
                    docId,
                    parId,
                    rowId,
                    datablockOnly,
                })
            );
            if (!response.ok) {
                return;
            }
            this.data = response.result.data;
        }
        this.reInitialize();
    }

    async handleClickAddColumn() {
        await this.addColumn(-1);
        this.c();
    }

    async handleClickRemoveColumn() {
        await this.removeColumn(-1);
        this.c();
    }

    async handleClickAddRow() {
        await this.addRow(-1);
        this.c();
    }

    async handleClickRemoveRow() {
        await this.removeRow(-1);
        this.c();
    }

    private initUserData(
        userData: DataEntity | undefined
    ): asserts userData is DataEntity {
        if (!this.userdata) {
            this.userdata = {
                type: "Relative",
                cells: {},
            };
        }
    }

    /**
     * Tells the server to add a new column into this table.
     */
    async addColumn(colId: number) {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        const rowLen = this.cellDataMatrix[0].length;
        if (colId < 0) {
            colId = rowLen;
        }

        if (this.currentCell && colId <= this.currentCell?.col) {
            this.closeSmallEditor();
        }

        if (this.task) {
            this.initUserData(this.userdata);
            const rowCount = this.totalRows;
            // TODO: Move all previous columns if colId is not the new column. We'd need to take into account hardcoded columns.
            for (let row = 0; row < rowCount; row++) {
                const coords = colnumToLetters(colId) + (row + 1);
                this.userdata.cells[coords] = "";
            }
        } else {
            const route = this.isInDataInputMode()
                ? "/timTable/addDatablockColumn"
                : "/timTable/addColumn";

            const response = await to(
                $http.post<TimTable>(route, {docId, parId, colId, rowLen})
            );
            if (!response.ok) {
                return;
            }
            this.data = response.result.data;
        }

        this.reInitialize();
    }

    /**
     * Tells the server to remove a column from this table.
     */
    async removeColumn(colId: number) {
        if (this.viewctrl == null) {
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;
        if (colId == -1) {
            colId = this.getColumnCount() - 1;
        }
        const datablockOnly = this.isInDataInputMode();

        if (colId < 0) {
            return;
        }

        if (this.currentCell && colId <= this.currentCell?.col) {
            this.closeSmallEditor();
        }

        if (this.task && this.userdata) {
            const rowCount = this.totalRows;
            for (let row = 0; row < rowCount; row++) {
                const coords = colnumToLetters(colId) + (row + 1);
                delete this.userdata.cells[coords];
            }
        } else {
            const response = await to(
                $http.post<TimTable>("/timTable/removeColumn", {
                    docId,
                    parId,
                    colId,
                    datablockOnly,
                })
            );
            if (!response.ok) {
                return;
            }
            this.data = response.result.data;
        }
        this.reInitialize(ClearSort.No);
    }

    /**
     * Initializes the cell data matrix, reads the data block and sets its values
     * to the cell data matrix.
     * Call this when the whole table's content is refreshed.
     */
    public reInitialize(clearSort: ClearSort = ClearSort.Yes) {
        this.initializeCellDataMatrix(clearSort);
        this.processDataBlockAndSpanInfo();
        this.initialRowCount = this.totalRows;
        this.initialColCount = this.totalCols;
        if (this.userdata) {
            this.processDataBlock(this.userdata.cells);
        } else {
            this.initUserData(this.userdata);
        }
        if (clearSort == ClearSort.Yes) {
            this.clearSortOrder();
        }
        this.dataViewComponent?.refresh();
    }

    /**
     * Calculates and returns the number of columns in the table.
     */
    private getColumnCount() {
        let highestCellCount = 0;

        this.cellDataMatrix.forEach((row) => {
            if (row.length > highestCellCount) {
                highestCellCount = row.length;
            }
        });

        return highestCellCount;
    }

    /**
     * Checks whether the table is in global append mode.
     * @returns {boolean} True if the table is in global append mode, otherwise false.
     */
    private isInGlobalAppendMode() {
        if (this.data.globalAppendMode) {
            return this.data.globalAppendMode;
        }

        return false;
    }

    /**
     * Checks whether the table is in data input mode.
     * In data input mode, new rows are added to the datablock instead of the regular YAML.
     */
    private isInDataInputMode() {
        return this.data.dataInput === true;
    }

    /**
     * Returns the ID of the paragraph related to the current table instance.
     */
    private getOwnParId() {
        return this.getPar()?.originalPar.id;
    }

    /**
     * Closes the simple cell content editor.
     */
    private closeSmallEditor() {
        this.currentCell = undefined;
        // if (this.editInput) {
        //     this.editInput.nativeElement.style.display = "none";
        // }
        this.getPar()?.removeClass("live-update-pause");
        this.c();
    }

    private saveCloseSmallEditor(save: boolean) {
        if (save) {
            this.saveAndCloseSmallEditor();
            this.c();
            return;
        }
        this.closeSmallEditor();
    }

    /**
     * Saves the currently edited cell and closes the simple cell content editor.
     */
    public async saveAndCloseSmallEditor() {
        await this.saveCurrentCell();
        this.closeSmallEditor();
    }

    async handleToolbarAddColumn(offset: number) {
        if (this.activeCell) {
            const r = await this.addColumn(this.activeCell.col + offset);
            this.c();
            return r;
        }
    }

    async handleToolbarAddRow(offset: number) {
        if (this.activeCell) {
            const r = await this.addRow(this.activeCell.row + offset);
            this.c();
            return r;
        }
    }

    async handleToolbarRemoveColumn() {
        if (this.activeCell) {
            const r = await this.removeColumn(this.activeCell.col);
            this.c();
            return r;
        }
    }

    async handleToolbarRemoveRow() {
        if (this.activeCell) {
            const r = await this.removeRow(this.activeCell.row);
            this.c();
            return r;
        }
    }

    findRectLimits(cells: ICellIndex[]): IRectLimits {
        const r: IRectLimits = {
            minx: 1e100,
            maxx: -1,
            miny: 1e100,
            maxy: -1,
        };
        for (const c of cells) {
            if (c.x < r.minx) {
                r.minx = c.x;
            }
            if (c.x > r.maxx) {
                r.maxx = c.x;
            }
            if (c.y < r.miny) {
                r.miny = c.y;
            }
            if (c.y > r.maxy) {
                r.maxy = c.y;
            }
        }
        return r;
    }

    getRectValue(
        rect: Record<string, IToolbarTemplate>,
        rectLimits: IRectLimits,
        c: ICellIndex
    ): IToolbarTemplate {
        if (
            rectLimits.minx == rectLimits.maxx &&
            rectLimits.miny == rectLimits.maxy
        ) {
            return rect.one;
        }

        if (rectLimits.minx == rectLimits.maxx) {
            // one column
            if (rectLimits.miny == c.y) {
                return rect.c1t;
            }
            if (rectLimits.maxy == c.y) {
                return rect.c1b;
            }
            return rect.c1c;
        }

        if (rectLimits.miny == rectLimits.maxy) {
            // one row
            if (rectLimits.minx == c.x) {
                return rect.r1l;
            }
            if (rectLimits.maxx == c.x) {
                return rect.r1r;
            }
            return rect.r1c;
        }

        // Now we have at least 2x2 rect
        if (rectLimits.minx == c.x && rectLimits.miny == c.y) {
            return rect.lt;
        }
        if (rectLimits.maxx == c.x && rectLimits.miny == c.y) {
            return rect.rt;
        }
        if (rectLimits.miny == c.y) {
            return rect.t;
        }

        if (rectLimits.minx == c.x && rectLimits.maxy == c.y) {
            return rect.lb;
        }
        if (rectLimits.maxx == c.x && rectLimits.maxy == c.y) {
            return rect.rb;
        }
        if (rectLimits.maxy == c.y) {
            return rect.b;
        }

        if (rectLimits.minx == c.x) {
            return rect.l;
        }
        if (rectLimits.maxx == c.x) {
            return rect.r;
        }
        return rect.c;
    }

    doHandleToolbarSetCell(
        cell: ICellCoord | undefined,
        value: IToolbarTemplate,
        cellsToSave: CellAttrToSave[],
        cells: ICellIndex[]
    ) {
        // , selectedCells: ISelectedCells) {
        // no close your eyes!  This is not for children
        if (
            !cell ||
            cell.row >= this.cellDataMatrix.length ||
            cell.col >= this.cellDataMatrix[cell.row].length
        ) {
            return;
        }
        for (const [key, ss] of Object.entries(value)) {
            try {
                if (key.startsWith("$$")) {
                    continue;
                }
                if (key === "cell" || key === "toggleCell") {
                    if (!cell) {
                        continue;
                    }
                    let newValue = String(ss);
                    let currentKey = key;
                    // this.saveToCell(cell, svalue, selectedCells).then();  // TODO: think if this can be done with same query
                    for (const c of cells) {
                        if (value.onlyEmpty) {
                            const cvalue = this.cellDataMatrix[c.y][c.x];
                            if (cvalue.cell) {
                                continue;
                            }
                        }
                        if (currentKey === "toggleCell") {
                            const oldvalue = this.cellDataMatrix[c.y][c.x].cell;
                            if (oldvalue === newValue) {
                                newValue = "";
                            }
                            currentKey = "cell"; // no more toggle in next cells, do like this cell
                        }
                        cellsToSave.push({
                            col: c.x,
                            row: c.y,
                            key: "cell",
                            c: newValue,
                        });
                        this.cellDataMatrix[c.y][c.x].cell = newValue;
                    }
                    if (
                        this.currentCell &&
                        this.currentCell.row == cell.row &&
                        this.currentCell.col == cell.col
                    ) {
                        this.currentCell.editedCellContent = newValue;
                    }
                    continue;
                }
                if (key === "rect") {
                    const rectLimits = this.findRectLimits(cells);
                    const rect = value.rect;
                    if (!rect) {
                        continue;
                    }
                    for (const c of cells) {
                        const ccells = [c];
                        const val = this.getRectValue(rect, rectLimits, c);
                        if (!val) {
                            continue;
                        }
                        this.doHandleToolbarSetCell(
                            cell,
                            val,
                            cellsToSave,
                            ccells
                        ); // , this.selectedCells);
                    }
                    continue;
                }
                if (key === "area") {
                    const area = value.area;
                    if (!area || !cell) {
                        continue;
                    }

                    for (let r = 0; r < area.length; r++) {
                        const cellIdx = {row: cell.row + r, col: cell.col};
                        for (let c = 0; c < area[r].length; c++) {
                            cellIdx.col = cell.col + c;
                            const celXY = {x: cellIdx.col, y: cellIdx.row};
                            const ccells = [celXY];
                            // const selCels: ISelectedCells = {cells: [celXY], srows: [],
                            //                                 scol1: cellIdx.col, scol2: cellIdx.col,
                            //                                 srow1: cellIdx.row, srow2: cellIdx.row};
                            const val = area[r][c];
                            if (!val) {
                                continue;
                            }
                            this.doHandleToolbarSetCell(
                                cellIdx,
                                val,
                                cellsToSave,
                                ccells
                            ); // , selCels);
                        }
                    }
                    continue;
                }
                function handleStyleList(
                    table: TimTableComponent,
                    vstyle1:
                        | Record<string, string | string[]>
                        | string
                        | string[]
                        | undefined,
                    c: ICellIndex,
                    toggle: boolean,
                    areaClearOrSet: number
                ): number {
                    if (!vstyle1) {
                        return areaClearOrSet;
                    }
                    let vstyle: Record<string, string | string[]> = {};

                    if (typeof vstyle1 === "string") {
                        vstyle[vstyle1] = "";
                    } else if (Array.isArray(vstyle1)) {
                        for (const s of vstyle1) {
                            vstyle[s] = "";
                        }
                    } else {
                        vstyle = vstyle1;
                    }

                    // eslint-disable-next-line guard-for-in
                    for (const skey in vstyle) {
                        // sometimes there is extra # in colors?
                        let clearOrSet = 0; // 1 = set, 2 = clear
                        let cellStyle = vstyle[skey];
                        const scellStyle = "" + vstyle[skey];
                        if (
                            !Array.isArray(cellStyle) &&
                            scellStyle.startsWith("##")
                        ) {
                            cellStyle = cellStyle.substr(1);
                        }
                        let s = cellStyle;
                        let change = false;
                        if (skey === "class") {
                            // includes so that it is possible to use class1, class2
                            const oldCls = table.cellDataMatrix[c.y][c.x].class;
                            if (oldCls) {
                                // todo toggle
                                let clss;
                                let newCls = oldCls;
                                if (Array.isArray(s)) {
                                    clss = s;
                                } else {
                                    clss = [s];
                                }

                                for (const as1 of clss) {
                                    const s1: string = "" + as1;
                                    if (
                                        !newCls.includes(s1) &&
                                        areaClearOrSet != 2
                                    ) {
                                        // is not already in classes
                                        newCls = newCls + " " + s1;
                                        clearOrSet = 1;
                                    } else if (!toggle && areaClearOrSet != 2) {
                                        // newCls = "";  // no need to do anything because it was there
                                        clearOrSet = 1;
                                    } else {
                                        newCls = replaceAll(newCls, s1, "");
                                        clearOrSet = 2;
                                    }
                                }
                                if (newCls != oldCls) {
                                    s = newCls.trim();
                                    change = true;
                                } else {
                                    s = "";
                                }
                            } else {
                                if (areaClearOrSet == 2) {
                                    s = "";
                                }
                                clearOrSet = 1;
                            }
                        } else {
                            if (
                                (toggle && areaClearOrSet == 0) ||
                                areaClearOrSet == 2
                            ) {
                                if (areaClearOrSet == 2) {
                                    s = "";
                                }
                                const styleCache =
                                    table.cellDataMatrix[c.y][c.x].styleCache;
                                const skeyHtml = styleToHtml[skey];
                                if (
                                    styleCache &&
                                    skeyHtml in styleCache &&
                                    styleCache[skeyHtml]
                                ) {
                                    s = "";
                                    change = true;
                                    clearOrSet = 2;
                                } else {
                                    clearOrSet = 1;
                                }
                            } else {
                                clearOrSet = 1;
                            }
                        }
                        if (!areaClearOrSet && toggle) {
                            areaClearOrSet = clearOrSet;
                        }
                        if (s || change) {
                            s = String(s);
                            cellsToSave.push({
                                col: c.x,
                                row: c.y,
                                key: skey,
                                c: s,
                            });
                        }
                    }
                    return areaClearOrSet;
                }

                if (key.includes("tyle")) {
                    // because there is Style and style
                    let toggle = true;
                    let toggleAreaClearOrSet = 0; // 1 = set, 2 = clear, first set or clear decides what to do
                    let sstyle: Record<string, string | string[]> | undefined;
                    if (key == "style") {
                        sstyle = value.style;
                        toggleAreaClearOrSet = 1; // TODO: enum
                        toggle = false;
                    }
                    if (key == "removeStyle") {
                        sstyle = value.removeStyle;
                        toggleAreaClearOrSet = 2;
                        toggle = false;
                    }
                    if (key == "toggleStyle") {
                        sstyle = value.toggleStyle;
                    }
                    if (!sstyle) {
                        continue;
                    }
                    for (const c of cells) {
                        if (value.onlyEmpty) {
                            const cvalue = this.cellDataMatrix[c.y][c.x];
                            if (cvalue.cell) {
                                continue;
                            }
                        }
                        toggleAreaClearOrSet = handleStyleList(
                            this,
                            sstyle,
                            c,
                            toggle,
                            toggleAreaClearOrSet
                        );
                        toggle = false; // used only on first round
                    }
                }
            } catch (e) {
                // continue;
            } // for key
        }
    }

    getColumn() {
        if (!this.activeCell) {
            return -1;
        }
        return this.activeCell.col;
    }

    getCurrentCellAsString() {
        if (!this.currentCell) {
            return "";
        }
        const str = this.currentCell.editedCellContent;
        // const row = this.cellDataMatrix[this.activeCell.row]; // TODO: check if sorted rowindex?
        // const cell = row[this.activeCell.col];
        // if (cell.cell === null) return "";
        // return "" + cell.cell;
        return str;
    }

    getColumnName() {
        const col = this.getColumn();
        if (col < 0) {
            return "";
        }
        if (!this.data) {
            return "";
        }
        if (!this.data.headers) {
            return "";
        }
        return this.data.headers[col];
    }

    setToColumnByIndex(col: number) {
        if (!this.activeCell) {
            return -1;
        }
        if (col < 0) {
            return -1;
        }
        const row = this.cellDataMatrix[this.activeCell.row]; // TODO: check if sorted rowindex?
        if (col >= row.length) {
            return -1;
        }

        this.activeCell.col = col;
        return col;
    }

    setToColumnByName(name: string) {
        if (!this.data) {
            return -1;
        }
        if (!this.data.headers) {
            return -1;
        }
        const col = this.data.headers.indexOf(name);
        return this.setToColumnByIndex(col);
    }

    async handleToolbarSetCell(value: IToolbarTemplate) {
        const cellsToSave: CellAttrToSave[] = [];
        this.doHandleToolbarSetCell(
            this.activeCell,
            value,
            cellsToSave,
            this.selectedCells.cells
        ); // , this.selectedCells);
        if (cellsToSave) {
            await this.setCellStyleAttribute(cellsToSave);
        }
        if (this.selectedCells.cells.length === 1 && value.delta) {
            //
            if (value.delta.x > 0) {
                await this.doCellMovementN(
                    value.delta.x,
                    Direction.Right,
                    false
                );
            }
            if (value.delta.x < 0) {
                await this.doCellMovementN(
                    -value.delta.x,
                    Direction.Left,
                    false
                );
            }
            if (value.delta.y > 0) {
                await this.doCellMovementN(
                    value.delta.y,
                    Direction.Down,
                    false
                );
            }
            if (value.delta.y < 0) {
                await this.doCellMovementN(-value.delta.y, Direction.Up, false);
            }
        }
        this.c();
    }

    getTemplContent(rowId: number, colId: number) {
        const parId = this.getOwnParId();
        if (!this.viewctrl || !parId || this.currentCell?.editorOpen) {
            return undefined;
        }
        const cellObj = this.cellDataMatrix[rowId][colId];
        const templ: IToolbarTemplate = {};
        let value;

        if (this.task) {
            value = this.getCellContentString(rowId, colId);
        } else {
            value = this.getCellContentString(rowId, colId);
            // value = await this.getCellData(this.viewctrl.item.id, parId, rowId, colId);
        }

        for (const [key, val] of Object.entries(cellObj)) {
            if (key.startsWith("render") || key.startsWith("border")) {
                continue;
            }
            if (key.startsWith("$$")) {
                continue;
            }
            if (key == "cell" || (key == "class" && !val)) {
                continue;
            }
            if (typeof val !== "string") {
                continue;
            }
            if (!templ.style) {
                templ.style = {};
            }
            templ.style[key] = val;
        }
        templ.cell = value;
        return templ;
    }

    handleToolbarAddToTemplates() {
        const parId = this.getOwnParId();
        if (
            !this.activeCell ||
            !this.viewctrl ||
            !parId ||
            this.currentCell?.editorOpen
        ) {
            return;
        }
        let templ: IToolbarTemplate | undefined = {favorite: true};
        if (this.selectedCells.cells && this.selectedCells.cells.length > 1) {
            // do area template
            const iy1 = this.selectedCells.srow1;
            const iy2 = this.selectedCells.srow2;
            const ix1 = this.selectedCells.scol1;
            const ix2 = this.selectedCells.scol2;
            const tempArea: IToolbarTemplate = {
                text: "",
                favorite: true,
                area: new Array(iy2 - iy1 + 1)
                    .fill(undefined)
                    .map(() => new Array(ix2 - ix1 + 1).fill(undefined)),
            };
            for (const c of this.selectedCells.cells) {
                const ix = c.x;
                const iy = c.y;
                templ = this.getTemplContent(iy, ix);
                if (!templ || !tempArea.area) {
                    continue;
                }
                tempArea.area[iy - iy1][ix - ix1] = templ;
                if (templ.text) {
                    tempArea.text += templ.text;
                } else if (templ.cell) {
                    tempArea.text += templ.cell;
                }
            }
            templ = tempArea;
        } else {
            // Just on shell
            const rowId = this.activeCell.row;
            const colId = this.activeCell.col;
            templ = this.getTemplContent(rowId, colId);
        }
        if (!templ) {
            return;
        }
        if (this.data.toolbarTemplates === undefined) {
            this.data.toolbarTemplates = [];
        }
        let isUnique = true;
        for (const ob of this.data.toolbarTemplates) {
            if (angular.equals(ob, templ)) {
                isUnique = false;
                break;
            }
        }
        if (isUnique) {
            this.data.toolbarTemplates.push(templ);
            this.c();
        }
    }

    clearSmallEditorStyles() {
        const editInputElement = this.getEditInputElement();
        if (!editInputElement) {
            return;
        }
        this.editInputStyles = "";
        this.editInputClass = "";
        // this.getEditInputElement()!.style.cssText = "";
        this.getEditInputElement()!.className = "";
        const stylesNotToClear = ["position", "top", "left", "width", "height"];
        for (const key of Object.keys(styleToHtml)) {
            // TODO: For some reason, the index signature of style property is number, so we need a cast.
            // See https://github.com/microsoft/TypeScript/issues/17827
            const k = key as unknown as number;
            if (stylesNotToClear.includes(key) || !editInputElement.style[k]) {
                continue;
            }
            editInputElement.style[k] = "";
        }
        /*
        this.editInput[0].style.backgroundColor = "white";
        this.editInput[0].style.textAlign = "left";  // TODO: clear all know styles
         */
    }

    /**
     * Tells the server to set a cell style attribute.
     * @param cellsToSave list of cells to save
     */
    async setCellStyleAttribute(cellsToSave: CellAttrToSave[]) {
        if (!this.viewctrl || !this.activeCell) {
            return;
        }

        this.clearSmallEditorStyles();
        if (this.currentCell) {
            for (const c of cellsToSave) {
                if (
                    this.currentCell.row == c.row &&
                    this.currentCell.col == c.col
                ) {
                    if (c.key === "class") {
                        this.editInputClass += " " + c.c;
                    } else {
                        const k: string = styleToHtml[c.key];
                        if (k) {
                            this.editInputStyles += k + ": " + c.c + ";";
                        }
                    }
                }
            }
            this.getEditInputElement()!.style.cssText +=
                " " + this.editInputStyles;
            this.getEditInputElement()!.className += this.editInputClass;
        }

        if (this.task) {
            for (const c of cellsToSave) {
                this.setUserAttribute(c);
                this.dataViewComponent?.updateStyleForCell(c.row, c.col);
            }

            if (this.data.saveStyleCallBack) {
                this.data.saveStyleCallBack(cellsToSave);
            }
            this.edited = true;
            this.updateListeners();
            this.result = "";
            if (this.data.autosave) {
                await this.sendDataBlockAsync();
            }
            return;
        }

        const parId = this.getOwnParId();
        const docId = this.viewctrl.item.id;

        const response = await to(
            $http.post<TimTable>("/timTable/setCell", {
                docId,
                parId,
                cellsToSave,
            })
        );
        if (!response.ok) {
            return;
        }
        const toolbarTemplates = this.data.toolbarTemplates;
        this.data = response.result.data;
        this.data.toolbarTemplates = toolbarTemplates;

        // Update display
        this.reInitialize();
        this.c();
    }

    /**
     * Checks whether a cell is the currently active cell of the table.
     * The active cell is the cell that is being edited, or if no cell is being edited,
     * the cell that was edited last.
     * Suppose only rectangle areas
     * @param {number} rowi Table row index.
     * @param {number} coli Table column index.
     * @returns {boolean} True if the cell is active, otherwise false.
     */
    isActiveCell(rowi: number, coli: number) {
        if (!this.isInEditMode()) {
            return false;
        }

        /* if (this.currentCell && this.currentCell.editorOpen) {
            return this.currentCell.row === rowi && this.currentCell.col === coli;
        }*/
        if (!this.activeCell) {
            return false;
        }

        const srow = this.permTableToScreen[rowi];
        const scol = coli;

        if (
            scol < this.selectedCells.scol1 ||
            this.selectedCells.scol2 < scol
        ) {
            return false;
        }
        if (this.selectedCells.srows[srow]) {
            return true;
        }

        /*  too slow:
        for (const c of this.selectedCells.cells ) {
            if ( c.x == coli && c.y == rowi ) { return true; }
        }
        */

        return false;
    }

    /**
     * Returns true if given row should be visible
     * @param index row number
     */
    showRow(index: number) {
        // return this.currentHiddenRows.includes(this.data.table.rows[index]);
        return !this.currentHiddenRows.has(index);
    }

    /**
     * Returns true if given column should be visible
     * @param index row number
     */
    showColumn(index: number) {
        return (
            !this.data.hiddenColumns || !this.data.hiddenColumns.includes(index)
        );
    }

    async handleClickClearFilters() {
        this.filters.fill("");
        this.cbFilter = false;
        await this.updateFilter();
        this.clearSortOrder();
        this.c();
    }

    /**
     * Returns the name given to the plugin.
     */
    getName(): string | undefined {
        const taskId = this.getTaskId();
        if (taskId) {
            return taskId.name;
        }
    }

    getTaskId(): TaskId | undefined {
        return this.taskid ?? this.pluginMeta.getTaskId();
    }

    // getContent: () => string | undefined;
    getContent() {
        return JSON.stringify(this.data.userdata);
    }

    // getContentArray?: () => string[] | undefined;
    getAreas(): string[] {
        const returnList: string[] = [];
        const parents = this.element.parents(".area");
        if (parents[0]) {
            const areaList = parents[0].classList;
            areaList.forEach((value) => {
                const m = value.match(/^area_(\S+)$/);
                if (m) {
                    returnList.push(m[1]);
                }
            });
        }
        return returnList;
    }

    belongsToArea(area: string): boolean {
        return this.getAreas().includes(area);
    }

    isUnSaved(userChange?: boolean) {
        if (!this.task) {
            return false;
        }
        if (userChange && this.data.nonUserSpecific) {
            return false;
        }
        return this.edited || this.currentCell != undefined;
    }

    // save: () => Promise<{saved: boolean, message: (string | undefined)}>;
    async save() {
        if (!this.task) {
            return {saved: false, message: "Not in task mode"};
        }
        if (!this.isUnSaved()) {
            return {saved: false, message: "No changes"};
        }
        const r = await this.sendDataBlockAsync();
        if (r?.ok) {
            return {saved: true, message: ""};
        }
        return {saved: false, message: "Error saving table"};
    }

    public getPar() {
        const res = this.element.parents(".par")[0];
        if (!res) {
            return undefined;
        }
        return createParContext(res);
    }

    resetField() {
        return undefined;
    }

    async tryResetChanges(e?: Event) {
        if (e) {
            e.preventDefault();
        }
        if (this.undoConfirmation) {
            if (
                !(await showConfirm(
                    this.undoConfirmationTitle ?? this.undoConfirmation,
                    this.undoConfirmation
                ))
            ) {
                return;
            }
        }
        this.resetChanges();
    }

    resetChanges() {
        // TODO: Check if all three are needed (need more)
        this.userdata = clone(this.prevUserdata);
        this.cellDataMatrix = clone(this.prevCellDataMatrix);
        this.data = clone(this.prevData);
        this.edited = false;
        this.saveFailed = false;
        this.updateListeners();
        this.reInitialize();
        this.c();
    }

    updateListeners() {
        if (!this.viewctrl) {
            return;
        }
        const taskId = this.pluginMeta.getTaskId();
        if (!taskId) {
            return;
        }
        this.viewctrl.informChangeListeners(
            taskId,
            this.edited ? ChangeType.Modified : ChangeType.Saved,
            this.data.tag ? this.data.tag : undefined
        );
    }

    setAnswer(_content: Record<string, unknown>): ISetAnswerResult {
        return {ok: false, message: "Plugin doesn't support setAnswer"};
    }

    formBehavior(): FormModeOption {
        return FormModeOption.NoForm;
    }

    async setData(data: unknown, save: boolean = false) {
        if (!this.userdata) {
            return;
        }
        if (!this.data.saveAttrs) {
            this.data.saveAttrs = [];
        }
        const dataType = t.intersection([
            t.type({
                matrix: t.array(
                    t.array(t.union([CellTypeR, t.type({cell: CellTypeR})]))
                ),
            }),
            t.partial({
                headers: t.array(t.string),
            }),
        ]);
        if (dataType.is(data)) {
            this.userdata.cells = {};
            this.cellDataMatrix = [];
            // TODO: empty the rest of the table
            for (let row = 0; row < data.matrix.length; row++) {
                const r = data.matrix[row];
                for (let col = 0; col < r.length; col++) {
                    const coordinate = colnumToLetters(col) + "" + (row + 1);
                    this.userdata.cells[coordinate] = r[col];
                    // this.setUserContent(row, col, r[col]);
                }
            }
            if (data.headers) {
                this.data.headers = data.headers;
                // this.data.saveAttrs.push("headers");
            }
            for (const [key, value] of Object.entries(data)) {
                if (key in ["matrix", "headers"]) {
                    continue;
                }
                // @ts-expect-error
                this.data[key] = value;
                this.data.saveAttrs.push(key);
            }
            // this.reInitialize();
            this.processDataBlock(this.userdata.cells);
            this.clearSortOrder();
            if (save) {
                await this.sendDataBlock();
            }
            this.c();
        } else {
            console.error(
                "timTable.setData: unexpected data format: " +
                    JSON.stringify(data)
            );
        }
    }

    getColumnHeaderContents(columnIndex: number): string {
        if (!this.data.headers) {
            return "";
        }
        return this.data.headers[columnIndex];
    }

    getDimension(): {rows: number; columns: number} {
        return {
            rows: this.cellDataMatrix.length,
            columns: this.data.headers?.length ?? 0,
        };
    }

    getRowHeight(rowIndex: number): number | undefined {
        return this.dataView?.rowHeight;
    }

    getColumnWidth(columnIndex: number): [number, boolean] {
        if (!this.dataView) {
            return [0, false];
        }
        if (!this.dataView.columnWidths) {
            this.dataView.columnWidths = {};
        }
        const idx = `${columnIndex}`;
        const res = this.dataView.columnWidths[idx];
        if (res) {
            let resolveState = this.columnResolveState[columnIndex];
            if (resolveState === undefined) {
                this.columnResolveState[columnIndex] = true;
                resolveState = true;
            }
            return [res, resolveState];
        }
        if (columnIndex <= 0) {
            return [0, false];
        }
        const [w, _] = this.getColumnWidth(columnIndex - 1);
        this.columnResolveState[columnIndex] = false;
        this.dataView.columnWidths[idx] = w;
        return [w, false];
    }

    getCellContents(rowIndex: number, columnIndex: number): string {
        return (
            this.cellDataMatrix[rowIndex][columnIndex].cell ?? "null"
        ).toString();
    }

    getRowContents(rowIndex: number): string[] {
        return this.cellDataMatrix[rowIndex].map((c) =>
            (c.cell ?? "null").toString()
        );
    }

    setRowFilter(columnIndex: number, value: string): void {
        this.filters[columnIndex] = value;
    }

    getRowFilter(columnIndex: number): string {
        return this.filters[columnIndex] ?? "";
    }

    clearChecked() {
        this.cbAllFilter = false;
        for (let i = 0; i < this.cbs.length; i++) {
            this.cbs[i] = false;
        }
        this.c();
    }

    setRowChecked(rowIndex: number, checked: boolean): void {
        this.cbs[rowIndex] = checked;
    }

    setSelectAll(state: boolean): void {
        this.cbAllFilter = state;
    }

    isRowChecked(rowIndex: number): boolean {
        return this.cbs[rowIndex];
    }

    setSelectedFilter(state: boolean): void {
        this.cbFilter = state;
    }

    getSortSymbolInfo(columnIndex: number): {
        symbol: string;
        style: Record<string, string>;
    } {
        return {
            style: this.sortSymbolStyle[columnIndex],
            symbol: this.sortSymbol[columnIndex],
        };
    }

    isPreview(): boolean {
        return this.data.isPreview;
    }

    isRowSelectable(rowIndex: number): boolean {
        return true;
    }
}

@NgModule({
    declarations: [TimTableComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        PurifyModule,
        DataViewModule,
    ],
    exports: [TimTableComponent],
})
export class TimTableModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("tim-table", TimTableModule, TimTableComponent);
