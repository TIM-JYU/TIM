/**
 * General DOM-based data view component. The component can be used to display large datasets without the penalty
 * of Angular's change detection.
 *
 * DataView component is made of four tables combined with CSS grid:
 * - Summary table (contains number of all elements, number of filter elements and helper checkboxes)
 * - Column header table (contains column headers and the column filter inputs)
 * - Row header table (contains row IDs and selection checkboxes)
 * - Data table (contains the actual data)
 *
 * All tables (except summary) have a cache which contain all DOM elements in the table.
 * In addition, only data table is scrollable, while the other tables are static (headers are autoscrolled to correct pos)
 *
 * DataView also contains information about the row/column axes and the viewport. The viewport represents the
 * visible area of the table.
 *
 * DataView operates in two modes: DOM table and virtual scrolling:
 *
 * In DOM table:
 *   - All data is sanitized put into DOM
 *   - Table caches contain all DOM elements in the order they were initially put into there
 *   - Items are hidden by applying hidden=true to relevant DOM elements
 *   - Viewport is static (contains number of visible elements)
 *   - GridAxisManager.itemOrder maps cell's ordinal to the cell's index
 *   - GridAxisManager.positionStart is not defined as all sizes are automatically computed by the browser
 *
 * In vscroll mode:
 *   - Only visible data is put into DOM
 *   - Data is sanitized concurrently on a separate Web Worker
 *   - Table caches only contain visible elements (plus some overflow)
 *   - Viewport and DOM contents are updated on scroll or resize
 *   - Viewport contains information about currently visible items
 *   - GridAxisManager.indexToOrdinal can map data index to its current ordinal in the visible DOM
 *
 *  Finally, DataView uses two values to refer to data:
 *  - cell's index is its absolute position in the table -- used to get the actual data and style of the cell
 *  - cell's ordinal (or cell's "number") is its current position in the visible DOM
 *  Cell's index can be different from its ordinal when filtering or sorting data.
 *  GridAxisManager.visibleItems maps cell's ordinal to its index and GridAxisManager.indexToOrdinal does the reverse.
 */
import type {AfterViewInit, OnInit} from "@angular/core";
import {
    ChangeDetectionStrategy,
    ChangeDetectorRef,
    Component,
    ElementRef,
    EventEmitter,
    Input,
    NgZone,
    Output,
    Renderer2,
    ViewChild,
} from "@angular/core";
import DOMPurify from "dompurify";
import {showCopyWidthsDialog} from "tim/plugin/dataview/copy-table-width-dialog.component";
import {GridAxisManager} from "tim/plugin/dataview/gridAxisManager";
import {TableDOMCache} from "tim/plugin/dataview/tableDOMCache";
import {scrollToViewInsideParent, timeout} from "tim/util/utils";
import type {Changes} from "tim/util/angularchanges";
import type {
    CellIndex,
    PurifyData,
    TableArea,
    Viewport,
    VisibleItems,
} from "tim/plugin/dataview/util";
import {
    applyBasicStyle,
    columnInCache,
    el,
    joinCss,
    px,
    runMultiFrame,
    viewportsEqual,
} from "tim/plugin/dataview/util";
// uncomment to enable method logging and also before @Component
// import {LogAllMethods} from "tim/util/dataWatcher";

/**
 * General interface for an object that provides the data model for DataViewComponent.
 */

export interface ICellCoord {
    row: number;
    col: number;
}

export interface DataModelProvider {
    isRowSelectable(rowIndex: number): boolean;

    getDimension(): {rows: number; columns: number};

    getColumnHeaderContents(columnIndex: number): string;

    getRowHeight(rowIndex: number): number | undefined;

    getColumnWidth(columnIndex: number): [number, boolean];

    stylingForRow(rowIndex: number): Record<string, string>;

    stylingForCell(
        rowIndex: number,
        columnIndex: number
    ): Record<string, string>;

    classForCell(rowIndex: number, columnIndex: number): string;

    handleClickCell(rowIndex: number, columnIndex: number): Promise<void>;

    getCellContents(rowIndex: number, columnIndex: number): string;

    getRowContents(rowIndex: number): string[];

    showColumn(colIndex: number): boolean;

    showRow(rowIndex: number): boolean;

    setRowFilter(
        filterRowIndex: number,
        columnIndex: number,
        value: string
    ): void;

    getRowFilter(filterRowIndex: number, columnIndex: number): string;

    getFilterRowCount(): number;

    addFilterRow(): Promise<void>;

    deleteFilterRow(): Promise<void>;

    handleChangeFilter(): void;

    setRowChecked(rowIndex: number, checked: boolean): void;

    isRowChecked(rowIndex: number): boolean;

    handleChangeCheckbox(rowIndex: number): void;

    setSelectAll(state: boolean): void;

    setSelectedFilter(state: boolean): void;

    clearFilters(): void;

    getSortSymbolInfo(columnIndex: number): {
        symbol: string;
        style: Record<string, string>;
    };

    handleClickHeader(columnIndex: number): void;

    isPreview(): boolean;

    isTinyFilters(): boolean;

    isNrColumn(def: boolean): boolean;

    isCbColumn(def: boolean): boolean;

    isLastVisible(tx: number, ty: number, d: CellIndex): boolean;
}

/**
 * Options related to virtual scrolling behaviour.
 */
export interface VirtualScrollingOptions {
    enabled: boolean;
    viewOverflow: TableArea;
}

enum EditorPosition {
    MainData,
    FixedColumn,
}

const DEFAULT_VIRTUAL_SCROLL_SETTINGS: VirtualScrollingOptions = {
    enabled: true,
    viewOverflow: {horizontal: 1, vertical: 1},
};
// TODO: Right now, default TimTable style uses collapsed borders, in which case there is no need for spacing. Does this need a setting?
const VIRTUAL_SCROLL_TABLE_BORDER_SPACING = 0;
const SLOW_SIZE_MEASURE_THRESHOLD = 0;

// @LogAllMethods
/**
 * A DOM-based data view component that supports virtual scrolling.
 * The component handles DOM generation and updating based on the DataModelProvider.
 */
@Component({
    selector: "tim-data-view",
    changeDetection: ChangeDetectionStrategy.OnPush,
    template: `
        <div class="loader" *ngIf="isLoading">
            <tim-loading></tim-loading>
        </div>
        <tim-alert class="data-view-alert" severity="info" *ngIf="showSlowLoadMessage" [closeable]="true" (closing)="hideSlowMessageDialog()">
            <div class="message" i18n>
                <strong>Column size computation took {{sizeComputationTime}} seconds.</strong>
                You can speed up loading by <a href="#" class="alert-link" (click)="showTableWidthExportDialog($event)">setting
                static column widths</a>.
            </div>
        </tim-alert>
        <tim-alert class="data-view-alert" severity="warning" *ngIf="showIncompleteSizeMessage" [closeable]="true" (closing)="hideIncompleteSizeMessageDialog()">
            <div class="message" i18n>
                <strong>The column count has changed since the last time.</strong>
                Remove this warning by <a href="#" class="alert-link" (click)="showTableWidthExportDialog($event, true)"> setting static column widths</a>.
            </div>
            <tim-loading *ngIf="recomputingSize"></tim-loading>
        </tim-alert>
        <div tabindex="-1" class="data-view" [class.virtual]="isVirtual" [style.width]="tableMaxWidth" #dataViewContainer>
            <div class="header" #headerContainer>
                <table class="timTableHeader" [ngStyle]="tableStyle" #headerTable>
                    <thead #headerIdBody></thead>
                    <tbody #filterBody  class ="filter-rows"></tbody>
                </table>
            </div>
            <ng-container *ngIf="fixedColumnCount > 0">
                <div class="fixed-col-header" #fixedColHeaderContainer>
                    <table [ngStyle]="tableStyle" #fixedColHeaderTable>
                        <thead #fixedColHeaderIdBody></thead>
                        <tbody #fixedColFilterBody class="filter-rows"></tbody>
                    </table>
                </div>
                <div class="fixed-col-data" [style.maxHeight]="tableMaxHeight" #fixedDataContainer>
                    <table [ngClass]="tableClass" [ngStyle]="tableStyle" [class.virtual]="virtualScrolling.enabled"
                           #fixedDataTable>
                        <tbody class="content" #fixedDataBody></tbody>
                    </table>
                    <ng-container *ngIf="editorInFixedColumn">
                        <ng-container *ngTemplateOutlet="editor"></ng-container>
                    </ng-container>
                </div>
            </ng-container>
            <div class="summary">
                <table [ngStyle]="tableStyle" #summaryTable>
                    <thead>
                    <tr class="header-row">
                        <td  *ngIf="this.nrColumn"
                            class="nr-column total-nr"
                            title="Click to show all"
                            (click)="clearFilters()"
                            #allVisibleCell>{{totalRows}}</td>
                        <td *ngIf="this.cbColumn"
                            class="cb-column">
                            <input [(ngModel)]="cbAllVisibleRows"
                                   (ngModelChange)="setAllVisible()"
                                   type="checkbox"
                                   title="Check for all visible rows">
                        </td>
                    </tr>
                    </thead>
                    <tbody class="filter-rows" [class.tiny]="tinyFilters">
                    <tr class="filters-row" [hidden]="filterRowCount===0">
                        <td *ngIf="this.nrColumn"
                            class="nr-column matching-nr">
                            <span *ngIf="totalRows != visibleRows" title="Number of matching rows">{{visibleRows}}</span>
                            <div class="add-new-row" (click)="addFilterRow()" title="Add new filter row" >+</div>
                        </td>
                        <td class="cb-column" *ngIf="this.cbColumn">
                            <input type="checkbox"
                                   title="Check to show only checked rows"
                                   [(ngModel)]="cbFilter"
                                   (ngModelChange)="setFilterSelected()">
                        </td>
                    </tr>
                    <tr class="filters-row" *ngFor="let frow of filterRowPlaceholders; let i = index; let last= last">
                        <td *ngIf="this.nrColumn" 
                            class="nr-column" style="position: relative;">
                            <!-- if last row show - -->
                            <div 
                              class ="delete-row" *ngIf="last" (click)="deleteFilterRow()"
                              title="Delete filter row">
                              -
                            </div>
                            &nbsp;
                        </td>                            
                        <td class="cb-column" *ngIf="this.cbColumn">
                            &nbsp;
                        </td>
                    </tr>
                    </tbody>
                </table>
            </div>
            <div class="ids" #idContainer>
                <table [ngStyle]="tableStyle" #idTable>
                    <tbody #idBody></tbody>
                </table>
            </div>
            <div tabindex="-1" class="data" [style.maxHeight]="tableMaxHeight" #mainDataContainer>
                <table tabindex="-1"  [ngClass]="tableClass" [ngStyle]="tableStyle" [class.virtual]="virtualScrolling.enabled"
                       #mainDataTable>
                    <tbody tabindex="-1" class="content" #mainDataBody [class.nowrap]="noWrap"></tbody>
                </table>
                <ng-container *ngIf="editorInData">
                    <ng-container *ngTemplateOutlet="editor"></ng-container>
                </ng-container>
            </div>
            <ng-template #editor>
                <ng-content></ng-content>
            </ng-template>
        </div>
    `,
    styleUrls: ["./data-view.component.scss", "../timTable/table-common.scss"],
})
export class DataViewComponent implements AfterViewInit, OnInit {
    // region Fields
    @Input() modelProvider!: DataModelProvider;
    @Input() virtualScrolling: Partial<VirtualScrollingOptions> =
        DEFAULT_VIRTUAL_SCROLL_SETTINGS;
    @Input() tableClass: Record<string, unknown> = {};
    @Input() tableStyle: Record<string, string> = {};
    @Input() headerStyle: Record<string, string> | null = {};
    @Input() columnIdStart: number = 1;
    @Input() tableMaxHeight: string = "85vh";
    @Input() tableMaxWidth: string = "max-content";
    @Input() fixedColumnCount: number = 0;
    @Input() columnFilters: string[] = [];
    @Input() selectedIndices = new Set<number>();
    @Input() noWrap = false;
    @Input() reportSlowLoad = true;
    @Output() selectedIndicesChange = new EventEmitter<Set<number>>();
    showSlowLoadMessage = false;
    showIncompleteSizeMessage = false;
    sizeComputationTime = 0;
    isLoading = true;
    totalRows: number = 0;
    visibleRows: number = 0;
    cbAllVisibleRows = false;
    @Input() cbFilter = false;
    isVirtual: boolean = false;
    recomputingSize = false;
    dataViewWidth = "100%";
    @ViewChild("headerContainer")
    private headerContainer!: ElementRef<HTMLDivElement>;
    @ViewChild("headerTable")
    private headerTable!: ElementRef<HTMLTableElement>;
    @ViewChild("headerIdBody")
    private headerIdBody!: ElementRef<HTMLTableSectionElement>;
    @ViewChild("filterBody")
    private filterBody!: ElementRef<HTMLTableSectionElement>;
    @ViewChild("fixedColHeaderContainer")
    private fixedColHeaderContainer?: ElementRef<HTMLDivElement>;
    @ViewChild("fixedColHeaderTable")
    private fixedColHeaderTable?: ElementRef<HTMLTableElement>;
    @ViewChild("fixedColHeaderIdBody")
    private fixedColHeaderIdBody?: ElementRef<HTMLTableSectionElement>;
    @ViewChild("fixedColFilterBody")
    private fixedColFilterBody?: ElementRef<HTMLTableSectionElement>;
    @ViewChild("idContainer") private idContainer!: ElementRef<HTMLDivElement>;
    @ViewChild("idTable") private idTable!: ElementRef<HTMLTableElement>;
    @ViewChild("idBody") private idBody!: ElementRef<HTMLTableSectionElement>;
    @ViewChild("mainDataBody")
    private mainDataBody!: ElementRef<HTMLTableSectionElement>;
    @ViewChild("mainDataTable")
    private mainDataTable!: ElementRef<HTMLTableElement>;
    @ViewChild("mainDataContainer")
    private mainDataContainer!: ElementRef<HTMLDivElement>;
    @ViewChild("fixedDataBody")
    private fixedDataBody?: ElementRef<HTMLTableSectionElement>;
    @ViewChild("fixedDataTable")
    private fixedDataTable?: ElementRef<HTMLTableElement>;
    @ViewChild("fixedDataContainer")
    private fixedDataContainer?: ElementRef<HTMLDivElement>;
    @ViewChild("summaryTable")
    private summaryTable!: ElementRef<HTMLTableElement>;
    @ViewChild("allVisibleCell")
    private allVisibleCell!: ElementRef<HTMLTableDataCellElement>;
    @ViewChild("dataViewContainer")
    private dataViewContainer!: ElementRef<HTMLDivElement>;
    private scrollDiff: TableArea = {vertical: 0, horizontal: 0};
    private cellValueCache: Record<number, string[]> = {};
    private dataTableCache!: TableDOMCache;
    private fixedTableCache?: TableDOMCache;
    private idTableCache!: TableDOMCache;
    private headerIdTableCache!: TableDOMCache;
    private fixedColHeaderIdTableCache?: TableDOMCache;
    private fixedColFilterTableCache?: TableDOMCache;
    private filterTableCache!: TableDOMCache;
    private scheduledUpdate = false;
    private viewport!: Viewport;
    private rowAxis!: GridAxisManager;
    private colAxis!: GridAxisManager;
    private fixedColAxis!: GridAxisManager;
    private vScroll: VirtualScrollingOptions = DEFAULT_VIRTUAL_SCROLL_SETTINGS;
    private idealColHeaderWidth: number[] = [];
    private tableBaseBorderWidth: number = -1;
    private sizeContainer?: HTMLDivElement;
    private sizeContentContainer?: HTMLDivElement;
    private idealColWidths: number[] = [];
    private editorPosition = EditorPosition.MainData;
    private prevEditorDOMPosition: TableArea = {horizontal: -1, vertical: -1};
    private selectedCells: CellIndex[] = [];
    private shouldRenderTable = true;
    protected isPreview = false;
    protected nrColumn = true;
    protected cbColumn = true;
    protected tinyFilters = false;

    // endregion

    constructor(
        private r2: Renderer2,
        private zone: NgZone,
        private cdr: ChangeDetectorRef
    ) {}

    // region Properties

    get editorInData() {
        return this.editorPosition == EditorPosition.MainData;
    }

    get editorInFixedColumn() {
        return this.editorPosition == EditorPosition.FixedColumn;
    }

    private get tableBaseBorderWidthPx(): number {
        if (this.tableBaseBorderWidth < 0) {
            const nrCelEl = this.allVisibleCell?.nativeElement;
            if (!nrCelEl) {
                // fallback when the element is not available yet
                this.tableBaseBorderWidth = 0;
            } else {
                const borderLeft = getComputedStyle(nrCelEl).borderLeftWidth;
                this.tableBaseBorderWidth = Number.parseFloat(borderLeft) || 0;
            }
        }
        return this.tableBaseBorderWidth;
    }

    private get dataTableWidth(): number {
        // Get the most fitting width out of the following:
        const widths = [
            this.mainDataContainer.nativeElement.offsetWidth, // Width of the the whole container (prevents table from overflowing over the visible area)
            this.mainDataBody.nativeElement.offsetWidth, // Width of the table body (makes visible area smaller when filtering)
            this.dataViewContainer.nativeElement.offsetWidth -
                this.summaryTable.nativeElement.offsetWidth -
                (this.fixedDataContainer?.nativeElement.offsetWidth ?? 0), // Ideal width of the data area (prevents table from expanding too much when filtering)
        ].filter((w) => w != 0);
        // Add table border width to prevent possible border cutoff when filtering in DOM mode
        return Math.min(...widths) + this.tableBaseBorderWidthPx;
    }

    private get dataTableHeight(): number {
        return Math.min(
            this.mainDataContainer.nativeElement.clientHeight,
            this.mainDataTable.nativeElement.clientHeight
        );
    }

    private get verticalScrollbar(): number {
        const e = this.mainDataContainer.nativeElement;
        return e.offsetWidth - e.clientWidth;
    }

    // endregion

    // region Event handlers for summary table

    setAllVisible() {
        if (this.isPreview) {
            return;
        }
        this.modelProvider.setSelectAll(this.cbAllVisibleRows);
        this.modelProvider.handleChangeCheckbox(-1);
    }

    c() {
        this.cdr.detectChanges();
    }

    async addFilterRow() {
        await this.modelProvider.addFilterRow();
        this.getFilterRowCount(); // updates the filterRowPlaceholders
        this.c();
    }

    async deleteFilterRow() {
        await this.modelProvider.deleteFilterRow();
        this.getFilterRowCount(); // updates the filterRowPlaceholders
        this.c();
    }

    setFilterSelected() {
        this.modelProvider.setSelectedFilter(this.cbFilter);
        this.modelProvider.handleChangeFilter();
    }

    private clearFilterCache(cache: TableDOMCache | undefined) {
        if (!cache) {
            return;
        }
        for (const row of cache.rows) {
            for (const cell of row.cells) {
                const input = cell.getElementsByTagName("input")[0];
                input.value = "";
            }
        }
    }

    clearFilters() {
        if (this.isPreview) {
            return;
        }
        this.modelProvider.clearFilters();
        this.cbFilter = false;
        this.clearFilterCache(this.fixedColFilterTableCache);
        this.clearFilterCache(this.filterTableCache);
    }

    // endregion

    // region Public update functions

    refresh() {
        this.cellValueCache = {};
        this.updateVisible();
        this.updateAllSelected();
        this.c();
    }

    /**
     * Updates all visible elements in the table
     */
    updateVisible() {
        if (!this.shouldRenderTable) {
            return;
        }
        this.rowAxis.refresh();
        this.colAxis.refresh();
        this.fixedColAxis.refresh();
        if (this.vScroll.enabled) {
            this.setTableSizes();
            this.updateVTable({
                updateHeader: true,
            });
        } else {
            this.updateColumnHeaders(true); // to update filter inputs
            // For normal mode: simply hide rows that are no more visible/show hidden rows
            for (const [rowIndex, row] of this.dataTableCache.rows.entries()) {
                const shouldHide = !this.modelProvider.showRow(rowIndex);
                const hidden = row.rowElement.hidden;
                // Apparently always setting hidden will cause layout even if the value hasn't changed (?)
                if (shouldHide != hidden) {
                    row.rowElement.hidden = shouldHide;
                    if (this.fixedTableCache) {
                        this.fixedTableCache.getRow(rowIndex).hidden =
                            shouldHide;
                    }
                    if (this.idTableCache) {
                        this.idTableCache.getRow(rowIndex).hidden = shouldHide;
                    }
                }
            }
            this.updateHeaderCellSizes();
        }
        this.updateTableSummary();
    }

    /**
     * Updates table summary info (row count and visible row count)
     */
    updateTableSummary() {
        this.visibleRows = this.rowAxis.visibleItems.length;
        this.totalRows = this.modelProvider.getDimension().rows;
        this.c();
    }

    /**
     * Updates selected row info of the visible items
     */
    updateAllSelected() {
        if (!this.idTableCache) {
            return;
        }
        if (this.vScroll.enabled) {
            this.updateVTable();
            return;
        }
        for (const rowIndex of this.rowAxis.visibleItems) {
            const input = this.idTableCache
                .getCell(rowIndex, 1)
                .getElementsByTagName("input")[0];
            input.checked = this.modelProvider.isRowChecked(rowIndex);
        }
    }

    /**
     * Updates sort order of rows and updates visible sort markers
     * @param order New sort order to use. This is an array of row indices in the order they should be shown to the user.
     */
    updateRowSortOrder(order: number[]): void {
        if (!this.shouldRenderTable) {
            return;
        }
        this.rowAxis.itemOrder = order;
        this.rowAxis.refresh();

        if (this.vScroll.enabled) {
            this.updateVTable({
                updateHeader: true,
            });
            return;
        }

        for (const rowIndex of order) {
            const tableRow = this.dataTableCache.getRow(rowIndex);
            this.mainDataBody.nativeElement.appendChild(tableRow);
            if (this.fixedTableCache && this.fixedDataBody) {
                const fixedTableRow = this.fixedTableCache.getRow(rowIndex);
                this.fixedDataBody.nativeElement.appendChild(fixedTableRow);
            }

            if (this.idTableCache && this.idBody) {
                const rowHeader = this.idTableCache.getRow(rowIndex);
                this.idBody.nativeElement.appendChild(rowHeader);
            }
        }

        const updateSymbol = (
            cache?: TableDOMCache,
            colAxis?: GridAxisManager
        ) => {
            if (!cache || !colAxis) {
                return;
            }
            for (const columnIndex of colAxis.visibleItems) {
                const cell = cache.getCell(
                    0,
                    colAxis.indexToOrdinal[columnIndex]
                );
                const sortSymbolEl = cell.getElementsByTagName("span")[1];
                const {symbol, style} =
                    this.modelProvider.getSortSymbolInfo(columnIndex);
                applyBasicStyle(sortSymbolEl, style);
                sortSymbolEl.textContent = symbol;
            }
        };

        updateSymbol(this.headerIdTableCache, this.colAxis);
        updateSymbol(this.fixedColHeaderIdTableCache, this.fixedColAxis);
    }

    /**
     * Marks the given cells as selected.
     * Updates styles of selected cells and possibly previously selected ones too.
     *
     * @param cells Cells to select.
     */
    markCellsSelected(cells: CellIndex[]): void {
        if (this.vScroll.enabled) {
            this.updateVTable();
            return;
        }

        for (const {x, y} of this.selectedCells) {
            this.updateStyleForCell(y, x);
        }
        for (const {x, y} of cells) {
            this.updateStyleForCell(y, x);
        }
        this.selectedCells = cells;
    }

    /**
     * Updates styling for a single cell
     * @param row Row index of the cell
     * @param column Column index of the cell
     */
    updateStyleForCell(row: number, column: number): void {
        const cell = this.getDataCell(row, column);
        if (cell) {
            this.updateCellStyle(cell, row, column);
        }
    }

    /**
     * Updates contents of cells
     * @param cells Cells to update
     */
    updateCellsContents(cells: {row: number; col: number}[]): void {
        if (this.vScroll.enabled) {
            for (const {row, col} of cells) {
                this.invalidateCacheAt(row, col);
                if (!this.colAxis.isVirtual) {
                    this.idealColWidths[col] = Math.max(
                        this.measureText(row, col).width,
                        this.idealColWidths[col]
                    );
                }
            }
            this.updateVTable();
        } else {
            for (const {row, col} of cells) {
                const cell = this.getDataCell(row, col);
                this.updateCell(
                    cell,
                    row,
                    col,
                    this.modelProvider.getCellContents(row, col)
                );
                DOMPurify.sanitize(cell, {IN_PLACE: true});
            }
        }
        requestAnimationFrame(() => {
            if (this.vScroll.enabled) {
                this.updateHeaderCellSizes();
            } else {
                for (const {row, col} of cells) {
                    this.updateHeaderSizesForCell(row, col);
                }
            }

            this.syncHeaderScroll();
        });
    }

    clearEditorPosition() {
        this.prevEditorDOMPosition = {horizontal: -1, vertical: -1};
    }

    /**
     * Updates position of the editor to specified cell
     * @param row Currently selected row
     * @param col Currently selected cell
     */
    updateEditorPosition(row: number, col: number): void {
        // Because of Angular, we need to pass the editor as ng-content (otherwise Angular seems to lose reference to
        // the element and stop updating it properly)
        // We must use querySelector here because the editor can jump between fixed data table and main data table
        // TODO: Figure out a better way to integrate the editor
        if (
            row == this.prevEditorDOMPosition.vertical &&
            col == this.prevEditorDOMPosition.horizontal
        ) {
            return;
        }
        this.prevEditorDOMPosition.horizontal = col;
        this.prevEditorDOMPosition.vertical = row;
        const prevPos = this.editorPosition;
        this.editorPosition = columnInCache(
            col,
            this.fixedColAxis,
            this.fixedTableCache
        )
            ? EditorPosition.FixedColumn
            : EditorPosition.MainData;
        if (prevPos != this.editorPosition) {
            this.c();
        }
        const container =
            this.editorPosition == EditorPosition.MainData
                ? this.mainDataContainer
                : this.fixedDataContainer;
        if (!container) {
            return;
        }
        const editor = container.nativeElement.querySelector(".timTableEditor");
        const editInput =
            container.nativeElement.querySelector<HTMLTextAreaElement>(
                ".timTableEditor>textarea"
            );
        const inlineEditorButtons = container.nativeElement.querySelector(
            ".timTableEditor>span"
        );
        if (!editor) {
            return;
        }
        applyBasicStyle(editor as HTMLElement, {
            position: "relative",
            height: px(0),
            display: "block",
        });
        const {x, y, w, h} = this.getCellPosition(row, col);
        if (editInput) {
            applyBasicStyle(editInput as HTMLElement, {
                position: "absolute",
                top: px(y - this.mainDataTable.nativeElement.offsetHeight),
                left: px(x),
                width: px(w),
            });
        }
        if (inlineEditorButtons) {
            const e = inlineEditorButtons as HTMLElement;
            const isLast = this.modelProvider.isLastVisible(col, row, {
                x: 0,
                y: -1,
            });
            const dir = isLast ? 1 : -1;
            applyBasicStyle(e, {
                position: "absolute",
                top: px(
                    y -
                        this.mainDataTable.nativeElement.offsetHeight +
                        dir * e.offsetHeight
                ),
                left: px(x),
            });
        }
        if (this.editorPosition == EditorPosition.MainData) {
            const cell = this.getDataCell(row, col);
            scrollToViewInsideParent(
                cell,
                this.mainDataContainer.nativeElement,
                0,
                3 * h,
                0,
                h
            );
        }
        if (editInput) {
            editInput.focus();
            editInput.select();
        }
    }

    /**
     * Begins table reset procedure. This blocks all render events that can occur during model resetting.
     */
    startReset() {
        this.shouldRenderTable = false;
    }

    /**
     * Rebuilds the whole table, invalidating all caches and fetching the latest data from the model view.
     */
    endReset() {
        this.shouldRenderTable = true;
        const clear = <T extends HTMLElement>(e?: ElementRef<T>) => {
            if (e) {
                e.nativeElement.textContent = "";
            }
        };
        clear(this.mainDataBody);
        clear(this.headerIdBody);
        clear(this.filterBody);
        clear(this.fixedDataBody);
        clear(this.fixedColHeaderIdBody);
        clear(this.idBody);
        clear(this.fixedColFilterBody);
        this.idealColWidths = [];
        this.idealColHeaderWidth = [];
        this.cellValueCache = {};
        this.scrollDiff = {horizontal: 0, vertical: 0};

        // Reset all the axes, should be pretty fast
        this.doInit();
        // Resets the whole table data, might take more time!
        // TODO: maybe run diff between data and insert/remove items as needed? Going to be faster but much more logic.
        this.initView();
    }

    // endregion

    // region Initialization

    ngOnChanges(
        c: Changes<this, "selectedIndices" | "columnFilters" | "cbFilter">
    ) {
        if (c.selectedIndices) {
            for (const i of c.selectedIndices.currentValue) {
                this.modelProvider.setRowChecked(i, true);
            }
        }
        if (c.columnFilters) {
            for (let i = 0; i < c.columnFilters.currentValue.length; ++i) {
                this.modelProvider.setRowFilter(
                    0, // TODO: when more filter rows, look correct row
                    i,
                    c.columnFilters.currentValue[i]
                );
            }
        }
        if (c.cbFilter) {
            this.modelProvider.setSelectedFilter(c.cbFilter.currentValue);
        }
        if (
            (c.cbFilter && !c.cbFilter.isFirstChange()) ||
            (c.selectedIndices && !c.selectedIndices.isFirstChange()) ||
            (c.columnFilters && !c.columnFilters.isFirstChange())
        ) {
            this.updateVisible();
            this.updateAllSelected();
        }
    }

    ngOnInit() {
        this.doInit();
    }

    doInit(): void {
        this.dataViewWidth = this.tableMaxWidth;
        this.vScroll = {
            ...DEFAULT_VIRTUAL_SCROLL_SETTINGS,
            ...this.virtualScrolling,
        };
        this.vScroll.viewOverflow.horizontal ??= 1;
        this.vScroll.viewOverflow.vertical ??= 1;
        this.nrColumn = this.modelProvider.isNrColumn(true);
        this.cbColumn = this.modelProvider.isCbColumn(true);
        this.isPreview = this.modelProvider.isPreview();
        this.tinyFilters = this.modelProvider.isTinyFilters();
        if (this.isPreview) {
            this.vScroll.enabled = false;
            this.dataViewWidth = "fit-content";
        }
        this.isVirtual = this.vScroll.enabled;
        // Detach change detection because most of this component is based on pure DOM manipulation
        this.cdr.detach();
        if (this.vScroll.enabled) {
            this.startCellPurifying();
        }
        const {rows, columns} = this.modelProvider.getDimension();
        this.rowAxis = new GridAxisManager(
            rows,
            this.vScroll.enabled,
            VIRTUAL_SCROLL_TABLE_BORDER_SPACING,
            (i) => [this.modelProvider.getRowHeight(i) ?? 0, true],
            (i) => this.modelProvider.showRow(i)
        );
        this.colAxis = new GridAxisManager(
            columns,
            this.vScroll.enabled,
            VIRTUAL_SCROLL_TABLE_BORDER_SPACING,
            (i) => this.modelProvider.getColumnWidth(i),
            (i) => this.modelProvider.showColumn(i),
            (o) => o < this.fixedColumnCount
        );
        this.fixedColAxis = new GridAxisManager(
            columns,
            this.vScroll.enabled,
            VIRTUAL_SCROLL_TABLE_BORDER_SPACING,
            (i) => this.modelProvider.getColumnWidth(i),
            (i) => this.modelProvider.showColumn(i),
            (o) => o >= this.fixedColumnCount
        );
        this.updateTableSummary();
    }

    async ngAfterViewInit() {
        // Scrolling can cause change detection on some cases, which slows down the table
        // Since scrolling is
        // * Only used in vscrolling mode
        // * Doesn't change the template
        // it's better to run scroll events outside zones
        this.zone.runOutsideAngular(() => {
            this.r2.listen(this.mainDataContainer.nativeElement, "scroll", () =>
                this.handleScroll()
            );
        });
        this.zone.runOutsideAngular(() => {
            // Some versions of iOS seem to trigger resize events on scroll:
            // https://stackoverflow.com/questions/8898412/iphone-ipad-triggering-unexpected-resize-events
            let prevWidth = window.innerWidth;
            window.addEventListener("resize", () => {
                const curWidth = window.innerWidth;
                if (prevWidth == curWidth) {
                    return;
                }
                prevWidth = curWidth;
                this.handleWindowResize();
            });
        });
        await this.initView();
        this.modelProvider.handleChangeFilter();
        this.updateVisible();
    }

    private async initView() {
        this.initTableCaches();
        // Run table building in multiple frames to ensure layout happens so that size of elements is known
        await runMultiFrame(this.buildTable());
    }

    async showTableWidthExportDialog(evt: MouseEvent, recompute = false) {
        evt.preventDefault();
        if (this.recomputingSize) {
            return;
        }
        let colWidths = this.idealColHeaderWidth;
        if (recompute) {
            this.recomputingSize = true;
            this.c();
            await timeout();
            colWidths = this.computeIdealColumnWidth(true);
            this.recomputingSize = false;
            this.c();
            await timeout();
        }
        await showCopyWidthsDialog({columnWidths: colWidths});
    }

    private initTableCaches() {
        this.dataTableCache = new TableDOMCache(
            this.mainDataBody.nativeElement
        );
        if (this.fixedDataBody) {
            this.fixedTableCache = new TableDOMCache(
                this.fixedDataBody.nativeElement
            );
        }
        this.idTableCache = new TableDOMCache(
            this.idBody.nativeElement,
            "td",
            (cell, rowOrdinal, colOrdinal) => {
                if (colOrdinal == 0) {
                    cell.className = "nr-column";
                    if (!this.nrColumn) {
                        cell.setAttribute("do-not-add", "true");
                    }
                    return;
                }
                if (!this.cbColumn) {
                    cell.setAttribute("do-not-add", "true");
                    return;
                }
                cell.className = "cb-column";
                const cb = el("input", {
                    type: "checkbox",
                });
                cell.appendChild(cb);
                const rowIndex = this.rowAxis.visibleItems[rowOrdinal];
                cb.checked = this.selectedIndices.has(rowIndex);
                cb.disabled = !this.modelProvider.isRowSelectable(rowIndex);
            }
        );
        const makeHeader = (cell: HTMLTableCellElement) => {
            applyBasicStyle(cell, this.headerStyle);
            cell.classList.add("headers");
            cell.appendChild(el("span")); // Header text
            cell.appendChild(
                el("span", {
                    className: "sort-marker",
                })
            ); // Sort marker
        };
        this.headerIdTableCache = new TableDOMCache(
            this.headerIdBody.nativeElement,
            "td",
            makeHeader
        );
        if (this.fixedColHeaderIdBody) {
            this.fixedColHeaderIdTableCache = new TableDOMCache(
                this.fixedColHeaderIdBody.nativeElement,
                "td",
                makeHeader
            );
        }
        const makeFilter = (
            cell: HTMLTableCellElement,
            rowOrdinal: number,
            colOrdinal: number
        ) => {
            const inp = el("input", {
                type: "text",
            });
            cell.appendChild(inp);
            cell.className = "filter";

            const f = this.columnFilters[this.colAxis.visibleItems[colOrdinal]];
            if (f) {
                inp.value = f;
            }
        };
        let cls = "filters-row";
        if (this.tinyFilters) {
            cls += " tiny";
        }
        this.filterTableCache = new TableDOMCache(
            this.filterBody.nativeElement,
            "td",
            makeFilter,
            cls
        );
        if (this.fixedColFilterBody) {
            this.fixedColFilterTableCache = new TableDOMCache(
                this.fixedColFilterBody.nativeElement,
                "td",
                makeFilter,
                cls
            );
        }
    }

    // endregion

    // region Resizing

    private handleWindowResize(): void {
        this.updateHeaderCellSizes();
        if (!this.vScroll.enabled) {
            return;
        }
        this.viewport = this.getViewport();
        runMultiFrame(this.renderViewport());
    }

    private updateHeaderTableSizes(): void {
        if (!this.viewport) {
            return;
        }
        // Apparently to correctly handle column header table, we have to set its size to match that of data
        // and then add margin to pad the scrollbar width
        if (
            // !this..isPreview &&
            this.rowAxis.visibleItems.length != 0
        ) {
            this.headerContainer.nativeElement.style.width = px(
                this.dataTableWidth
            );
            this.headerContainer.nativeElement.style.marginRight = px(
                this.verticalScrollbar
            );
        } else {
            this.headerContainer.nativeElement.style.width = `auto`;
        }
        // For height it looks like it's enough to just set the height correctly
        this.idContainer.nativeElement.style.maxHeight = px(
            this.dataTableHeight + 2
        );
        if (this.fixedDataContainer) {
            this.fixedDataContainer.nativeElement.style.maxHeight = px(
                this.dataTableHeight + 2
            );
        }
    }

    private updateHeaderCellSizes(): void {
        this.updateHeaderTableSizes();
        this.updateColumnHeaderCellSizes();
        if (!this.isPreview) {
            // no idea to go thru all rows in preview mode
            this.updateRowHeaderCellSizes();
        }
        this.updateSummaryCellSizes();
    }

    private updateHeaderSizesForCell(row: number, column: number) {
        const rowHeight = this.getHeaderRowHeight(row);
        const columnWidth = this.getHeaderColumnWidth(column);

        const trRow = this.idTableCache.getRow(
            this.rowAxis.indexToOrdinal[row]
        );
        trRow.style.height = px(rowHeight);

        let axis = this.colAxis;
        let headers = this.headerIdTableCache;
        let filters = this.filterTableCache;
        const inCache = (cache?: TableDOMCache): cache is TableDOMCache =>
            columnInCache(column, this.fixedColAxis, cache);
        if (
            inCache(this.fixedColHeaderIdTableCache) &&
            inCache(this.fixedColFilterTableCache)
        ) {
            headers = this.fixedColHeaderIdTableCache;
            filters = this.fixedColFilterTableCache;
            axis = this.fixedColAxis;
        }

        const headerCell = headers.getCell(0, axis.indexToOrdinal[column]);
        const filterCell = filters.getCell(0, axis.indexToOrdinal[column]);
        headerCell.style.width = px(columnWidth);
        headerCell.style.maxWidth = px(columnWidth);
        filterCell.style.width = px(columnWidth);
    }

    private updateRowHeaderCellSizes(): void {
        const {vertical} = this.viewport;
        this.idTableCache.setSize(this.viewport.vertical.count, 2);
        // Get sizes in batch for speed
        const sizes = Array.from(new Array(vertical.count)).map(
            (value, index) =>
                this.getHeaderRowHeight(
                    this.rowAxis.visibleItems[index + vertical.startOrdinal]
                )
        );
        // Ensure the ID column is at least the size of the summary number column (needed for filtering)
        const nrColEl =
            this.summaryTable.nativeElement.querySelector(".nr-column");
        const minWidth =
            nrColEl instanceof HTMLElement ? nrColEl.offsetWidth : 0;
        for (let row = 0; row < vertical.count; row++) {
            const tr = this.idTableCache.getRow(row);
            tr.style.height = px(sizes[row]);
            const idCell = this.idTableCache.getCell(row, 0);
            idCell.style.minWidth = px(minWidth);
        }
    }

    protected filterRowPlaceholders: number[] = [];
    protected filterRowCount: number = 0;

    private getFilterRowCount(): number {
        const newCount = this.modelProvider.getFilterRowCount();
        const cur = this.filterRowPlaceholders.length;

        if (newCount - 1 > cur) {
            // Kasvatus ilman silmukoita
            this.filterRowPlaceholders.length = newCount - 1;
            this.filterRowPlaceholders.fill(0, 0, newCount - 1);
        } else if (newCount - 1 < cur) {
            // Pienennys ilman silmukoita
            this.filterRowPlaceholders.length = Math.max(newCount - 1, 0);
        }
        this.filterRowCount = newCount;
        return newCount;
    }

    private updateColumnHeaderCellSizes(updateFixed = true): void {
        const update = (
            axis: GridAxisManager,
            start: number,
            count: number,
            headers?: TableDOMCache,
            filters?: TableDOMCache
        ) => {
            if (!headers || !filters) {
                return;
            }
            headers.setSize(1, count);
            const filterRows = this.getFilterRowCount();
            filters.setSize(filterRows, count);
            const sizes = Array.from(new Array(count)).map((value, index) =>
                this.getHeaderColumnWidth(axis.visibleItems[index + start])
            );
            for (let column = 0; column < count; column++) {
                const width = sizes[column];
                const headerCell = headers.getCell(0, column);
                headerCell.hidden = false;
                headerCell.style.width = px(width);
                headerCell.style.maxWidth = px(width);
                for (let irow = 0; irow < filterRows; irow++) {
                    const filterCell = filters.getCell(irow, column);
                    filterCell.hidden = false;
                    filterCell.style.width = px(width);
                }
            }
        };
        if (!this.viewport) {
            return;
        }
        const {horizontal} = this.viewport;

        update(
            this.colAxis,
            horizontal.startOrdinal,
            horizontal.count,
            this.headerIdTableCache,
            this.filterTableCache
        );
        if (updateFixed) {
            update(
                this.fixedColAxis,
                0,
                this.fixedColumnCount,
                this.fixedColHeaderIdTableCache,
                this.fixedColFilterTableCache
            );
        }
    }

    // endregion

    // region Virtual scrolling

    private updateSummaryCellSizes(): void {
        let width = "auto";
        const nrCelEl = this.allVisibleCell?.nativeElement;
        if (!nrCelEl) {
            return;
        }
        // TODO: Maybe this could be used in every case?
        width = px(nrCelEl.offsetWidth);
        if (width) {
            this.summaryTable.nativeElement
                .querySelectorAll(".nr-column")
                .forEach((e) => {
                    if (e instanceof HTMLElement) {
                        e.style.width = width;
                    }
                });
        }

        if (this.getFilterRowCount() === 0) {
            return;
        }
        const summaryTotalHeaderHeight =
            // this.headerIdTableCache?.getRow(0).offsetHeight;
            this.headerIdTableCache?.getRow(0).getBoundingClientRect().height;
        const filterHeaderHeight =
            // this.filterTableCache?.getRow(0).offsetHeight;
            this.filterTableCache?.getRow(0).getBoundingClientRect().height;
        if (summaryTotalHeaderHeight && filterHeaderHeight) {
            const [summaryHeader, filterHeader] =
                this.summaryTable.nativeElement.getElementsByTagName("tr");
            summaryHeader.style.height = px(summaryTotalHeaderHeight);
            if (filterHeader) {
                filterHeader.style.height = px(filterHeaderHeight);
            }
        }
    }

    private updateVTable(opts?: {
        skipIfSame?: boolean;
        updateHeader?: boolean;
    }) {
        if (!this.shouldRenderTable) {
            return;
        }
        const options = {
            skipIfSame: false,
            updateHeader: false,
            ...opts,
        };
        // Set viewport already here to account for subsequent handlers
        const newViewport = this.getViewport();
        if (options.skipIfSame && viewportsEqual(this.viewport, newViewport)) {
            this.scheduledUpdate = false;
            return;
        }
        this.scrollDiff = {
            vertical:
                newViewport.vertical.startOrdinal -
                    this.viewport?.vertical.startOrdinal || 0,
            horizontal:
                newViewport.horizontal.startOrdinal -
                this.viewport?.horizontal.startOrdinal,
        };
        this.viewport = newViewport;
        this.updateTableTransform();
        void runMultiFrame(this.renderViewport(options.updateHeader));
    }

    private isOutsideSafeViewZone(): boolean {
        const isOver = (
            clientScroll: number,
            clientSize: number,
            items: VisibleItems,
            axis: GridAxisManager
        ) => {
            const overTop =
                items.startOrdinal > 0 &&
                clientScroll - items.startPosition < 0;
            const isNotAtEnd =
                items.startOrdinal + items.count < axis.visibleItems.length;
            const overBottom =
                isNotAtEnd &&
                items.startPosition + items.size - clientScroll < clientSize;
            return overTop || overBottom;
        };
        const data = this.mainDataContainer.nativeElement;
        const overVertical =
            this.rowAxis.isVirtual &&
            isOver(
                data.scrollTop,
                data.clientHeight,
                this.viewport.vertical,
                this.rowAxis
            );
        const overHorizontal =
            this.colAxis.isVirtual &&
            isOver(
                data.scrollLeft,
                data.clientWidth,
                this.viewport.horizontal,
                this.colAxis
            );
        return overHorizontal || overVertical;
    }

    private handleScroll(): void {
        this.syncHeaderScroll();
        if (!this.vScroll.enabled) {
            return;
        }
        if (this.scheduledUpdate) {
            return;
        }
        if (!this.isOutsideSafeViewZone()) {
            return;
        }
        this.scheduledUpdate = true;
        this.updateVTable({
            skipIfSame: true,
        });
    }

    private *renderViewport(updateHeader: boolean = false): Generator {
        // Do not get "global" values, because they may change during yields
        // const {vertical, horizontal} = this.viewport;
        // console.log(vertical, horizontal);
        this.dataTableCache.setSize(
            this.viewport.vertical.count,
            this.viewport.horizontal.count
        );
        this.fixedTableCache?.setSize(
            this.viewport.vertical.count,
            this.fixedColumnCount
        );
        this.idTableCache?.setSize(this.viewport.vertical.count, 2);
        this.headerIdTableCache?.setSize(1, this.viewport.horizontal.count);
        const filterRows = this.getFilterRowCount();
        this.filterTableCache?.setSize(
            filterRows,
            this.viewport.horizontal.count
        );
        const updateCache = (
            rowNumber: number,
            colStart: number,
            colCount: number,
            colAxis?: GridAxisManager,
            dataCache?: TableDOMCache,
            updateCellStyle: boolean = true
        ) => {
            if (!dataCache || !colAxis) {
                return;
            }
            const tr = dataCache.getRow(rowNumber);
            tr.hidden = false;
            const vertical = this.viewport.vertical;
            const rowIndex =
                this.rowAxis.visibleItems[vertical.startOrdinal + rowNumber];
            this.updateRow(tr, rowIndex);
            for (
                let columnNumber = 0;
                columnNumber < colCount;
                columnNumber++
            ) {
                const td = dataCache.getCell(rowNumber, columnNumber);
                td.hidden = false;
                const columnIndex =
                    colAxis.visibleItems[colStart + columnNumber];
                this.updateCell(
                    td,
                    rowIndex,
                    columnIndex,
                    this.getCellValue(rowIndex, columnIndex),
                    updateCellStyle
                );
            }
        };
        const render = (
            startRowOrdinal: number,
            endRowOrdinal: number,
            updateCellStyles: boolean
        ) => {
            for (
                let rowOrdinal = startRowOrdinal;
                rowOrdinal < endRowOrdinal;
                rowOrdinal++
            ) {
                const {vertical, horizontal} = this.viewport;
                const rowNumber = rowOrdinal - vertical.startOrdinal;
                const rowIndex = this.rowAxis.visibleItems[rowOrdinal];
                updateCache(
                    rowNumber,
                    horizontal.startOrdinal,
                    horizontal.count,
                    this.colAxis,
                    this.dataTableCache,
                    updateCellStyles
                );
                updateCache(
                    rowNumber,
                    0,
                    this.fixedColumnCount,
                    this.fixedColAxis,
                    this.fixedTableCache
                );

                const idRow = this.idTableCache.getRow(rowNumber);
                idRow.style.height = px(
                    this.modelProvider.getRowHeight(rowIndex) ?? 0
                );
                if (this.nrColumn) {
                    const idCell = this.idTableCache.getCell(rowNumber, 0);
                    idCell.textContent = `${rowIndex + this.columnIdStart}`;
                }
                if (this.cbColumn) {
                    const input = this.idTableCache
                        .getCell(rowNumber, 1)
                        .getElementsByTagName("input")[0];
                    input.checked = this.modelProvider.isRowChecked(rowIndex);
                    input.oninput = this.onRowCheckedHandler(input, rowIndex);
                }
            }
        };
        const updateDataCellStylesOnRender = this.scrollDiff.horizontal == 0;
        // Render in three parts:
        // * The main visible area
        // * The top part
        // * The bottom part
        // The order of top/bottom depends on the scrolling direction to reduce flickering
        let {vertical, horizontal} = this.viewport;
        // Save update times by updating column headers only when scrolling horizontally
        if (
            updateHeader ||
            (this.scrollDiff.horizontal != 0 && this.colAxis.isVirtual)
        ) {
            this.updateColumnHeaderCellSizes(true);
            this.updateColumnHeaders(true);

            for (
                let rowOrdinal = 0;
                rowOrdinal < vertical.count;
                rowOrdinal++
            ) {
                for (let col = 0; col < horizontal.count; col++) {
                    this.updateCellStyle(
                        this.dataTableCache.getCell(rowOrdinal, col),
                        this.rowAxis.visibleItems[
                            rowOrdinal + vertical.startOrdinal
                        ],
                        this.colAxis.visibleItems[col + horizontal.startOrdinal]
                    );
                }
            }
        }
        render(
            vertical.visibleStartOrdinal,
            vertical.visibleStartOrdinal + vertical.visibleCount,
            updateDataCellStylesOnRender
        );
        yield;
        vertical = this.viewport.vertical;
        horizontal = this.viewport.horizontal;
        let renderOrder = [
            () =>
                render(
                    vertical.startOrdinal,
                    vertical.visibleStartOrdinal,
                    updateDataCellStylesOnRender
                ),
            () =>
                render(
                    vertical.visibleStartOrdinal + vertical.visibleCount,
                    vertical.startOrdinal + vertical.count,
                    updateDataCellStylesOnRender
                ),
        ];
        if (this.scrollDiff.vertical > 0) {
            renderOrder = renderOrder.reverse();
        }
        for (const r of renderOrder) {
            r();
            yield;
        }
        const setTableVisibility = (vis: string) => {
            this.mainDataBody.nativeElement.style.visibility = vis;
            if (this.fixedDataTable) {
                this.fixedDataTable.nativeElement.style.visibility = vis;
            }
        };
        setTableVisibility("visible");
        // If we veered off the new safe view zone, we need to update it again!
        if (this.isOutsideSafeViewZone()) {
            // This could have been likely caused by fast scrolling, in which case hide the element to prevent
            // flickering
            setTableVisibility("hidden");
            this.viewport = this.getViewport();
            this.updateTableTransform();
            void runMultiFrame(this.renderViewport(updateHeader));
        } else {
            if (updateHeader) {
                this.updateHeaderTableSizes();
            }
            this.scheduledUpdate = false;
        }
    }

    // endregion

    // region Table building

    private syncHeaderScroll(): void {
        const header = this.headerContainer.nativeElement;
        const fixedColData = this.fixedDataContainer?.nativeElement;
        const data = this.mainDataContainer.nativeElement;
        const ids = this.idContainer.nativeElement;
        header.scrollLeft = data.scrollLeft;
        ids.scrollTop = data.scrollTop;
        if (fixedColData) {
            fixedColData.scrollTop = data.scrollTop;
        }
    }

    private updateTableTransform(): void {
        if (!this.vScroll.enabled) {
            return;
        }
        const idTable = this.idBody.nativeElement;
        const headerIdTable = this.headerIdBody.nativeElement;
        const filterIdTable = this.filterBody.nativeElement;
        this.mainDataBody.nativeElement.style.transform = `translateX(${this.viewport.horizontal.startPosition}px) translateY(${this.viewport.vertical.startPosition}px)`;
        if (this.fixedDataBody) {
            this.fixedDataBody.nativeElement.style.transform = `translateY(${this.viewport.vertical.startPosition}px)`;
        }
        idTable.style.transform = `translateY(${this.viewport.vertical.startPosition}px)`;
        headerIdTable.style.transform =
            filterIdTable.style.transform = `translateX(${this.viewport.horizontal.startPosition}px)`;
    }

    private setTableSizes(): void {
        if (!this.vScroll.enabled) {
            return;
        }
        const table = this.mainDataTable.nativeElement;
        const fixedColTable = this.fixedDataTable?.nativeElement;
        const idTable = this.idTable.nativeElement;
        const headerTable = this.headerTable.nativeElement;
        const totalWidth = this.colAxis.totalSize;
        const totalHeight = this.rowAxis.totalSize;
        if (totalWidth) {
            table.style.width = px(totalWidth);
            table.style.minWidth = px(totalWidth);
            table.style.maxWidth = px(totalWidth);
            headerTable.style.width = px(totalWidth);
        }
        if (totalHeight) {
            table.style.height = px(totalHeight + 2);
            idTable.style.height = px(totalHeight + 2);
            if (fixedColTable) {
                fixedColTable.style.height = px(totalHeight + 2);
            }
        }
        table.style.borderSpacing = px(VIRTUAL_SCROLL_TABLE_BORDER_SPACING);
        idTable.style.borderSpacing = px(VIRTUAL_SCROLL_TABLE_BORDER_SPACING);
        headerTable.style.borderSpacing = px(
            VIRTUAL_SCROLL_TABLE_BORDER_SPACING
        );
        if (fixedColTable) {
            fixedColTable.style.borderSpacing = px(
                VIRTUAL_SCROLL_TABLE_BORDER_SPACING
            );
        }
    }

    private getViewport(): Viewport {
        const data = this.mainDataContainer.nativeElement;
        const empty = (size: number): VisibleItems => ({
            startPosition: 0,
            count: size,
            startOrdinal: 0,
            visibleCount: 0,
            visibleStartOrdinal: 0,
            size: 0,
        });
        if (this.vScroll.enabled) {
            const viewportWidth =
                data.clientWidth *
                (1 + 2 * this.vScroll.viewOverflow.horizontal);
            const viewportHeight =
                data.clientHeight *
                (1 + 2 * this.vScroll.viewOverflow.vertical);
            return {
                horizontal: this.colAxis.getVisibleItemsInViewport(
                    data.scrollLeft -
                        data.clientWidth * this.vScroll.viewOverflow.horizontal,
                    viewportWidth,
                    data.scrollLeft,
                    data.clientWidth
                ),
                vertical: this.rowAxis.getVisibleItemsInViewport(
                    data.scrollTop -
                        data.clientHeight * this.vScroll.viewOverflow.vertical,
                    viewportHeight,
                    data.scrollTop,
                    data.clientHeight
                ),
            };
        }
        return {
            horizontal: empty(this.colAxis.visibleCount),
            // In non-vscroll mode all items are always in viewport, but they can be hidden
            vertical: empty(
                this.vScroll.enabled
                    ? this.rowAxis.visibleCount
                    : this.rowAxis.allCount
            ),
        };
    }

    private *buildTable(): Generator {
        // Visually hide the table to prevent any flickering owing to size syncing
        this.dataViewContainer.nativeElement.style.visibility = "hidden";
        // Sometimes table size style is not fully applied yet (e.g. open editor + click save quickly)
        // So we wait for a single frame to ensure DOM is laid out
        yield;
        const build = this.isPreview
            ? this.buildPreviewTable()
            : this.buildMainTable();
        for (const _ of build) {
            yield;
        }
        this.dataViewContainer.nativeElement.style.visibility = "visible";
        this.isLoading = false;
        this.c();
    }

    private *buildPreviewTable() {
        // Turn off vscroll in preview mode as no items are rendered in the first place
        this.viewport = this.getViewport();
        this.buildColumnHeaderTable();
        yield;
        this.updateHeaderCellSizes();
    }

    private *buildMainTable() {
        this.setTableSizes();
        this.viewport = this.getViewport();
        this.updateTableTransform();
        this.updateHeaderTableSizes();
        yield;

        this.buildColumnHeaderTable();
        this.buildRowHeaderTable();
        this.buildDataTable();

        // Force the main table to layout first so that we can compute the header sizes
        yield;

        this.updateHeaderCellSizes();
        const incomplete = this.colAxis.totalSizeIncomplete;
        if (this.vScroll.enabled && !this.colAxis.isVirtual) {
            this.updateIdealColumnWidth();
            this.updateVTable();
            yield;
            this.updateHeaderCellSizes();
        }
        if (this.reportSlowLoad) {
            if (this.sizeComputationTime > SLOW_SIZE_MEASURE_THRESHOLD) {
                this.showSlowMessageDialog();
            } else if (incomplete) {
                this.showIncompleteSizeMessageDialog();
            }
        }
    }

    private computeIdealColumnWidth(force = false) {
        if (!this.vScroll.enabled && !force) {
            return [];
        }
        const start = performance.now();
        const result: number[] = [];
        const measure = (colAxis?: GridAxisManager) => {
            if (!colAxis || (colAxis.isVirtual && !force)) {
                return [];
            }
            for (const col of colAxis.visibleItems) {
                let cWidth = 0;
                for (const row of this.rowAxis.visibleItems) {
                    const c = this.measureText(row, col);
                    cWidth = Math.max(c.width, cWidth);
                }
                result[col] = cWidth;
            }
        };
        measure(this.colAxis);
        measure(this.fixedColAxis);
        if (this.sizeContentContainer) {
            this.sizeContentContainer.textContent = "";
        }
        const end = performance.now();
        this.sizeComputationTime = Math.round((end - start) / 1000.0);
        return result;
    }

    private updateIdealColumnWidth(): void {
        this.idealColWidths = this.computeIdealColumnWidth();
    }

    private measureText(row: number, column: number) {
        if (!this.sizeContainer || !this.sizeContentContainer) {
            this.sizeContainer = document.createElement("div");
            this.sizeContentContainer = document.createElement("div");
            this.sizeContainer.appendChild(this.sizeContentContainer);
            this.sizeContainer.style.cssText = joinCss(this.tableStyle);
            applyBasicStyle(this.sizeContainer, {
                position: "absolute",
                float: "left",
                whiteSpace: "nowrap",
                visibility: "hidden",
            });
            this.dataViewContainer.nativeElement.appendChild(
                this.sizeContainer
            );
        }
        const colWidth = this.idealColHeaderWidth[column];
        if (colWidth) {
            this.sizeContentContainer.style.minWidth = px(colWidth);
        }
        this.sizeContentContainer.innerHTML =
            this.modelProvider.getCellContents(row, column);
        const size = this.sizeContentContainer.getBoundingClientRect();
        return {
            width: Math.ceil(size.width * 1.1),
            height: Math.ceil(size.height * 1.1),
        };
    }

    private buildDataTable(): void {
        const {vertical, horizontal} = this.viewport;
        const build = (
            colAxis: GridAxisManager,
            colStart: number,
            colCount: number,
            tbody?: HTMLTableSectionElement,
            cache?: TableDOMCache
        ) => {
            if (!tbody || !cache) {
                return;
            }
            const itemMap = this.vScroll.enabled
                ? this.rowAxis.visibleItems
                : this.rowAxis.itemOrder;
            cache.setSize(vertical.count, colCount);
            for (let rowNumber = 0; rowNumber < vertical.count; rowNumber++) {
                const rowIndex = itemMap[vertical.startOrdinal + rowNumber];
                this.updateRow(cache.getRow(rowNumber), rowIndex);
                for (
                    let columnNumber = 0;
                    columnNumber < colCount;
                    columnNumber++
                ) {
                    const columnIndex =
                        colAxis.visibleItems[colStart + columnNumber];
                    const cell = cache.getCell(rowNumber, columnNumber);
                    this.updateCell(
                        cell,
                        rowIndex,
                        columnIndex,
                        this.getCellValue(rowIndex, columnIndex)
                    );
                }
            }
            // Optimization in normal mode: sanitize whole tbody in place
            if (!this.vScroll.enabled) {
                DOMPurify.sanitize(tbody, {IN_PLACE: true});
            }
        };

        build(
            this.colAxis,
            horizontal.startOrdinal,
            horizontal.count,
            this.mainDataBody.nativeElement,
            this.dataTableCache
        );
        build(
            this.fixedColAxis,
            0,
            this.fixedColumnCount,
            this.fixedDataBody?.nativeElement,
            this.fixedTableCache
        );
    }

    private buildColumnHeaderTable(): void {
        this.headerIdTableCache.setSize(1, this.viewport.horizontal.count);
        const filterRows = this.getFilterRowCount();
        this.filterTableCache.setSize(
            filterRows,
            this.viewport.horizontal.count
        );
        this.fixedColHeaderIdTableCache?.setSize(1, this.fixedColumnCount);
        this.fixedColFilterTableCache?.setSize(
            filterRows,
            this.fixedColumnCount
        );
        const colIndices = this.updateColumnHeaders();
        for (const [cell, columnIndex] of colIndices) {
            this.idealColHeaderWidth[columnIndex] = Math.ceil(
                cell.getBoundingClientRect().width
            );
        }
    }

    private buildRowHeaderTable(): void {
        this.idTableCache.setSize(this.viewport.vertical.count, 2);
        const {vertical} = this.viewport;
        for (let row = 0; row < vertical.count; row++) {
            const rowIndex =
                this.rowAxis.visibleItems[row + vertical.startOrdinal];
            if (this.nrColumn) {
                const idCell = this.idTableCache.getCell(row, 0);
                idCell.textContent = `${rowIndex + this.columnIdStart}`;
            }
            if (this.cbColumn) {
                const cbCell = this.idTableCache.getCell(row, 1);
                const cb = cbCell.getElementsByTagName("input")[0];
                cb.oninput = this.onRowCheckedHandler(cb, rowIndex);
            }
        }
    }

    private updateColumnHeaders(
        updateFixed = true
    ): [HTMLTableCellElement, number][] {
        const update = (
            axis: GridAxisManager,
            start: number,
            count: number,
            headers?: TableDOMCache,
            filters?: TableDOMCache
        ) => {
            if (!headers || !filters) {
                return [];
            }
            const colIndices: [HTMLTableCellElement, number][] = [];
            for (let column = 0; column < count; column++) {
                const columnIndex = axis.visibleItems[column + start];
                const headerCell = headers.getCell(0, column);
                colIndices.push([headerCell, columnIndex]);
                const headerTitle = headerCell.getElementsByTagName("span")[0];
                headerTitle.textContent = `${this.modelProvider.getColumnHeaderContents(
                    columnIndex
                )}`;
                headerCell.onclick = this.onHeaderColumnClick(columnIndex);

                const sortSymbolEl = headerCell.getElementsByTagName("span")[1];
                const {symbol, style} =
                    this.modelProvider.getSortSymbolInfo(columnIndex);
                applyBasicStyle(sortSymbolEl, style);
                sortSymbolEl.textContent = symbol;

                for (let fr = 0; fr < this.getFilterRowCount(); fr++) {
                    const filterCell = filters.getCell(fr, column);
                    const input = filterCell.getElementsByTagName("input")[0];
                    input.value = this.modelProvider.getRowFilter(
                        fr,
                        columnIndex
                    );
                    input.oninput = this.onFilterInput(input, fr, columnIndex);
                    input.onfocus = this.onFilterFocus(input, fr, columnIndex);
                    input.onblur = this.onFilterBlur(input);
                    // if (this.lastFocusedIndex === columnIndex) {
                    //    input.focus();
                    // }
                }
            }
            return colIndices;
        };

        const {horizontal} = this.viewport;
        return [
            ...update(
                this.colAxis,
                horizontal.startOrdinal,
                horizontal.count,
                this.headerIdTableCache,
                this.filterTableCache
            ),
            ...(updateFixed
                ? update(
                      this.fixedColAxis,
                      0,
                      this.fixedColumnCount,
                      this.fixedColHeaderIdTableCache,
                      this.fixedColFilterTableCache
                  )
                : []),
        ];
    }

    private updateRow(
        row: HTMLTableRowElement,
        rowIndex: number
    ): HTMLTableRowElement {
        row.style.cssText = joinCss(this.modelProvider.stylingForRow(rowIndex));
        row.hidden =
            !this.vScroll.enabled && !this.modelProvider.showRow(rowIndex);
        const rowHeight = this.modelProvider.getRowHeight(rowIndex);
        if (rowHeight) {
            if (this.vScroll.enabled) {
                row.style.minHeight = px(rowHeight);
                row.style.maxHeight = px(rowHeight);
            }
            row.style.height = px(rowHeight);
            row.style.overflow = "hidden";
        }
        return row;
    }

    // endregion

    // region DOM handlers for common elements

    private updateCell(
        cell: HTMLTableCellElement,
        rowIndex: number,
        columnIndex: number,
        contents?: string,
        updateStyle = true
    ): HTMLTableCellElement {
        if (rowIndex === undefined || columnIndex === undefined) {
            return cell;
        }
        cell.hidden =
            !this.vScroll.enabled &&
            !this.modelProvider.showColumn(columnIndex);
        cell.onclick = async () => {
            await this.modelProvider.handleClickCell(rowIndex, columnIndex);
        };
        if (updateStyle) {
            this.updateCellStyle(cell, rowIndex, columnIndex);
        }
        if (contents !== undefined) {
            cell.innerHTML = contents;
            if (this.isVirtual) {
                cell.title = contents;
            }
        }
        return cell;
    }

    private updateCellStyle(
        cell: HTMLTableCellElement,
        rowIndex: number,
        columnIndex: number
    ): void {
        cell.className = this.modelProvider.classForCell(rowIndex, columnIndex);
        cell.style.cssText = joinCss(
            this.modelProvider.stylingForCell(rowIndex, columnIndex)
        );
        const colWidth = this.getDataColumnWidth(columnIndex);
        const idealWidth = this.idealColWidths[columnIndex];
        if (colWidth) {
            cell.style.minWidth = px(colWidth);
            // if (this.colAxis.isVirtual) {
            // TODO: why not update max and width here in non-virtual, does ot work if not done
            cell.style.width = px(colWidth);
            cell.style.maxWidth = px(colWidth);
        } else if (idealWidth) {
            cell.style.minWidth = px(idealWidth);
            cell.style.maxWidth = px(idealWidth);
            cell.style.width = px(idealWidth);
        }
        // }
    }

    private onRowCheckedHandler(checkBox: HTMLInputElement, rowIndex: number) {
        return () => {
            this.modelProvider.setRowChecked(rowIndex, checkBox.checked);
            this.modelProvider.handleChangeCheckbox(rowIndex);
        };
    }

    private onHeaderColumnClick(columnIndex: number) {
        return () => {
            if (this.isPreview) {
                return;
            }
            this.modelProvider.handleClickHeader(columnIndex);
        };
    }

    private onFilterInput(
        input: HTMLInputElement,
        filterRowIndex: number,
        columnIndex: number
    ) {
        return () => {
            if (this.isPreview) {
                return;
            }
            this.modelProvider.setRowFilter(
                filterRowIndex,
                columnIndex,
                input.value
            );
            this.modelProvider.handleChangeFilter();
        };
    }

    private lastFocusedRowIndex?: number;
    private lastFocusedIndex?: number;

    private onFilterFocus(
        input: HTMLInputElement,
        filterRowIndex: number,
        columnIndex: number
    ) {
        return () => {
            if (this.isPreview) {
                return;
            }
            this.lastFocusedRowIndex = filterRowIndex;
            this.lastFocusedIndex = columnIndex;
        };
    }

    private onFilterBlur(input: HTMLInputElement) {
        return () => {
            if (this.isPreview) {
                return;
            }
            this.lastFocusedRowIndex = undefined;
            this.lastFocusedIndex = undefined;
        };
    }

    // endregion

    // region Utils

    private showSlowMessageDialog() {
        this.showSlowLoadMessage = true;
        this.c();
    }

    hideSlowMessageDialog() {
        this.showSlowLoadMessage = false;
        this.c();
    }

    private showIncompleteSizeMessageDialog() {
        this.showIncompleteSizeMessage = true;
        this.c();
    }

    hideIncompleteSizeMessageDialog() {
        this.showIncompleteSizeMessage = false;
        this.c();
    }

    private getCellPosition(row: number, col: number) {
        const cell = this.getDataCell(row, col);
        const itemRowOrdinal = this.rowAxis.indexToOrdinal[row];
        const itemColOrdinal = this.colAxis.indexToOrdinal[col];
        if (!cell) {
            return {x: 0, y: 0, w: 0, h: 0};
        }
        return {
            x: this.colAxis.isVirtual
                ? this.colAxis.positionStart[itemColOrdinal]
                : cell.offsetLeft,
            y: this.rowAxis.isVirtual
                ? this.rowAxis.positionStart[itemRowOrdinal]
                : cell.offsetTop,
            w: cell.offsetWidth,
            h: cell.offsetHeight,
        };
    }

    private getDataCell(row: number, col: number) {
        const [cache, colAxis] = this.getDataCacheForColumn(col);
        const itemRowOrdinal = this.rowAxis.indexToOrdinal[row];
        const itemColOrdinal = colAxis.indexToOrdinal[col];
        const vpRowOrdinal = this.viewport.vertical.startOrdinal;
        const vpColOrdinal = this.viewport.horizontal.startOrdinal;
        if (this.rowAxis.isVirtual) {
            return cache.getCell(
                itemRowOrdinal - vpRowOrdinal,
                itemColOrdinal - vpColOrdinal
            );
        }
        // TODO: how should this be done in non-virtual mode? This seems to work?.
        return cache.getCell(row, itemColOrdinal);
    }

    private getCellValue(rowIndex: number, columnIndex: number): string {
        if (!this.vScroll.enabled || !this.cellValueCache) {
            return this.modelProvider.getCellContents(rowIndex, columnIndex);
        }
        const row = this.cellValueCache[rowIndex];
        if (row?.[columnIndex]) {
            return this.cellValueCache[rowIndex][columnIndex];
        }
        if (!row) {
            this.cellValueCache[rowIndex] = [];
        }
        const contents = this.modelProvider.getCellContents(
            rowIndex,
            columnIndex
        );
        if (contents) {
            // If the web worker hasn't sanitized the contents yet, do it ourselves
            return (this.cellValueCache[rowIndex][columnIndex] =
                DOMPurify.sanitize(contents));
        }
        return contents;
    }

    private invalidateCacheAt(rowIndex: number, columnIndex: number) {
        // In vscroll mode, we make use of the cell value cache to store all the sanitized table values for
        // quick lookup. To update the value, we invalidate by emptying it. The next time table is refreshed,
        // the new value will be polled from data model via getCellContents
        if (!this.vScroll.enabled) {
            return;
        }
        if (!this.cellValueCache?.[rowIndex]) {
            return;
        }
        this.cellValueCache[rowIndex][columnIndex] = "";
    }

    private startCellPurifying(): void {
        if (typeof Worker !== "undefined") {
            const worker = new Worker(
                new URL("./table-purify.worker", import.meta.url),
                {
                    type: "module",
                }
            );
            worker.onmessage = ({data}: {data: PurifyData}) => {
                this.cellValueCache[data.row] = data.data;
            };
            const {rows} = this.modelProvider.getDimension();
            for (let row = 0; row < rows; row++) {
                worker.postMessage({
                    row,
                    data: this.modelProvider.getRowContents(row),
                } as PurifyData);
            }
        }
    }

    private getDataColumnWidth(columnIndex: number): number {
        const [res, _] = this.modelProvider.getColumnWidth(columnIndex);
        if (res) {
            return res;
        }
        return this.idealColHeaderWidth[columnIndex];
    }

    private getDataCacheForColumn(
        columnIndex: number
    ): [TableDOMCache, GridAxisManager] {
        if (
            columnInCache(columnIndex, this.fixedColAxis, this.fixedTableCache)
        ) {
            return [this.fixedTableCache, this.fixedColAxis];
        }
        return [this.dataTableCache, this.colAxis];
    }

    private getHeaderColumnWidth(columnIndex: number): number {
        if (columnIndex == undefined || columnIndex < 0) {
            return 30;
        }
        if (this.idealColWidths[columnIndex]) {
            return this.idealColWidths[columnIndex];
        }
        const [res, _] = this.modelProvider.getColumnWidth(columnIndex);
        if (res) {
            return res;
        }
        const idealHeaderWidth = this.idealColHeaderWidth[columnIndex];
        if (
            this.rowAxis.visibleItems.length == 0 ||
            this.dataTableCache.rows.length == 0
        ) {
            return idealHeaderWidth;
        }
        const [cache, colAxis] = this.getDataCacheForColumn(columnIndex);
        try {
            const firstCellWidth = cache
                .getCell(
                    this.rowAxis.visibleItems[
                        this.viewport.vertical.startOrdinal
                    ],
                    colAxis.indexToOrdinal[columnIndex]
                )
                .getBoundingClientRect().width;
            return Math.max(idealHeaderWidth, firstCellWidth);
        } catch {
            return idealHeaderWidth;
        }
    }

    private getHeaderRowHeight(rowIndex: number): number {
        const res = this.modelProvider.getRowHeight(rowIndex);
        if (res !== undefined) {
            return res;
        }
        if (this.rowAxis.visibleItems.length == 0) {
            return 0;
        }
        // We make use of getBoundingClientRect because it returns proper fractional size
        // (which is needed for at least on Firefox for table size sync to work)
        return this.dataTableCache
            .getRow(
                this.rowAxis.visibleItems[this.viewport.vertical.startOrdinal]
            )
            .getBoundingClientRect().height;
    }

    // endregion
}
