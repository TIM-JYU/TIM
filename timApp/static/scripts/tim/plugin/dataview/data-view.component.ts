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
import {
    AfterViewInit,
    ChangeDetectionStrategy,
    ChangeDetectorRef,
    Component,
    ElementRef,
    HostBinding,
    Input,
    NgZone,
    OnInit,
    Renderer2,
    ViewChild,
} from "@angular/core";
import * as DOMPurify from "dompurify";

/**
 * General interface for an object that provides the data model for DataViewComponent.
 */
export interface DataModelProvider {
    getDimension(): { rows: number, columns: number };

    getColumnHeaderContents(columnIndex: number): string;

    getRowHeight(rowIndex: number): number | undefined;

    getColumnWidth(columnIndex: number): number | undefined;

    stylingForRow(rowIndex: number): Record<string, string>;

    stylingForCell(rowIndex: number, columnIndex: number): Record<string, string>;

    classForCell(rowIndex: number, columnIndex: number): string;

    handleClickCell(rowIndex: number, columnIndex: number): void;

    getCellContents(rowIndex: number, columnIndex: number): string;

    getRowContents(rowIndex: number): string[];

    showColumn(colIndex: number): boolean;

    showRow(rowIndex: number): boolean;

    setRowFilter(columnIndex: number, value: string): void;

    handleChangeFilter(): void;

    setRowChecked(rowIndex: number, checked: boolean): void;

    isRowChecked(rowIndex: number): boolean;

    handleChangeCheckbox(rowIndex: number): void;

    setSelectAll(state: boolean): void;

    setSelectedFilter(state: boolean): void;

    handleClickClearFilters(): void;

    getSortSymbolInfo(columnIndex: number): { symbol: string, style: Record<string, string> };

    handleClickHeader(columnIndex: number): void;

    isPreview(): boolean;
}

/**
 * Options related to virtual scrolling behaviour.
 */
export interface VirtualScrollingOptions {
    enabled: boolean;
    viewOverflow: TableArea;
}

/**
 * Table area, starting from (0, 0).
 */
interface TableArea {
    horizontal: number;
    vertical: number;
}

/**
 * Information about the visible items in a viewport's axis.
 */
interface VisibleItems {
    startIndex: number;
    startPosition: number;
    count: number;
    visibleStartIndex: number;
    visibleCount: number;
}

/**
 * A viewport that represents the actual visible portion of the table.
 */
interface Viewport {
    horizontal: VisibleItems;
    vertical: VisibleItems;
}

/**
 * A single axis manager for rows or columns. Handles caching visible items, item positions and order.
 * Provides simple API for determining the visible items.
 */
class GridAxisManager {
    /**
     * An offset list that represents start positions of each visible item, taking into account possible border size
     */
    positionStart: number[] = [];

    /**
     * List of all visible items in their correct final order. Use this to map visible row index to actual row index.
     */
    visibleItems: number[] = [];

    /**
     * List of all items in their display order.
     */
    itemOrder: number[] = [];

    indexToOrdinal: Record<number, number> = {};

    constructor(size: number,
                private isDataViewVirtual: boolean,
                private borderSpacing: number,
                private getSize: (i: number) => number,
                private showItem: (i: number) => boolean) {
        this.itemOrder = Array.from(new Array(size)).map((e, i) => i);
        this.refresh();
    }

    /**
     * Total size of the axis. In other words, the sum of all grid item sizes.
     */
    get totalSize(): number {
        return this.positionStart[this.positionStart.length - 1];
    }

    /**
     * Whether the scrolling for the axis is virtual.
     * Non-virtually scrolling axis doesn't have positionStart precomputed (as they can be determined from DOM)
     * and getVisibleItemsInViewport returns all items in the axis.
     */
    get isVirtual(): boolean {
        // If the total size of the axis is precomputed, we know the axis needs virtual scrolling
        return !!this.totalSize;
    }

    get visibleCount(): number {
        return this.visibleItems.length;
    }

    /**
     * Updates the visible items and recomputes the positions if needed.
     */
    refresh(): void {
        this.visibleItems = this.itemOrder.filter((i) => this.showItem(i));
        this.indexToOrdinal = {};
        for (const [ord, index] of this.visibleItems.entries()) {
            this.indexToOrdinal[index] = ord;
        }
        if (!this.isDataViewVirtual) {
            return;
        }
        this.positionStart = [0];
        for (let i = 0; i <= this.visibleItems.length - 1; i++) {
            const index = this.visibleItems[i];
            let size = this.getSize(index);
            if (size) {
                size += this.borderSpacing;
            }
            this.positionStart[i + 1] = this.positionStart[i] + size;
        }
    }

    /**
     * Gets all items that are visible in the given area.
     * Allows to specify the viewport and visible areas separately to account for overflow.
     *
     * @param vpStartPosition Start position of the viewport, in pixels.
     * @param vpSize Viewport's total size, in pixels.
     * @param visibleStartPosition Start position of the visible area of the viewport, in pixels.
     * @param visibleSize Size of the visible area of the viewport, in pixels.
     * @return A VisibleItems object that contains information about visible item indices.
     */
    getVisibleItemsInViewport(vpStartPosition: number, vpSize: number, visibleStartPosition: number, visibleSize: number): VisibleItems {
        if (!this.isVirtual) {
            return {
                startPosition: 0,
                count: this.visibleItems.length,
                startIndex: 0,
                visibleCount: 0,
                visibleStartIndex: 0,
            };
        }
        vpStartPosition = clamp(vpStartPosition, 0, this.totalSize);
        visibleStartPosition = clamp(visibleStartPosition, 0, this.totalSize);
        vpSize = Math.min(vpSize, this.totalSize - vpStartPosition);
        visibleSize = Math.min(visibleSize, this.totalSize - visibleStartPosition);
        const startIndex = this.search(vpStartPosition);
        const viewStartIndex = this.search(visibleStartPosition);
        const endIndex = this.search(vpStartPosition + vpSize);
        const viewEndIndex = this.search(visibleStartPosition + visibleSize);
        return {
            startIndex: startIndex,
            count: Math.min(endIndex - startIndex + 1, this.visibleItems.length - startIndex),
            startPosition: this.positionStart[startIndex],
            visibleStartIndex: viewStartIndex - startIndex,
            visibleCount: Math.min(viewEndIndex - viewStartIndex + 1, this.visibleItems.length - viewStartIndex),
        };
    }

    private search(position: number): number {
        let start = 0;
        let end = this.positionStart.length - 1;
        while (start < end) {
            const mid = Math.floor((start + end) / 2);
            const posStart = this.positionStart[mid];
            if (position < posStart) {
                end = mid - 1;
            } else if (position > posStart) {
                start = mid + 1;
            } else {
                return mid;
            }
        }
        return end;
    }
}

/**
 * A cache that contains all references to DOM elements in a table.
 * Additionally contains helper methods for resizing the table.
 *
 * The row/column numbering of the cache represents the positions of DOM elements in the visible table
 * and not actual row/column indices of the data.
 */
class TableDOMCache {
    rows: {
        rowElement: HTMLTableRowElement;
        cells: HTMLTableCellElement[];
    }[] = [];
    private activeArea: TableArea = {horizontal: 0, vertical: 0};

    constructor(private tbody: HTMLTableSectionElement,
                private cellElement: "td" | "th" = "td",
                private createCellContent?: (cell: HTMLTableCellElement, rowIndex: number, columnIndex: number) => void) {
    }

    /**
     * Gets the `tr` element at the given row.
     *
     * @param rowNumber The row number of the row.
     * @return HTMLTableRowElement of the row.
     */
    getRow(rowNumber: number): HTMLTableRowElement {
        if (rowNumber > this.activeArea.vertical) {
            throw new Error(`No row ${rowNumber} found! This should be unreachable!`);
        } else {
            return this.rows[rowNumber].rowElement;
        }
    }

    /**
     * Gets the cell element in the table.
     *
     * @param rowNumber Row number of the cell.
     * @param columnNumber Column number of the cell.
     * @return The HTMLTableCellElement of the cell at the given position.
     */
    getCell(rowNumber: number, columnNumber: number): HTMLTableCellElement {
        if (rowNumber > this.activeArea.vertical || columnNumber > this.activeArea.horizontal) {
            throw new Error(`No cell ${rowNumber}, ${columnNumber} found! This should be unreachable!`);
        }
        return this.rows[rowNumber].cells[columnNumber];
    }

    /**
     * Set the size of the visible table by adding or hiding DOM elements.
     *
     * @param rows Number of rows to show.
     * @param columns Number of columns to show.
     * @return True, if the table was resized in either axis.
     */
    setSize(rows: number, columns: number): boolean {
        const rowDelta = rows - this.activeArea.vertical;
        const colDelta = columns - this.activeArea.horizontal;
        if (rowDelta > 0) {
            // Too few rows => grow
            // Readd possible hidden rows
            for (let rowNumber = 0; rowNumber < rows; rowNumber++) {
                let row = this.rows[rowNumber];
                if (row) {
                    row.rowElement.hidden = false;
                    continue;
                }
                row = this.rows[rowNumber] = {
                    rowElement: el("tr"),
                    cells: [],
                };
                // Don't update col count to correct one yet, handle just rows first
                for (let columnNumber = 0; columnNumber < columns; columnNumber++) {
                    const cell = row.cells[columnNumber] = el(this.cellElement);
                    if (this.createCellContent) {
                        this.createCellContent(cell, rowNumber, columnNumber);
                    }
                    row.rowElement.appendChild(cell);
                }
                this.tbody.appendChild(row.rowElement);
            }
        } else if (rowDelta < 0) {
            // Too many rows => hide unused ones
            for (let rowNumber = rows; rowNumber < this.rows.length; rowNumber++) {
                this.rows[rowNumber].rowElement.hidden = true;
            }
        }

        if (colDelta > 0) {
            // Columns need to be added => make use of colcache here
            for (let rowNumber = 0; rowNumber < rows; rowNumber++) {
                const row = this.rows[rowNumber];
                for (let columnNumber = 0; columnNumber < columns; columnNumber++) {
                    let cell = row.cells[columnNumber];
                    if (cell) {
                        cell.hidden = false;
                    } else {
                        cell = row.cells[columnNumber] = el(this.cellElement);
                        if (this.createCellContent) {
                            this.createCellContent(cell, rowNumber, columnNumber);
                        }
                        row.rowElement.appendChild(cell);
                    }
                }
            }
        } else if (colDelta < 0) {
            // Need to hide columns
            for (let rowNumber = 0; rowNumber < rows; rowNumber++) {
                const row = this.rows[rowNumber];
                for (let colNumber = columns; colNumber < row.cells.length; colNumber++) {
                    row.cells[colNumber].hidden = true;
                }
            }
        }
        this.activeArea = {
            horizontal: columns,
            vertical: rows,
        };
        return rowDelta !== 0 && colDelta !== 0;
    }
}

const DEFAULT_VIRTUAL_SCROLL_SETTINGS: VirtualScrollingOptions = {
    enabled: false,
    viewOverflow: {horizontal: 1, vertical: 1},
};

// TODO: Right now, default TimTable style uses collapsed borders, in which case there is no need for spacing. Does this need a setting?
const VIRTUAL_SCROLL_TABLE_BORDER_SPACING = 0;

/**
 * A DOM-based data view component that supports virtual scrolling.
 * The component handles DOM generation and updating based on the DataModelProvider.
 */
@Component({
    selector: "tim-data-view",
    changeDetection: ChangeDetectionStrategy.OnPush,
    host: {
        class: "data-view",
    },
    template: `
        <div class="header" #headerContainer>
            <table [ngStyle]="tableStyle" #headerTable>
                <thead #headerIdBody></thead>
                <tbody #filterBody></tbody>
            </table>
        </div>
        <div *ngIf="fixedColumnCount > 0" class="fixed-col-header" #fixedColHeaderContainer>
            <span>foo</span>
            <table [ngStyle]="tableStyle" #fixedColHeaderTable>
                <thead #fixedColHeaderIdBody></thead>
                <tbody #fixedColFilterBody></tbody>
            </table>
        </div>
        <div class="summary">
            <table [ngStyle]="tableStyle" #summaryTable>
                <thead>
                <tr>
                    <td [style.width]="idHeaderCellWidth"
                        class="nrcolumn totalnr"
                        title="Click to show all"
                        (click)="clearFilters()"
                        #allVisibleCell>{{totalRows}}</td>
                    <td class="cbColumn" *ngIf="!this.modelProvider.isPreview()">
                        <input [(ngModel)]="cbAllVisibleRows"
                               (ngModelChange)="setAllVisible()"
                               type="checkbox"
                               title="Check for all visible rows">
                    </td>
                </tr>
                </thead>
                <tbody>
                <tr>
                    <td [style.width]="idHeaderCellWidth" class="nrcolumn totalnr">
                        <ng-container *ngIf="totalRows != visibleRows">{{visibleRows}}</ng-container>
                    </td>
                    <td class="cbColumn" *ngIf="!this.modelProvider.isPreview()">
                        <input type="checkbox"
                               title="Check to show only checked rows"
                               [(ngModel)]="cbFilter"
                               (ngModelChange)="setFilterSelected()">
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
        <div class="data" [style.maxHeight]="tableMaxHeight" #mainDataContainer>
            <table [ngClass]="tableClass" [ngStyle]="tableStyle" [class.virtual]="virtualScrolling.enabled"
                   #mainDataTable>
                <tbody class="content" #mainDataBody></tbody>
            </table>
            <ng-content></ng-content>
        </div>
    `,
    styleUrls: ["./data-view.component.scss"],
})
export class DataViewComponent implements AfterViewInit, OnInit {
    // region Fields
    @Input() modelProvider!: DataModelProvider; // TODO: Make optional and error out if missing
    @Input() virtualScrolling: Partial<VirtualScrollingOptions> = DEFAULT_VIRTUAL_SCROLL_SETTINGS;
    @Input() tableClass: { [klass: string]: unknown } = {};
    @Input() tableStyle: { [klass: string]: string } = {};
    @Input() headerStyle: Record<string, string> | null = {};
    @Input() columnIdStart: number = 1;
    @Input() tableMaxHeight: string = "2000em";
    @Input() tableMaxWidth: string = "100%";
    @Input() fixedColumnCount: number = 0;
    totalRows: number = 0;
    visibleRows: number = 0;
    idHeaderCellWidth: string = "";
    cbAllVisibleRows = false;
    cbFilter = false;
    @HostBinding("class.virtual") private isVirtual: boolean = false;
    @HostBinding("style.width") private tableWidth: string = "100%";
    @ViewChild("headerContainer") private headerContainer?: ElementRef<HTMLDivElement>;
    @ViewChild("headerTable") private headerTable?: ElementRef<HTMLTableElement>;
    @ViewChild("headerIdBody") private headerIdBody?: ElementRef<HTMLTableSectionElement>;
    @ViewChild("filterBody") private filterBody?: ElementRef<HTMLTableSectionElement>;
    @ViewChild("fixedColHeaderContainer") private fixedColHeaderContainer?: ElementRef<HTMLDivElement>;
    @ViewChild("fixedColHeaderTable") private fixedColHeaderTable?: ElementRef<HTMLTableElement>;
    @ViewChild("fixedColHeaderIdBody") private fixedColHeaderIdBody?: ElementRef<HTMLTableSectionElement>;
    @ViewChild("fixedColFilterBody") private fixedColFilterBody?: ElementRef<HTMLTableSectionElement>;
    @ViewChild("idContainer") private idContainer?: ElementRef<HTMLDivElement>;
    @ViewChild("idTable") private idTable?: ElementRef<HTMLTableElement>;
    @ViewChild("idBody") private idBody?: ElementRef<HTMLTableSectionElement>;
    @ViewChild("mainDataBody") private mainDataBody!: ElementRef<HTMLTableSectionElement>;
    @ViewChild("mainDataTable") private mainDataTable!: ElementRef<HTMLTableElement>;
    @ViewChild("mainDataContainer") private mainDataContainer!: ElementRef<HTMLDivElement>;
    @ViewChild("summaryTable") private summaryTable!: ElementRef<HTMLTableElement>;
    @ViewChild("allVisibleCell") private allVisibleCell!: ElementRef<HTMLTableDataCellElement>;
    private scrollDY = 0;
    private cellValueCache: Record<number, string[]> = {};
    private dataTableCache!: TableDOMCache;
    private idTableCache?: TableDOMCache;
    private headerIdTableCache?: TableDOMCache;
    private filterTableCache?: TableDOMCache;
    private scheduledUpdate = false;
    private viewport!: Viewport;
    private rowAxis!: GridAxisManager;
    private colAxis!: GridAxisManager;
    private vScroll: VirtualScrollingOptions = DEFAULT_VIRTUAL_SCROLL_SETTINGS;
    private idealColHeaderWidth: number[] = [];
    private tableBaseBorderWidth: number = -1;
    private sizeContainer?: HTMLDivElement;
    private sizeContentContainer?: HTMLDivElement;
    private idealColWidths: number[] = [];

    // endregion

    constructor(private r2: Renderer2, private zone: NgZone, private componentRef: ElementRef<HTMLElement>, private cdr: ChangeDetectorRef) {
    }

    private get tableBaseBorderWidthPx(): number {
        if (this.tableBaseBorderWidth < 0) {
            this.tableBaseBorderWidth = Number.parseFloat(getComputedStyle(this.allVisibleCell.nativeElement).borderLeftWidth);
        }
        return this.tableBaseBorderWidth;
    }

    private get dataTableWidth(): number {
        const widths = [
            this.mainDataContainer.nativeElement.offsetWidth,
            this.mainDataBody.nativeElement.offsetWidth,
            this.componentRef.nativeElement.offsetWidth - this.summaryTable.nativeElement.offsetWidth,
        ].filter((w) => w != 0);
        // Add table border width to prevent possible border cutoff when filtering in DOM mode
        return Math.min(...widths) + this.tableBaseBorderWidthPx;
    }

    private get dataTableHeight(): number {
        return Math.min(this.mainDataContainer.nativeElement.clientHeight, this.mainDataTable.nativeElement.clientHeight);
    }

    private get verticalScrollbar(): number {
        const e = this.mainDataContainer.nativeElement;
        return e.offsetWidth - e.clientWidth;
    }

    // region Event handlers for summary table

    setAllVisible() {
        this.modelProvider.setSelectAll(this.cbAllVisibleRows);
        this.modelProvider.handleChangeCheckbox(-1);
    }

    setFilterSelected() {
        this.modelProvider.setSelectedFilter(this.cbFilter);
        this.modelProvider.handleChangeFilter();
    }

    clearFilters() {
        if (this.modelProvider.isPreview()) {
            return;
        }
        this.modelProvider.handleClickClearFilters();
        this.cbFilter = false;
        if (this.filterTableCache) {
            for (const cell of this.filterTableCache.rows[0].cells) {
                const input = cell.getElementsByTagName("input")[0];
                input.value = "";
            }
        }
        // TODO: Clear sorting
    }

    // endregion

    // region Public update functions

    /**
     * Updates all visible elements in the table
     */
    updateVisible() {
        this.rowAxis.refresh();
        this.colAxis.refresh();
        if (this.vScroll.enabled) {
            this.setTableSizes();
            this.updateVTable({
                updateHeader: true,
            });
        } else {
            // For normal mode: simply hide rows that are no more visible/show hidden rows
            for (const [rowIndex, row] of this.dataTableCache.rows.entries()) {
                const shouldHide = !this.modelProvider.showRow(rowIndex);
                const hidden = row.rowElement.hidden;
                // Apparently always setting hidden will cause layout even if the value hasn't changed (?)
                if (shouldHide != hidden) {
                    row.rowElement.hidden = shouldHide;
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
        this.cdr.detectChanges();
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
            const input = this.idTableCache.getCell(rowIndex, 1).getElementsByTagName("input")[0];
            input.checked = this.modelProvider.isRowChecked(rowIndex);
        }
    }

    /**
     * Updates sort order of rows and updates visible sort markers
     * @param order New sort order to use. This is an array of row indices in the order they should be shown to the user.
     */
    updateRowSortOrder(order: number[]): void {
        this.rowAxis.itemOrder = order;
        this.rowAxis.refresh();

        if (this.vScroll.enabled) {
            this.updateVTable();
        } else {
            for (const rowIndex of order) {
                const tableRow = this.dataTableCache.getRow(rowIndex);
                this.mainDataBody.nativeElement.appendChild(tableRow);

                if (this.idTableCache && this.idBody) {
                    const rowHeader = this.idTableCache.getRow(rowIndex);
                    this.idBody.nativeElement.appendChild(rowHeader);
                }
            }
        }

        if (!this.headerIdTableCache) {
            return;
        }

        for (const columnIndex of this.colAxis.visibleItems) {
            const cell = this.headerIdTableCache.getCell(0, this.colAxis.indexToOrdinal[columnIndex]);
            const sortSymbolEl = cell.getElementsByTagName("span")[1];
            const {symbol, style} = this.modelProvider.getSortSymbolInfo(columnIndex);
            applyBasicStyle(sortSymbolEl, style);
            sortSymbolEl.textContent = symbol;
        }
    }

    /**
     * Updates style information for all cells
     */
    updateStyles(): void {
        if (this.vScroll.enabled) {
            this.updateVTable();
            return;
        }

        for (const rowIndex of this.dataTableCache.rows.keys()) {
            for (const columnIndex of this.colAxis.visibleItems) {
                const cell = this.dataTableCache.getCell(rowIndex, this.colAxis.indexToOrdinal[columnIndex]);
                this.updateCellStyle(cell, rowIndex, columnIndex);
            }
        }
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
    updateCellsContents(cells: { row: number, col: number }[]): void {
        if (this.vScroll.enabled) {
            for (const {row, col} of cells) {
                this.invalidateCacheAt(row, col);
                if (!this.colAxis.isVirtual) {
                    this.idealColWidths[col] = Math.max(this.measureText(row, col).width, this.idealColWidths[col]);
                }
            }
            this.updateVTable();
        } else {
            for (const {row, col} of cells) {
                const cell = this.getDataCell(row, col);
                this.updateCell(cell, row, col, this.modelProvider.getCellContents(row, col));
                DOMPurify.sanitize(cell, {IN_PLACE: true});
            }
        }
        requestAnimationFrame(() => {
            this.updateHeaderCellSizes();
            this.syncHeaderScroll();
        });
    }

    /**
     * Updates position of the editor to specified cell
     * @param row Currently selected row
     * @param col Currently selected cell
     */
    updateEditorPosition(row: number, col: number): void {
        // Because of Angular, we need to pass the editor as ng-content (otherwise Angular seems to loose reference to
        // the element and stop updating it properly)
        // TODO: Figure out a better way to integrate the editor
        const editor = this.mainDataContainer.nativeElement.querySelector(".timTableEditor");
        const editInput = this.mainDataContainer.nativeElement.querySelector(".timTableEditor>input");
        const inlineEditorButtons = this.mainDataContainer.nativeElement.querySelector(".timTableEditor>span");
        if (!editor) {
            return;
        }
        applyBasicStyle(editor as HTMLElement, {
            position: "relative",
            height: "0px",
            display: "block",
        });
        const {x, y, w} = this.getCellPosition(row, col);
        if (editInput) {
            applyBasicStyle(editInput as HTMLElement, {
                position: "absolute",
                top: `${y - this.mainDataTable.nativeElement.offsetHeight}px`,
                left: `${x}px`,
                width: `${w}px`,
            });
        }
        if (inlineEditorButtons) {
            const e = inlineEditorButtons as HTMLElement;
            const dir = row == 0 ? 1 : -1;
            applyBasicStyle(e, {
                position: "absolute",
                top: `${y - this.mainDataTable.nativeElement.offsetHeight + dir * e.offsetHeight}px`,
                left: `${x}px`,
            });
        }
    }

    // endregion

    // region Initialization

    ngOnInit(): void {
        this.tableWidth = this.tableMaxWidth;
        this.vScroll = {...DEFAULT_VIRTUAL_SCROLL_SETTINGS, ...this.virtualScrolling};
        if (this.modelProvider.isPreview()) {
            this.vScroll.enabled = false;
            this.tableWidth = "fit-content";
        }
        this.isVirtual = this.vScroll.enabled;
        // Detach change detection because most of this component is based on pure DOM manipulation
        this.cdr.detach();
        if (this.vScroll.enabled) {
            this.startCellPurifying();
        }
        const {rows, columns} = this.modelProvider.getDimension();
        this.rowAxis = new GridAxisManager(rows,
            this.vScroll.enabled,
            VIRTUAL_SCROLL_TABLE_BORDER_SPACING,
            (i) => this.modelProvider.getRowHeight(i) ?? 0,
            (i) => this.modelProvider.showRow(i));
        this.colAxis = new GridAxisManager(columns,
            this.vScroll.enabled,
            VIRTUAL_SCROLL_TABLE_BORDER_SPACING,
            (i) => this.modelProvider.getColumnWidth(i) ?? 0,
            (i) => this.modelProvider.showColumn(i));
        this.updateTableSummary();
    }

    ngAfterViewInit(): void {
        this.initTableCaches();
        // Run table building in multiple frames to ensure layout happens so that size of elements is known
        runMultiFrame(this.buildTable());

        // Scrolling can cause change detection on some cases, which slows down the table
        // Since scrolling is
        // * Only used in vscrolling mode
        // * Doesn't change the template
        // it's better to run scroll events outside zones
        this.zone.runOutsideAngular(() => {
            this.r2.listen(this.mainDataContainer.nativeElement, "scroll", () => this.handleScroll());
        });
        this.zone.runOutsideAngular(() => {
            window.addEventListener("resize", () => this.handleWindowResize());
        });
    }

    private initTableCaches() {
        this.dataTableCache = new TableDOMCache(this.mainDataBody.nativeElement);
        if (this.idBody) {
            this.idTableCache = new TableDOMCache(
                this.idBody.nativeElement,
                "td",
                (cell, rowIndex, columnIndex) => {
                    if (columnIndex == 0) {
                        cell.className = "nrcolumn";
                        return;
                    }
                    cell.className = "cbColumn";
                    cell.appendChild(el("input", {
                        type: "checkbox",
                    }));
                }
            );
        }
        if (this.headerIdBody) {
            this.headerIdTableCache = new TableDOMCache(this.headerIdBody.nativeElement, "td", (cell) => {
                applyBasicStyle(cell, this.headerStyle);
                cell.appendChild(el("span")); // Header text
                cell.appendChild(el("span", {
                    className: "sort-marker",
                })); // Sort marker
            });
        }
        if (this.filterBody) {
            this.filterTableCache = new TableDOMCache(
                this.filterBody.nativeElement,
                "td",
                (cell) => {
                    cell.appendChild(el("input", {
                        type: "text",
                    }));
                });
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
        if (!this.headerContainer || !this.idContainer || !this.headerTable || !this.viewport) {
            return;
        }
        // Apparently to correctly handle column header table, we have to set its size to match that of data
        // and then add margin to pad the scrollbar width
        if (!this.modelProvider.isPreview() && this.rowAxis.visibleItems.length != 0) {
            this.headerContainer.nativeElement.style.width = `${this.dataTableWidth}px`;
            this.headerContainer.nativeElement.style.marginRight = `${this.verticalScrollbar}px`;
        } else {
            this.headerContainer.nativeElement.style.width = `auto`;
        }
        // For height it looks like it's enough to just set the height correctly
        this.idContainer.nativeElement.style.maxHeight = `${this.dataTableHeight}px`;
    }

    private updateHeaderCellSizes(): void {
        this.updateHeaderTableSizes();
        this.updateColumnHeaderCellSizes();
        if (!this.modelProvider.isPreview()) {
            this.updateRowHeaderCellSizes();
        }
        this.updateSummaryCellSizes();
    }

    private updateRowHeaderCellSizes(): void {
        if (!this.idTableCache) {
            return;
        }
        const {vertical} = this.viewport;

        this.idTableCache.setSize(this.viewport.vertical.count, 2);
        // Get sizes in batch for speed
        const sizes = Array.from(new Array(vertical.count)).map((value, index) =>
            this.getHeaderRowHeight(this.rowAxis.visibleItems[index + vertical.startIndex])
        );
        // Ensure the ID column is at least the size of the summary number column (needed for filtering)
        const minWidth = (this.summaryTable.nativeElement.querySelector(".nrcolumn") as HTMLElement).offsetWidth;
        for (let row = 0; row < vertical.count; row++) {
            const tr = this.idTableCache.getRow(row);
            tr.style.height = `${sizes[row]}px`;
            const idCell = this.idTableCache.getCell(row, 0);
            idCell.style.minWidth = `${minWidth}px`;
        }
    }

    private updateColumnHeaderCellSizes(): void {
        if (!this.headerIdTableCache || !this.filterTableCache) {
            return;
        }
        const {horizontal} = this.viewport;

        this.headerIdTableCache.setSize(1, this.viewport.horizontal.count);
        this.filterTableCache.setSize(1, this.viewport.horizontal.count);
        const sizes = Array.from(new Array(horizontal.count)).map((value, index) =>
            this.getHeaderColumnWidth(this.colAxis.visibleItems[index + horizontal.startIndex])
        );
        for (let column = 0; column < horizontal.count; column++) {
            const width = sizes[column];
            const headerCell = this.headerIdTableCache.getCell(0, column);
            const filterCell = this.filterTableCache.getCell(0, column);
            headerCell.style.width = `${width}px`;
            headerCell.style.maxWidth = `${width}px`;
            filterCell.style.width = `${width}px`;
        }
    }

    private updateSummaryCellSizes(): void {
        if (this.rowAxis.visibleItems.length == 0) {
            return;
        }
        const width = !this.modelProvider.isPreview() ? this.idTableCache?.getCell(this.rowAxis.visibleItems[0], 0)?.offsetWidth : undefined;
        const summaryTotalHeaderHeight = this.headerIdTableCache?.getRow(0).offsetHeight;
        const filterHeaderHeight = this.filterTableCache?.getRow(0).offsetHeight;
        if (width) {
            this.summaryTable.nativeElement.querySelectorAll(".nrcolumn").forEach((e) => {
                if (e instanceof HTMLElement) {
                    e.style.width = `${width}px`;
                }
            });
        }
        if (summaryTotalHeaderHeight && filterHeaderHeight) {
            const [summaryHeader, filterHeader] = this.summaryTable.nativeElement.getElementsByTagName("tr");
            summaryHeader.style.height = `${summaryTotalHeaderHeight}px`;
            filterHeader.style.height = `${filterHeaderHeight}px`;
        }
    }

    // endregion

    // region Virtual scrolling

    private updateVTable(opts?: {
        skipIfSame?: boolean,
        updateHeader?: boolean
    }) {
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
        this.scrollDY = newViewport.vertical.startIndex - this.viewport.vertical.startIndex;
        this.viewport = newViewport;
        this.updateTableTransform();
        runMultiFrame(this.renderViewport(options.updateHeader));
    }

    private isOutsideSafeViewZone(): boolean {
        const data = this.mainDataContainer.nativeElement;
        const h = data.clientHeight * this.vScroll.viewOverflow.vertical;
        const w = data.clientWidth * this.vScroll.viewOverflow.horizontal;
        // Don't consider outside bounds if the axis is not virtual
        const overVertical = this.rowAxis.isVirtual && Math.abs(this.viewport.vertical.startPosition - data.scrollTop + h) > h;
        const overHorizontal = this.colAxis.isVirtual && Math.abs(this.viewport.horizontal.startPosition - data.scrollLeft + w) > w;
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

    private* renderViewport(updateHeader: boolean = false): Generator {
        const {vertical, horizontal} = this.viewport;
        this.dataTableCache.setSize(this.viewport.vertical.count, this.viewport.horizontal.count);
        this.idTableCache?.setSize(this.viewport.vertical.count, 2);
        this.headerIdTableCache?.setSize(1, this.viewport.horizontal.count);
        this.filterTableCache?.setSize(1, this.viewport.horizontal.count);
        const render = (startRow: number, endRow: number) => {
            for (let rowNumber = startRow; rowNumber < endRow; rowNumber++) {
                const tr = this.dataTableCache.getRow(rowNumber);
                tr.hidden = false;
                const rowIndex = this.rowAxis.visibleItems[vertical.startIndex + rowNumber];
                this.updateRow(tr, rowIndex);
                for (let columnNumber = 0; columnNumber < horizontal.count; columnNumber++) {
                    const td = this.dataTableCache.getCell(rowNumber, columnNumber);
                    td.hidden = false;
                    const columnIndex = this.colAxis.visibleItems[horizontal.startIndex + columnNumber];
                    this.updateCell(td, rowIndex, columnIndex, this.getCellValue(rowIndex, columnIndex));
                }

                if (this.idTableCache) {
                    const idRow = this.idTableCache.getRow(rowNumber);
                    idRow.style.height = `${this.modelProvider.getRowHeight(rowIndex)}px`;
                    const idCell = this.idTableCache.getCell(rowNumber, 0);
                    idCell.textContent = `${rowIndex + this.columnIdStart}`;
                    const input = this.idTableCache.getCell(rowNumber, 1).getElementsByTagName("input")[0];
                    input.checked = this.modelProvider.isRowChecked(rowIndex);
                    input.oninput = this.onRowCheckedHandler(input, rowIndex);
                }

                // If static size is set, there is possibility for vscrolling
                if (this.colAxis.isVirtual) {
                    this.updateColumnHeaders();
                }
            }
        };
        // Render in three parts:
        // * The main visible area
        // * The top part
        // * The bottom part
        let renderOrder = [
            () => render(0, vertical.visibleStartIndex),
            () => render(vertical.visibleStartIndex + vertical.visibleCount, vertical.count),
        ];
        if (this.scrollDY > 0) {
            renderOrder = renderOrder.reverse();
        }
        render(vertical.visibleStartIndex, vertical.visibleStartIndex + vertical.visibleCount);
        yield;
        for (const r of renderOrder) {
            r();
            yield;
        }
        this.mainDataBody.nativeElement.style.visibility = "visible";
        // If we veered off the new safe view zone, we need to update it again!
        if (this.isOutsideSafeViewZone()) {
            // This could have been likely caused by fast scrolling, in which case hide the element to prevent
            // flickering
            this.mainDataBody.nativeElement.style.visibility = "hidden";
            this.viewport = this.getViewport();
            this.updateTableTransform();
            runMultiFrame(this.renderViewport());
        } else {
            if (updateHeader) {
                this.updateHeaderTableSizes();
            }
            this.scheduledUpdate = false;
        }
    }

    private syncHeaderScroll(): void {
        if (!this.headerContainer || !this.idContainer) {
            return;
        }
        const header = this.headerContainer.nativeElement;
        const data = this.mainDataContainer.nativeElement;
        const ids = this.idContainer.nativeElement;
        header.scrollLeft = data.scrollLeft;
        ids.scrollTop = data.scrollTop;
    }

    private updateTableTransform(): void {
        if (!this.vScroll.enabled || !this.idBody || !this.headerTable || !this.filterBody) {
            return;
        }
        const idTable = this.idBody.nativeElement;
        const headerIdTable = this.headerTable.nativeElement;
        const filterTable = this.filterBody.nativeElement;
        this.mainDataBody.nativeElement.style.transform = `translateX(${this.viewport.horizontal.startPosition}px) translateY(${this.viewport.vertical.startPosition}px)`;
        idTable.style.transform = `translateY(${this.viewport.vertical.startPosition}px)`;
        headerIdTable.style.transform = filterTable.style.transform = `translateX(${this.viewport.horizontal.startPosition}px)`;
    }

    // endregion

    // region Table building

    private setTableSizes(): void {
        if (!this.vScroll.enabled || !this.idTable || !this.headerTable) {
            return;
        }
        const table = this.mainDataTable.nativeElement;
        const idTable = this.idTable.nativeElement;
        const headerTable = this.headerTable.nativeElement;
        const totalWidth = this.colAxis.totalSize;
        const totalHeight = this.rowAxis.totalSize;
        if (totalWidth) {
            table.style.width = `${totalWidth}px`;
            headerTable.style.width = `${totalWidth}px`;
        }
        if (totalHeight) {
            table.style.height = `${totalHeight}px`;
            idTable.style.height = `${totalHeight}px`;
        }
        table.style.borderSpacing = `${VIRTUAL_SCROLL_TABLE_BORDER_SPACING}px`;
        idTable.style.borderSpacing = `${VIRTUAL_SCROLL_TABLE_BORDER_SPACING}px`;
        headerTable.style.borderSpacing = `${VIRTUAL_SCROLL_TABLE_BORDER_SPACING}px`;
    }

    private getViewport(): Viewport {
        const data = this.mainDataContainer.nativeElement;
        const empty = (size: number): VisibleItems => ({
            startPosition: 0,
            count: size,
            startIndex: 0,
            visibleCount: 0,
            visibleStartIndex: 0,
        });
        if (this.vScroll.enabled) {
            const viewportWidth = data.clientWidth * (1 + 2 * this.vScroll.viewOverflow.horizontal);
            const viewportHeight = data.clientHeight * (1 + 2 * this.vScroll.viewOverflow.vertical);
            return {
                horizontal: this.colAxis.getVisibleItemsInViewport(
                    data.scrollLeft - data.clientWidth * this.vScroll.viewOverflow.horizontal,
                    viewportWidth,
                    data.scrollLeft,
                    data.clientWidth),
                vertical: this.rowAxis.getVisibleItemsInViewport(
                    data.scrollTop - data.clientHeight * this.vScroll.viewOverflow.vertical,
                    viewportHeight,
                    data.scrollTop,
                    data.clientHeight),
            };
        }
        return {
            horizontal: empty(this.colAxis.visibleCount),
            vertical: empty(this.rowAxis.visibleCount),
        };
    }

    private* buildTable(): Generator {
        // Visually hide the table to prevent any flickering owing to size syncing
        this.componentRef.nativeElement.style.visibility = "hidden";
        // Sometimes table size style is not fully applied yet (e.g. open editor + click save quickly)
        // So we wait for a single frame to ensure DOM is laid out
        yield;
        const build = this.modelProvider.isPreview() ? this.buildPreviewTable() : this.buildMainTable();
        for (const _ of build) {
            yield;
        }
        this.componentRef.nativeElement.style.visibility = "visible";
    }

    private* buildPreviewTable() {
        // Turn off vscroll in preview mode as no items are rendered in the first place
        this.viewport = this.getViewport();
        this.buildColumnHeaderTable();
        yield;
        this.updateHeaderCellSizes();
    }

    private* buildMainTable() {
        this.setTableSizes();
        this.viewport = this.getViewport();
        this.updateTableTransform();

        this.buildColumnHeaderTable();
        this.buildRowHeaderTable();
        this.buildDataTable();

        // Force the main table to layout first so that we can compute the header sizes
        yield;

        this.updateHeaderCellSizes();
        if (this.vScroll.enabled && !this.colAxis.isVirtual) {
            this.computeIdealColumnWidth();
            this.updateVTable();
            yield;
            this.updateHeaderCellSizes();
        }
    }

    private computeIdealColumnWidth(): void {
        if (!this.vScroll.enabled || this.colAxis.isVirtual) {
            return;
        }
        this.idealColWidths = [];
        for (const col of this.colAxis.visibleItems) {
            let cWidth = 0;
            for (const row of this.rowAxis.visibleItems) {
                const c = this.measureText(row, col);
                cWidth = Math.max(c.width, cWidth);
            }
            this.idealColWidths[col] = cWidth;
        }
        if (this.sizeContentContainer) {
            this.sizeContentContainer.textContent = "";
        }
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
            this.componentRef.nativeElement.appendChild(this.sizeContainer);
        }
        const colWidth = this.getDataColumnWidth(column);
        this.sizeContentContainer.style.minWidth = `${colWidth}px`;
        this.sizeContentContainer.innerHTML = this.modelProvider.getCellContents(row, column);
        const size = this.sizeContentContainer.getBoundingClientRect();
        return {width: Math.ceil(size.width * 1.1), height: Math.ceil(size.height * 1.1)};
    }

    private buildDataTable(): void {
        const tbody = this.mainDataBody.nativeElement;
        const {vertical, horizontal} = this.viewport;
        this.dataTableCache.setSize(vertical.count, horizontal.count);
        for (let rowNumber = 0; rowNumber < vertical.count; rowNumber++) {
            const rowIndex = this.rowAxis.visibleItems[vertical.startIndex + rowNumber];
            this.updateRow(this.dataTableCache.getRow(rowNumber), rowIndex);
            for (let columnNumber = 0; columnNumber < horizontal.count; columnNumber++) {
                const columnIndex = this.colAxis.visibleItems[horizontal.startIndex + columnNumber];
                const cell = this.dataTableCache.getCell(rowNumber, columnNumber);
                this.updateCell(cell, rowIndex, columnIndex, this.getCellValue(rowIndex, columnIndex));
            }
        }
        // Optimization in normal mode: sanitize whole tbody in place
        if (!this.vScroll.enabled) {
            DOMPurify.sanitize(tbody, {IN_PLACE: true});
        }
    }

    private buildColumnHeaderTable(): void {
        if (!this.headerIdTableCache || !this.filterTableCache) {
            return;
        }
        this.headerIdTableCache.setSize(1, this.viewport.horizontal.count);
        this.filterTableCache.setSize(1, this.viewport.horizontal.count);
        const colIndices = this.updateColumnHeaders();
        for (const [cell, columnIndex] of colIndices) {
            this.idealColHeaderWidth[columnIndex] = cell.offsetWidth;
        }
    }

    private buildRowHeaderTable(): void {
        if (!this.idTableCache) {
            return;
        }
        this.idTableCache.setSize(this.viewport.vertical.count, 2);
        const {vertical} = this.viewport;
        for (let row = 0; row < vertical.count; row++) {
            const rowIndex = this.rowAxis.visibleItems[row + vertical.startIndex];
            const idCell = this.idTableCache.getCell(row, 0);
            idCell.textContent = `${rowIndex + this.columnIdStart}`;

            const cbCell = this.idTableCache.getCell(row, 1);
            const cb = cbCell.getElementsByTagName("input")[0];
            cb.oninput = this.onRowCheckedHandler(cb, rowIndex);
        }
    }

    private updateColumnHeaders(): [HTMLTableCellElement, number][] {
        if (!this.headerIdTableCache || !this.filterTableCache) {
            return [];
        }
        const {horizontal} = this.viewport;
        const colIndices: [HTMLTableCellElement, number][] = [];
        for (let column = 0; column < horizontal.count; column++) {
            const columnIndex = this.colAxis.visibleItems[column + horizontal.startIndex];
            const headerCell = this.headerIdTableCache.getCell(0, column);
            colIndices.push([headerCell, columnIndex]);
            const headerTitle = headerCell.getElementsByTagName("span")[0];
            headerTitle.textContent = `${this.modelProvider.getColumnHeaderContents(columnIndex)}`;

            // TODO: Make own helper method because column index changes in vscroll mode
            headerCell.onclick = () => {
                if (this.modelProvider.isPreview()) {
                    return;
                }
                this.modelProvider.handleClickHeader(columnIndex);
            };

            const filterCell = this.filterTableCache.getCell(0, column);
            const input = filterCell.getElementsByTagName("input")[0];
            // TODO: Make own helper method because column index changes in vscroll mode
            input.oninput = () => {
                if (this.modelProvider.isPreview()) {
                    return;
                }
                this.modelProvider.setRowFilter(columnIndex, input.value);
                this.modelProvider.handleChangeFilter();
            };
        }
        return colIndices;
    }

    private updateRow(row: HTMLTableRowElement, rowIndex: number): HTMLTableRowElement {
        row.style.cssText = joinCss(this.modelProvider.stylingForRow(rowIndex));
        row.hidden = !this.vScroll.enabled && !this.modelProvider.showRow(rowIndex);
        const rowHeight = this.modelProvider.getRowHeight(rowIndex);
        if (rowHeight) {
            if (this.vScroll.enabled) {
                row.style.minHeight = `${rowHeight}px`;
                row.style.maxHeight = `${rowHeight}px`;
            }
            row.style.height = `${rowHeight}px`;
            row.style.overflow = "hidden";
        }
        return row;
    }

    private updateCell(cell: HTMLTableCellElement, rowIndex: number, columnIndex: number, contents?: string): HTMLTableCellElement {
        cell.hidden = !this.vScroll.enabled && !this.modelProvider.showColumn(columnIndex);
        cell.onclick = () => this.modelProvider.handleClickCell(rowIndex, columnIndex);
        this.updateCellStyle(cell, rowIndex, columnIndex);
        if (contents !== undefined) {
            cell.innerHTML = contents;
        }
        return cell;
    }

    private updateCellStyle(cell: HTMLTableCellElement, rowIndex: number, columnIndex: number): void {
        cell.className = this.modelProvider.classForCell(rowIndex, columnIndex);
        cell.style.cssText = joinCss(this.modelProvider.stylingForCell(rowIndex, columnIndex));
        const colWidth = this.getDataColumnWidth(columnIndex);
        const idealWidth = this.idealColWidths[columnIndex];
        if (colWidth) {
            cell.style.minWidth = `${colWidth}px`;
            cell.style.overflow = "hidden";
            if (this.colAxis.isVirtual) {
                cell.style.width = `${colWidth}px`;
                cell.style.maxWidth = `${colWidth}px`;
            } else if (idealWidth) {
                cell.style.minWidth = `${idealWidth}px`;
                cell.style.maxWidth = `${idealWidth}px`;
                cell.style.width = `${idealWidth}px`;
            }
        }
    }

    // endregion

    // region DOM handlers for common elements

    private onRowCheckedHandler(checkBox: HTMLInputElement, rowIndex: number) {
        return () => {
            this.modelProvider.setRowChecked(rowIndex, checkBox.checked);
            this.modelProvider.handleChangeCheckbox(rowIndex);
        };
    }

    // endregion

    // region Utils

    private getCellPosition(row: number, col: number) {
        const cell = this.getDataCell(row, col);
        const itemRowOrdinal = this.rowAxis.indexToOrdinal[row];
        const itemColOrdinal = this.colAxis.indexToOrdinal[col];
        if (!cell) {
            return {x: 0, y: 0, w: 0};
        }
        return {
            x: this.colAxis.isVirtual ? this.colAxis.positionStart[itemColOrdinal] : cell.offsetLeft,
            y: this.rowAxis.isVirtual ? this.rowAxis.positionStart[itemRowOrdinal] : cell.offsetTop,
            w: cell.offsetWidth,
        };
    }

    private getDataCell(row: number, col: number) {
        const itemRowOrdinal = this.rowAxis.indexToOrdinal[row];
        const itemColOrdinal = this.colAxis.indexToOrdinal[col];
        const vpRowOrdinal = this.viewport.vertical.startIndex;
        const vpColOrdinal = this.viewport.horizontal.startIndex;
        return this.dataTableCache.getCell(itemRowOrdinal - vpRowOrdinal, itemColOrdinal - vpColOrdinal);
    }

    private getCellValue(rowIndex: number, columnIndex: number): string {
        if (!this.vScroll.enabled) {
            return this.modelProvider.getCellContents(rowIndex, columnIndex);
        }
        const row = this.cellValueCache[rowIndex];
        if (row?.[columnIndex]) {
            return this.cellValueCache[rowIndex][columnIndex];
        }
        if (!row) {
            this.cellValueCache[rowIndex] = [];
        }
        const contents = this.modelProvider.getCellContents(rowIndex, columnIndex);
        if (contents) {
            // If the web worker hasn't sanitized the contents yet, do it ourselves
            this.cellValueCache[rowIndex][columnIndex] = DOMPurify.sanitize(contents);
            return contents;
        }
        return contents;
    }

    private invalidateCacheAt(rowIndex: number, columnIndex: number) {
        // In vscroll mode,
        if (!this.vScroll.enabled) {
            return;
        }
        this.cellValueCache[rowIndex][columnIndex] = "";
    }

    private startCellPurifying(): void {
        if (typeof Worker !== "undefined") {
            // Note: this triggers worker-plugin to be run
            // As of 27.7., worker-plugin triggers the following warning
            //
            // WARNING in new Worker() will only be bundled if passed a String.
            //
            // Because of ACE editor using workers without {type: "module"}
            // This was fixed in worker-plugin master:
            // https://github.com/GoogleChromeLabs/worker-plugin/pull/73
            // but it's not yet released nor part of Angular CLI.
            const worker = new Worker("./table-purify.worker", {type: "module"});
            worker.onmessage = ({data}: { data: PurifyData }) => {
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
        const res = this.modelProvider.getColumnWidth(columnIndex);
        if (res !== undefined) {
            return res;
        }
        return this.idealColHeaderWidth[columnIndex];
    }

    private getHeaderColumnWidth(columnIndex: number): number {
        if (this.idealColWidths[columnIndex]) {
            return this.idealColWidths[columnIndex];
        }
        const res = this.modelProvider.getColumnWidth(columnIndex);
        if (res !== undefined) {
            return res;
        }
        const idealHeaderWidth = this.idealColHeaderWidth[columnIndex];
        if (this.rowAxis.visibleItems.length == 0 || this.dataTableCache.rows.length == 0) {
            return idealHeaderWidth;
        }
        const firstCellWidth = this.dataTableCache.getCell(this.rowAxis.visibleItems[this.viewport.vertical.startIndex], this.colAxis.indexToOrdinal[columnIndex]).getBoundingClientRect().width;
        return Math.max(idealHeaderWidth, firstCellWidth);
    }

    private getHeaderRowHeight(rowIndex: number): number | undefined {
        const res = this.modelProvider.getRowHeight(rowIndex);
        if (res !== undefined) {
            return res;
        }
        if (this.rowAxis.visibleItems.length == 0) {
            return 0;
        }
        // We make use of getBoundingClientRect because it returns proper fractional size
        // (which is needed for at least on Firefox for table size sync to work)
        return this.dataTableCache.getRow(this.rowAxis.visibleItems[this.viewport.vertical.startIndex]).getBoundingClientRect().height;
    }

    // endregion
}

function el<K extends keyof HTMLElementTagNameMap>(tag: K, props?: { [k in keyof HTMLElementTagNameMap[K]]?: (HTMLElementTagNameMap[K])[k] }): HTMLElementTagNameMap[K] {
    return Object.assign(document.createElement(tag), props);
}

function clamp(val: number, min: number, max: number): number {
    return Math.max(Math.min(val, max), min);
}

function runMultiFrame(iter: Generator): void {
    const cb = () => {
        const result = iter.next();
        if (!result.done) {
            requestAnimationFrame(cb);
        }
    };
    requestAnimationFrame(cb);
}

function joinCss(obj: Record<string, string>) {
    let result = "";
    // eslint-disable-next-line guard-for-in
    for (const k in obj) {
        // noinspection JSUnfilteredForInLoop
        result = `${result}; ${k}:${obj[k]}`;
    }
    return result;
}

function applyBasicStyle(element: HTMLElement, style: Record<string, string> | null) {
    if (style != null) {
        Object.assign(element.style, style);
    }
}

function viewportsEqual(vp1: Viewport, vp2: Viewport) {
    const visItemsEqual = (v1: VisibleItems, v2: VisibleItems) => v1.startIndex == v2.startIndex && v1.count == v2.count;
    return visItemsEqual(vp1.vertical, vp2.vertical) && visItemsEqual(vp1.horizontal, vp2.horizontal);
}

interface PurifyData {
    row: number;
    data: string[];
}
