import type {TableArea} from "tim/plugin/dataview/util";
import {el} from "tim/plugin/dataview/util";

/**
 * A cache that contains all references to DOM elements in a table.
 * Additionally contains helper methods for resizing the table.
 *
 * The row/column numbering of the cache represents the positions of DOM elements in the visible table
 * and not actual row/column indices of the data.
 */
export class TableDOMCache {
    rows: {
        rowElement: HTMLTableRowElement;
        cells: HTMLTableCellElement[];
    }[] = [];
    private activeArea: TableArea = {horizontal: 0, vertical: 0};

    constructor(
        private tbody: HTMLTableSectionElement,
        private cellElement: "td" | "th" = "td",
        private createCellContent?: (
            cell: HTMLTableCellElement,
            rowIndex: number,
            columnIndex: number
        ) => void,
        private rowClassName?: string
    ) {}

    /**
     * Gets the `tr` element at the given row.
     *
     * @param rowNumber The row number of the row.
     * @return HTMLTableRowElement of the row.
     */
    getRow(rowNumber: number): HTMLTableRowElement {
        if (rowNumber > this.activeArea.vertical) {
            throw new Error(
                `No row ${rowNumber} found! This should be unreachable!`
            );
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
        if (
            rowNumber > this.activeArea.vertical ||
            columnNumber > this.activeArea.horizontal
        ) {
            throw new Error(
                `No cell ${rowNumber}, ${columnNumber} found! This should be unreachable!`
            );
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
                if (this.rowClassName) {
                    row.rowElement.className = this.rowClassName;
                }
                // Don't update col count to correct one yet, handle just rows first
                for (
                    let columnNumber = 0;
                    columnNumber < columns;
                    columnNumber++
                ) {
                    const cell = (row.cells[columnNumber] = el(
                        this.cellElement
                    ));
                    if (this.createCellContent) {
                        this.createCellContent(cell, rowNumber, columnNumber);
                    }
                    row.rowElement.appendChild(cell);
                }
                this.tbody.appendChild(row.rowElement);
            }
        } else if (rowDelta < 0) {
            // Too many rows => hide unused ones
            for (
                let rowNumber = rows;
                rowNumber < this.rows.length;
                rowNumber++
            ) {
                this.rows[rowNumber].rowElement.hidden = true;
            }
        }

        if (colDelta > 0) {
            // Columns need to be added => make use of colcache here
            for (let rowNumber = 0; rowNumber < rows; rowNumber++) {
                const row = this.rows[rowNumber];
                for (
                    let columnNumber = 0;
                    columnNumber < columns;
                    columnNumber++
                ) {
                    let cell = row.cells[columnNumber];
                    if (cell) {
                        cell.hidden = false;
                    } else {
                        cell = row.cells[columnNumber] = el(this.cellElement);
                        if (this.createCellContent) {
                            this.createCellContent(
                                cell,
                                rowNumber,
                                columnNumber
                            );
                        }
                        row.rowElement.appendChild(cell);
                    }
                }
            }
        } else if (colDelta < 0) {
            // Need to hide columns
            for (let rowNumber = 0; rowNumber < rows; rowNumber++) {
                const row = this.rows[rowNumber];
                for (
                    let colNumber = columns;
                    colNumber < row.cells.length;
                    colNumber++
                ) {
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
