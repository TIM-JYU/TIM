import {ComparatorFilter} from "tim/util/comparatorfilter";
import type {CellType} from "tim/plugin/timTable/tim-table.component";

/**
 * Check if all filter expressions match to row values. It's kind of "and operation"
 * over all filters if there is some condition
 * TODO: add value compare by < and > operators
 * @param filters comparator filters
 * @param row where to check values
 * @param contentGetter function to get cell content
 */
export function isMatch<T>(
    filters: (ComparatorFilter | RegExp)[],
    row: T,
    contentGetter: (s: T, colIndex: number) => CellType
) {
    for (let i = 0; i < filters.length; i++) {
        const cell = contentGetter(row, i);
        if (cell == null) {
            continue;
        }
        const s = cell.toString().toLowerCase();
        if (!filters[i].test(s)) {
            return false;
        }
    }
    return true;
}

export function buildFilters(
    filters: (string | undefined)[][]
): (ComparatorFilter | RegExp)[][] {
    const filterObjs: (ComparatorFilter | RegExp)[][] = [];
    for (const [i, fr] of filters.entries()) {
        let numberOfNonEmpty = 0;
        const filterObjsRow: (ComparatorFilter | RegExp)[] = [];
        for (const f of fr) {
            if (!f) {
                filterObjsRow.push(new RegExp(""));
                continue;
            }
            const cmpfltr = ComparatorFilter.makeNumFilter(f);
            filterObjsRow.push(cmpfltr ?? new RegExp(f.toLowerCase()));
            numberOfNonEmpty++;
        }
        if (i === 0 || numberOfNonEmpty > 0) {
            filterObjs.push(filterObjsRow);
        }
    }
    return filterObjs;
}

// filters may have many rows and they are consider as OR.
// So if any filter row in corresponding column matches, it is a match.
export function computeHiddenRowsFromFilters<T>(
    data: T[],
    isChecked: (rowIndex: number) => boolean,
    filters: (string | undefined)[][],
    checkedFilter: boolean,
    contentGetter: (c: T, colIndex: number) => CellType
) {
    const hiddenIndices = new Set<number>();
    const filterObjs = buildFilters(filters);

    // Early return if there's nothing to filter.
    if (!checkedFilter && filterObjs.length === 0) {
        return hiddenIndices;
    }

    dataRow: for (let i = 0; i < data.length; i++) {
        if (checkedFilter && !isChecked(i)) {
            hiddenIndices.add(i);
            continue;
        }
        for (const [icol, fr] of filterObjs.entries()) {
            if (!fr || fr.length === 0) {
                if (icol === 0) {
                    continue dataRow;
                }
                continue;
            }
            if (isMatch(fr, data[i], contentGetter)) {
                continue dataRow;
            }
        }
        hiddenIndices.add(i);
    }
    return hiddenIndices;
}
