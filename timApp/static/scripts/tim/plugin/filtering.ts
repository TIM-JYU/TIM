import {ComparatorFilter} from "tim/util/comparatorfilter";
import {CellType} from "tim/plugin/timTable";

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

export function buildFilters(filters: string[]) {
    const filterObjs = [];
    for (const f of filters) {
        const cmpfltr = ComparatorFilter.makeNumFilter(f);
        filterObjs.push(cmpfltr ?? new RegExp(f.toLowerCase()));
    }
    return filterObjs;
}

export function computeHiddenRowsFromFilters<T>(
    data: T[],
    isChecked: (rowIndex: number) => boolean,
    filters: string[],
    checkedFilter: boolean,
    contentGetter: (c: T, colIndex: number) => CellType
) {
    const hiddenIndices = new Set<number>();
    const filterObjs = buildFilters(filters);

    // Early return if there's nothing to filter.
    if (!checkedFilter && filterObjs.length === 0) {
        return hiddenIndices;
    }

    for (let i = 0; i < data.length; i++) {
        if (checkedFilter && !isChecked(i)) {
            hiddenIndices.add(i);
            continue;
        }
        if (isMatch(filterObjs, data[i], contentGetter)) {
            continue;
        }
        hiddenIndices.add(i);
    }
    return hiddenIndices;
}
