import {GridAxisManager} from "tim/plugin/dataview/gridAxisManager";
import {TableDOMCache} from "tim/plugin/dataview/tableDOMCache";

export interface CellIndex {
    x: number;
    y: number;
}

export interface PurifyData {
    row: number;
    data: string[];
}

/**
 * A viewport that represents the actual visible portion of the table.
 */
export interface Viewport {
    horizontal: VisibleItems;
    vertical: VisibleItems;
}

/**
 * Table area, starting from (0, 0).
 */
export interface TableArea {
    horizontal: number;
    vertical: number;
}

/**
 * Information about the visible items in a viewport's axis.
 */
export interface VisibleItems {
    startOrdinal: number;
    startPosition: number;
    count: number;
    visibleStartOrdinal: number;
    visibleCount: number;
    size: number;
}

export type HTMLElementProps<K extends keyof HTMLElementTagNameMap> = {
    [k in keyof HTMLElementTagNameMap[K]]: (HTMLElementTagNameMap[K])[k]
};

export function el<K extends keyof HTMLElementTagNameMap>(tag: K, props?: Partial<HTMLElementProps<K>>): HTMLElementTagNameMap[K] {
    return Object.assign(document.createElement(tag), props);
}

export function clamp(val: number, min: number, max: number): number {
    return Math.max(Math.min(val, max), min);
}

export function runMultiFrame(iter: Generator): void {
    const cb = () => {
        const result = iter.next();
        if (!result.done) {
            requestAnimationFrame(cb);
        }
    };
    requestAnimationFrame(cb);
}

export function joinCss(obj: Record<string, string>) {
    let result = "";
    for (const [key, val] of Object.entries(obj)) {
        result = `${result}; ${key}:${val}`;
    }
    return result;
}

export function applyBasicStyle(element: HTMLElement, style: Record<string, string> | null) {
    if (style != null) {
        Object.assign(element.style, style);
    }
}

export function viewportsEqual(vp1: Viewport, vp2: Viewport) {
    const visItemsEqual = (v1: VisibleItems, v2: VisibleItems) => v1.startOrdinal == v2.startOrdinal && v1.count == v2.count;
    return visItemsEqual(vp1.vertical, vp2.vertical) && visItemsEqual(vp1.horizontal, vp2.horizontal);
}

export function columnInCache(columnIndex: number, axis: GridAxisManager, cache?: TableDOMCache): cache is TableDOMCache {
    return cache !== undefined && axis.indexToOrdinal[columnIndex] !== undefined;
}

export function px(n: number) {
    return `${n}px`;
}
