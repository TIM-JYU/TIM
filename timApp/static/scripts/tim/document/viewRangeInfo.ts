import {documentglobals} from "../util/globals";
import {$http} from "../util/ngimport";
import {to} from "../util/utils";
import {ViewCtrl} from "./viewctrl";

// TODO: Are b and e always in the same order?
export const viewRangeRegExp = new RegExp("b=\\d+\&e=\\d+\(&preamble=(true|false)|)");
export const viewRangeCookieRegExp = new RegExp("r=\\d+;");

export interface IViewRange {
    b: number;
    e: number;
    name?: string;
}

/**
 * Disable document partitioning by reloading.
 */
export async function unpartitionDocument() {
    await unsetPieceSize();
    if (!viewRangeRegExp.test(document.location.search)) {
        location.reload();
    } else {
        document.location.search = document.location.search.replace(viewRangeRegExp, "");
    }
}

/**
 * Partition document by reloading. If there's existing partitioning, replace it,
 * otherwise add to url parameters.
 * @param b First shown par index.
 * @param e Last shown par index.
 * @param loadPreamble Load preamble at the beginning of each part.
 */
export function partitionDocument(b: number, e: number, loadPreamble: boolean) {
    const allParams = document.location.search;
    const newParams = `b=${b}&e=${e}&preamble=${loadPreamble}`;
    if (viewRangeRegExp.test(allParams)) {
        document.location.search = document.location.search.replace(viewRangeRegExp, newParams);
    } else {
        document.location.search += `${newParams}`;
    }
}

export async function unsetPieceSize() {
    const r = await to($http.get<number>(`/viewrange/unset/piecesize`));
    if (!r.ok) {
        console.error("Failed to remove view range cookie!");
    }
}

export async function setPieceSize(pieceSize: number) {
    const data = {pieceSize: pieceSize};
    const r = await to($http.post(`/viewrange/set/piecesize`, data));
    if (!r.ok) {
        console.error("Failed to set view range cookie!");
    }
}

export async function getPieceSize() {
    const cookie = document.cookie.match(viewRangeCookieRegExp);
    let value = 0;
    if (cookie != null) {
        value = +cookie[0].replace(/[^\d-]/g, "");
    }
    // Convert to null since +string is 0 if string to integer conversion fails.
    if (cookie == null || value == 0) {
        return undefined;
    } else {
        return value;
    }
}

/**
 * Get next or previous view range indices for the document.
 * These depend on starting index, direction, and document size.
 * @param docId Document id.
 * @param index Begin (or end, if fetching previous range) index.
 * @param forwards True if next piece, false if previous.
 */
export async function getViewRange(docId: number, index: number, forwards: boolean) {
    if (!index) {
        index = 0;
    }
    let forwardsInt = 1;
    if (!forwards) {
        forwardsInt = 0;
    }
    const r = await to($http.get<IViewRange>(`/viewrange/get/${docId}/${index}/${forwardsInt}`));
    if (!r.ok) {
        return undefined;
    } else {
        return r.result.data;
    }
}

/**
 *
 * @param docId
 * @param headerId
 */
export async function getViewRangeWithHeaderId(docId: number, headerId: string) {
    const r = await to($http.get<IViewRange>(`/viewrange/getWithHeaderId/${docId}/${headerId}`));
    if (!r.ok) {
        return undefined;
    } else {
        return r.result.data;
    }
}

/**
 * Toggle document partitioning with part starting from index 0.
 * @param docId Document id.
 * @param pieceSize Size of the document part.
 */
export async function toggleViewRange(docId: number, pieceSize: number) {
    const currentViewRange = await getCurrentViewRange();
    if (currentViewRange) {
        await unpartitionDocument();
    } else {
        await setPieceSize(pieceSize);
        const range = await getViewRange(docId, 0, true);
        if (range) {
            partitionDocument(range.b, range.e, true);
        } else {
            await unpartitionDocument();
        }
    }
}

/**
 * Get currently active view range (if it exists) from document variables.
 */
export function getCurrentViewRange() {
    const viewRange = documentglobals().current_view_range;
    const pieceSize = getPieceSize();
    if (viewRange && pieceSize) {
        return viewRange;
    } else {
        return undefined;
    }
}

export class ViewRangeInfo {
    public ranges?: IViewRange[];
    public lastIndex?: number;
    private vctrl: ViewCtrl;

    constructor(view: ViewCtrl) {
        // TODO: Handle user manually entering view range into the URL.
        this.vctrl = view;
    }

    /**
     * Get view ranges for nav component and duplicates and those pointing to current range.
     */
    public async loadRanges() {
        const ranges = documentglobals().nav_ranges;
        const current = getCurrentViewRange();
        if (!current || !ranges || ranges.length != 4) {
            return;
        }
        const first = ranges[0];
        const previous = ranges[1];
        const next = ranges[2];
        const last = ranges[3];
        const filteredRanges = [];
        this.lastIndex = ranges[3].e;

        // Remove unnecessary ranges.
        if (this.lastIndex) {
            if (current.b != 0) {
                if (this.isOverlappingRange(first, previous, true, false)) {
                    filteredRanges.push(first);
                } else {
                    filteredRanges.push(first);
                    filteredRanges.push(previous);
                }
            }
            if (current.e != this.lastIndex) {
                if (this.isOverlappingRange(next, last, false, true)) {
                    next.name = last.name;
                    filteredRanges.push(next);
                } else {
                    filteredRanges.push(next);
                    filteredRanges.push(last);
                }
            }
        }
        this.ranges = filteredRanges;
    }

    /**
     * Check whether the range B is contained within range A.
     * @param rangeA Range that may be greater.
     * @param rangeB Range that may be lesser.
     * @param lockBeginIndex Begin index needs to be same.
     * @param lockEndIndex End index needs to be same.
     * @returns True if range A contains range B within set limits.
     */
    public isOverlappingRange(rangeA: IViewRange, rangeB: IViewRange, lockBeginIndex: boolean, lockEndIndex: boolean) {
        let beginOk = false;
        let endOk = false;
        if (lockBeginIndex) {
            beginOk = (rangeA.b == rangeB.b);
        } else {
            beginOk = (rangeA.b <= rangeB.b);
        }
        if (lockEndIndex) {
            endOk = (rangeA.e == rangeB.e);
        } else {
            endOk = (rangeA.e >= rangeB.e);
        }
        return beginOk && endOk;
    }
}
