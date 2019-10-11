import {documentglobals} from "../util/globals";
import {$http} from "../util/ngimport";
import {to} from "../util/utils";
import {ViewCtrl} from "./viewctrl";

export interface IRangeData {
   range?: IViewRange;
   name: string;
}

// TODO: Are b and e always in the same order?
export const viewRangeRegExp = new RegExp("b=\\d+\&e=\\d+\(&preamble=(true|false)|)");
export const viewRangeCookieRegExp = new RegExp("r=\\d+;");

export interface IViewRange {
    b: number;
    e: number;
}

export interface IParCount {
    pars: number;
    preambles: number;
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

/**
 * Get document par count. Requires view rights.
 * @param docId Document id.
 */
export async function getParCount(docId: number) {
    const r = await to($http.get<IParCount>(`/viewrange/parCount/${docId}`));
    if (!r.ok) {
        return undefined;
    } else {
        return r.result.data;
    }
}

export class ViewRangeInfo {
    public ranges?: IRangeData[];
    public lastIndex?: number;
    private vctrl: ViewCtrl;

    constructor(view: ViewCtrl) {
        // TODO: Handle user manually entering view range into the URL.
        this.vctrl = view;
    }

    /**
     * Get current view range and get first, next, previous, and last ranges based on it.
     */
    public async loadRanges(docId: number) {
        this.ranges = [];
        const currentRange = getCurrentViewRange();
        const parCount = await getParCount(this.vctrl.item.id);
        if (!parCount) {
            return;
        }
        this.lastIndex = parCount.pars; // + parCount.preambles;
        if (currentRange && this.lastIndex) {
            let firstRange;
            let lastRange;
            let nextRange;
            let prevRange;
            if (currentRange.b != 0) {
                firstRange = await getViewRange(docId, 0, true);
                prevRange = await getViewRange(docId, currentRange.b, false);
            }
            if (currentRange.e != this.lastIndex) {
                nextRange = await getViewRange(docId, currentRange.e, true);
                lastRange = await getViewRange(docId, this.lastIndex, false);
            }
            // Remove redundant range links and add others to a list.
            if (prevRange && prevRange.b == 0) {
                firstRange = prevRange;
                prevRange = undefined;
            }
            if (nextRange && nextRange.e == this.lastIndex) {
                lastRange = nextRange;
                nextRange = undefined;
            }
            if (firstRange) {
                this.ranges.push({range: firstRange, name: "First"});
            }
            if (prevRange) {
                this.ranges.push({range: prevRange, name: "Previous"});
            }
            if (nextRange) {
                this.ranges.push({range: nextRange, name: "Next"});
            }
            if (lastRange) {
                this.ranges.push({range: lastRange, name: "Last"});
            }
        }
    }
}
