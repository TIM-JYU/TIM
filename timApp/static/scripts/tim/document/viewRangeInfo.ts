import {documentglobals} from "../util/globals";
import {$http} from "../util/ngimport";
import {getCookie, to} from "../util/utils";
import {ViewCtrl} from "./viewctrl";

export interface IViewRangeUnnamed {
    b: number;
    e: number;
    is_full: boolean;
}

export interface IViewRange extends IViewRangeUnnamed {
    name?: string;
    disabled?: boolean;
}

export function getCurrentPartitionURLParams() {
    const params = new URLSearchParams(document.location.search);
    const b = params.get("b");
    const e = params.get("e");
    const preamble = params.get("preamble");
    if (b == null || e == null || preamble == null) {
        return undefined;
    }
    return params;
}

export function getRangeBeginParam() {
    const params = new URLSearchParams(document.location.search);
    const b = params.get("b");
    if (!b) {
        return undefined;
    }
    const r = parseInt(b, 10);
    if (!isNaN(r)) {
        return r;
    }
    return undefined;
}

function setURLSearchParams(params: URLSearchParams) {
    document.location.search = params.toString();
}

/**
 * Disable document partitioning by reloading.
 */
export async function unpartitionDocument() {
    await unsetPieceSize();
    const params = getCurrentPartitionURLParams();
    if (!params) {
        location.reload();
    } else {
        params.delete("b");
        params.delete("e");
        params.delete("preamble");
        setURLSearchParams(params);
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
    const params = new URLSearchParams(document.location.search);
    params.set("b", b.toString());
    params.set("e", e.toString());
    params.set("preamble", loadPreamble.toString());
    setURLSearchParams(params);
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
    const cookie = getCookie("r");
    let value = 0;
    if (cookie != null) {
        value = +cookie;
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
    if (currentViewRange && !currentViewRange.is_full) {
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
        return {
            ...viewRange,
            name: "Current",
        };
    } else {
        return undefined;
    }
}

export class ViewRangeInfo {
    public ranges?: [IViewRange, IViewRange, IViewRange, IViewRange];
    public lastIndex?: number;
    private vctrl: ViewCtrl;

    constructor(view: ViewCtrl) {
        // TODO: Handle user manually entering view range into the URL.
        this.vctrl = view;
    }

    /**
     * Get view ranges for nav component and duplicates and those pointing to current range.
     */
    public loadRanges() {
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
            if (current.b == 0) {
                first.disabled = true;
                previous.disabled = true;
            }
            if (current.e == this.lastIndex) {
                last.disabled = true;
                next.disabled = true;
            }
        }
        this.ranges = [first, previous, next, last];
    }
}
