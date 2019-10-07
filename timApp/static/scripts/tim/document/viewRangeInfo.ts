import {getCurrentViewRange, getParCount, getViewRange, IViewRange} from "../item/viewRangeEditDialog";

import {ViewCtrl} from "./viewctrl";

export interface IRangeData {
   range?: IViewRange;
   name: string;
}

export class ViewRangeInfo {
    public ranges?: IRangeData[];
    public lastIndex?: number;
    private vctrl: ViewCtrl;

    constructor(view: ViewCtrl) {
        // TODO: Handle user manually entering view range into the URL.
        // TODO: Get results for both instances of the component on page with same call.
        // TODO: Nav component disappears if partitioning is done only via URL params (i.e. disabled on other tab & reloaded).
        this.vctrl = view;
    }

    /**
     * Get current view range and get first, next, previous, and last ranges based on it.
     */
    public async loadRanges(docId: number) {
        this.ranges = [];
        const currentRange = getCurrentViewRange();
        this.lastIndex = await getParCount(this.vctrl.item.id);
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
