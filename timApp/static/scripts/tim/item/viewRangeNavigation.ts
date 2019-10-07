/**
 * Component for navigating between partitioned document pieces.
 * Only shown when view range is active.
 */

import {IController} from "angular";
import {Binding} from "tim/util/utils";
import {IItem} from "./IItem";

import {timApp} from "../app";
import {
    getCurrentViewRange, getParCount,
    getViewRange,
    IViewRange,
    partitionDocument,
    showViewRangeEditDialog,
    unpartitionDocument,
} from "./viewRangeEditDialog";

export interface IRangeData {
   range?: IViewRange;
   name: string;
}

class ViewRangeNavigation implements IController {
    static $inject = ["$element", "$scope"];
    private item!: Binding<IItem, "<">;
    private ranges: IRangeData[] = [];
    private lastIndex?: number;

    async $onInit() {
        // TODO: Handle user manually entering view range into the URL.
        // TODO: Get results for both instances of the component on page with same call.
        // TODO: Nav component disappears if partitioning is done only via URL params (i.e. disabled on other tab & reloaded).
        this.lastIndex = await getParCount(this.item.id); // TODO: Better way to get this?
        if (!this.lastIndex) {
            return;
        }
        this.ranges = await this.getRanges(this.item.id, this.lastIndex);
    }

    /**
     * Get current view range and get first, next, previous, and last ranges based on it.
     */
    private async getRanges(docId: number, lastIndex: number) {
        const currentRange = getCurrentViewRange();
        const ranges = [];
        if (currentRange) {
            let nextRange = await getViewRange(docId, currentRange.e, true);
            let prevRange = await getViewRange(docId, currentRange.b, false);
            let firstRange = await getViewRange(docId, 0, true);
            let lastRange = await getViewRange(docId, lastIndex, false);
            // Remove redundant range links and add others to a list.
            if (prevRange && prevRange.b == 0) {
                firstRange = prevRange;
                prevRange = undefined;
            }
            if (nextRange && nextRange.e == this.lastIndex) {
                lastRange = nextRange;
                nextRange = undefined;
            }
            if (currentRange.b == 0) {
                firstRange = undefined;
            }
            if (currentRange.e == this.lastIndex) {
                lastRange = undefined;
            }
            if (firstRange) {
                ranges.push({range: firstRange, name: "First"});
            }
            if (prevRange) {
                ranges.push({range: prevRange, name: "Previous"});
            }
            if (nextRange) {
                ranges.push({range: nextRange, name: "Next"});
            }
            if (lastRange) {
                ranges.push({range: lastRange, name: "Last"});
            }
        }
        return ranges;
    }

    /**
     * Move to next or previous document part.
     * @param targetRange Indices for the target part.
     */
    private move(targetRange: IViewRange) {
        if (targetRange) {
            partitionDocument(targetRange.b, targetRange.e, true);
        }
        void this.getRanges();
    }

    /**
     * Opens dialog for editing view range settings.
     */
    private openViewRangeMenu() {
        void showViewRangeEditDialog(this.item);
        // this.currentRange = getCurrentViewRange();
    }

    /**
     * Remove partitioning and reload full document.
     */
    private async close() {
        await unpartitionDocument();
    }
}

timApp.component("viewRangeNavigation", {
    bindings: {
        item: "<",
    },
    controller: ViewRangeNavigation,
    template: `
    <div class="view-range-container" ng-if="$ctrl.ranges">
        <div class="view-range-buttons">
            <span ng-repeat="r in $ctrl.ranges">
                <a ng-if="r.range" ng-click="$ctrl.move(r.range)"
                    uib-tooltip="Navigate to part {{r.range.b}} - {{r.range.e}}">{{r.name}} part</a>
                <span ng-if="!$last && r.range">|</span>
            </span>
            <a style="display: inline-block" ng-click="$ctrl.openViewRangeMenu()"
                uib-tooltip="Open document partitioning settings">
                <span class="glyphicon glyphicon-cog"></span>
            </a>
        </div>
    </div>
    `,
});
