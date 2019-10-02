/**
 * Component for navigating between partitioned document pieces.
 * Only shown when view range is active.
 */

import {IController} from "angular";
import {Binding} from "tim/util/utils";
import {IItem} from "./IItem";

import {timApp} from "../app";
import {
    getCurrentViewRange,
    getViewRange,
    IViewRange,
    partitionDocument,
    showViewRangeEditDialog,
} from "./viewRangeEditDialog";

class ViewRangeNavigation implements IController {
    static $inject = ["$element", "$scope"];
    private item!: Binding<IItem, "<">;
    private nextRange?: IViewRange;
    private prevRange?: IViewRange;
    private currentRange?: IViewRange;

    async $onInit() {
        // TODO: Handle user manually entering view range into the URL.
        void this.getRanges();
    }

    /**
     * Get current view range and get next and previous ranges based on it.
     */
    private async getRanges() {
        this.currentRange = getCurrentViewRange();
        if (this.currentRange) {
            this.nextRange = await getViewRange(this.item.id, this.currentRange.e, true);
            this.prevRange = await getViewRange(this.item.id, this.currentRange.b, false);
        } else {
            this.nextRange = undefined;
            this.prevRange = undefined;
        }
    }

    /**
     * Move to next or previous document part.
     * @param targetRange Indices for the target part.
     * @param forwards True if moving onto next part, false if to previous.
     */
    private move(targetRange: IViewRange, forwards: boolean) {
        if ((!this.currentRange && !targetRange) ||
            (this.currentRange && !forwards && this.currentRange.b <= 0)) {
            return;
        }
        if (targetRange) {
            partitionDocument(targetRange.b, targetRange.e);
        }
        void this.getRanges();
    }

    /**
     * Opens dialog for editing view range settings.
     */
    private openViewRangeMenu() {
        void showViewRangeEditDialog(this.item);
        this.currentRange = getCurrentViewRange();
    }
}

timApp.component("viewRangeNavigation", {
    bindings: {
        item: "<",
    },
    controller: ViewRangeNavigation,
    template: `
    <div class="view-range-container" ng-if="$ctrl.currentRange">
        <div class="view-range-buttons">
            <a ng-if="$ctrl.currentRange.b > 0" ng-click="$ctrl.move($ctrl.prevRange, false)"
                uib-tooltip="Navigate to part {{$ctrl.prevRange.b}} - {{$ctrl.prevRange.e}}">Previous part</a>
            <span ng-if="$ctrl.currentRange.b > 0 && $ctrl.currentRange.e < $ctrl.nextRange.e">|</span>
            <a ng-if="$ctrl.currentRange.e < $ctrl.nextRange.e"
                uib-tooltip="Navigate to part {{$ctrl.nextRange.b}} - {{$ctrl.nextRange.e}}"
                ng-click="$ctrl.move($ctrl.nextRange, true)">Next part</a>
            <a ng-if="$ctrl.currentRange.b > 0 || $ctrl.currentRange.e < $ctrl.nextRange.e"
                style="display: inline-block" ng-click="$ctrl.openViewRangeMenu()"
                uib-tooltip="Open document partitioning settings">
                <span class="glyphicon glyphicon-cog"></span>
            </a>
        </div>
    </div>
    `,
});
