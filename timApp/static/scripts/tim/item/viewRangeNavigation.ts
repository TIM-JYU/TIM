/**
 * Component for navigating between partitioned document pieces.
 * Only shown when view range is active.
 */

import {IController} from "angular";
import {Require} from "tim/util/utils";
import {timApp} from "../app";
import {ViewCtrl} from "../document/viewctrl";
import {IRangeData, IViewRange, partitionDocument, unpartitionDocument} from "../document/viewRangeInfo";
import {showViewRangeEditDialog} from "./viewRangeEditDialog";

class ViewRangeNavigation implements IController {
    static $inject = ["$element", "$scope"];
    private vctrl!: Require<ViewCtrl>;
    private ranges?: IRangeData[] = [];

    async $onInit() {
        if (this.vctrl && this.vctrl.viewRangeInfo) {
            this.ranges = this.vctrl.viewRangeInfo.ranges;
        }
    }

    /**
     * Move to next or previous document part.
     * @param targetRange Indices for the target part.
     */
    private move(targetRange: IViewRange) {
        if (targetRange) {
            partitionDocument(targetRange.b, targetRange.e, true);
        }
        // void this.getRanges();
    }

    /**
     * Opens dialog for editing view range settings.
     */
    private openViewRangeMenu() {
        void showViewRangeEditDialog(this.vctrl.item);
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
    controller: ViewRangeNavigation,
    require: {
        vctrl: "^timView",
    },
    template: `
    <div class="view-range-container" ng-if="$ctrl.ranges && $ctrl.ranges.length > 0">
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
