/**
 * Component for navigating between partitioned document pieces.
 * Only shown when view range is active.
 */

import {IController} from "angular";
import {Require} from "tim/util/utils";
import {timApp} from "../app";
import {ViewCtrl} from "./viewctrl";
import {showViewRangeEditDialog} from "./viewRangeEditDialog";
import {IViewRange, partitionDocument} from "./viewRangeInfo";

class ViewRangeNavigation implements IController {
    static $inject = ["$element", "$scope"];
    private vctrl!: Require<ViewCtrl>;
    private ranges: IViewRange[] = [];
    private isOnly: boolean = false;

    $onInit() {
        if (this.vctrl?.viewRangeInfo?.ranges) {
            // Ranges come from document specific ViewRangeInfo to avoid duplicate requests.
            this.ranges = this.vctrl.viewRangeInfo.ranges;
            if (this.vctrl.viewRangeInfo.isOnly) {
                this.isOnly = true;
            }
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
}

timApp.component("viewRangeNavigation", {
    controller: ViewRangeNavigation,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="view-range-container" ng-if="$ctrl.ranges && $ctrl.ranges.length > 0 && !$ctrl.isOnly ">
    <div class="view-range-buttons">
        <span ng-repeat="r in $ctrl.ranges">
            <span ng-if="r.disabled">{{r.name}} part</span>
            <a ng-if="!r.disabled" ng-click="$ctrl.move(r)"
                uib-tooltip="Navigate to part {{r.b}} - {{r.e}}">{{r.name}} part</a>
            <span ng-if="!$last && r">|</span>
        </span>
        <a style="display: inline-block" ng-click="$ctrl.openViewRangeMenu()"
            uib-tooltip="Open document partitioning settings">
            <span class="glyphicon glyphicon-cog"></span>
        </a>
    </div>
</div>
    `,
});
