/**
 * Component for navigating between partitioned document pieces.
 */

import {IController} from "angular";
import {Binding} from "tim/util/utils";
import {IItem} from "./IItem";

import {timApp} from "../app";
import {getViewRange, IViewRange, partitionDocument} from "./viewRangeEditDialog";

class ViewRangeNavigation implements IController {
    static $inject = ["$element", "$scope"];
    private item!: Binding<IItem, "<">;
    private nextRange?: IViewRange;
    private prevRange?: IViewRange;
    private currentRange?: IViewRange;

    async $onInit() {
        void this.getRanges();
    }

    private async getRanges() {
        const b = new URL(document.location.href).searchParams.get("b");
        const e = new URL(document.location.href).searchParams.get("e");
        if (b != null && e != null) {
            this.currentRange = {b: +b, e: +e};
        } else {
            this.currentRange = undefined;
        }
        if (this.currentRange) {
            this.nextRange = await getViewRange(this.item.id, this.currentRange.e, true);
            this.prevRange = await getViewRange(this.item.id, this.currentRange.b, false);
        } else {
            this.nextRange = undefined;
            this.prevRange = undefined;
        }
    }

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
}

timApp.component("viewRangeNavigation", {
    bindings: {
        item: "<",
    },
    controller: ViewRangeNavigation,
    template: `
    <div class="view-range-container" ng-if="$ctrl.currentRange">
        <div class="view-range-buttons">
            <button ng-disabled="$ctrl.currentRange.b <= 0" class="timButton btn-sm"
                ng-click="$ctrl.move($ctrl.prevRange, false)">Previous part</button>
            <span ng-if="$ctrl.currentRange">[{{$ctrl.currentRange.b}}, {{$ctrl.currentRange.e}}]</span>
            <button ng-disabled="$ctrl.currentRange.e >= $ctrl.lastIndex"  class="timButton btn-sm"
                ng-click="$ctrl.move($ctrl.nextRange, true)">Next part</button>
        </div>
    </div>
    `,
});
