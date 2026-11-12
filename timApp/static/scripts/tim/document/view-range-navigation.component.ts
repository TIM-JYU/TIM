/**
 * Component for navigating between partitioned document pieces.
 * Only shown when view range is active.
 */

import {Component} from "@angular/core";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {showViewRangeEditDialog} from "tim/document/showViewRangeEditDialog";
import type {ViewCtrl} from "tim/document/viewctrl";
import type {IViewRange} from "tim/document/viewRangeInfo";
import {partitionDocument} from "tim/document/viewRangeInfo";

@Component({
    selector: "tim-view-range-navigation",
    template: `
        @if (ranges && ranges.length > 0 && !isOnly) {
          <div class="view-range-container">
            <div class="view-range-buttons">
              @for (r of ranges; track r; let isLast = $last) {
                <span>
                  @if (r.disabled) {
                    <span>{{r.name}} part</span>
                  }
                  @if (!r.disabled) {
                    <a (click)="move(r)"
                    tooltip="Navigate to part {{r.b}} - {{r.e}}">{{r.name}} part</a>
                  }
                  @if (!isLast && r) {
                    <span>&ngsp;|</span>
                    }<ng-container>&ngsp; </ng-container>
                  </span>
                }
                <a style="display: inline-block" (click)="openViewRangeMenu()"
                  tooltip="Open document partitioning settings">
                  <span class="glyphicon glyphicon-cog"></span>
                </a>
              </div>
            </div>
          }
        `,
    standalone: false,
})
export class ViewRangeNavigationComponent {
    private vctrl!: ViewCtrl;
    ranges: IViewRange[] = [];
    isOnly: boolean = false;

    ngOnInit() {
        this.vctrl = vctrlInstance!;
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
    move(targetRange: IViewRange) {
        if (targetRange) {
            partitionDocument(targetRange, true);
        }
    }

    /**
     * Opens dialog for editing view range settings.
     */
    openViewRangeMenu() {
        void showViewRangeEditDialog(this.vctrl.item);
    }
}
