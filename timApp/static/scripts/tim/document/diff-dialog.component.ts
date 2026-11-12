import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TextDiffModule} from "tim/document/text-diff.component";
import type {Pos} from "tim/ui/pos";
import {copyToClipboard, timeout} from "tim/util/utils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";

export interface IDiffParams {
    left: string;
    right: string;
    title: string;
    showToolbar: boolean;
    pos?: Pos;
}

@Component({
    selector: "tim-diff-dialog",
    template: `
        <tim-dialog-frame>
          <ng-container header>
            {{data.title}}
          </ng-container>
          <ng-container body>
            <tim-text-diff [left]="data.left"
              [right]="data.right"
              format="LineByLine"
              [showToolbar]="data.showToolbar">
            </tim-text-diff>
          </ng-container>
          <div footer style="text-align: left">
            <button class="btn btn-default"
              (click)="copy(data.left)">Copy
              @if (!isSame()) {
                old
              }
            </button>
            &ngsp;
            @if (!isSame()) {
              <button
                class="btn btn-default"
                (click)="copy(data.right)">
                Copy new
              </button>
            }
            <ng-container> &ngsp; </ng-container>
            @if (copied) {
              Copied
            }
            <button class="btn btn-default pull-right" (click)="close(undefined)">
              Close
            </button>
          </div>
        </tim-dialog-frame>
        `,
    standalone: false,
})
export class DiffDialogComponent extends AngularDialogComponent<
    IDiffParams,
    void
> {
    protected dialogName = "Diff";
    copied = false;

    isSame() {
        return this.data.left === this.data.right;
    }

    async copy(s: string) {
        copyToClipboard(s);
        this.copied = true;
        await timeout(500);
        this.copied = false;
    }
}

@NgModule({
    declarations: [DiffDialogComponent],
    imports: [CommonModule, DialogModule, TextDiffModule, TimUtilityModule],
})
export class DiffDialogModule {}
