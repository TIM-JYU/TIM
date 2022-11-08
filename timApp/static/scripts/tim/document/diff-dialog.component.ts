import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {NgxTextDiffModule} from "ngx-text-diff";
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
        <tim-dialog-frame [dialogOptions]="dialogOptions" [dialogName]="dialogName" [align]="'center'">
            <ng-container header>
                {{data.title}}
            </ng-container>
            <ng-container body>
                <td-ngx-text-diff [left]="data.left"
                                  [right]="data.right"
                                  outerContainerClass="view-source"
                                  format="LineByLine"
                                  [showToolbar]="data.showToolbar">
                </td-ngx-text-diff>
            </ng-container>
            <div footer style="text-align: left">
                <button class="btn btn-default"
                        (click)="copy(data.left)">Copy
                    <ng-container *ngIf="!isSame()">old</ng-container>
                </button>
                &ngsp;
                <button *ngIf="!isSame()"
                        class="btn btn-default"
                        (click)="copy(data.right)">
                    Copy new
                </button>
                &ngsp;
                <ng-container *ngIf="copied">Copied</ng-container>
                <button class="btn btn-default pull-right" (click)="close(undefined)">
                    Close
                </button>
            </div>
        </tim-dialog-frame>
    `,
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
    imports: [CommonModule, DialogModule, NgxTextDiffModule, TimUtilityModule],
})
export class DiffDialogModule {}
