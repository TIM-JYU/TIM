import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {BrowserModule} from "@angular/platform-browser";
import {NgxTextDiffModule} from "ngx-text-diff";
import {Pos} from "tim/ui/pos";
import {copyToClipboard, timeout} from "tim/util/utils";

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
                <td-ngx-text-diff [left]="data.left"
                                  [right]="data.right"
                                  outerContainerClass="view-source"
                                  format="LineByLine"
                                  [showToolbar]="data.showToolbar">
                </td-ngx-text-diff>
                <button class="btn btn-default"
                        (click)="copy(data.left)">Copy
                    <ng-container *ngIf="!isSame()">left</ng-container>
                </button>
                &ngsp;
                <button *ngIf="!isSame()"
                        class="btn btn-default"
                        (click)="copy(data.right)">
                    Copy right
                </button>
                &ngsp;
                <ng-container *ngIf="copied">Copied</ng-container>
            </ng-container>
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
    imports: [BrowserModule, DialogModule, NgxTextDiffModule],
})
export class DiffDialogModule {}
