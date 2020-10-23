import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {BrowserModule} from "@angular/platform-browser";
import {NgxTextDiffModule} from "ngx-text-diff";
import {Pos} from "../ui/draggable";

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
                                  outerContainerClass="hide-diff-rownums"
                                  format="LineByLine"
                                  [showToolbar]="data.showToolbar">
                </td-ngx-text-diff>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class DiffDialogComponent extends AngularDialogComponent<
    IDiffParams,
    void
> {
    protected dialogName = "Diff";
}

@NgModule({
    declarations: [DiffDialogComponent],
    imports: [BrowserModule, DialogModule, NgxTextDiffModule],
})
export class DiffDialogModule {}
