import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";

interface MessageDialogData {
    message: string;
    modal: boolean;
}

@Component({
    selector: "tim-message-dialog",
    template: `
        <div class="modal-bg" *ngIf="data.modal"></div>
        <tim-dialog-frame>
            <ng-container header>
                Message
            </ng-container>
            <ng-container body>
                <span [innerHTML]="data.message"></span>
            </ng-container>
            <ng-container footer>
                <button class="timButton" type="button" (click)="close({})">OK</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class MessageDialogComponent extends AngularDialogComponent<
    MessageDialogData,
    unknown
> {
    protected dialogName = "Message";
}

@NgModule({
    declarations: [MessageDialogComponent],
    imports: [CommonModule, DialogModule, TimUtilityModule],
})
export class MessageDialogModule {}
