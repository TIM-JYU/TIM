import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {BrowserModule} from "@angular/platform-browser";

@Component({
    selector: "tim-message-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                Message
            </ng-container>
            <ng-container body>
                <span [innerHTML]="data"></span>
            </ng-container>
            <ng-container footer>
                <button class="timButton" type="button" (click)="close({})">OK</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class MessageDialogComponent extends AngularDialogComponent<
    string,
    unknown
> {
    protected dialogName = "Message";
}

@NgModule({
    declarations: [MessageDialogComponent],
    imports: [BrowserModule, DialogModule],
})
export class MessageDialogModule {}
