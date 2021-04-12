import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {BrowserModule} from "@angular/platform-browser";

@Component({
    selector: "message-list-creation",
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
export class MessageListComponent extends AngularDialogComponent<
    string,
    unknown
> {
    protected dialogName = "MessageList";
}

@NgModule({
    declarations: [MessageListComponent],
    imports: [BrowserModule, DialogModule],
})
export class MessageListModule {}
