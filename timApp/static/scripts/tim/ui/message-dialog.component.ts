import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";

@Component({
    selector: "tim-message-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                Message
            </ng-container>
            <ng-container body>
            </ng-container>
            <ng-container footer>
                <button i18n class="btn btn-default" type="button" (click)="done()">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class MessageDialogComponent extends AngularDialogComponent<
    string,
    unknown
> {
    protected dialogName = "Message";

    done() {
        this.dismiss();
    }
}

@NgModule({
    declarations: [MessageDialogComponent],
    imports: [CommonModule, DialogModule, TimUtilityModule],
})
export class MessageDialogModule {}
