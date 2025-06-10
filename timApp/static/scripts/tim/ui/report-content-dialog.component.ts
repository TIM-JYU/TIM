import {Component, DoBootstrap, NgModule} from "@angular/core";
import {AngularDialogComponent} from "./angulardialog/angular-dialog-component.directive";
import {CommonModule} from "@angular/common";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {FormsModule} from "@angular/forms";

export interface IReportContentParams {}

@Component({
    selector: "report-content-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container i18n header>
                Report content
            </ng-container>
            <ng-container body>
                <h1>Report Content</h1>
                <form #form="ngForm">
                    <fieldset>
                        <div class="form-group">
                        </div>
                    </fieldset>
                </form>           
            </ng-container>
            <ng-container footer>
                <p>Footer</p>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class ReportContentDialogComponent extends AngularDialogComponent<
    IReportContentParams,
    any
> {
    protected dialogName = "ReportContent";

    constructor() {
        super();
    }
}

@NgModule({
    declarations: [ReportContentDialogComponent],
    exports: [ReportContentDialogComponent],
    imports: [CommonModule, TimUtilityModule, DialogModule, FormsModule],
})
export class ReportContentDialogModule {}
