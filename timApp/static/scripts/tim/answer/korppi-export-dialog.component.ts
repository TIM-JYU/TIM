import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {IExportOptions} from "tim/answer/userlistController";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";

@Component({
    selector: "tim-korppi-export-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <form class="form-horizontal">
                    <div class="form-group">
                        <label for="totalPointField" class="col-sm-4 control-label">Total point field name</label>
                        <div class="col-sm-8">
                            <input focusMe
                                   [(ngModel)]="options.totalPointField"
                                   name="totalPointField"
                                   type="text"
                                   class="form-control"
                                   id="totalPointField"
                                   placeholder="Leave blank to skip exporting total points">
                        </div>
                    </div>
                    <div class="form-group">
                        <label for="taskPointField" class="col-sm-4 control-label">Task point field name</label>
                        <div class="col-sm-8">
                            <input [(ngModel)]="options.taskPointField"
                                   name="taskPointField"
                                   type="text"
                                   class="form-control"
                                   id="taskPointField"
                                   placeholder="Leave blank to skip exporting task points">
                        </div>
                    </div>
                    <div class="form-group">
                        <label for="velpPointField" class="col-sm-4 control-label">Velp point field name</label>
                        <div class="col-sm-8">
                            <input [(ngModel)]="options.velpPointField"
                                   name="velpPointField"
                                   type="text"
                                   class="form-control"
                                   id="velpPointField"
                                   placeholder="Leave blank to skip exporting velp points">
                        </div>
                    </div>
                </form>
            </ng-container>
            <ng-container footer>
                <button [disabled]="!options.velpPointField && !options.taskPointField && !options.totalPointField"
                        class="timButton" type="button" (click)="copy()">Copy
                </button>
                <button [disabled]="!options.velpPointField && !options.taskPointField && !options.totalPointField"
                        class="timButton" type="button" (click)="ok()">Export
                </button>
                <button class="btn btn-default" type="button" (click)="dismiss()">Cancel</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class KorppiExportDialogComponent extends AngularDialogComponent<
    void,
    IExportOptions
> {
    protected dialogName = "KorppiExport";
    options: IExportOptions = {
        totalPointField: "",
        velpPointField: "",
        taskPointField: "",
        copy: false,
    };

    getTitle() {
        return "Export to Korppi";
    }

    ok() {
        this.close(this.options);
    }

    copy() {
        this.options.copy = true;
        this.close(this.options);
    }
}

@NgModule({
    declarations: [KorppiExportDialogComponent],
    imports: [BrowserModule, DialogModule, TimUtilityModule, FormsModule],
})
export class KorppiExportDialogModule {}
