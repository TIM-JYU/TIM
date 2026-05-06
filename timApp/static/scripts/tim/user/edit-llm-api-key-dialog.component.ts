import {Component, NgModule} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {toPromise} from "tim/util/utils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {IUserLLMApiKey} from "tim/user/IUser";
import {TooltipModule} from "ngx-bootstrap/tooltip";

/**
 * User can edit LLM model API key permissions.
 */
@Component({
    selector: "tim-edit-chattim-api-key-dialog",
    template: `
        <tim-dialog-frame [minimizable]="false">
            <ng-container header i18n>
                Edit LLM API-key permissions
            </ng-container>
            <ng-container body>
                <form>
                    <div class="form-group col-md-6 input-group-sm">
                        <label for="{{ listMode ? 'groupNameList' : 'groupName' }}">Groups: </label>
                        <div class="input-group">
                            <input id="groupName" class="form-control" type="text"
                                   [hidden]="listMode"
                                   placeholder="{{ getPlaceholder() }}">
                            <textarea id="groupNameList" class="form-control" type="text"
                                      [hidden]="!listMode"
                                      placeholder="{{ getPlaceholder() }}"></textarea>
                            <span class="input-group-addon btn btn-default"
                                  (click)="this.listMode = !this.listMode"
                                  tooltip="Toggle multiline mode"
                                  data-tooltip-placement="right">
                            <i class="glyphicon glyphicon-align-left"></i>
                            </span>
                        </div>
                    </div> 
                </form>
            </ng-container>
            <ng-container footer>
                <i class="glyphicon glyphicon-ok success" *ngIf="saved"></i>
                <tim-loading *ngIf="saving" style="margin-right: 1em;"></tim-loading>
                <button class="timButton"
                        (click)="savePermissions()"
                        [disabled]="!groupNames || saved" i18n>
                    Add
                </button>
                <button class="timButton" (click)="dismiss()">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class EditLLMAPIKeyDialogComponent extends AngularDialogComponent<
    {key: IUserLLMApiKey; onEdit: (key: IUserLLMApiKey) => void},
    void
> {
    async ngOnInit() {
        // TODO: fetch initial info
        console.log(this.data.key);
    }

    dialogName: string = "EditAPIKey";

    groupNames: string = "";
    listMode: boolean = false;
    saved = false;
    saving = false;
    editError?: string;
    edited = false;
    message: string = "";

    // Used only to get HttpClient initialized.
    constructor(private http: HttpClient) {
        super();
    }

    /**
     * Saves the permissions for the API-key.
     */
    async savePermissions() {
        // TODO: send and validate on server
        let groups = this.groupNames.split(/[\n;]/).map((n) => n.trim());

        this.saving = true;
        this.saving = false;
        this.dismiss();
    }

    getPlaceholder(): string {
        return (
            "enter username(s)/group name(s) separated by semicolons" +
            (this.listMode ? " or newlines" : "")
        );
    }
}

@NgModule({
    declarations: [EditLLMAPIKeyDialogComponent],
    imports: [
        DialogModule,
        FormsModule,
        TimUtilityModule,
        CommonModule,
        TooltipModule,
    ],
})
export class EditLLMAPIKeyDialogModule {}
