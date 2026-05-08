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
                    <div class="form-group input-group-sm">

                        <label for="{{ listMode ? 'groupNameList' : 'groupName' }}">User groups:</label>
                        <div class="input-group">
                            <input name="groupName" class="form-control" type="text"
                                   [hidden]="listMode"
                                   [(ngModel)]="groupNames"
                                   placeholder="{{ getPlaceholderGroups() }}">
                            <textarea name="groupNameList" class="form-control" type="text"
                                      [hidden]="!listMode"
                                      [(ngModel)]="groupNames"
                                      placeholder="{{ getPlaceholderGroups() }}"></textarea>
                            <span class="input-group-addon btn btn-default"
                                  (click)="this.listMode = !this.listMode"
                                  tooltip="Toggle multiline mode"
                                  data-tooltip-placement="right">
                            <i class="glyphicon glyphicon-align-left"></i>
                            </span>
                        </div>

                        <label for="docPaths">Document paths:</label>
                        <div class="input-group" style="width: 100%">
                            <textarea name="docPaths" class="form-control" type="text"
                                      placeholder="{{getPlaceholderDocs()}}"
                                      [(ngModel)]="documentPaths">
                            </textarea>
                        </div>

                        <tim-alert *ngIf="editError" severity="danger" i18n>
                            Failed to save: {{ editError }}
                        </tim-alert>
                    </div>
                </form>
            </ng-container>
            <ng-container footer>
                <tim-loading *ngIf="saving" style="margin-right: 1em;"></tim-loading>
                <button class="timButton"
                        (click)="savePermissions()"
                        [disabled]="" i18n>
                    Save
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
    ngOnInit() {
        const groups: string[] = this.data.key.groupNames ?? [];
        const paths: string[] = this.data.key.docPaths ?? [];
        this.groupNames = groups.join(";");
        this.documentPaths = paths.join("\n");
    }

    dialogName: string = "EditLLMAPIKey";
    groupNames: string = "";
    documentPaths: string = "";
    listMode: boolean = false;
    saving = false;
    editError?: string;

    // Used only to get HttpClient initialized.
    constructor(private http: HttpClient) {
        super();
    }

    /**
     * Saves the permissions for the API-key.
     */
    async savePermissions() {
        this.saving = true;
        const key: IUserLLMApiKey = this.data.key;
        const groups: string[] = this.splitInput(this.groupNames, "\n;");
        const paths: string[] = this.splitInput(this.documentPaths, "\n");

        const res = await toPromise(
            this.http.post<Response>("/chattim/saveApiKeyPermissions", {
                provider: key.provider,
                apikey: key.APIkey,
                alias: key.alias,
                groups: groups,
                paths: paths,
            })
        );
        this.saving = false;
        if (res.ok) {
            key.groupNames = groups;
            key.docPaths = paths;
            this.dismiss();
            return;
        }
        this.editError = res.result.error.error;
    }

    splitInput(input: string, splitter: string): string[] {
        return input
            .split(new RegExp(`[${splitter}]`))
            .map((n) => n.trim())
            .filter((n) => n);
    }

    getPlaceholderGroups(): string {
        return (
            "enter username(s)/group name(s) separated by semicolons" +
            (this.listMode ? " or newlines" : "")
        );
    }

    getPlaceholderDocs(): string {
        return "enter document paths separated by newlines";
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
