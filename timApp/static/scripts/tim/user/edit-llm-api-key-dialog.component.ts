import {Component, NgModule} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {toPromise} from "tim/util/utils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {IGroup, IUserLLMApiKey} from "tim/user/IUser";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {PurifyModule} from "tim/util/purify.module";
import {DirectoryPickerComponent} from "tim/folder/directory-picker.component";

/**
 * User can edit LLM model API key permissions.
 */
@Component({
    selector: "tim-edit-asktim-api-key-dialog",
    template: `
        <tim-dialog-frame [minimizable]="false">
            <ng-container header i18n>
                Edit LLM API-key permissions
            </ng-container>
            <ng-container body>
                <form>
                    <div class="form-group input-group-sm">
                        <label i18n for="{{ listMode ? 'groupNameList' : 'groupName' }}">User groups
                            <a tooltip="{{ getGroupTooltip }}"><i class="glyphicon glyphicon-info-sign"></i></a>
                        </label>
                        <ul class="rights-list">
                            <li *ngFor="let group of groups">
                                <span class="flex align-center">
                                    <span class="name">{{ group.name }}</span>
                                    <span class="action-btn remove">
                                        <button class="btn btn-default btn-xs"
                                                (click)="removeConfirm(group)">remove</button>
                                        <tim-loading *ngIf="removingRight === group && loading"></tim-loading>
                                    </span>
                                </span>
                            </li>
                        </ul>
                    </div>

                    <div class="form-group input-group-sm">
                        <div class="input-group">
                            <input name="groupName" class="form-control" type="text"
                                   [hidden]="listMode"
                                   (keydown.enter)="$event.preventDefault()"
                                   [(ngModel)]="groupNamesInput"
                                   placeholder="{{ getPlaceholderGroups }}">
                            <textarea name="groupNameList" class="form-control" type="text"
                                      [hidden]="!listMode"
                                      [(ngModel)]="groupNamesInput"
                                      placeholder="{{ getPlaceholderGroups }}"></textarea>
                            <span class="input-group-addon btn btn-default"
                                  (click)="this.listMode = !this.listMode"
                                  tooltip="Toggle multiline mode"
                                  data-tooltip-placement="right">
                            <i class="glyphicon glyphicon-align-left"></i>
                            </span>
                        </div>
                    </div>

                    <div class="form-group input-group-sm">
                        <label i18n for="docPaths">Folder or document paths
                            <a tooltip="{{ getPathTooltip }}"><i class="glyphicon glyphicon-info-sign"></i></a>
                        </label>
                        <tim-directory-picker
                            [(selection)]="paths"
                        ></tim-directory-picker>
                    </div>
                </form>
                <tim-alert *ngIf="editError" severity="danger">
                    Failed to save: {{ editError }}
                </tim-alert>
            </ng-container>
            <ng-container footer>
                <tim-loading *ngIf="saving" style="margin-right: 1em;"></tim-loading>
                <button class="timButton"
                        (click)="savePermissions()" i18n>
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
        this.groups = this.data.key.groups ?? [];
        this.paths = this.data.key.itemPaths ?? [];
        this.groupNamesInput = "";
    }

    dialogName: string = "EditLLMAPIKey";
    groupNamesInput: string = "";

    groups: IGroup[] = [];
    paths: string[] = [];

    removingRight?: IGroup;
    loading: boolean = false;
    listMode: boolean = false;
    saving = false;
    editError?: string;

    // Used only to get HttpClient initialized.
    constructor(private http: HttpClient) {
        super();
    }

    /* Saves the permissions for the API-key. */
    async savePermissions() {
        this.saving = true;
        const key: IUserLLMApiKey = this.data.key;
        const groups: string[] = this.splitInput(this.groupNamesInput, "\n;");
        const paths: string[] = this.paths;

        const res = await toPromise(
            this.http.post<{groups: IGroup[]; paths: string[]}>(
                "/asktim/addApiKeyPermissions",
                {
                    provider: key.provider,
                    apikey: key.APIkey,
                    alias: key.alias,
                    groups: groups,
                    paths: paths,
                }
            )
        );
        this.saving = false;
        if (res.ok) {
            this.data.onEdit({
                ...this.data.key,
                groups: res.result.groups,
                itemPaths: res.result.paths,
            });
            this.groups = res.result.groups;
            this.paths = res.result.paths;
            this.groupNamesInput = "";
            this.editError = undefined;
            return;
        }
        this.editError = res.result.error.error;
    }

    /* Remove access right from the user group. */
    async removeRight(group: IGroup) {
        const id = group.id;
        const key = this.data.key;
        const res = await toPromise(
            this.http.post<Response>(`/asktim/removeGroupRight/${id}`, {
                public_key: key.alias,
            })
        );
        if (res.ok) {
            this.groups.splice(this.groups.indexOf(group), 1);
            this.editError = undefined;
            return;
        }
        this.editError = res.result.error.error;
    }

    /* Confirm the removal of access right. */
    async removeConfirm(group: IGroup) {
        if (window.confirm(`${this.getConfirmDesc(group)}`)) {
            this.removingRight = group;
            await this.removeRight(group);
            this.removingRight = undefined;
        }
    }

    getConfirmDesc(group: IGroup): string {
        return `Remove rights from ${group.name}?`;
    }

    splitInput(input: string, splitter: string): string[] {
        return input
            .split(new RegExp(`[${splitter}]`))
            .map((n) => n.trim())
            .filter((n) => n);
    }

    get getPlaceholderGroups(): string {
        return (
            "enter username(s)/group name(s) separated by semicolons" +
            (this.listMode ? " or newlines" : "")
        );
    }

    get getGroupTooltip(): string {
        return "User groups that have access to the API key using the alias.";
    }

    get getPathTooltip(): string {
        return (
            "Specify folders or documents where the API key is permitted. " +
            "Selecting a folder grants access to all documents directly within it."
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
        PurifyModule,
        DirectoryPickerComponent,
    ],
})
export class EditLLMAPIKeyDialogModule {}
