import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import type {IDocument} from "tim/item/IItem";
import {toPromise} from "tim/util/utils";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";

/**
 * Defines additional parameters for creating new user groups via the UserGroupDialog.
 */
export interface UserGroupDialogParams {
    defaultGroupFolder?: string;
    canChooseFolder?: boolean;
    // Whether to encode the group name in base64; avoids naming clashes as group names are required to be unique,
    // but we still want to enable non-unique display names for certain (temporary) uses.
    // NOTE: setting encodeGroupName to true will bypass other group naming constraints
    //       (no non-alphanumeric or upper case characters).
    encodeGroupName?: boolean;
}

@Component({
    selector: "tim-user-group-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container i18n header>
                Create group
            </ng-container>
            <ng-container body>
                <form #form="ngForm" class="form-horizontal">
                    <div class="form-group"
                         [ngClass]="{'has-error': ngModelName.invalid && ngModelName.dirty}">

                        <label i18n for="name" class="col-sm-2 control-label">Name</label>
                        <div class="col-sm-10">
                            <input i18n-placeholder type="text" required
                                   [(ngModel)]="name" #ngModelName="ngModel"
                                   (ngModelChange)="setMessage()"
                                   pattern="[^/]*"
                                   id="name" name="name"
                                   class="form-control"
                                   placeholder="Enter the name of the user group"/>
                            </div>
                    </div>

                    <!--
                    Do not mix form groups or grid column classes directly with input groups.
                    Instead, nest the input group inside of the form group or grid-related element.
                    Refer to documentation: https://getbootstrap.com/docs/3.3/components/#input-groups
                    -->

                    <div class="form-group">
                        <label i18n for="folder" class="col-sm-2 control-label">Folder</label>
                        <div class="col-sm-10">
                            <div class="input-group">
                                <span class="input-group-addon">groups/</span>
                                <input i18n-placeholder type="text"
                                       [(ngModel)]="folder"
                                       (ngModelChange)="setMessage()"
                                       [disabled]="!canChooseFolder()"
                                       id="folder" name="folder"
                                       class="form-control"
                                       placeholder="{{getDefaultFolder() ?? 'Enter the location or leave empty'}}">
                            </div>
                        </div>
                    </div>
                </form>

                <!--
                User group validation has two (2) stages.
                1) Browser validates that at least a name is required and does not contain a character slash
                before sending a request to the server.
                2) Server responses with an error message if a given folder or name of the user group failed.
                Refer to documentation: https://angular.io/guide/form-validation#validating-form-input
                -->

                <tim-alert *ngIf="ngModelName.invalid && ngModelName.dirty" severity="danger">
                    <ng-container i18n *ngIf="ngModelName.errors?.['required']">
                        Name is required.
                    </ng-container>
                    <ng-container i18n *ngIf="ngModelName.errors?.['pattern']">
                        Name should not contain the slash character.
                    </ng-container>
                </tim-alert>
                <tim-alert *ngIf="message" severity="danger">
                    <ng-container *ngIf="message">
                        {{message}}
                    </ng-container>
                </tim-alert>

            </ng-container>
            <ng-container footer>
                <button i18n class="timButton" type="button" (click)="saveGroup()" [disabled]="form.invalid">
                    Create
                </button>
                <button i18n class="btn btn-default" type="button" (click)="dismiss()">
                    Cancel
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class UserGroupDialogComponent extends AngularDialogComponent<
    UserGroupDialogParams,
    IDocument
> {
    protected dialogName = "UserGroupCreate";
    name = "";
    folder = "";
    message?: string;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {
        if (!this.canChooseFolder() && this.data.defaultGroupFolder) {
            this.folder = this.getDefaultFolder() ?? "";
        }
    }

    async saveGroup(): Promise<void> {
        const encodeName = this.data.encodeGroupName
            ? `?encodeGroupName=${this.data.encodeGroupName.toString()}`
            : "";
        const request = `/groups/create/${this.getFolderName()}${encodeName}`;
        const response = await toPromise(this.http.get<IDocument>(request));

        if (response.ok) {
            this.close(response.result);
        } else {
            this.setMessage(response.result.error.error);
        }
    }

    /**
     * User group must always have a name, but folder is optional.
     */
    private getFolderName(): string {
        return this.folder ? this.folder + "/" + this.name : this.name;
    }

    setMessage(message?: string): void {
        this.message = message;
    }

    canChooseFolder(): boolean {
        // if the `canChooseFolder` param isn't set, user should be able to set the folder
        return this.data?.canChooseFolder ?? true;
    }

    getDefaultFolder(): string | undefined {
        // TODO return empty string instead of undefined?
        return this.data?.defaultGroupFolder;
    }
}

@NgModule({
    declarations: [UserGroupDialogComponent],
    imports: [
        DialogModule,
        FormsModule,
        TimUtilityModule,
        CommonModule,
        HttpClientModule,
    ],
    exports: [UserGroupDialogComponent],
})
export class UserGroupDialogModule {}
