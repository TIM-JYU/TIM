import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import type {GroupMember} from "tim/ui/group-management.component";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {toPromise} from "tim/util/utils";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

/**
 * Defines additional parameters for creating new users via the UserImportDialog.
 *
 *  - group (number): id of the group that the new user(s) will be added to
 */
export interface UserImportDialogParams {
    group: number;
}

@Component({
    selector: "tim-user-import-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container i18n header>
                Import users
            </ng-container>
            <ng-container body>
                <form #form="ngForm">
                    <div class="instructions" i18n>
                        <span>
                            If your school uses Wilma, you can import students by copy-pasting the student list from Wilma.
                            <br>
                            The required format for the input is as follows:
                        <pre>< class > < last name > < first name ></pre>
                            For example:
                        <pre>9b Duck Donald<br>9b Duck Daisy<br>9c Duck Scrooge</pre>
                            Students are separated by line-breaks so that one line of text represents one student. 
                            Each piece of information (class, last name, first name) is separated by a single space.
                        </span>
                    </div>
                    
                    <div class="form-group">
                        <textarea i18n-placeholder [(ngModel)]="user_import_text"
                                  rows="10"
                               id="user_import_text" name="user_import_text"
                               class="form-control"
                               placeholder="Enter student information in the format <class> <last name> <first name>, one student per line."></textarea>
                    
                    </div>
                </form>
                
            </ng-container>
            <ng-container footer>
                <button i18n class="timButton" type="button" (click)="saveUser()" [disabled]="form.invalid">
                    Add
                </button>
                <button i18n class="btn btn-default" type="button" (click)="dismiss()">
                    Cancel
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["user-import-dialog.component.scss"],
})
export class UserImportDialogComponent extends AngularDialogComponent<
    UserImportDialogParams,
    GroupMember[]
> {
    protected dialogName = "UserImport";
    user_import_text: string = "";

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {}

    async saveUser(): Promise<void> {
        const url = `/loginCode/importUsers/${this.getGroup()}`;
        const response = await toPromise(
            this.http.post<GroupMember[]>(url, {
                // TODO sanitize
                text: this.user_import_text,
            })
        );

        if (response.ok) {
            this.close(response.result);
        }
    }

    private getGroup(): number {
        return this.data.group;
    }
}

@NgModule({
    declarations: [UserImportDialogComponent],
    imports: [
        DialogModule,
        FormsModule,
        TimUtilityModule,
        CommonModule,
        HttpClientModule,
    ],
    exports: [UserImportDialogComponent],
})
export class UserImportDialogModule {}
