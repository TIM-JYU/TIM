import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {toPromise} from "tim/util/utils";
import type {GroupMember} from "tim/plugin/examGroupManager/exam-group-manager.component";

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
                        <p>
                            If your school uses Wilma, you can import students by copy-pasting the student list from
                            Wilma.
                        </p>
                        <p>
                            The required format for the input is as follows:
                        </p>
                        <pre>< class > < last name > < first name ></pre>
                        <p>
                            For example:
                        </p>
                        <pre>9b Duck Donald<br>9b Duck Daisy<br>9c Duck Scrooge</pre>
                        <p>
                            Students are separated by line-breaks so that one line of text represents one student.
                            Each piece of information (class, last name, first name) is separated by a single space.
                        </p>
                        <p>
                            You can obtain the student list from Wilma by selecting "Print course book" and selecting the students in the table.
                        </p>
                    </div>

                    <div class="form-group">
                        <textarea i18n-placeholder [(ngModel)]="importText"
                                  rows="10"
                                  id="user_import_text" name="user_import_text"
                                  class="form-control"
                                  placeholder="Enter student information in the format <class> <last name> <first name>, one student per line."></textarea>

                    </div>
                </form>

                <tim-alert *ngIf="message" severity="danger">
                    <ng-container *ngIf="message">
                        {{ message }}
                    </ng-container>
                </tim-alert>

            </ng-container>
            <ng-container footer>
                <tim-loading *ngIf="loading"></tim-loading>
                <button i18n class="timButton" type="button" (click)="saveUser()" [disabled]="form.invalid || loading">
                    Add
                </button>
                <button i18n class="btn btn-default" type="button" (click)="dismiss()" [disabled]="loading">
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
    importText: string = "";
    loading: boolean = false;
    message?: string = undefined;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {}

    async saveUser(): Promise<void> {
        this.loading = true;
        this.message = undefined;
        const url = `/examGroupManager/importUsers/${this.getGroup()}`;
        const response = await toPromise(
            this.http.post<GroupMember[]>(url, {
                text: this.importText,
            })
        );
        this.loading = false;

        if (!response.ok) {
            this.message = response.result.error.error;
        } else {
            this.close(response.result);
        }
    }

    private getGroup(): number {
        return this.data.group;
    }
}
