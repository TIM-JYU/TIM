import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
import type {ExamGroup} from "tim/plugin/examGroupManager/exam-group-manager.component";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";

export interface ExamGroupDialogParams {
    folderPath: string;
    groupPrefix?: string;
}

@Component({
    selector: "tim-exam-group-create-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container i18n header>
                Create a new exam group
            </ng-container>
            <ng-container body>
                <form #form="ngForm" class="form-horizontal">
                    <fieldset [disabled]="loading">
                        <div class="form-group">
                            <label i18n for="name" class="col-sm-2 control-label">Group name</label>
                            <div class="col-sm-10">
                                <input i18n-placeholder type="text" required
                                       [(ngModel)]="name"
                                       (ngModelChange)="setMessage()"
                                       id="name" name="name"
                                       class="form-control"
                                       placeholder="Enter the name of the exam group"/>
                            </div>
                        </div>
                    </fieldset>

                </form>

                <tim-alert *ngIf="message" severity="danger">
                    <ng-container *ngIf="message">
                        {{ message }}
                    </ng-container>
                </tim-alert>

            </ng-container>
            <ng-container footer>
                <div>
                    <tim-loading *ngIf="loading"></tim-loading>
                    <button i18n class="timButton" type="button" (click)="saveGroup()" [disabled]="form.invalid">
                        Create
                    </button>
                    <button i18n class="btn btn-default" type="button" (click)="dismiss()">
                        Cancel
                    </button>
                </div>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class ExamGroupCreateDialogComponent extends AngularDialogComponent<
    ExamGroupDialogParams,
    ExamGroup
> {
    protected dialogName = "ExamGroupCreate";
    name = "";
    message?: string;
    loading = false;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {}

    async saveGroup(): Promise<void> {
        const response = await toPromise(
            this.http.post<ExamGroup>(`/examGroupManager/createGroup`, {
                name: this.name,
                group_folder_path: this.data.folderPath,
                group_prefix: this.data.groupPrefix ?? "",
            })
        );

        if (response.ok) {
            this.close(response.result);
        } else {
            this.setMessage(response.result.error.error);
        }
    }

    setMessage(message?: string): void {
        this.message = message;
    }
}
