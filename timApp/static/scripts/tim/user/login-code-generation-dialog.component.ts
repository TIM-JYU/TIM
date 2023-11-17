import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {toPromise} from "tim/util/utils";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {GroupMember} from "tim/ui/group-management.component";

export interface LoginCodeGenerationDialogParams {
    members: GroupMember[];
}

@Component({
    selector: "tim-login-code-generation-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container i18n header>
                Generate login codes
            </ng-container>
            <ng-container body>
                <form #form="ngForm" class="form-horizontal">
                    
                    <div class="form-group">
                        <label i18n for="activation_start" class="col-sm-2 control-label">Activation start</label>
                        <div class="col-sm-10">
                            <input type="datetime-local"
                                   [(ngModel)]="activation_start"
                                   (ngModelChange)="setMessage()"
                                   id="activation_start" name="activation_start"
                                   class="form-control">
                        </div>
                    </div>
                    <div class="form-group">
                        <label i18n for="activation_end" class="col-sm-2 control-label">Activation end</label>
                        <div class="col-sm-10">
                            <input type="datetime-local"
                                   [(ngModel)]="activation_end"
                                   (ngModelChange)="setMessage()"
                                   id="activation_end" name="activation_end"
                                   class="form-control">
                        </div>
                    </div>
                </form>
                
                <tim-alert *ngIf="message" severity="danger">
                    <ng-container *ngIf="message">
                        {{message}}
                    </ng-container>
                </tim-alert>

            </ng-container>
            <ng-container footer>
                <button i18n class="timButton" type="button" (click)="generateCodes()" [disabled]="form.invalid">
                    Generate login codes
                </button>
                <button i18n class="btn btn-default" type="button" (click)="dismiss()">
                    Cancel
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["login-code-generation-dialog.component.scss"],
})
export class LoginCodeGenerationDialogComponent extends AngularDialogComponent<
    LoginCodeGenerationDialogParams,
    undefined
> {
    protected dialogName = "GenerateLoginCode";
    activation_start?: string = "";
    activation_end?: string = "";
    activation_status?: string = "";
    // extra_info?: string = "";

    message?: string;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {}

    private getMembers(): GroupMember[] {
        return this.data.members;
    }

    async generateCodes(): Promise<void> {
        const url = `/loginCode/generateCodes`;

        const response = await toPromise(
            this.http.post<undefined>(url, {
                members: this.getMembers(),
                activation_start: this.activation_start,
                activation_end: this.activation_end,
                activation_status: this.activation_status,
                // extra_info: this.extra_info ?? "",
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

@NgModule({
    declarations: [LoginCodeGenerationDialogComponent],
    imports: [
        DialogModule,
        FormsModule,
        TimUtilityModule,
        CommonModule,
        HttpClientModule,
    ],
    exports: [LoginCodeGenerationDialogComponent],
})
export class LoginCodeGenerationDialogModule {}
