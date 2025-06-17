import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {FormsModule} from "@angular/forms";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {toPromise} from "tim/util/utils";

export interface IReportContentParams {
    currentUrl: string;
}

@Component({
    selector: "report-content-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container i18n header>
                Report content
            </ng-container>
            <ng-container body>
                <div class="page-header">
                    <h1 i18n>Report Content</h1>
                </div>
                <div i18n class="help-block">
                    <p>You can use this form to report any harmful or inappropriate content you have encountered while using TIM.</p>
                    <p>If you would like the TIM administrators to follow up with you, you may optionally include you email address.</p>
                </div>
                <form #reportForm="ngForm">
                    <div class="form-group">
                        <label i18n for="idUrl">
                            Page URL
                            <span>(optional)</span>
                        </label>
                        <input 
                                class="form-control"
                                id="idUrl" 
                                type="url" 
                                name="url" 
                                [(ngModel)]="reportedUrl"
                                i18n>
                        <small class="help-block" i18n>This is the page where you encountered the issue. You can change it if you're reporting a different page.</small>
                    </div>
                    <div class="form-group">
                        <label i18n for="idEmail">
                            Your Email
                            <span>(optional)</span>
                        </label> 
                        <input 
                                class="form-control"
                                id="idEmail" 
                                type="email" 
                                #emailField="ngModel"
                                name="emailInput" 
                                [(ngModel)]="email" 
                                placeholder="Email"
                                i18n>
                        <small class="help-block" i18n>Enter your email if you'd like us to contact you about your report.</small>
                    </div>
                    <div class="form-group" [ngClass]="{'has-error': reasonfield.invalid && reasonfield.touched}">
                        <label for="idReportText" i18n>Describe the issue</label>
                        <textarea 
                                class="form-control" 
                                id="idReportText" 
                                name="reportText" 
                                rows="5"
                                [(ngModel)]="reason" 
                                #reasonfield="ngModel" 
                                required></textarea>
                        <div *ngIf="reasonfield.invalid && reasonfield.touched" class="alert alert-danger">
                            <small *ngIf="reasonfield.errors?.required" i18n>This field is required.</small>    
                        </div>
                        <small class="help-block" i18n>Please explain what you found harmful or inappropriate. Be as specific as possible.</small>
                    </div>
                </form>           
            </ng-container>
            <ng-container footer>
                <button class="timButton" (click)="sendMail()" [disabled]="reportForm.invalid" i18n>Report Page</button>
                <button class="btn btn-default" (click)="dismiss()" i18n>Cancel</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class ReportContentDialogComponent extends AngularDialogComponent<
    IReportContentParams,
    any
> {
    protected dialogName = "ReportContent";
    reason = "";
    name = "";
    email = "";
    reportedUrl = "";

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {
        this.reportedUrl = this.data.currentUrl;
    }

    // TODO: Sanitize user input
    async sendMail() {
        this.close({
            reason: this.reason,
            name: this.name,
            email: this.email,
            reportedUrl: this.reportedUrl,
        });

        const json_payload = {
            reason: this.reason,
            name: this.name,
            email: this.email,
            reportedUrl: this.reportedUrl,
        };

        const r = await toPromise(this.http.post("/report", json_payload));
        if (r.ok) {
            console.log(r.result);
        }
    }

    protected readonly JSON = JSON;
}

@NgModule({
    declarations: [ReportContentDialogComponent],
    exports: [ReportContentDialogComponent],
    imports: [
        CommonModule,
        TimUtilityModule,
        DialogModule,
        FormsModule,
        HttpClientModule,
    ],
})
export class ReportContentDialogModule {}
