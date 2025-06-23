import {Component, HostListener, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {FormsModule} from "@angular/forms";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {Users} from "tim/user/userService";

export interface IReportContentParams {
    currentUrl: string;
}

export interface IContentReport {
    reason: string;
    email: string | null;
    reportedUrl: string;
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
                        <div class="row">
                            <div class="col-lg-6">
                                <label i18n for="idUrl">
                                    Page URL
                                    <span>(optional)</span>
                                </label>
                                <div class="input-group">
                                    <input 
                                            class="form-control"
                                            id="idUrl" 
                                            type="url" 
                                            name="url" 
                                            [(ngModel)]="reportedUrl"
                                            i18n>
                                    <span class="input-group-btn">
                                        <button class="btn btn-info" (click)="getUrl()" tooltip="Retrieve current Url"><span class="glyphicon glyphicon-repeat"></span></button>
                                    </span>
                                </div>
                                <small class="help-block" i18n>This is the page where you encountered the issue. You can change it if you're reporting a different page.</small>
                            </div>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="row">
                            <div class="col-lg-6">
                                <label i18n for="idEmail">
                                    Your Email
                                    <span>(optional)</span>
                                </label>
                                <div class="input-group">
                                    <input 
                                            class="form-control"
                                            id="idEmail" 
                                            type="email" 
                                            #emailField="ngModel"
                                            name="emailInput" 
                                            [(ngModel)]="email" 
                                            placeholder="Email"
                                            maxlength="256"
                                            i18n>
                                    <span class="input-group-btn">
                                        <button class="btn btn-info" tooltip="Get user email" (click)="getUserMail()">@</button>
                                    </span>
                                    <div *ngIf="emailField.invalid && (emailField.dirty || emailField.touched)" class="alert alert-danger">
                                        <small *ngIf="emailField.errors?.email" i18n>Please enter a valid email address.</small>
                                        <small *ngIf="emailField.errors?.maxlenght" i18n>The email address cannot exceed 256 characters.</small>
                                    </div>
                                </div>
                                    <small class="help-block" i18n>Enter your email if you'd like us to contact you about your report.</small>
                            </div>
                        </div>
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
                                maxlength="2000"
                                required></textarea>
                        <div *ngIf="reasonfield.invalid && (reasonfield.dirty || reasonfield.touched)" class="alert alert-danger">
                            <small *ngIf="reasonfield.errors?.required" i18n><span class="glyphicon glyphicon-exclamation-sign"></span> This field is required.</small>    
                            <small *ngIf="reasonfield.errors?.maxlenght" i18n>The description cannot exceed 2000 characters.</small>    
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
    IContentReport
> {
    protected dialogName = "ReportContent";
    reason = "";
    name = "";
    email: string | null = "";
    reportedUrl = "";
    private currentUser = Users.getCurrent();

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {
        this.reportedUrl = this.data.currentUrl;
    }

    getUrl() {
        this.reportedUrl = window.location.href;
    }

    protected getUserMail() {
        this.email = this.currentUser.email;
    }

    // TODO: Sanitize user input
    async sendMail() {
        this.close({
            reason: this.reason,
            email: this.email,
            reportedUrl: this.reportedUrl,
        });

        const json_payload = {
            reason: this.reason,
            email: this.email,
            reportedUrl: this.reportedUrl,
        };

        const r = await toPromise(
            this.http.post<{status: "ok"}>("/report", json_payload)
        );
        if (!r.ok) {
            await showMessageDialog(r.result.error.error);
            return;
        } else {
            await showMessageDialog(
                "Thank you for your report. The report has been sent to TIM administrators."
            );
        }
    }
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
        TooltipModule,
    ],
})
export class ReportContentDialogModule {}
