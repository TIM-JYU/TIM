import {Component, NgModule} from "@angular/core";
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

export interface IContentReportResponse {
    status: string;
    description?: string;
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
                    <fieldset [disabled]="showOk" [ngClass]="{'text-muted': showOk}">
                        <div class="form-group">
                            <div class="row">
                                <div class="col-lg-12">
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
                                                (ngModelChange)="clearUrlError()"
                                                i18n>
                                        <span class="input-group-btn">
                                            <button class="btn btn-primary" (click)="getUrl()" i18n>Get current url</button>
                                        </span>
                                    </div>
                                    <div *ngIf="urlError" class="alert alert-danger">
                                        <small *ngIf="urlError" i18n>Please check the url address. You can only report addresses from TIM.</small>
                                    </div>
                                    <small class="help-block" i18n>This is the page where you encountered the issue. You can change it if you're reporting a different page.</small>
                                </div>
                            </div>
                        </div>
                        <div class="form-group">
                            <div class="row">
                                <div class="col-lg-12">
                                    <label i18n for="idEmail">
                                        Your Email
                                        <span>(optional)</span>
                                    </label>
                                    <div class="input-group">
                                        <input 
                                                class="form-control"
                                                id="idEmail" 
                                                type="email" 
                                                #emailfield="ngModel"
                                                name="emailInput" 
                                                [(ngModel)]="email" 
                                                placeholder="Email"
                                                maxlength="256"
                                                (ngModelChange)="clearEmailError()"
                                                i18n>
                                        <span class="input-group-btn">
                                            <button class="btn btn-primary" (click)="getUserMail()" [disabled]="!isLoggedIn()" i18n>Fill with user email</button>
                                        </span>
                                        </div>
                                        <div *ngIf="emailfield.invalid && (emailfield.dirty || emailfield.touched) || email_error" class="alert alert-danger">
                                            <small *ngIf="emailfield.errors?.email || email_error" i18n>Please enter a valid email address.</small>
                                            <small *ngIf="emailfield.errors?.maxlenght" i18n>The email address cannot exceed 256 characters.</small>
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
                    </fieldset>
                </form>           
            </ng-container>
            <ng-container footer>
                <div *ngIf="!showOk">
                    <button class="timButton" (click)="sendMail()" [disabled]="reportForm.invalid" i18n>Report Page</button>
                    <button class="btn btn-default" (click)="dismiss()" i18n>Cancel</button>
                </div>
                <div *ngIf="showOk">
                    <div class="alert alert-success">
                        <p class="text-left" i18n><strong>Thank you for your report.</strong></p>
                        <p class="text-left" i18n>Your message has been sent to the TIM administrators. We appreciate your help in keeping our content accurate and safe.</p>
                    </div>
                    <button class="timButton" (click)="dismiss()" i18n>Done</button>
                </div>
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
    showOk: boolean = false;
    email_error: boolean = false;
    urlError: boolean = false;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {
        this.reportedUrl = this.data.currentUrl;
    }

    getUrl() {
        this.reportedUrl = window.location.href;
        this.clearUrlError();
    }

    protected isLoggedIn(): boolean {
        return Users.isLoggedIn();
    }

    protected getUserMail() {
        this.email = this.currentUser.email;
        this.clearEmailError();
    }

    clearEmailError() {
        this.email_error = false;
    }

    clearUrlError() {
        this.urlError = false;
    }

    async sendMail() {
        const json_payload = {
            reason: this.reason,
            email: this.email,
            reportedUrl: this.reportedUrl,
        };

        const r = await toPromise(
            this.http.post<IContentReportResponse>("/report", json_payload)
        );
        if (!r.ok) {
            await showMessageDialog(r.result.error.error);
            return;
        } else {
            if (r.result.status == "error") {
                if (r.result.description == "invalid_email") {
                    this.email_error = true;
                } else if (r.result.description == "invalid_url") {
                    this.urlError = true;
                }
            } else {
                this.showOk = true;
            }
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
