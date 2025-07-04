import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {FormsModule} from "@angular/forms";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
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
    ok: boolean;
    result: {
        data?: string;
        error?: string;
        status?: string;
    };
}

@Component({
    selector: "report-content-dialog",
    template: `
        <tim-dialog-frame [minimizable]="false">
            <ng-container i18n header>
                Report inappropriate content
            </ng-container>
            <ng-container body>
                <div class="page-header">
                    <h1 i18n>Report inappropriate content</h1>
                </div>
                <div i18n class="help-block">
                    <p>Use this form to report any harmful or inappropriate content you encounter on TIM. If you would like a response from the TIM administrators, please provide your email address.</p>
                </div>
                <form #reportForm="ngForm">
                    <fieldset [disabled]="showOk" [ngClass]="{'text-muted': showOk}">
                        <div class="form-group">
                            <div class="row">
                                <div class="col-lg-12">
                                    <label i18n for="idUrl">
                                        Page address
                                        <span>(optional)</span>
                                    </label>
                                    <div class="input-group" [ngClass]="{'has-error': urlError}">
                                        <input 
                                                class="form-control"
                                                id="idUrl" 
                                                type="url" 
                                                name="url" 
                                                [(ngModel)]="reportedUrl"
                                                (ngModelChange)="clearUrlError()"
                                                i18n>
                                        <span class="input-group-btn">
                                            <button class="btn btn-primary" (click)="getUrl()" i18n>Fill current address</button>
                                        </span>
                                    </div>
                                    <div *ngIf="urlError" class="alert alert-danger">
                                        <small *ngIf="urlError" i18n>Please check the page address. You can only report addresses from TIM.</small>
                                    </div>
                                    <small class="help-block" i18n>This is the page address where you encountered the issue. If you are reporting a problem on a different page, please paste the correct address here.</small>
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
                                    <div class="input-group" [ngClass]="{'has-error': email_error}">
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
                                            <button class="btn btn-primary" (click)="getUserMail()" [disabled]="!isLoggedIn()" i18n>Fill user email</button>
                                        </span>
                                        </div>
                                        <div *ngIf="emailfield.invalid && (emailfield.dirty || emailfield.touched) || email_error" class="alert alert-danger">
                                            <small *ngIf="emailfield.errors?.email || email_error" i18n>Please enter a valid email address.</small>
                                            <small *ngIf="emailfield.errors?.maxlenght" i18n>The email address cannot exceed 256 characters.</small>
                                    </div>
                                    <small class="help-block" i18n>Please provide your email address if you wish for the TIM administrators to follow up with you regarding this report.</small>
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
                            <small class="help-block" i18n>Explain why you think the content is inappropriate. Be as specific as possible.</small>
                        </div>
                    </fieldset>
                </form>           
            </ng-container>
            <ng-container footer>
                <div *ngIf="showError">
                    <div class="alert alert-danger alert-dismissible">
                        <button type="button" class="close" (click)="dismissAlert()"><span>&times;</span></button>
                        <p class="text-left" i18n><strong><span class="glyphicon glyphicon-exclamation-sign"></span> There was an error.</strong></p>
                        <p class="text-left">{{errorMsg}}</p>
                        <p class="text-left" i18n>Your report was not sent.</p>
                    </div>
                </div>
                <div *ngIf="!showOk">
                    <button class="timButton" (click)="sendMail()" [disabled]="reportForm.invalid || showError || urlError || email_error" i18n>Report Page</button>
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
    styleUrls: ["./report-content-dialog.component.scss"],
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
    showError: boolean = false;
    email_error: boolean = false;
    errorMsg: string = "";
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

    dismissAlert() {
        this.showError = false;
        this.errorMsg = "";
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
            this.showError = true;
            const status = r.result.status ?? 0;
            if (status <= 0) {
                this.errorMsg = $localize`Could not connect to server. Check your internet connection and try again.`;
            } else if (r.result.error.error == "invalid_email") {
                this.showError = false;
                this.email_error = true;
            } else if (r.result.error.error == "invalid_url") {
                this.showError = false;
                this.urlError = true;
            } else if (status >= 500) {
                this.errorMsg = $localize`There is an issue with the server. Make a copy of your message and refresh the page, or contact TIM support directly at tim@jyu.fi.`;
            }
        } else {
            this.showOk = true;
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
