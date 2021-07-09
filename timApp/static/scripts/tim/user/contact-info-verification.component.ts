import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {verificationglobals, VerificationType} from "tim/util/globals";
import {to2} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {Channel} from "tim/messaging/listOptionTypes";

// Component for verifying user's additional contact information. See verification.py and path
// /contact/<verification_token> for additional details.
@Component({
    selector: "contact-info-verification",
    template: `
        <h1>Verification</h1>
        <bootstrap-panel title="{{panelTitle}}">
            <div *ngIf="!error && verificationToken">
                <p>Are you sure you wish to verify this contact information?</p>
                <p>Type: {{channel}}</p>
                <p>Information: {{contactInfo}} </p>
                <button class="timButton" (click)="callVerify()" [disabled]="verificationSucceeded">Verify</button>
            </div>
            <div *ngIf="error">
                <p>An error has occured.<span *ngIf="errorCode"> Details below:</span></p>
                <tim-alert severity="danger" *ngIf="errorCode == '01'">Invalid verification link. Check that the link in
                    the browser's address bar is the same as in the message you used to arrive to this site.
                </tim-alert>
                <tim-alert severity="danger" *ngIf="errorCode == '02'">This contact info is already verified.
                </tim-alert>
                <tim-alert severity="danger" *ngIf="errorCode == '03'">This contact info is already verified.
                </tim-alert>
            
            </div>
            <div *ngIf="verificationSucceeded !== undefined">
                <tim-alert severity="success" *ngIf="verificationSucceeded">Verification succeeded! Your new contact
                    information has been linked with your TIM profile.
                </tim-alert>
                <tim-alert severity="danger" *ngIf="!verificationSucceeded">Verification failed! If the problem
                    persists, please contact TIM's support (details in the site footer).
                </tim-alert>
            </div>
        </bootstrap-panel>
    `,
})
export class ContactInfoVerificationComponent implements OnInit {
    // Template variables.

    // A string token identifying a verification on TIM.
    verificationToken?: string;
    // A flag if fetching a verification token failed.
    error?: boolean;
    // An error message specifying what failed in fetching. See Python function 'contact_info_verification' for error
    // codes.
    errorCode?: string;
    // The type of verification. For the value of undefined we assume some type of error happened at the server.
    verificationType?: VerificationType;
    // Title for panel. Should be overridden from server side.
    panelTitle: string = "Verification";
    // The type of contact information.
    channel?: Channel;
    // The contact information.
    contactInfo?: string;

    // Other variables.

    // A flag if the verification succeeded. For the value of undefined assume that the verification POST call has not
    // yet been made.
    verificationSucceeded?: boolean = undefined;

    // For initializing HttpClient.
    constructor(private http: HttpClient) {}

    ngOnInit(): void {
        // Initialize template variables.
        this.verificationToken = verificationglobals().verification_token;
        this.verificationType = verificationglobals().type;
        this.error = verificationglobals().error;
        this.errorCode = verificationglobals().error_code;
        this.panelTitle = verificationglobals().title
            ? verificationglobals().title
            : this.panelTitle;
        this.channel = verificationglobals().channel;
        this.contactInfo = verificationglobals().contact_info;
    }

    /**
     * HTTP POST to server to finalize verifying a user's new contact information.
     */
    async callVerify() {
        // If verification would for some reason be undefined, then there is no reason to call the server with a POST
        // method. Also helps in typing.
        if (!this.verificationToken) {
            return;
        }
        // If any kind of error flag is up, then calling for verification doesn't make sense.
        if (this.error) {
            return;
        }

        const r = await to2(
            this.http
                .post(`/verification/contact/${this.verificationToken}`, {})
                .toPromise()
        );

        if (r.ok) {
            this.verificationSucceeded = true;
        } else {
            this.error = true;
            this.errorCode = r.result.error.error;
        }
    }
}

@NgModule({
    declarations: [ContactInfoVerificationComponent],
    exports: [ContactInfoVerificationComponent],
    imports: [CommonModule, TimUtilityModule],
})
export class ContactInfoVerificationModule {}
