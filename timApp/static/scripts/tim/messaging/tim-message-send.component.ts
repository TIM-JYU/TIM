import {Component, EventEmitter, Input, NgModule, Output} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BrowserModule} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {TimepickerModule} from "ngx-bootstrap/timepicker";
import {DatetimePickerModule} from "tim/ui/datetime-picker/datetime-picker.component";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {Users} from "../user/userService";
import {to2} from "../util/utils";

interface TimMessageOptions {
    archive: boolean;
    important: boolean;
    messageChannel: boolean;
    pageList: string;
    isPrivate: boolean;
    reply: boolean;
    readReceipt: boolean;
    expires: Date | undefined;
    sender: string | null;
    senderEmail: string | null;
}

@Component({
    selector: "tim-message-send",
    template: `
        <tim-alert *ngIf="errorMessage">{{errorMessage}}</tim-alert>
        <div class="csRunDiv tableEmail" style="padding: 1em;" *ngIf="recipientList">
            <tim-close-button (click)="closeComponent()"></tim-close-button>
            <form>
                <div class="message-info">
                    <label for="recipients" i18n>Recipients:</label>
                    <textarea class="form-control" id="recipients" name="recipients" [(ngModel)]="recipientList"
                              rows="4" (input)="somethingChanged()"></textarea>

                    <label for="subject" i18n>Subject:</label>
                    <input class="form-control" [(ngModel)]="messageSubject" (input)="somethingChanged()" id="subject"
                           name="subject">
                </div>
                <label for="message" i18n>Message content:</label>
                <textarea class="form-control" [(ngModel)]="messageBody" rows="10" (input)="somethingChanged()"
                          id="message" name="message"></textarea>
                <div class="extra-options" (click)="toggleOptions(); somethingChanged()">
                    <i class="glyphicon" [ngClass]="showOptions ? 'glyphicon-minus' : 'glyphicon-plus'"></i>
                    <ng-container *ngIf="showOptions; else hideOptions" i18n>Hide message options</ng-container>
                    <ng-template #hideOptions i18n>Show message options</ng-template>
                </div>
                <div *ngIf="showOptions">
                    <div class="cb-collection" *ngIf="!defaultEmail">
                        <div>
                            <input type="checkbox" [(ngModel)]="timMessageOptions.important" name="important-message"
                                   id="important-message" disabled>
                            <label for="important-message" *ngIf="!defaultEmail" i18n>Archive message</label>
                            <a tooltip="Currently only applies to TIM messages" i18n-tooltip><i
                                    class="glyphicon glyphicon-info-sign"></i></a>
                        </div>
                    </div>

                    <div class="send-as-label">
                        <span i18n>Send as</span>
                        <a tooltip="Select at least one channel" i18n-tooltip><i
                                class="glyphicon glyphicon-info-sign"></i></a>
                    </div>

                    <div class="cb-collection">
                        <div>
                            <input type="checkbox" (change)="notDefault()" [(ngModel)]="email" id="send-email"
                                   name="send-email" i18n>
                            <label for="send-email" i18n>email</label>
                        </div>
                        <div class="cb-collection" *ngIf="email">
                            <div>
                                <input type="radio" [(ngModel)]="defaultEmail" name="send-tim-message" id="send-tim"
                                       [value]="false">
                                <label for="send-tim" i18n>Use TIM to send</label>
                            </div>
                            <div>
                                <input type="radio" [(ngModel)]="defaultEmail" name="email-use-local-app"
                                       id="email-use-local-app" [value]="true">
                                <label for="email-use-local-app" i18n>Use your default email client</label>
                                <a tooltip="Recipients will see each others' addresses" i18n-tooltip><i
                                        class="glyphicon glyphicon-info-sign"></i></a>
                            </div>
                            <ng-container *ngIf="email && !defaultEmail">
                                <div class="top-space">
                                    <input type="radio" [(ngModel)]="replyAll" name="email-reply-sender"
                                           id="email-reply-sender" [value]="false" checked>
                                    <label for="email-reply-sender" i18n>Recipient only replies to sender</label>
                                    <a tooltip="Recipients can only reply to you" i18n-tooltip><i
                                            class="glyphicon glyphicon-info-sign"></i></a>
                                </div>
                                <div>
                                    <input type="radio" [(ngModel)]="replyAll" name="email-reply-all"
                                           id="email-reply-all" [value]="true">
                                    <label for="email-reply-all" i18n>Recipient replies all by default</label>
                                    <a tooltip="Recipients can reply to all other recipients" i18n-tooltip><i
                                            class="glyphicon glyphicon-info-sign"></i></a>
                                </div>
                            </ng-container>
                        </div>
                        <div *ngIf="!defaultEmail">
                            <input type="checkbox" (change)="emptyPageList()" [(ngModel)]="timMessage"
                                   name="send-tim-message" id="send-tim-message">
                            <label for="send-tim-message" i18n>TIM message</label>
                        </div>
                        <div class="cb-collection" *ngIf="timMessage && !defaultEmail">
                            <div class="page-list">
                                <span class="pages-label">
                                    <label for="tim-message-pages" i18n>Pages to send TIM message to</label>
                                    <a tooltip="Enter URL addresses of the pages one per line. URLs will be automatically shortened."
                                       i18n-tooltip><i class="glyphicon glyphicon-info-sign"></i></a>
                                </span>
                                <tim-alert *ngIf="urlError" severity="danger">
                                    {{ urlError }}
                                </tim-alert>
                                <textarea class="form-control" [(ngModel)]="timMessageOptions.pageList"
                                          (input)="checkUrls()" rows="4" name="tim-message-pages"
                                          id="tim-message-pages"></textarea>
                            </div>
                            <div>
                                <input type="checkbox" [(ngModel)]="timMessageOptions.isPrivate"
                                       name="tim-message-private" id="tim-message-private" disabled>
                                <label for="tim-message-private" i18n>Recipient sees TIM message as private</label>
                            </div>
                            <div>
                                <input type="checkbox" [(ngModel)]="timMessageOptions.reply"
                                       name="tim-message-can-reply" id="tim-message-can-reply">
                                <label for="tim-message-can-reply" i18n>TIM message can be replied to</label>
                            </div>
                            <div>
                                <input type="checkbox" [(ngModel)]="timMessageOptions.readReceipt"
                                       name="tim-message-can-mark-read" id="tim-message-can-mark-read" disabled>
                                <label for="tim-message-can-mark-read" i18n>TIM message can be marked as read</label>
                            </div>
                            <div class="message-expiration">
                                <label for="expiration-selector" i18n>TIM message will be removed on:</label>
                                <tim-datetime-picker inputId="expiration-selector"
                                                     [(time)]="timMessageOptions.expires"
                                                     placeholder="No automatic date" i18n-placeholder>
                                </tim-datetime-picker>
                            </div>
                        </div>
                    </div>
                </div>
                <div>
                    <button class="timButton" id="sendButton" [disabled]="disableSendButton()"
                            (click)="sendMessage()" i18n>
                        Send
                    </button>
                    <span class="savedtext" *ngIf="messageMsg"> {{messageMsg}}</span>
                </div>
            </form>
        </div>
    `,
    styleUrls: ["./tim-message-send.scss"],
})
export class TimMessageSendComponent {
    /**
     *  TODO: This component has a minor bug. If the textfield of recipients is emptied by hand, the component closes
     *  and it can't be reopened unless the recipientList variable changes. A hypothetical fix would be to use a
     *  separate flag in the *ngIf, instead of just the recipientList variable. Then this flag would only be operated to
     *  close when the component is closed from the x. It would propably requrie change detection for the recipientList
     *  variable, as its length grows beoynd 0 the the flag is set on?
     */

    @Input()
    recipientList: string = "";
    @Output() recipientListChange = new EventEmitter<string>();
    messageSubject: string = "";
    messageBody: string = "";
    showOptions: boolean = false;
    emailbcc: boolean = false;
    emailbccme: boolean = true;
    email: boolean = true;
    defaultEmail: boolean = false;
    replyAll: boolean | undefined = false;
    messageMsg: string = "";
    timMessage: boolean = false;
    urlError: string = "";
    errorMessage?: string;
    formChanged: boolean = true;
    timMessageOptions: TimMessageOptions = {
        messageChannel: false,
        archive: false,
        important: false,
        isPrivate: false,
        pageList: "",
        readReceipt: true,
        reply: false,
        expires: undefined,
        sender: Users.getCurrent().real_name,
        senderEmail: Users.getCurrent().email,
    };
    @Input()
    docId?: number;

    constructor(private http: HttpClient) {}

    /**
     * Close the component and propagate the information to parent component.
     */
    closeComponent() {
        this.recipientList = "";
        this.recipientListChange.emit(this.recipientList);
    }

    toggleOptions() {
        this.showOptions = !this.showOptions;
    }

    notDefault() {
        this.defaultEmail = false;
    }

    emptyPageList() {
        this.timMessageOptions.pageList = "";
    }

    somethingChanged() {
        this.formChanged = true;
    }

    // Checks if all mandatory fields have values
    disableSendButton() {
        return (
            !this.formChanged ||
            this.urlError ||
            (!this.timMessageOptions.messageChannel &&
                !this.email &&
                !this.timMessage) ||
            (this.timMessage && !this.timMessageOptions.pageList)
        );
    }

    // Checks if the URLs that the user wants to save TIM message to actually exist in TIM
    // and that the user has at least edit access to them
    // Also shortens existing URLs
    async checkUrls() {
        this.urlError = "";
        const result = await to2(
            this.http
                .post<{shortened_urls: string}>("/timMessage/url_check", {
                    urls: this.timMessageOptions.pageList,
                })
                .toPromise()
        );
        if (!result.ok) {
            this.urlError = result.result.error.error;
        } else {
            this.timMessageOptions.pageList = result.result.shortened_urls;
        }
    }

    // resets form to its initial values
    resetForm() {
        this.messageMsg = "Sent!";
        setTimeout((): void => {
            this.messageMsg = "";
        }, 5000);
        this.showOptions = false;
        this.emailbcc = false;
        this.emailbccme = true;
        this.email = true;
        this.defaultEmail = false;
        this.replyAll = false;
        this.timMessage = false;
        this.formChanged = false;
        this.timMessageOptions = {
            messageChannel: false,
            archive: false,
            important: false,
            isPrivate: false,
            pageList: "",
            readReceipt: true,
            reply: false,
            expires: undefined,
            sender: Users.getCurrent().real_name,
            senderEmail: Users.getCurrent().email,
        };
    }

    public async sendMessage() {
        this.errorMessage = undefined;
        // send as TIM message
        if (this.timMessage) {
            const result = await this.postTimMessage(this.timMessageOptions);
            if (!result.ok) {
                this.errorMessage = $localize`Failed to send as TIM message: ${result.result.error.error}`;
                return;
            }
        }
        // send as email in TIM
        if (this.email && !this.defaultEmail) {
            await this.sendEmailTim();
            return;
        }
        // TODO: iPad do not like ;
        if (this.email && this.defaultEmail) {
            let addrs = this.recipientList.replace(/\n/g, ",");
            let bcc = "";
            if (this.emailbcc) {
                bcc = addrs;
                addrs = "";
            }
            if (this.emailbccme) {
                if (bcc) {
                    bcc += ",";
                }
                bcc += Users.getCurrent().email;
            }
            window.location.href =
                "mailto:" +
                addrs +
                "?" +
                "subject=" +
                this.messageSubject +
                "&" +
                "body=" +
                this.messageBody +
                "&" +
                "bcc=" +
                bcc;
            this.resetForm();
        }
        this.resetForm();
    }

    private postTimMessage(options: TimMessageOptions) {
        const message = {
            messageBody: this.messageBody,
            messageSubject: this.messageSubject,
            recipients: this.recipientList.split(/\n/g),
        };
        return to2(
            this.http.post("/timMessage/send", {options, message}).toPromise()
        );
    }

    async sendEmailTim() {
        if (!this.docId) {
            this.messageMsg = $localize`Cannot send email without docId`;
            return;
        }
        this.messageMsg = ""; // JSON.stringify(response);
        const url = `/multiSendEmail/${this.docId}`;
        let response;
        // if reply all is chosen
        if (this.replyAll) {
            response = await to2(
                this.http
                    .post<string[]>(url, {
                        rcpt: this.recipientList.replace(/\n/g, ";"),
                        subject: this.messageSubject,
                        msg: this.messageBody,
                        bccme: this.emailbccme,
                    })
                    .toPromise()
            );
            if (!response.ok) {
                this.messageMsg = response.result.error.error;
            } else {
                this.resetForm();
            }
        } else {
            // if only reply to sender is chosen
            const recipients = this.recipientList.split(/\n/g);
            for (const recipient of recipients) {
                response = await to2(
                    this.http
                        .post<string[]>(url, {
                            rcpt: recipient,
                            subject: this.messageSubject,
                            msg: this.messageBody,
                            bccme: this.emailbccme,
                        })
                        .toPromise()
                );
                if (!response.ok) {
                    this.messageMsg = response.result.error.error;
                }
            }
            this.resetForm();
        }
    }
}

@NgModule({
    declarations: [TimMessageSendComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        BsDropdownModule.forRoot(),
        TimepickerModule.forRoot(),
        TooltipModule.forRoot(),
        DatetimePickerModule,
    ],
    exports: [TimMessageSendComponent],
})
export class TimMessageSendModule {}
