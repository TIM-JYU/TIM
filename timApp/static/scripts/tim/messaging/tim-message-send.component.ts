import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {
    Component,
    EventEmitter,
    Input,
    NgModule,
    Output,
    ViewChild,
} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule, NgForm} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {TimepickerModule} from "ngx-bootstrap/timepicker";
import {DatetimePickerModule} from "tim/ui/datetime-picker/datetime-picker.component";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {isAdmin, Users} from "tim/user/userService";
import {TimStorage, toPromise} from "tim/util/utils";
import {CommonModule} from "@angular/common";
import {BrowserModule} from "@angular/platform-browser";
import {NoopAnimationsModule} from "@angular/platform-browser/animations";
import * as t from "io-ts";

const TimSavedMessageOptions = t.type({
    email: t.boolean,
    defaultEmail: t.boolean,
    replyAll: t.union([t.boolean, t.undefined]),
    timMessage: t.boolean,
    archive: t.boolean,
    important: t.boolean,
    messageChannel: t.boolean,
    pageList: t.string,
    isPrivate: t.boolean,
    reply: t.boolean,
    readReceipt: t.boolean,
});

interface TimMessageOptions extends t.TypeOf<typeof TimSavedMessageOptions> {
    expires: Date | undefined;
    sender: string | null;
    senderEmail: string | null;
}

@Component({
    selector: "tim-message-send",
    template: `
        <div class="csRunDiv tableEmail" style="padding: 1em;" *ngIf="recipientList || sendGlobal">
            <tim-close-button *ngIf="!sendGlobal" (click)="closeComponent()"></tim-close-button>
            <form #form="ngForm">
                <fieldset [disabled]="sending || !canUseComponent">
                    <div class="message-info">
                        <ng-container *ngIf="!sendGlobal">
                            <label for="recipients" i18n>Recipients:</label>
                            <textarea class="form-control" id="recipients" name="recipients" [(ngModel)]="recipientList"
                                      rows="4" (input)="somethingChanged()"></textarea>
                        </ng-container>

                        <label for="subject" i18n>Subject:</label>
                        <input class="form-control" [(ngModel)]="messageSubject" (input)="somethingChanged()"
                               id="subject"
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
                        <div class="send-as-label" *ngIf="!sendGlobal">
                            <span i18n>Send as</span>
                            <a tooltip="Select at least one channel" i18n-tooltip><i
                                    class="glyphicon glyphicon-info-sign"></i></a>
                        </div>

                        <div class="cb-collection">
                            <div *ngIf="!sendGlobal">
                                <input type="checkbox" (change)="notDefault()" [(ngModel)]="timMessageOptions.email" id="send-email"
                                       name="send-email" i18n>
                                <label for="send-email" i18n>email</label>
                            </div>
                            <div class="cb-collection" *ngIf="timMessageOptions.email && !sendGlobal">
                                <div>
                                    <input type="radio" [(ngModel)]="timMessageOptions.defaultEmail" name="send-tim-email" id="send-tim"
                                           [value]="false">
                                    <label for="send-tim" i18n>Use TIM to send</label>
                                </div>
                                <div>
                                    <input type="radio" [(ngModel)]="timMessageOptions.defaultEmail" name="email-use-local-app"
                                           id="email-use-local-app" [value]="true">
                                    <label for="email-use-local-app" i18n>Use your default email client</label>
                                    <a tooltip="Recipients will see each others' addresses" i18n-tooltip><i
                                            class="glyphicon glyphicon-info-sign"></i></a>
                                </div>
                                <ng-container *ngIf="timMessageOptions.email && !timMessageOptions.defaultEmail">
                                    <div class="top-space">
                                        <input type="radio" [(ngModel)]="timMessageOptions.replyAll" name="email-reply-sender"
                                               id="email-reply-sender" [value]="false" checked>
                                        <label for="email-reply-sender" i18n>Recipient only replies to sender</label>
                                        <a tooltip="Recipients can only reply to you" i18n-tooltip><i
                                                class="glyphicon glyphicon-info-sign"></i></a>
                                    </div>
                                    <div>
                                        <input type="radio" [(ngModel)]="timMessageOptions.replyAll" name="email-reply-all"
                                               id="email-reply-all" [value]="true">
                                        <label for="email-reply-all" i18n>Recipient replies all by default</label>
                                        <a tooltip="Recipients can reply to all other recipients" i18n-tooltip><i
                                                class="glyphicon glyphicon-info-sign"></i></a>
                                    </div>
                                </ng-container>
                            </div>
                            <div *ngIf="!timMessageOptions.defaultEmail && !sendGlobal">
                                <input type="checkbox" (change)="emptyPageList()" [(ngModel)]="timMessageOptions.timMessage"
                                       name="send-tim-message" id="send-tim-message">
                                <label for="send-tim-message" i18n>TIM message</label>
                                <a href="/view/tim/ohjeita/kayttoohjeet-tim-viesteille"
                                   target="_blank"
                                   title="Help for TIM messages (in Finnish)"
                                   i18n-title>
                                    <i class="helpButton glyphicon glyphicon-question-sign"></i>
                                </a>
                            </div>
                            <div class="cb-collection" *ngIf="timMessageOptions.timMessage && !timMessageOptions.defaultEmail">
                                <div class="page-list" *ngIf="!sendGlobal">
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
                                    <input type="checkbox" [(ngModel)]="timMessageOptions.important"
                                           name="tim-message-important" id="tim-message-important">
                                    <label for="tim-message-important" i18n>Message is important</label>
                                    <a tooltip="Message will persist on user's screen until they mark it as read"
                                       i18n-tooltip>
                                        <i class="glyphicon glyphicon-info-sign"></i>
                                    </a>
                                </div>
                                <div>
                                    <input type="checkbox" [(ngModel)]="timMessageOptions.isPrivate"
                                           name="tim-message-private" id="tim-message-private" disabled>
                                    <label for="tim-message-private" i18n>Recipient sees TIM message as private</label>
                                </div>
                                <div *ngIf="!sendGlobal">
                                    <input type="checkbox" [(ngModel)]="timMessageOptions.reply"
                                           name="tim-message-can-reply" id="tim-message-can-reply">
                                    <label for="tim-message-can-reply" i18n>TIM message can be replied to</label>
                                </div>
                                <div>
                                    <input type="checkbox" [(ngModel)]="timMessageOptions.readReceipt"
                                           name="tim-message-can-mark-read" id="tim-message-can-mark-read" disabled>
                                    <label for="tim-message-can-mark-read" i18n>TIM message can be marked as
                                        read</label>
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
                    <div class="button-panel">
                        <button class="timButton" id="sendButton" [disabled]="disableSendButton()"
                                (click)="sendMessage()" i18n>
                            Send
                        </button>
                        <tim-loading *ngIf="sending"></tim-loading>
                    </div>
                </fieldset>
                <tim-alert severity="success" *ngIf="messageSentOk">
                    <ng-container i18n>
                        Message sent!
                    </ng-container>
                    <ng-container *ngIf="timMessageDoc">
                        <a [href]="timMessageDoc" target="_blank" i18n>View sent TIM message</a>
                    </ng-container>
                </tim-alert>
                <tim-alert severity="danger" *ngIf="messageSendError">{{messageSendError}}</tim-alert>
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
     *  close when the component is closed from the x. It would probably require change detection for the recipientList
     *  variable, as its length grows beoynd 0 the the flag is set on?
     */

    @ViewChild("form") form?: NgForm;

    @Input() recipientList: string = "";
    @Output() recipientListChange = new EventEmitter<string>();
    @Input()
    sendGlobal: boolean = false;
    @Input()
    docId?: number;
    @Input()
    storageKey?: string;
    @Input()
    replyToEmail?: string | null;
    messageSubject: string = "";
    messageBody: string = "";
    showOptions: boolean = false;
    emailbcc: boolean = false;
    emailbccme: boolean = true;
    messageSendError?: string;
    messageSentOk: boolean = false;
    timMessageDoc?: string;
    urlError?: string;
    formChanged: boolean = true;
    sending = false;
    canUseComponent: boolean = true;
    timMessageOptions: TimMessageOptions = {
        email: true,
        defaultEmail: false,
        messageChannel: false,
        replyAll: false,
        timMessage: false,
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
    private resetOnChange: boolean = false;
    private prevForm?: NgForm;
    private messageStorage?: TimStorage<
        t.TypeOf<typeof TimSavedMessageOptions>
    >;

    ngOnInit() {
        if (this.storageKey) {
            this.messageStorage = new TimStorage(
                `TimMessageSendComponent.Options.${this.storageKey}`,
                TimSavedMessageOptions
            );

            const storedOptions = this.messageStorage.get();
            if (storedOptions) {
                this.timMessageOptions = {
                    ...this.timMessageOptions,
                    ...storedOptions,
                };
                this.showOptions = true;
            }
            console.log(this.timMessageOptions);
        }

        if (this.sendGlobal) {
            if (!isAdmin()) {
                this.messageSendError = $localize`You are not authorized to send global messages`;
                this.canUseComponent = false;
                return;
            }
            this.timMessageOptions.timMessage = true;
            this.timMessageOptions.email = false;
        }
    }

    get recipients() {
        return this.recipientList.trim();
    }

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
        this.timMessageOptions.defaultEmail = false;
    }

    emptyPageList() {
        this.timMessageOptions.pageList = "";
    }

    somethingChanged() {
        this.formChanged = true;
    }

    // Checks if all mandatory fields have values
    disableSendButton() {
        if (this.sendGlobal) {
            return (
                this.messageSubject.trim().length == 0 ||
                this.messageBody.trim().length == 0
            );
        }

        return (
            !this.formChanged ||
            this.urlError ||
            (!this.timMessageOptions.messageChannel &&
                !this.timMessageOptions.email &&
                !this.timMessageOptions.timMessage) ||
            (this.timMessageOptions.timMessage &&
                !this.timMessageOptions.pageList)
        );
    }

    // Checks if the URLs that the user wants to save TIM message to actually exist in TIM
    // and that the user has at least edit access to them
    // Also shortens existing URLs
    async checkUrls() {
        this.urlError = "";
        const result = await toPromise(
            this.http.post<{shortened_urls: string}>("/timMessage/url_check", {
                urls: this.timMessageOptions.pageList,
            })
        );
        if (!result.ok) {
            this.urlError = result.result.error.error;
        } else {
            this.timMessageOptions.pageList = result.result.shortened_urls;
        }
    }

    // resets form to its initial values
    resetForm = () => {
        this.messageSendError = undefined;
        this.messageSentOk = false;
        this.timMessageDoc = undefined;
    };

    private saveAndResetFormOnChange() {
        if (this.storageKey) {
            this.messageStorage?.set(this.timMessageOptions);
        }

        this.resetOnChange = true;
        if (this.form && this.form !== this.prevForm) {
            this.form.valueChanges?.subscribe(() => {
                if (this.resetOnChange) {
                    this.resetOnChange = false;
                    this.resetForm();
                }
            });
            this.prevForm = this.form;
        }
    }

    public async sendMessage() {
        this.sending = true;
        this.resetForm();
        // send as TIM message
        if (this.timMessageOptions.timMessage) {
            const result = await this.postTimMessage(this.timMessageOptions);
            if (!result.ok) {
                this.messageSendError = $localize`Failed to send as TIM message: ${result.result.error.error}`;
                this.sending = false;
                return;
            } else {
                this.messageSentOk = true;
                this.timMessageDoc = `/view/${result.result.docPath}`;
            }
        }
        // send as email in TIM
        if (
            this.timMessageOptions.email &&
            !this.timMessageOptions.defaultEmail
        ) {
            await this.sendEmailTim();
            this.sending = false;
            this.saveAndResetFormOnChange();
            return;
        }
        // TODO: iPad do not like ;
        if (
            this.timMessageOptions.email &&
            this.timMessageOptions.defaultEmail
        ) {
            let addrs = this.recipients.replace(/\n/g, ",");
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
        }
        this.sending = false;
        this.saveAndResetFormOnChange();
    }

    async sendEmailTim() {
        if (!this.docId) {
            this.messageSendError = $localize`Cannot send email without docId`;
            return;
        }
        this.messageSendError = ""; // JSON.stringify(response);
        const url = `/multiSendEmail/${this.docId}`;
        const response = await toPromise(
            this.http.post<string[]>(url, {
                rcpt: this.recipients.replace(/\n/g, ";"),
                subject: this.messageSubject,
                msg: this.messageBody,
                bccme: this.emailbccme,
                replyall: this.timMessageOptions.replyAll,
                reply_to: this.replyToEmail ?? null,
            })
        );
        if (!response.ok) {
            this.messageSendError = response.result.error.error;
        } else {
            this.messageSentOk = true;
            this.saveAndResetFormOnChange();
        }
    }

    private postTimMessage(opts: TimMessageOptions) {
        const message: Record<string, unknown> = {
            messageBody: this.messageBody,
            messageSubject: this.messageSubject,
            recipients: undefined,
        };
        const options: Record<string, unknown> = {...opts};
        delete options.defaultEmail;
        delete options.replyAll;
        delete options.email;
        delete options.timMessage;

        if (!this.sendGlobal) {
            message.recipients = this.recipients.split(/\n/g);
        }
        return toPromise(
            this.http.post<{docPath: string}>("/timMessage/send", {
                message,
                options,
            })
        );
    }
}

@NgModule({
    declarations: [TimMessageSendComponent],
    imports: [
        CommonModule,
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
export class TimMessageSendModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

// Needed because doDowngrade doesn't include BrowserModule by default
@NgModule({
    declarations: [],
    imports: [
        BrowserModule,
        NoopAnimationsModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        BsDropdownModule.forRoot(),
        TimepickerModule.forRoot(),
        TooltipModule.forRoot(),
        DatetimePickerModule,
    ],
    exports: [],
})
export class TimMessageSendModuleAngularJS implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                TimMessageSendModuleAngularJS
            )
        ),
        "timMessageSend",
        TimMessageSendComponent
    ),
];
