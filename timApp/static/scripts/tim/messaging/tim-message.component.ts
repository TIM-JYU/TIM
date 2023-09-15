import type {OnInit} from "@angular/core";
import {Component, HostBinding, Input, NgModule} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {markAsRead} from "tim/messaging/messagingUtils";
import {FormsModule} from "@angular/forms";
import {toPromise} from "tim/util/utils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {TimMessageData} from "tim/messaging/tim-message-view.component";
import {CommonModule} from "@angular/common";
import * as moment from "moment";

interface ReplyOptions {
    archive: boolean;
    messageChannel: boolean;
    pageList: string;
    recipient: string | null;
    readReceipt: boolean;
    repliesTo?: number;
}

const MAX_DISPLAY_LENGTH = 210;

@Component({
    selector: "tim-message",
    template: `
        <ng-container *ngIf="showMessage">
            <tim-alert *ngIf="errorMessage">
                {{errorMessage}}
            </tim-alert>
            <div class="timMessageDisplay">
                <tim-close-button *ngIf="!isGlobal" class="closeButton" (click)="closeMessage()"></tim-close-button>
                <p class="messageInformation" *ngIf="message.sender">
                    <span class="from" i18n>From: </span>
                    <span class="sender">{{message.sender}}</span>
                    <!-- TODO Display what group the message is related to
                    <span class="group" *ngIf="messageToGroup">, {{group}}</span>
                    -->
                </p>
                <p class="messageDate">
                    <span i18n>Sent: </span>
                    <span class="sentDate">{{messageDate}}</span>
                </p>
                <p class="messageHeading">{{message.message_subject}}</p>
                <div class="fullMessageContent" *ngIf="showFullContent">
                    <div class="fullContentText" [innerHTML]="message.message_body"></div>
                    <p class="toggleReadMore" *ngIf="messageOverMaxLength">
                        <a (click)="toggleDisplayedContentLength()" i18n>Read less</a>
                    </p>
                </div>
                <div class="cutMessageContent" *ngIf="!showFullContent">
                    <div class="shownContentText" [innerHTML]="shownContent"></div>
                    <p class="toggleReadMore"><a (click)="toggleDisplayedContentLength()" i18n>Read more</a></p>
                </div>
                <div class="buttonArea">
                    <button class="timButton" *ngIf="message.can_reply" (click)="reply()" i18n>Reply</button>
                </div>
                <div class="replyArea" *ngIf="showReply && message.sender">
                    <p class="replyTo">To: {{message.sender}}</p>
                    <textarea class="replyTextarea" id="reply-message" name="reply-message"
                              [(ngModel)]="replyMessage"></textarea>
                    <div class="sent">
                        <button class="timButton" [disabled]="!canSendReply" (click)="sendReply()" i18n>Send</button>
                        <span class="replySent" *ngIf="replySent" i18n>Sent!</span>
                    </div>
                </div>
                <form class="readReceiptArea">
                    <button class="timButton btn-sm mark-as-read"
                            (click)="markAsRead()"
                            [disabled]="!canMarkAsRead || markedAsRead" title="Marks message as read and permanently hides it" i18n-title i18n>
                        Mark as read
                    </button>
                    <span class="readReceiptLink" *ngIf="markedAsRead" i18n>
                        Read receipt can be cancelled in <a href="/view/{{message.doc_path}}">the message document</a>
                    </span>
                    <button class="timButton hide-message" (click)="closeMessage()">
                        <ng-container *ngIf="!markedAsRead" i18n>Hide</ng-container>
                        <ng-container *ngIf="markedAsRead" i18n>Close</ng-container>
                    </button>
                </form>
            </div>
        </ng-container>
    `,
    styleUrls: ["tim-message.component.scss"],
})
export class TimMessageComponent implements OnInit {
    @Input()
    message!: TimMessageData;
    errorMessage?: string;
    @HostBinding("class.global-message") isGlobal: boolean = false;

    messageDate!: string;
    messageOverMaxLength: boolean = false;
    showMessage: boolean = true;
    shownContent?: string;
    showFullContent: boolean = true;
    showReply: boolean = false;
    canMarkAsRead: boolean = true;
    markedAsRead: boolean = false;
    replyMessage: string = "";
    replySent: boolean = false;
    canSendReply: boolean = true; // enable/disable 'Send' button
    messageToGroup: boolean = true; // can't get from database
    group?: string; // can't get from database
    replyOptions: ReplyOptions = {
        archive: true,
        messageChannel: false,
        pageList: "messages/tim-messages",
        recipient: "",
        readReceipt: true,
        repliesTo: undefined,
    };

    constructor(private http: HttpClient) {}

    /**
     * Toggles between showing the full message content and the shortened version.
     */
    toggleDisplayedContentLength(): void {
        this.showFullContent = !this.showFullContent;
    }

    /**
     * Hides the message view; shows an alert about sending a read receipt and an option to cancel.
     */
    async markAsRead() {
        const result = await markAsRead(this.http, this.message.id);
        if (result.ok) {
            this.markedAsRead = true;
        } else {
            this.errorMessage = $localize`Could not mark as read: ${result.result.error.error}`;
        }
    }

    /**
     * Shows or hides the reply area.
     */
    reply(): void {
        this.showReply = !this.showReply;
    }

    /**
     * Allows/disallows marking as read for testing purposes.
     */
    readReceipt(): void {
        this.canMarkAsRead = !this.canMarkAsRead;
    }

    /**
     * Sends reply to sender
     */
    async sendReply() {
        this.replySent = true;
        this.canSendReply = false;
        if (!this.message.sender) {
            return;
        }
        this.replyOptions.recipient = this.message.sender;
        this.replyOptions.repliesTo = this.message.id;
        const result = await toPromise(
            this.http.post("/timMessage/reply", {
                options: this.replyOptions,
                message: {
                    messageBody: this.replyMessage,
                    messageSubject: `[Re] ${this.message.message_subject}`,
                    recipients: [this.replyOptions.recipient],
                },
            })
        );
        if (!result.ok) {
            this.errorMessage = $localize`Could not reply to message: ${result.result.error.error}`;
        }
    }

    /**
     * Hides the entire message. If the message can't be replied to or marked as read
     * by recipient, closing it hides it permanently by marking it as read in database.
     */
    async closeMessage() {
        this.showMessage = false;

        if (!this.message.can_reply && !this.canMarkAsRead) {
            await markAsRead(this.http, this.message.id);
        }
    }

    ngOnInit(): void {
        // TODO Display what group the message is related to; currently can't retrieve from database
        this.isGlobal = this.message.doc_path.includes("/global/");
        if (
            this.message.message_body.length > MAX_DISPLAY_LENGTH &&
            !this.isGlobal
        ) {
            this.messageOverMaxLength = true;
            this.showFullContent = false;
            this.shownContent = `${this.message.message_body.substr(
                0,
                MAX_DISPLAY_LENGTH
            )}...`;
        }

        this.messageDate = moment(this.message.created)
            .local()
            .format("dddd, MMMM Do YYYY, HH:mm:ss");
    }
}

@NgModule({
    declarations: [TimMessageComponent],
    exports: [TimMessageComponent],
    imports: [CommonModule, FormsModule, TimUtilityModule],
})
export class TimMessageModule {}
