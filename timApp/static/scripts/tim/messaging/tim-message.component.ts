import {Component, Input, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {HttpClient} from "@angular/common/http";
import {markAsRead} from "tim/messaging/messagingUtils";
import {FormsModule} from "@angular/forms";
import {to2} from "tim/util/utils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {TimMessageData} from "./tim-message-view.component";

interface ReplyOptions {
    archive: boolean;
    messageChannel: boolean;
    pageList: string;
    recipient: string | null;
    readReceipt: boolean;
    repliesTo?: number;
}

@Component({
    selector: "tim-message",
    template: `
        <ng-container *ngIf="showMessage">
            <tim-alert *ngIf="errorMessage">
                {{errorMessage}}
            </tim-alert>
            <div class="timMessageDisplay">
                <tim-close-button class="closeButton" (click)="closeMessage()"></tim-close-button>
                <p class="messageInformation">
                    <span class="from" i18n>From: </span>
                    <span class="sender">{{sender}}</span>
                    <!-- TODO Display what group the message is related to
                    <span class="group" *ngIf="messageToGroup">, {{group}}</span> 
                    -->
                </p>
                <p class="messageHeading">{{heading}}</p>
                <div class="fullMessageContent" *ngIf="showFullContent">
                    <div class="fullContentText" [innerHTML]="fullContent"></div>
                    <p class="toggleReadMore" *ngIf="messageOverMaxLength">
                        <a (click)="toggleDisplayedContentLength()" i18n>Read less</a>
                    </p>
                </div>
                <div class="cutMessageContent" *ngIf="!showFullContent">
                    <div class="shownContentText" [innerHTML]="shownContent"></div>
                    <p class="toggleReadMore"><a (click)="toggleDisplayedContentLength()" i18n>Read more</a></p>
                </div>
                <div class="buttonArea">
                    <button class="timButton" *ngIf="canReply" (click)="reply()" i18n>Reply</button>
                </div>
                <div class="replyArea" *ngIf="showReply">
                    <p class="replyTo">To: {{sender}}</p>
                    <textarea class="replyTextarea" id="reply-message" name="reply-message"
                              [(ngModel)]="replyMessage"></textarea>
                    <div class="sent">
                        <button class="timButton" [disabled]="!canSendReply" (click)="sendReply()" i18n>Send</button>
                        <span class="replySent" *ngIf="replySent" i18n>Sent!</span>
                    </div>
                </div>
                <form class="readReceiptArea">
                    <label class="markAsReadLabel"><input class="markAsReadCheckbox" 
                                                          type="checkbox" 
                                                          name="mark-as-read"
                                                          id="mark-as-read"
                                                          [disabled]="!canMarkAsRead || markedAsRead"
                                                          (click)="markAsRead()" i18n/> Mark as Read</label>
                    <span class="readReceiptLink" *ngIf="markedAsRead" i18n>
                        Read receipt can be cancelled in <a href="/view/messages/tim-messages">your messages</a>
                    </span>
                    <button class="timButton" title="Close Message" (click)="closeMessage()" i18n>
                        Close
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

    messageMaxLength: number = 210;
    messageOverMaxLength: boolean = false;
    showMessage: boolean = true;
    fullContent?: string;
    shownContent?: string;
    showFullContent: boolean = true;
    showReply: boolean = false;
    canMarkAsRead: boolean = true;
    markedAsRead: boolean = false;
    replyMessage: string = "";
    replySent: boolean = false;
    canReply: boolean = true; // show/hide 'Reply' button
    canSendReply: boolean = true; // enable/disable 'Send' button
    sender?: string;
    messageToGroup: boolean = true; // can't get from database
    group?: string; // can't get from database
    heading?: string;
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
        if (!this.sender) {
            return;
        }
        this.replyOptions.recipient = this.sender;
        this.replyOptions.repliesTo = this.message.id;
        const result = await to2(
            this.http
                .post("/timMessage/reply", {
                    options: this.replyOptions,
                    messageBody: {
                        messageBody: this.replyMessage,
                        messageSubject: this.heading + " [Re]",
                        recipients: [this.replyOptions.recipient],
                    },
                })
                .toPromise()
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

        if (!this.canReply && !this.canMarkAsRead) {
            await markAsRead(this.http, this.message.id);
        }
    }

    ngOnInit(): void {
        this.setValues(this.message);
    }

    setValues(timMessage: TimMessageData) {
        this.sender = timMessage.sender;
        // TODO Display what group the message is related to; currently can't retrieve from database
        // this.group = "ohj1k21";
        this.heading = timMessage.message_subject;
        this.fullContent = timMessage.message_body;
        this.canMarkAsRead = timMessage.can_mark_as_read;
        this.canReply = timMessage.can_reply;

        if (this.fullContent.length > this.messageMaxLength) {
            this.messageOverMaxLength = true;
            this.showFullContent = false;
            this.shownContent =
                this.fullContent.substr(0, this.messageMaxLength) + "...";
        }
    }
}

@NgModule({
    declarations: [TimMessageComponent],
    exports: [TimMessageComponent],
    imports: [CommonModule, FormsModule, TimUtilityModule],
})
export class TimMessageModule {}
