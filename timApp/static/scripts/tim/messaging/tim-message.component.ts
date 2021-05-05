import {Component, Input, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {HttpClient} from "@angular/common/http";
import {to2} from "tim/util/utils";
import {TimMessageData} from "./tim-message-view.component";

@Component({
    selector: "tim-message",
    template: `
        <ng-container *ngIf="showMessage">
            <div class="timMessageDisplay">
                <p>
                    <span>From: </span>
                    <span>{{sender}}</span>
                    <span *ngIf="messageToGroup">, {{group}}</span>
                </p>
                <p class="message-heading">{{heading}}</p>
                <div class="fullMessageContent" *ngIf="showFullContent">
                    <div [innerHTML]="fullContent"></div>
                    <p *ngIf="messageOverMaxLength" class="toggleReadMore"><a (click)="toggleDisplayedContentLength()">Read less</a></p>
                </div>
                <div class="cutMessageContent" *ngIf="!showFullContent">
                    <div [innerHTML]="shownContent"></div>
                    <p class="toggleReadMore"><a (click)="toggleDisplayedContentLength()">Read more</a></p>
                </div>
                <div class="buttonArea">
                    <button class="timButton" *ngIf="canReply" (click)="reply()">Reply</button>
                </div>
                <div class="replyArea" *ngIf="showReply">
                    <p>To: {{sender}}</p>
                    <textarea id="reply-message" name="reply-message"></textarea>
                    <div class="sent">
                        <button class="timButton" [disabled]="!canSendReply" (click)="sendReply()">Send</button>
                        <span *ngIf="replySent">Sent!</span>
                    </div>
                </div>
                <div class="readReceiptArea">
                    <input type="checkbox" name="mark-as-read" id="mark-as-read"
                           [disabled]="!canMarkAsRead || markedAsRead" (click)="markAsRead()"/>
                    <label for="mark-as-read">Mark as Read</label>
                    <span class="readReceiptLink"
                          *ngIf="markedAsRead">Read receipt can be cancelled in <a>your messages</a></span>
                    <button class="timButton" title="Close Message"
                            [disabled]="(!canMarkAsRead && !replySent) || (canMarkAsRead && !markedAsRead)"
                            (click)="closeMessage()">
                        Close
                    </button>
                </div>
            </div>
        </ng-container>
    `,
    styleUrls: ["tim-message.component.scss"],
})
export class TimMessageComponent implements OnInit {
    @Input()
    message: TimMessageData | undefined;

    messageMaxLength: number = 210;
    messageOverMaxLength: boolean = false;
    showMessage: boolean = true;
    fullContent?: string;
    shownContent?: string;
    showFullContent: boolean = true;
    showReply: boolean = false;
    canMarkAsRead: boolean = true;
    markedAsRead: boolean = false;
    replySent: boolean = false;
    canReply: boolean = true; // show/hide 'Reply' button
    canSendReply: boolean = true; // enable/disable 'Send' button
    sender?: string;
    messageToGroup: boolean = true; // can't get from database
    group?: string; // can't get from database
    heading?: string;

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
        this.markedAsRead = true;
        const result = await to2(
            this.http
                .post("/timMessage/mark_as_read", {
                    message_id: this.message?.id,
                })
                .toPromise()
        );
        if (!result.ok) {
            console.error(result.result.error.error);
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
     * Shows a "sent" alert after sending a reply.
     */
    sendReply(): void {
        this.replySent = true;
        this.canSendReply = false;
    }

    /**
     * Hides the entire message.
     */
    closeMessage(): void {
        this.showMessage = false;
    }

    ngOnInit(): void {
        if (this.message) {
            this.setValues(this.message);
            // TODO Read group from database
            this.group = "ohj1k21";
        }
    }

    setValues(timMessage: TimMessageData) {
        this.sender = timMessage.sender;
        this.heading = timMessage.message_subject;
        this.fullContent = timMessage.message_body; // TODO handle html properly
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
    imports: [CommonModule],
})
export class TimMessageModule {}
