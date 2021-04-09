import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";

// import {TimMessage} from "tim/timApp/messaging/timMessage/timMessage";

@Component({
    selector: "tim-message",
    template: `
        <div class="timMessageDisplay" *ngIf="showMessage">
            <p>From: {{sender}}</p>
            <p>{{content}}</p>
            <div class="buttonArea">
                <button class="timButton" [disabled]="!canMarkAsRead" title="Read Receipt"
                        (click)="markAsRead()">
                    Mark as Read
                </button>
                <button class="timButton" *ngIf="canReply" (click)="reply()">Reply</button>
                <button class="timButton" (click)="readReceipt()">TEST: allow/disallow read receipt</button>
            </div>
            <div class="replyArea" *ngIf="showReply">
                <textarea id="reply-message" name="reply-message"></textarea>
                <div class="sent">
                    <button class="timButton" (click)="send()">Send</button>
                    <span *ngIf="replySent">Sent!</span>
                </div>
            </div>
        </div>
        <ng-container *ngIf="markedAsRead">
            <p>Read receipt delivered to sender</p>
            <button class="timButton" (click)="cancel()">Cancel</button>
        </ng-container>
    `,
})
export class TimMessageComponent implements OnInit {
    showMessage: boolean = true;
    showReply: boolean = false;
    canMarkAsRead: boolean = false;
    markedAsRead: boolean = false;
    replySent: boolean = false;
    canReply: boolean = true;
    sender?: string;
    content?: string;

    /**
     * Hides the message contents; shows an alert about message being read and an option to cancel.
     */
    markAsRead(): void {
        this.markedAsRead = true;
        this.showMessage = false;
    }

    /**
     * Cancel read receipt and re-display the message.
     */
    cancel(): void {
        this.markedAsRead = false;
        this.showMessage = true;
    }

    /**
     * Shows or hides the reply area.
     */
    reply(): void {
        this.showReply = !this.showReply;
    }

    /**
     * Allow/disallow marking as read for testing purposes.
     */
    readReceipt(): void {
        this.canMarkAsRead = !this.canMarkAsRead;
    }

    /**
     * Hides the message and shows a "sent" alert.
     */
    send(): void {
        this.replySent = true;
    }

    ngOnInit(): void {
        // TODO Read sender, content etc. from JSON?
        this.sender = "ohj1k21";
        this.content =
            "Sinulta puuttuu demo 3 ja 4 pisteitä. Miten jatketaan kurssin kanssa?";
    }
}

@NgModule({
    declarations: [TimMessageComponent],
    exports: [TimMessageComponent],
    imports: [CommonModule],
})
export class TimMessageModule {}
