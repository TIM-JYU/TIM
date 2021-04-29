import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {itemglobals} from "tim/util/globals";
import {HttpClient} from "@angular/common/http";
import {to2} from "tim/util/utils";
import {getItem, IItem} from "tim/item/IItem";

// import {TimMessage} from "tim/timApp/messaging/timMessage/timMessage";

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
                    <p>{{fullContent}}</p>
                    <p *ngIf="messageOverMaxLength"><a (click)="toggleDisplayedContentLength()">Read less</a></p>
                </div>
                <div class="cutMessageContent" *ngIf="!showFullContent">
                    <p>{{shownContent}}<span>...</span></p>
                    <p><a (click)="toggleDisplayedContentLength()">Read more</a></p>
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
    messageMaxLength: number = 210;
    messageOverMaxLength: boolean = true;
    showMessage: boolean = true;
    fullContent?: string;
    shownContent?: string;
    showFullContent: boolean = false;
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
    markAsRead(): void {
        this.markedAsRead = !this.markedAsRead;
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
        const itemId = itemglobals().curr_item.id;
        void this.loadValues(itemId);

        // TODO Read group and content from database
        this.group = "ohj1k21";
        // this.fullContent =
        //     "Sinulta puuttuu demo 3 ja 4 pisteitä. Miten jatketaan kurssin kanssa?";
        this.fullContent =
            "Sinulta puuttuu demo 3 ja 4 pisteitä. Miten jatketaan kurssin kanssa? \
            Kullakin demokerralla 100% oikeuttava määrä on 8 pistettä. Kultakin \
            demokerralta lasketaan jakson 1 aikana max. 10 p, eli vaikka saisit \
            kerättyä lisätehtävillä enemmänkin pisteitä, otetaan jokaiselta demokerralta \
            lukuun enintään 10 pistettä. Jakso 2 max 8p/kerta. Muutos: paitsi demo 12 \
            jolloin määrällä ei ole ylärajaa. HUOM! 2 tehtävää / kerta ei kuitenkaan \
            riitä kurssin läpäisemiseen, sillä ensinnäkään siitä ei tule yhteensä 40% \
            ja toisekseen joka kerta jää 6 tehtävää muita jälkeen. 12 kerran jälkeen on \
            melkoisen paljon muita jäljessä! 105 % suoritustavassa on JOKA demokerta \
            tehtävä vähintään yksi Bonus- tai Guru-tehtävä (vuonna 2018 ekassa jaksossa, \
            jatkossa molemmissa jaksoissa).";
        if (this.fullContent.length < this.messageMaxLength) {
            this.messageOverMaxLength = false;
            this.showFullContent = true;
        } else {
            this.shownContent = this.fullContent.substr(
                0,
                this.messageMaxLength
            );
        }
    }

    async loadValues(itemId: number) {
        const message = await to2(
            // get message shown on current page
            this.http
                .get<MessageOptions>(`/timMessage/get/${itemId}`)
                .toPromise()
        );

        if (message.ok) {
            const parId = message.result.par_id; // TODO retrieve par contents
            const messageDoc = await getItem(message.result.doc_id); // get message's document
            if (!messageDoc) {
                return;
            }
            this.setValues(message.result, messageDoc);
        } else {
            console.error(message.result.error.error);
        }
    }

    setValues(options: MessageOptions, doc: IItem) {
        // TODO set message contents
        this.sender = doc.owners[0].name;
        this.heading = doc.title;
        this.canMarkAsRead = options.can_mark_as_read;
        this.canReply = options.reply;
    }
}

interface MessageOptions {
    can_mark_as_read: boolean;
    display_type: number;
    doc_id: number;
    id: number;
    par_id: string;
    reply: boolean;
}

@NgModule({
    declarations: [TimMessageComponent],
    exports: [TimMessageComponent],
    imports: [CommonModule],
})
export class TimMessageModule {}
