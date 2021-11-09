import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {itemglobals} from "tim/util/globals";
import {timeout, to2} from "tim/util/utils";
import {HttpClient, HttpParams} from "@angular/common/http";
import {markAsRead} from "tim/messaging/messagingUtils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import moment from "moment";

@Component({
    selector: "manage-read-receipt",
    template: `
        <hr>
        <p class="small" *ngIf="expires">Note: this message expires on {{expires}}</p>
        <div class="copy-query-panel" *ngIf="showQueryCopy">
            <span>Copy TableForm user query to clipboard:</span>
            <button class="timButton btn-sm" title="Copy read users as TableForm query" (click)="copyReceiptQuery(true, false)">Read users</button>
            <button class="timButton btn-sm" title="Copy unread users as TableForm query" (click)="copyReceiptQuery(false, true)">Unread users</button>
            <button class="timButton btn-sm" title="Copy all users as TableForm query" (click)="copyReceiptQuery(true, true)">All users</button>
            <span [hidden]="!copied">Copied!</span>
        </div>
        <ng-container *ngIf="canMarkAsRead">
            <tim-alert *ngIf="errorMessage">
                {{errorMessage}}
            </tim-alert>
            <div class="manageReadReceipt">
                <button class="timButton" title="Mark as Read" *ngIf="!markedAsRead" (click)="markAsRead()" i18n>
                    Mark as Read
                </button>
                <div class="cancelReadReceipt" *ngIf="markedAsRead">
                    <button class="timButton" title="Cancel Read Receipt" (click)="cancelReadReceipt()" i18n>
                        Cancel Read Receipt
                    </button>
                    <p i18n>Cancelling the read receipt re-displays the message on designated TIM pages</p>
                </div>
            </div>
        </ng-container>
    `,
    styleUrls: ["manage-read-receipt.component.scss"],
})
export class ManageReadReceiptComponent implements OnInit {
    showQueryCopy = false;
    markedAsRead: boolean = false;
    receipt?: TimMessageReadReceipt;
    canMarkAsRead: boolean = false;
    errorMessage?: string;
    expires?: string;
    copied = false;
    docId: number;

    constructor(private http: HttpClient) {
        const item = itemglobals().curr_item;
        this.docId = item.id;
        this.showQueryCopy = item.rights.manage;
    }

    ngOnInit(): void {
        void this.getReadReceipt();
    }

    async copyReceiptQuery(read: boolean, unread: boolean) {
        this.copied = false;
        const r = await to2(
            this.http
                .get(`/timMessage/readReceipts`, {
                    params: new HttpParams()
                        .set("message_doc", this.docId.toString())
                        .set("include_read", read.toString())
                        .set("include_unread", unread.toString())
                        .set("receipt_format", "tableform-query"),
                    responseType: "text",
                })
                .toPromise()
        );

        if (r.ok) {
            await navigator.clipboard.writeText(r.result);
            this.copied = true;
            await timeout(1000);
            this.copied = false;
        }
    }

    /**
     * Retrieves information about the read receipt from database;
     * displays the read receipt component if marking the message as read
     * is allowed and sets the read receipt status correctly.
     *
     * @param docId Identifier for the message's document
     */
    async getReadReceipt() {
        this.errorMessage = undefined;
        const message = await to2(
            this.http
                .get<{expires: string | null; receipt?: TimMessageReadReceipt}>(
                    `/timMessage/get_read_receipt/${this.docId}`
                )
                .toPromise()
        );

        if (message.ok) {
            this.receipt = message.result.receipt;
            this.canMarkAsRead = !!this.receipt?.can_mark_as_read;
            this.markedAsRead = !!this.receipt?.marked_as_read_on;
            if (message.result.expires) {
                this.expires = moment(message.result.expires)
                    .local()
                    .format("dddd, MMMM Do YYYY, h:mm:ss a");
            }
        } else {
            this.errorMessage = $localize`Could not load read information: ${message.result.error.error}`;
        }
    }

    /**
     * Marks the message as read and displays the option to cancel read receipt.
     */
    async markAsRead() {
        if (!this.receipt) {
            return;
        }
        this.errorMessage = undefined;

        const result = await markAsRead(this.http, this.receipt.message_id);
        if (result.ok) {
            this.markedAsRead = true;
        } else {
            this.errorMessage = $localize`Could not mark as read: ${result.result.error.error}`;
            return;
        }

        window.location.reload();
    }

    /**
     * Cancels the read receipt and displays the option to mark the message as read.
     */
    async cancelReadReceipt() {
        this.errorMessage = undefined;
        const result = await to2(
            this.http
                .post("/timMessage/cancel_read_receipt", {
                    message_id: this.receipt?.message_id,
                })
                .toPromise()
        );
        if (!result.ok) {
            this.errorMessage = $localize`Could not cancel read receipt: ${result.result.error.error}`;
            return;
        }

        this.markedAsRead = false;
        window.location.reload();
    }
}

interface TimMessageReadReceipt {
    // Information about the read receipt retrieved from server
    rcpt_id: number;
    message_id: number;
    user_id: number;
    marked_as_read_on: Date;
    can_mark_as_read: boolean;
}

@NgModule({
    declarations: [ManageReadReceiptComponent],
    exports: [ManageReadReceiptComponent],
    imports: [CommonModule, TimUtilityModule],
})
export class ManageReadReceiptModule {}
