import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {itemglobals} from "tim/util/globals";
import {to2} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {markAsRead} from "tim/messaging/messagingUtils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

@Component({
    selector: "manage-read-receipt",
    template: `
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
    markedAsRead: boolean = false;
    receipt: TimMessageReadReceipt | undefined;
    canMarkAsRead: boolean = false;
    errorMessage?: string;

    constructor(private http: HttpClient) {}

    ngOnInit(): void {
        const docId = itemglobals().curr_item.id;

        void this.getReadReceipt(docId);
    }

    /**
     * Retrieves information about the read receipt from database;
     * displays the read receipt component if marking the message as read
     * is allowed and sets the read receipt status correctly.
     *
     * @param docId Identifier for the message's document
     */
    async getReadReceipt(docId: number) {
        this.errorMessage = undefined;
        const message = await to2(
            this.http
                .get<TimMessageReadReceipt>(
                    `/timMessage/get_read_receipt/${docId}`
                )
                .toPromise()
        );

        if (message.ok) {
            this.receipt = message.result;
            this.canMarkAsRead = message.result.can_mark_as_read;
            if (message.result.marked_as_read_on != null) {
                this.markedAsRead = true;
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
