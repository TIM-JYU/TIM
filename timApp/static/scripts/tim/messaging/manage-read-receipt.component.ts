import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {itemglobals} from "tim/util/globals";
import {to2} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";

@Component({
    selector: "manage-read-receipt",
    template: `
        <ng-container>
            <button class="timButton" title="Mark as Read" *ngIf="!markedAsRead" (click)="markAsRead()">
                Mark as Read
            </button>
            <div class="cancelReadReceipt" *ngIf="markedAsRead">
                <button class="timButton" title="Cancel Read Receipt" (click)="cancelReadReceipt()">
                    Cancel Read Receipt
                </button>
                <p>Cancelling the read receipt re-displays the message on designated TIM pages.</p>
            </div>
        </ng-container>
    `,
})
export class ManageReadReceiptComponent implements OnInit {
    markedAsRead: boolean = false;
    receipt: TimMessageReadReceipt | undefined;

    constructor(private http: HttpClient) {}

    ngOnInit(): void {
        const docId = itemglobals().curr_item.id;

        void this.getReadReceipt(docId);
        if (this.receipt?.marked_as_read_on != null) {
            this.markedAsRead = true;
        }
    }

    async getReadReceipt(docId: number) {
        const message = await to2(
            // get messages shown on current page
            this.http
                .get<TimMessageReadReceipt>(
                    `/timMessage/get_read_receipt/${docId}`
                )
                .toPromise()
        );

        if (message.ok) {
            this.receipt = message.result;
        } else {
            console.error(message.result.error.error);
        }
    }

    markAsRead(): void {}

    cancelReadReceipt(): void {}
}

interface TimMessageReadReceipt {
    // Information about the message retrieved from server
    rcpt_id: number;
    message_id: number;
    user_id: number;
    marked_as_read_on: Date;
}

@NgModule({
    declarations: [ManageReadReceiptComponent],
    exports: [ManageReadReceiptComponent],
    imports: [CommonModule],
})
export class ManageReadReceiptModule {}
