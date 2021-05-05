import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {itemglobals} from "tim/util/globals";
import {to2} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {TimMessageModule} from "./tim-message.component";

@Component({
    selector: "tim-message-view",
    template: `
        <ng-container *ngFor="let timMessage of timMessages">
            <tim-message [message]=timMessage></tim-message>
        </ng-container>
    `,
    styleUrls: ["tim-message-view.component.scss"],
})
export class TimMessageViewComponent implements OnInit {
    ngOnInit(): void {
        const itemId = itemglobals().curr_item.id;
        void this.loadValues(itemId);
    }

    constructor(private http: HttpClient) {}

    timMessages: [TimMessageData] | undefined;

    async loadValues(itemId: number) {
        const messages = await to2(
            // get messages shown on current page
            this.http
                .get<[TimMessageData]>(`/timMessage/get/${itemId}`)
                .toPromise()
        );

        if (messages.ok) {
            this.timMessages = messages.result;
            console.log(this.timMessages);
        } else {
            console.error(messages.result.error.error);
        }
    }
}

export interface TimMessageData {
    // Information about the message retrieved from server
    id: number;
    sender: string;
    doc_id: number;
    par_id: string;
    can_mark_as_read: boolean;
    can_reply: boolean;
    display_type: number;
    message_body: string;
    message_subject: string;
    recipients: [string];
}

@NgModule({
    declarations: [TimMessageViewComponent],
    exports: [TimMessageViewComponent],
    imports: [CommonModule, TimMessageModule],
})
export class TimMessageViewModule {}
