import type {OnInit} from "@angular/core";
import {Component, NgModule, ViewChild, ViewContainerRef} from "@angular/core";
import {itemglobals, someglobals} from "tim/util/globals";
import {toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";

@Component({
    selector: "tim-message-view",
    template: `
        <tim-alert *ngIf="messageError" i18n>
            Could not load messages for the page: {{messageError}}
        </tim-alert>
        <div class="sticky">
            <ng-container #messageContainerSticky></ng-container>
        </div>
        <div>
            <ng-container #messageContainerNormal></ng-container>
        </div>

    `,
    styleUrls: ["tim-message-view.component.scss"],
})
export class TimMessageViewComponent implements OnInit {
    @ViewChild("messageContainerNormal", {read: ViewContainerRef, static: true})
    containerNormal!: ViewContainerRef;
    @ViewChild("messageContainerSticky", {read: ViewContainerRef, static: true})
    containerSticky!: ViewContainerRef;
    messageError?: string;

    ngOnInit(): void {
        void this.loadMessages(
            "curr_item" in someglobals()
                ? itemglobals().curr_item.id
                : undefined
        );
    }

    constructor(private http: HttpClient) {}

    timMessages?: TimMessageData[];

    async loadMessages(itemId?: number) {
        this.containerNormal.clear();
        const url =
            itemId && itemId > 0
                ? `/timMessage/get/${itemId}`
                : "/timMessage/get";
        const messages = await toPromise(this.http.get<TimMessageData[]>(url));

        if (messages.ok) {
            const component = (await import("./tim-message.component"))
                .TimMessageComponent;

            const normal = messages.result.filter(
                (m) => m.display_type == DisplayType.TOP_OF_PAGE
            );
            const sticky = messages.result.filter(
                (m) => m.display_type == DisplayType.STICKY
            );

            const addMessages = (
                msgs: TimMessageData[],
                vcr: ViewContainerRef
            ) => {
                for (const message of msgs) {
                    const res = vcr.createComponent(component);
                    res.instance.message = message;
                }
            };

            addMessages(normal, this.containerNormal);
            addMessages(sticky, this.containerSticky);
        } else {
            this.messageError = messages.result.error.error;
        }
    }
}

export enum DisplayType {
    TOP_OF_PAGE = 1,
    STICKY = 2,
}

export interface TimMessageData {
    // Information about the message retrieved from server
    id: number;
    created: string;
    sender?: string;
    doc_path: string;
    can_mark_as_read: boolean;
    can_reply: boolean;
    display_type: DisplayType;
    message_body: string;
    message_subject: string;
}

@NgModule({
    declarations: [TimMessageViewComponent],
    exports: [TimMessageViewComponent],
    imports: [CommonModule, TimUtilityModule],
})
export class TimMessageViewModule {}
