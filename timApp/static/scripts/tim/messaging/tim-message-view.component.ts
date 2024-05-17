import type {OnInit} from "@angular/core";
import {
    Component,
    NgModule,
    ViewChild,
    ViewContainerRef,
    ChangeDetectorRef,
} from "@angular/core";
import {itemglobals, someglobals} from "tim/util/globals";
import {toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {TimMessageModule} from "tim/messaging/tim-message.component";

@Component({
    selector: "tim-message-view",
    template: `
        <tim-alert *ngIf="messageError" i18n>
            Could not load messages for the page: {{messageError}}
        </tim-alert>
        <div class="sticky" *ngIf="globalUserMessage">
            <tim-message [message]="globalUserMessageObject" [isGlobal]="true"></tim-message>
        </div>
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

    globalUserMessage?: string;
    globalUserMessageObject: TimMessageData = {
        can_mark_as_read: false,
        display_type: DisplayType.STICKY,
        can_reply: false,
        can_hide: false,
        doc_path: "",
        created: "",
        id: 0,
        message_body: "",
        message_subject: $localize`System message`,
    };

    ngOnInit(): void {
        void this.loadMessages(
            "curr_item" in someglobals()
                ? itemglobals().curr_item.id
                : undefined
        );

        vctrlInstance?.listen("docViewInfoUpdate", (info) => {
            const prev = this.globalUserMessage;
            this.globalUserMessage = info.global_message;
            if (this.globalUserMessage) {
                this.globalUserMessageObject.message_body =
                    this.globalUserMessage;
            }
            if (prev !== this.globalUserMessage) {
                this.cdr.detectChanges();
            }
        });
    }

    constructor(private http: HttpClient, private cdr: ChangeDetectorRef) {}

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
    created?: string;
    sender?: string;
    doc_path: string;
    can_mark_as_read: boolean;
    can_hide?: boolean;
    can_reply: boolean;
    display_type: DisplayType;
    message_body: string;
    message_subject: string;
}

@NgModule({
    declarations: [TimMessageViewComponent],
    exports: [TimMessageViewComponent],
    imports: [CommonModule, TimUtilityModule, TimMessageModule],
})
export class TimMessageViewModule {}
