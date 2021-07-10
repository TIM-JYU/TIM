import {
    Component,
    ComponentFactoryResolver,
    NgModule,
    OnInit,
    ViewChild,
    ViewContainerRef,
} from "@angular/core";
import {CommonModule} from "@angular/common";
import {itemglobals} from "tim/util/globals";
import {to2} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

@Component({
    selector: "tim-message-view",
    template: `
        <tim-alert *ngIf="messageError" i18n>
           Could not load messages for the page: {{messageError}}
        </tim-alert>
        <ng-container #messageContainer></ng-container>
    `,
    styleUrls: ["tim-message-view.component.scss"],
})
export class TimMessageViewComponent implements OnInit {
    @ViewChild("messageContainer", {read: ViewContainerRef, static: true})
    container!: ViewContainerRef;
    messageError?: string;

    ngOnInit(): void {
        const current_item = itemglobals().curr_item;
        if (current_item) {
            const itemId = current_item.id;
            void this.loadMessages(itemId);
        }
    }

    constructor(
        private http: HttpClient,
        private cfr: ComponentFactoryResolver
    ) {}

    timMessages?: TimMessageData[];

    async loadMessages(itemId: number) {
        this.container.clear();
        const messages = await to2(
            // get messages shown on current page
            this.http
                .get<TimMessageData[]>(`/timMessage/get/${itemId}`)
                .toPromise()
        );

        if (messages.ok) {
            const component = (await import("./tim-message.component"))
                .TimMessageComponent;
            const factory = this.cfr.resolveComponentFactory(component);

            for (const message of messages.result) {
                const res = this.container.createComponent(factory);
                res.instance.message = message;
            }
        } else {
            this.messageError = messages.result.error.error;
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
    imports: [CommonModule, TimUtilityModule],
})
export class TimMessageViewModule {}
