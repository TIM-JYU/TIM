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
        const current_item = itemglobals().curr_item;
        // TODO: Fetch messages for root when there are global messages available
        if (current_item?.path) {
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
        this.containerNormal.clear();
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
                    const res = vcr.createComponent(factory);
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
    sender: string;
    doc_id: number;
    par_id: string;
    can_mark_as_read: boolean;
    can_reply: boolean;
    display_type: DisplayType;
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
