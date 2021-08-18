import {Component, Input, OnInit, ViewEncapsulation} from "@angular/core";
import moment from "moment";
import {getViewName} from "tim/util/utils";
import {
    ArchivedMessageStateService,
    MessageData,
    SiblingMessages,
} from "./archived-message-state.service";

@Component({
    selector: "tim-archive-header",
    template: `
        <div class="message-header-jump-links">
            <div>
                <a *ngIf="siblings.prev" class="btn btn-default" href="/{{route}}/{{siblings.prev.path}}"
                   [tooltip]="siblings.prev.title">
                    <i class="glyphicon glyphicon-chevron-left"></i>
                </a>
            </div>
            <span>
                <a *ngIf="siblings.next" class="btn btn-default" href="/{{route}}/{{siblings.next.path}}"
                   [tooltip]="siblings.next.title">
                    <i class="glyphicon glyphicon-chevron-right"></i>
                </a>
            </span>
        </div>
        <div class="message-header-info">
            <span i18n>Subject:</span>
            <div class="message-subject">{{subject}}</div>
            <span i18n>Sender:</span>
            <div class="message-sender">
                <span>{{messageData.sender.name}} <a class="label label-primary"
                                                     href="mailto:{{messageData.sender.email}}">{{messageData.sender.email}}</a></span>
            </div>
            <span i18n>Recipients:</span>
            <div class="message-recipients">
            <span *ngFor="let recipient of messageData.recipients; last as isLast">
                {{recipient.name}} <a class="label label-primary"
                                      href="mailto:{{recipient.email}}">{{recipient.email}}</a>
                <span class="message-recipient-separator" *ngIf="!isLast"></span>
            </span>
            </div>
            <span i18n>Date:</span>
            <div class="message-date">
                {{messageDate}}
            </div>
        </div>
    `,
    styleUrls: ["./tim-archive-header.component.scss"],
    encapsulation: ViewEncapsulation.None,
})
export class TimArchiveHeaderComponent implements OnInit {
    @Input() message?: string;
    subject!: string;
    messageData!: MessageData;
    messageDate!: string;
    route: string;
    siblings: SiblingMessages = {next: undefined, prev: undefined};

    constructor(private state: ArchivedMessageStateService) {
        this.route = getViewName();
    }

    async ngOnInit() {
        if (this.message) {
            this.state.initState(this.message);
        }
        this.subject = this.state.messageSubject;
        this.messageData = this.state.messageData!;
        this.messageDate = moment(this.messageData.date).format(
            "dddd, MMMM Do YYYY, h:mm:ss a"
        );
        this.siblings = await this.state.getRelatedMessages();
    }
}
