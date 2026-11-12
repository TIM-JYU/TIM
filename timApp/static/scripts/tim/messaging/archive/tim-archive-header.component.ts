import type {OnInit} from "@angular/core";
import {Component, Input, ViewEncapsulation} from "@angular/core";
import {getViewName} from "tim/util/utils";
import type {
    MessageData,
    SiblingMessages,
} from "tim/messaging/archive/archived-message-state.service";
import {ArchivedMessageStateService} from "tim/messaging/archive/archived-message-state.service";

@Component({
    selector: "tim-archive-header",
    template: `
        <div class="message-header-jump-links">
          <div>
            @if (siblings.prev) {
              <a class="btn btn-default" href="/{{route}}/{{siblings.prev.path}}"
                [tooltip]="siblings.prev.title">
                <i class="glyphicon glyphicon-chevron-left"></i>
              </a>
            }
          </div>
          <span>
            @if (siblings.next) {
              <a class="btn btn-default" href="/{{route}}/{{siblings.next.path}}"
                [tooltip]="siblings.next.title">
                <i class="glyphicon glyphicon-chevron-right"></i>
              </a>
            }
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
            @for (recipient of messageData.recipients; track recipient; let isLast = $last) {
              <span>
                {{recipient.name}} <a class="label label-primary"
              href="mailto:{{recipient.email}}">{{recipient.email}}</a>
              @if (!isLast) {
                <span class="message-recipient-separator"></span>
              }
            </span>
          }
        </div>
        <span i18n>Date:</span>
        <div class="message-date">
          {{messageDate}}
        </div>
        </div>
        `,
    styleUrls: ["./tim-archive-header.component.scss"],
    encapsulation: ViewEncapsulation.None,
    standalone: false,
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
        this.messageDate = this.messageData.date
            .local()
            .format("dddd, MMMM Do YYYY, h:mm:ss a");
        this.siblings = await this.state.getRelatedMessages();
    }
}
