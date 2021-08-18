import {Component, Input, OnInit, ViewEncapsulation} from "@angular/core";
import {getViewName} from "../../util/utils";
import {
    ArchivedMessageStateService,
    SiblingMessages,
} from "./archived-message-state.service";

@Component({
    selector: "tim-archive-footer",
    template: `
        <div class="message-footer-jump-links">
            <a *ngIf="siblings.prev" href="/{{route}}/{{siblings.prev.path}}" i18n>
                Previous: {{siblings.prev.title}}
            </a>
            <a *ngIf="siblings.next" href="/{{route}}/{{siblings.next.path}}" i18n>
                Next: {{siblings.next.title}}
            </a>
        </div>
    `,
    styleUrls: ["./tim-archive-footer.component.scss"],
    encapsulation: ViewEncapsulation.None,
})
export class TimArchiveFooterComponent implements OnInit {
    @Input() message?: string;
    route: string;
    siblings: SiblingMessages = {next: undefined, prev: undefined};

    constructor(private state: ArchivedMessageStateService) {
        this.route = getViewName();
    }

    async ngOnInit() {
        if (this.message) {
            this.state.initState(this.message);
        }
        this.siblings = await this.state.getRelatedMessages();
    }
}
