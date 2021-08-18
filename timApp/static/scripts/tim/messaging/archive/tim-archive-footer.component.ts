import {Component, Input, OnInit, ViewEncapsulation} from "@angular/core";
import {ArchivedMessageStateService} from "./archived-message-state.service";

@Component({
    selector: "app-tim-archive-footer",
    template: `
        <p>
            tim-archive-footer works!
        </p>
    `,
    styleUrls: ["./tim-archive-footer.component.scss"],
    encapsulation: ViewEncapsulation.None,
})
export class TimArchiveFooterComponent implements OnInit {
    @Input() message?: string;

    constructor(private state: ArchivedMessageStateService) {}

    async ngOnInit() {
        if (this.message) {
            this.state.initState(this.message);
        }
        console.log(this.state.messageData);
        const siblings = await this.state.getRelatedMessages();
        console.log(siblings);
    }
}
