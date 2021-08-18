import {Component, Input, OnInit, ViewEncapsulation} from "@angular/core";
import {ArchivedMessageStateService} from "./archived-message-state.service";

@Component({
    selector: "tim-archive-header",
    template: `
        <p>Header!</p>
    `,
    styleUrls: ["./tim-archive-header.component.scss"],
    encapsulation: ViewEncapsulation.None,
})
export class TimArchiveHeaderComponent implements OnInit {
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
