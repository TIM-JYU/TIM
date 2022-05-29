import {Component, Input} from "@angular/core";

@Component({
    selector: "tim-video-link",
    template: `
        <a [href]="doclink"
           rel="noopener"
           [target]="target">
            <i class="glyphicon glyphicon-book"></i>&ngsp;<span [innerHTML]="doctext | purify"></span></a>
    `,
})
export class VideoLinkComponent {
    @Input() doclink!: string;
    @Input() doctext?: string | null;
    @Input() target!: string;
}
