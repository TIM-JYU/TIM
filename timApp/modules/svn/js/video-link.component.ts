import {Component, Input, ChangeDetectionStrategy} from "@angular/core";

@Component({
    selector: "tim-video-link",
    template: `
        <a [href]="doclink"
           rel="noopener"
           [target]="target">
            <i class="glyphicon glyphicon-book"></i>&ngsp;<span [innerHTML]="doctext | purify"></span></a>
    `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class VideoLinkComponent {
    @Input() doclink!: string;
    @Input() doctext?: string | null;
    @Input() target!: string;
}
