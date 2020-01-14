import {Component, Input} from "@angular/core";
import {PluginMarkupErrors} from "tim/plugin/util";

@Component({
    selector: "tim-plugin-frame[markupError]",
    template: `
        <div class="root no-popup-menu">
            <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <ng-content select="[header]"></ng-content>
            <div class="body">
                <ng-content select="[stem]"></ng-content>
                <ng-content select="[body]"></ng-content>
                <ng-content select="[footer]"></ng-content>
            </div>
        </div>
    `,
    styleUrls: ["./plugin-frame.component.scss"],
})
export class PluginFrameComponent {
    @Input() markupError?: PluginMarkupErrors;
}
