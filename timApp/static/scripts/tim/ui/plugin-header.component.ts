import {Component, ChangeDetectionStrategy} from "@angular/core";

@Component({
    selector: "tim-plugin-header",
    template: `
        <h4>
            <ng-content></ng-content>
        </h4>
    `,
    styleUrls: ["./plugin-header.component.scss"],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class PluginHeaderComponent {}
