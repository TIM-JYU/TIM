import {Component} from "@angular/core";

@Component({
    selector: "tim-plugin-header",
    template: `
        <h4>
            <ng-content></ng-content>
        </h4>
    `,
    styleUrls: ["./plugin-header.component.scss"],
})
export class PluginHeaderComponent {}
