import {Component, Input} from "@angular/core";
import {PluginMarkupErrors} from "tim/plugin/util";

@Component({
    selector: "tim-markup-error",
    template: `
        <div class="pluginError">
            sePlugin has invalid values for these markup fields:
            <ul>
                <li *ngFor="let e of data">
                    {{ e.name }}: expected {{ e.type }}
                </li>
            </ul>
        </div>
    `,
})
export class MarkupErrorComponent {
    @Input() data!: PluginMarkupErrors;
}
