import {Component, Input} from "@angular/core";
import {PluginMarkupErrors} from "tim/plugin/util";

@Component({
    selector: "tim-markup-error",
    template: `
        <div class="pluginError">
          Plugin has invalid values for these markup fields:
          <ul>
            @for (e of data; track e) {
              <li>
                {{ e.name }}: expected {{ e.type }}
              </li>
            }
          </ul>
        </div>
        `,
    standalone: false,
})
export class MarkupErrorComponent {
    @Input() data!: PluginMarkupErrors;
}
