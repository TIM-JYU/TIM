import {Component, ChangeDetectionStrategy} from "@angular/core";

@Component({
    selector: "tim-close-button",
    template: `<i title="Close"
                  i18n-title
                  class="glyphicon glyphicon-remove"></i>
    `,
    styleUrls: ["./close-button.component.scss"],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class CloseButtonComponent {}
