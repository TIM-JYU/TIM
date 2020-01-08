import {Component, Input} from "@angular/core";
import {AlertSeverity} from "tim/ui/formErrorMessage";

@Component({
    selector: "tim-alert",
    template: `
        <div class="alert alert-{{ severity }}">
            <span class="glyphicon glyphicon-{{getIcon()}}"></span>&nbsp;
            <ng-content></ng-content>
        </div>
    `,
})
export class TimAlertComponent {
    @Input() severity: AlertSeverity = "danger";

    getIcon() {
        switch (this.severity) {
            case "danger":
            case "warning":
                return "exclamation-sign";
            case "success":
                return "ok";
            default:
                return "exclamation-sign";
        }
    }
}
