import {Component, Input} from "@angular/core";
import {AlertSeverity} from "tim/ui/formErrorMessage";

@Component({
    selector: "tim-alert",
    template: `
        <div *ngIf="open" class="alert alert-{{ severity }}">
            <span class="glyphicon glyphicon-{{getIcon()}}"></span>&nbsp;
            <ng-content></ng-content>
            <i *ngIf="closeable" role="button" class="glyphicon glyphicon-remove" (click)="open = false"></i>
        </div>
    `,
    styleUrls: ["tim-alert.component.scss"],
})
export class TimAlertComponent {
    @Input() severity: AlertSeverity = "danger";
    @Input() closeable: boolean = false;
    open = true;

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
