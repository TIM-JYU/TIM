import {Component, EventEmitter, Input, Output} from "@angular/core";
import {AlertSeverity} from "tim/ui/formErrorMessage";

@Component({
    selector: "tim-alert",
    template: `
        <div *ngIf="open" class="alert alert-{{ severity }}">
            <span class="glyphicon glyphicon-{{getIcon()}}"></span>&nbsp;
            <div class="content"><ng-content></ng-content></div>
            <i *ngIf="closeable" role="button" class="glyphicon glyphicon-remove" (click)="onClose()"></i>
        </div>
    `,
    styleUrls: ["tim-alert.component.scss"],
})
export class TimAlertComponent {
    @Input() severity: AlertSeverity = "danger";
    @Input() closeable: boolean = false;
    @Output() closing: EventEmitter<void> = new EventEmitter<void>();
    open = true;

    onClose() {
        this.closing.emit();
        this.open = false;
    }

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
