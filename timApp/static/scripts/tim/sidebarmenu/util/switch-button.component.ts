import {Component} from "@angular/core";

@Component({
    selector: "tim-switch-button",
    template: `
        <!-- Button layout when the switch is ON -->
        <ng-container *ngIf="isOn === true">
            <button #buttonMarginLeft
                    i18n-title title="Aligned on the left"
                    class="btn btn-default btn-sm pull-left
                    glyphicon glyphicon-object-align-left"
                    (click)="toggleMarginLeft(buttonMarginLeft)">
            </button>
        </ng-container>
        
        <!-- Button layout when the switch is OFF -->
        <ng-container *ngIf="isOn === false">
            <button #buttonMarginLeft
                    i18n-title title="Aligned on the center"
                    class="btn btn-default btn-sm pull-left 
                    glyphicon glyphicon-object-align-vertical"
                    (click)="toggleMarginLeft(buttonMarginLeft)">
            </button>
        </ng-container>
    `,
    styleUrls: ["./switch-button.component.scss"],
})
export class SwitchButtonComponent {
    public isOn = false;

    toggleMarginLeft(button: HTMLButtonElement): void {
        // Switch between ON and OFF.
        this.isOn = !this.isOn;

        const elements = document.querySelectorAll(".row *");
        for (const element of elements) {
            // Make sure that all elements are in the same state.
            if (this.isOn !== element.classList.toggle("toggle-margin-left")) {
                element.classList.toggle("toggle-margin-left");
            }
        }
    }
}
