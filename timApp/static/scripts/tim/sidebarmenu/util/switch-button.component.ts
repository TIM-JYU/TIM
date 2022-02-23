import {Component} from "@angular/core";

@Component({
    selector: "tim-switch-button",
    template: `
        <button class="timButton btn-block" #buttonMarginLeft
                (click)="toggleMarginLeft(buttonMarginLeft)">
                <ng-container i18n *ngIf="isOn === true">Reset to the center</ng-container>
                <ng-container i18n *ngIf="isOn === false">Align to the left</ng-container>
        </button>
    `,
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
