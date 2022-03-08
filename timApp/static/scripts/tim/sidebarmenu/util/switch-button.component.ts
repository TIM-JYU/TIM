import {Component} from "@angular/core";
import {TimStorage} from "tim/util/utils";
import * as t from "io-ts";

@Component({
    selector: "tim-switch-button",
    template: `
        <!-- Button layout when the switch is ON -->
        <ng-container *ngIf="isOn === true">
            <button i18n-title title="Align to the center"
                    class="btn btn-default btn-sm pull-left
                    glyphicon glyphicon-align-center"
                    (click)="onValueChanged()">
            </button>
        </ng-container>
        
        <!-- Button layout when the switch is OFF -->
        <ng-container *ngIf="isOn === false">
            <button i18n-title title="Align to the left"
                    class="btn btn-default btn-sm pull-left 
                    glyphicon glyphicon-align-left"
                    (click)="onValueChanged()">
            </button>
        </ng-container>
    `,
    styleUrls: ["./switch-button.component.scss"],
})
export class SwitchButtonComponent {
    public isOn = false;

    private currentStateStorage = new TimStorage(
        "switchButton_currentState",
        t.boolean
    );

    ngOnInit(): void {
        // Listen to changes in the local storage.
        window.addEventListener("storage", () => {
            this.isOn = this.currentStateStorage.get() ?? this.isOn;
            this.updateElements();
        });
        // Synchronize local state with the local storage.
        this.isOn = this.currentStateStorage.get() ?? this.isOn;
        this.updateElements();
    }

    onValueChanged(): void {
        this.isOn = !this.isOn;
        // Publish changes to the local storage.
        this.currentStateStorage.set(this.isOn);
        this.updateElements();
    }

    private updateElements(): void {
        const elements = document.querySelectorAll(".row *");
        for (const element of elements) {
            // Make sure that all elements are in the current state.
            if (this.isOn !== element.classList.toggle("toggle-margin-left")) {
                element.classList.toggle("toggle-margin-left");
            }
        }
    }
}
