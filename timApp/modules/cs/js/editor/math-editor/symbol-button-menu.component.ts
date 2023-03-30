import {Component, EventEmitter, Input, Output} from "@angular/core";
import formulas from "./latex-commands";
import {ActiveEditorType} from "./formula-field.component";

/**
 * wrapper for pressed button text
 * Object wrapping is necessary to
 * make angular produce an event for each
 * button press.
 */
type ButtonState = {
    text: string;
};

@Component({
    selector: "symbol-button-menu",
    template: `
        <div class="symbol-button-menu">
            <button class="timButton" (click)="setButtonsVisible(buttonsVisible)">{{showFormulasText}}</button>
            <div class="buttons-container math display" [hidden]="!buttonsVisible" >
                <button class="symbol-button" *ngFor="let item of formulaArray;" (mousedown)="addFormula(item.text)"
                 >{{item.display}}</button>
            </div>
        </div>
    `,
    styleUrls: ["./formula-editor.component.scss"],
})
export class SymbolButtonMenuComponent {
    formulaArray = formulas;
    buttonsVisible = false;
    showFormulasText = "Show formulas";

    @Output() setFormula = new EventEmitter<string>();

    addFormula(formula: string) {
        this.setFormula.emit(formula);
    }

    /**
     * Changes buttons to visible or not visible
     * @param isVisible are buttons currently visible
     */
    setButtonsVisible(isVisible: boolean) {
        this.buttonsVisible = !isVisible;
        if (this.buttonsVisible) {
            this.showFormulasText = "Hide formulas";
        } else {
            this.showFormulasText = "Show formulas";
        }
    }
}
