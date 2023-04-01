import {Component, EventEmitter, Input, Output} from "@angular/core";
import formulas from "./latex-commands";

@Component({
    selector: "symbol-button-menu",
    template: `
        <div class="symbol-button-menu">
            <div class="templatebutton-container math display">
                <button class="symbol-button" *ngFor="let item of templateButtons" (mousedown)="addFormula(item.data)">
                    {{item.text}}
                </button>
            </div>
        </div>
    `,
    styleUrls: ["./formula-editor.component.scss"],
})
export class SymbolButtonMenuComponent {
    @Input() templateButtons: any;

    @Output() setFormula = new EventEmitter<string>();

    /**
     * Adds formula indicated by symbol of the button to LaTeX-editors both fields.
     * @param formula Formula to be added to the fields.
     */
    addFormula(formula: string) {
        this.setFormula.emit(formula);
    }
}
