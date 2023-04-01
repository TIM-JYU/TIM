import {Component, EventEmitter, Output} from "@angular/core";
import {FORMULAS} from "./latex-commands";

/**
 * Text is command in text format \frac{}{}
 * command is what mathquill accepts \frac
 * useWrite is needed to write some commands like \overline{\text{i}}
 */
export type FormulaEvent = {
    text: string;
    command: string;
    useWrite: boolean;
};

@Component({
    selector: "symbol-button-menu",
    template: `
        <div class="symbol-button-menu">
            <button class="timButton" (click)="setButtonsVisible(buttonsVisible)">{{showFormulasText}}</button>
            <div class="buttons-container math display" [hidden]="!buttonsVisible" >
                <button class="symbol-button" *ngFor="let item of formulaArray;" (mousedown)="addFormula(item.text, item.command, item.useWrite)"
                 >{{item.display}}</button>
            </div>
        </div>
    `,
    styleUrls: ["./formula-editor.component.scss"],
})
export class SymbolButtonMenuComponent {
    formulaArray = FORMULAS;
    buttonsVisible = true;
    showFormulasText = "Show formulas";

    @Output() setFormula = new EventEmitter<FormulaEvent>();

    addFormula(formula: string, command: string, useWrite: boolean = false) {
        this.setFormula.emit({
            text: formula,
            command: command,
            useWrite: useWrite,
        });
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
