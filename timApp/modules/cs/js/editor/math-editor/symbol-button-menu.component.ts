import {Component, EventEmitter, Input, Output} from "@angular/core";

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
            <div class="buttons-container math display">
                <button class="symbol-button" *ngFor="let item of templateButtons;" (mousedown)="addFormula(item.data, item.data, true)"
                 >{{item.text}}</button>
            </div>
        </div>
    `,
    styleUrls: ["./formula-editor.component.scss"],
})
export class SymbolButtonMenuComponent {
    @Output() setFormula = new EventEmitter<FormulaEvent>();

    @Input() templateButtons: any;

    addFormula(formula: string, command: string, useWrite: boolean = false) {
        this.setFormula.emit({
            text: formula,
            command: command,
            useWrite: useWrite,
        });
    }
}
