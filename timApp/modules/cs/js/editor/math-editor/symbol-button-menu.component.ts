import {Component} from "@angular/core";
import formulas from "./latex-commands";

@Component({
    selector: "symbol-button-menu",
    template: `
        <div class="symbol-button-menu">
            <button class="timButton" (click)="setButtonsVisible(buttonsVisible)">{{showFormulasText}}</button>
            <div class="buttons-container math display" [hidden]="!buttonsVisible" >
                <button class="symbolButton" *ngFor="let item of formulaArray;" 
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
