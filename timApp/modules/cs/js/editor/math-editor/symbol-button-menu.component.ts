import {
    Component,
    ContentChild,
    EventEmitter,
    Input,
    Output,
} from "@angular/core";
import type {ITemplateButton} from "../../csPlugin";
import {FileSelectManagerComponent} from "../../util/file-select";

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

/**
 * Button menu can be either
 * closed (buttons not visible)
 * open (buttons visible)
 * expanded (all buttons visible)
 */
enum ButtonMenuState {
    Closed = 0,
    Open = 1,
    Expanded = 2,
}

@Component({
    selector: "symbol-button-menu",
    template: `
        <div class="symbol-menu-container">
            <div class="symbol-button-menu" [class.symbol-button-menu-open]="isOpen()">
                <div class="buttons-container math display" [hidden]="!isOpen()">
                    <button class="symbol-button" title="{{item.expl}}" *ngFor="let item of templateButtons;" (mousedown)="addFormula(item.data, item.data, true)"
                     >{{item.text}}</button>
                </div>
            </div>
            
            <div class="button-menu-container" [class.button-menu-container-no-left]="formulaEditorOpen">
                <div class="button-menu-left" [hidden]="formulaEditorOpen">
                    <button class="timButton formula-button" (click)="toggleFormulaEditor()" i18n
                            title="Ctrl+e">Formula
                    </button>

                    <div class="file-select-button">
                        <ng-content></ng-content>                        
                    </div>                        
                </div>

                <div class="button-menu-right">
                    <button *ngIf="!isOpen(); else elseBlock" type="button" class="btn btn-default" (click)="openMenu()">
                      <span class="glyphicon glyphicon-menu-down"></span>
                    </button>
                   <ng-template #elseBlock>
                    <button type="button" class="btn btn-default" (click)="closeMenu()">
                      <span class="glyphicon glyphicon-menu-up"></span>
                    </button>                       
                   </ng-template>
                    
                    <a href="https://tim.jyu.fi/view/kurssit/tie/proj/2023/timath/dokumentit/ohjeet/kayttoohjeet"
                       target="_blank">
                        <span class="glyphicon glyphicon-question-sign help-icon" title="Instructions"
                              i18n-title></span>
                    </a>                        
                </div>
            </div>
        </div>

    `,
    styleUrls: ["./symbol-button-menu.component.scss"],
})
export class SymbolButtonMenuComponent {
    buttonMenuState: ButtonMenuState = ButtonMenuState.Open;

    @Output() setFormula = new EventEmitter<FormulaEvent>();

    @Input() templateButtons: ITemplateButton[] = [];

    @Output() toggle = new EventEmitter<void>();

    @Input() formulaEditorOpen: boolean = false;
    @ContentChild(FileSelectManagerComponent)
    fileSelector!: FileSelectManagerComponent;

    addFormula(formula: string, command: string, useWrite: boolean = false) {
        this.setFormula.emit({
            text: formula,
            command: command,
            useWrite: useWrite,
        });
    }

    toggleFormulaEditor() {
        this.toggle.emit();
    }

    openMenu() {
        this.buttonMenuState = ButtonMenuState.Open;
    }

    closeMenu() {
        this.buttonMenuState = ButtonMenuState.Closed;
    }

    isOpen() {
        return this.buttonMenuState === ButtonMenuState.Open;
    }
}
