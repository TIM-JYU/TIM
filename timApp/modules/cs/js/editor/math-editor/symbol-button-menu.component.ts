/**
 * Symbol button menu to add LaTeX by pressing buttons
 *
 * @author Jaakko Palm
 * @author Juha Reinikainen
 * @licence MIT
 * @date 30.3.2023
 */

import type {AfterViewInit, PipeTransform} from "@angular/core";
import {
    Component,
    ContentChild,
    ElementRef,
    EventEmitter,
    Input,
    Output,
    Pipe,
} from "@angular/core";
import {ParCompiler} from "tim/editor/parCompiler";
import type {ITemplateButton} from "../../csPlugin";
import {FileSelectManagerComponent} from "../../util/file-select";

/**
 * Text is command in text format \frac{}{}.
 * Command is what mathquill accepts \frac.
 * useWrite is needed to write some commands like \overline{\text{i}}.
 */
export type FormulaEvent = {
    text: string;
    command: string;
    useWrite: boolean;
};

/**
 * Button menu can be either:
 * closed (buttons not visible),
 * open (buttons visible),
 * expanded (all buttons visible).
 */
enum ButtonMenuState {
    Closed = 0,
    Open = 1,
    Expanded = 2,
}

/**
 * Filters symbols by type
 */
@Pipe({name: "symbols"})
export class SymbolsPipe implements PipeTransform {
    /**
     * @param buttons array of buttons to filter
     * @param type symbol, commonSymbol or non-symbol
     * Non-symbol returns ones that don't have type or math as a type,
     * timeditor returns ones that go to formulaeditor but are hidden when editor is open.
     */
    transform(
        buttons: ITemplateButton[],
        type: "commonSymbol" | "symbol" | "non-symbol" | "timSymbol"
    ) {
        switch (type) {
            case "non-symbol":
                return buttons.filter(
                    (button) => !button.isSymbol || button.isSymbol === "math"
                );
            case "commonSymbol":
                const commonSymbolButtons = buttons.filter(
                    (button) => button.isSymbol === "q"
                );
                if (commonSymbolButtons.length == 0) {
                    commonSymbolButtons.push(
                        {
                            text: "\\[ \\pi \\]",
                            data: "\\pi",
                            expl: "\\pi",
                            isSymbol: "q",
                        },
                        {
                            text: "\\[ \\sin \\]",
                            data: "\\sin",
                            expl: "\\sin",
                            isSymbol: "q",
                        },
                        {
                            text: "\\[ \\cos \\]",
                            data: "\\cos",
                            expl: "\\cos",
                            isSymbol: "q",
                        },
                        {
                            text: "\\[ \\tan \\]",
                            data: "\\tan",
                            expl: "\\tan",
                            isSymbol: "q",
                        }
                    );
                }
                return commonSymbolButtons;
            case "symbol":
                const symbolButtons = buttons.filter(
                    (button) => button.isSymbol === "s"
                );
                if (symbolButtons.length == 0) {
                    symbolButtons.push(
                        {
                            text: "\\[ \\overline{\\text{i}} \\]",
                            data: "\\overline{\\text{i}}",
                            expl: "\\overline{\\text{i}}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\overline{\\text{j}} \\]",
                            data: "\\overline{\\text{j}}",
                            expl: "\\overline{\\text{j}}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\overline{\\text{k}} \\]",
                            data: "\\overline{\\text{k}}",
                            expl: "\\overline{\\text{k}}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\frac{\\square}{\\square} \\]",
                            data: "\\frac{⁞}{}",
                            expl: "\\frac{}{}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ f(\\square) \\]",
                            data: "f(⁞)",
                            expl: "f()",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\sqrt{\\square} \\]",
                            data: "\\sqrt{⁞}",
                            expl: "\\sqrt{}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\square^n \\]",
                            data: "⁞^n",
                            expl: "^n",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\lim_{\\square} \\]",
                            data: "\\lim_{⁞}",
                            expl: "\\lim_{}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\sum_{\\square}^{\\square} \\]",
                            data: "\\sum_{⁞}^{}",
                            expl: "\\sum_{}^{}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\int_\\square^\\square \\]",
                            data: "\\int_{⁞}^{}",
                            expl: "\\int_{}^{}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\bigg/_{\\!\\!\\!\\!\\!{ \\square }}^{ \\square } \\]",
                            data: "\\bigg/_{\\!\\!\\!\\!\\!{}}^{}",
                            expl: "\\bigg/_{\\!\\!\\!\\!\\!{}}^{}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\overrightarrow{\\square} \\]",
                            data: "\\overrightarrow{⁞}",
                            expl: "\\overrightarrow{ }",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\overleftarrow{\\square} \\]",
                            data: "\\overleftarrow{⁞}",
                            expl: "\\overleftarrow{ }",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\binom{\\square}{\\square} \\]",
                            data: "\\binom{⁞}{}",
                            expl: "\\binom{}{}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\begin{cases}\\square&\\square\\\\\\square&\\square\\end{cases} \\]",
                            data: "\\begin{cases}⁞&\\\\&\\end{cases}",
                            expl: "\\begin{cases} &\\\\&\\end{cases}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\begin{matrix}\\square&\\square\\\\\\square&\\square\\end{matrix} \\]",
                            data: "\\begin{matrix}⁞&\\\\&\\end{matrix}",
                            expl: "\\begin{matrix} &\\\\&\\end{matrix}",
                            isSymbol: "s",
                        },
                        {
                            text: "\\[ \\begin{array}{l|l}\\square&\\square\\\\\\hline\\square&\\square\\end{array} \\]",
                            data: "\\begin{array}{l|l}⁞&\\\\\\hline&\\end{array}",
                            expl: "\\begin{array}{l|l}&\\\\\\hline&\\end{array}",
                            isSymbol: "s",
                        }
                    );
                }
                return symbolButtons;
            case "timSymbol":
                return buttons.filter((button) => button.isSymbol === "t");
            default:
                throw new Error("No button type defined for pipe");
        }
    }
}

@Component({
    selector: "symbol-button-menu",
    template: `
        <div class="symbol-menu-container" [class.symbol-menu-container-open]="isOpen()">
            <div class="button-menu-container">
                <div class="button-menu-left">
                    <div [hidden]="formulaEditorOpen" class="formula-controls">
                        <button class="timButton formula-button" (click)="toggleFormulaEditor()" i18n
                                title="Ctrl+e">Formula
                        </button>
    
                        <div class="file-select-button">
                            <ng-content></ng-content>                        
                        </div>                        
                    </div>
                    
                    <div class="common-symbol-buttons math display" [class.common-symbol-buttons-small]="!formulaEditorOpen">
                        <button 
                                [hidden]="formulaEditorOpen"
                                class="symbol-button" 
                                *ngFor="let item of templateButtons | symbols:'timSymbol'"
                                title="{{item.expl}}" (mouseup)="addFormula(item.data, item.data, true)" 
                         >{{item.text}}</button>
                        <button 
                                class="symbol-button" 
                                *ngFor="let item of templateButtons | symbols:'commonSymbol'"
                                title="{{item.expl}}" (mouseup)="addFormula(item.data, item.data, true)" 
                         >{{item.text}}</button>
                    </div>
                </div>
                
                <div class="button-menu-right">

                    <button *ngIf="!isOpen(); else elseBlock" type="button" class="btn btn-default" (click)="openMenu()"
                    title="Show more symbols" i18n-title>
                      <span class="glyphicon glyphicon-menu-down"></span>
                    </button>
                   <ng-template #elseBlock>
                    <button type="button" class="btn btn-default" (click)="closeMenu()" title="Show less symbols" i18n-title>
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
           
            <div class="symbol-button-menu" [class.symbol-button-menu-open]="isOpen()">
                <div class="buttons-container math display" [hidden]="!isOpen()">
                    <button class="symbol-button" 
                            title="{{item.expl}}" 
                            *ngFor="let item of templateButtons | symbols:'symbol'" 
                            (mouseup)="addFormula(item.data, item.data, true)"
                     >{{item.text}}</button>
                </div>
            </div>
           
        </div>

    `,
    styleUrls: ["./symbol-button-menu.component.scss"],
})
export class SymbolButtonMenuComponent implements AfterViewInit {
    buttonMenuState: ButtonMenuState = ButtonMenuState.Closed;

    @ContentChild(FileSelectManagerComponent)
    fileSelector?: FileSelectManagerComponent;

    @Input() formulaEditorOpen: boolean = false;
    @Input() templateButtons!: ITemplateButton[];

    @Output() setFormula = new EventEmitter<FormulaEvent>();

    @Output() toggle = new EventEmitter<void>();

    constructor(public el: ElementRef<HTMLElement>) {}

    addFormula(formula: string, command: string, useWrite: boolean = false) {
        this.setFormula.emit({
            text: formula,
            command: command,
            useWrite: useWrite,
        });
    }

    /**
     * Tells to open formula editor.
     */
    toggleFormulaEditor() {
        this.toggle.emit();
    }

    /**
     * Shows buttons.
     */
    openMenu() {
        this.buttonMenuState = ButtonMenuState.Open;
    }

    /**
     * Hides buttons.
     */
    closeMenu() {
        this.buttonMenuState = ButtonMenuState.Closed;
    }

    /**
     * Tells whether buttons should be visible.
     */
    isOpen() {
        return this.buttonMenuState === ButtonMenuState.Open;
    }

    ngAfterViewInit(): void {
        // render latex in button texts
        void ParCompiler.processAllMath($(this.el.nativeElement));
    }
}
