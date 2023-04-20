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
import {DEFAULT_SYMBOL_BUTTONS} from "./default-symbol-buttons";

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
 * Filters buttons by type.
 */
@Pipe({name: "symbols"})
export class SymbolsPipe implements PipeTransform {
    /**
     * Filters buttons by type.
     * @param buttons array of buttons to filter
     * @param type ItemplateButton.type defines which buttos are returned
     *             if undefined or math, math and ones with no type are returned
     */
    transform(buttons: ITemplateButton[], type?: "q" | "s" | "t") {
        if (!type) {
            return buttons.filter(
                (button) => !button.type || button.type === "math"
            );
        }
        if (buttons.length === 0) {
            return DEFAULT_SYMBOL_BUTTONS.filter(
                (button) => button.type === type
            );
        }
        return buttons.filter((button) => button.type === type);
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
                                *ngFor="let item of templateButtons | symbols:'t'"
                                title="{{item.expl}}" (mouseup)="addFormula(item.data, item.data, true)" 
                         >{{item.text}}</button>
                        <button 
                                class="symbol-button" 
                                *ngFor="let item of templateButtons | symbols:'q'"
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
                            *ngFor="let item of templateButtons | symbols:'s'" 
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
