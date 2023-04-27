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
 */
export type FormulaEvent = {
    text: string;
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
 * Wrapper for clientX,clientY coordinates.
 */
type Point = {
    x: number;
    y: number;
};

/**
 * Filters buttons by type.
 */
@Pipe({name: "symbols"})
export class SymbolsPipe implements PipeTransform {
    /**
     * Filters buttons by type.
     * @param buttons array of buttons to filter
     * @param type ItemplateButton.type defines which buttons are returned
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
                                title="{{item.expl}}" 
                                (mouseup)="addFormula($event, item.data)"
                                (touchend)="addFormula($event, item.data)"
                                (mousedown)="handleMouseDown($event)"
                                (touchstart)="handleMouseDown($event)"
                         >{{item.text}}</button>
                        <button 
                                class="symbol-button" 
                                *ngFor="let item of templateButtons | symbols:'q'"
                                title="{{item.expl}}" 
                                (mouseup)="addFormula($event, item.data)" 
                                (touchend)="addFormula($event, item.data)"
                                (mousedown)="handleMouseDown($event)"
                                (touchstart)="handleMouseDown($event)"
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
                            (mouseup)="addFormula($event, item.data)"
                            (touchend)="addFormula($event, item.data)"
                            (mousedown)="handleMouseDown($event)"
                            (touchstart)="handleMouseDown($event)"
                     >{{item.text}}</button>
                </div>
            </div>
           
        </div>

    `,
    styleUrls: ["./symbol-button-menu.component.scss"],
})
export class SymbolButtonMenuComponent implements AfterViewInit {
    buttonMenuState: ButtonMenuState = ButtonMenuState.Closed;

    pressStartPos?: Point;

    @ContentChild(FileSelectManagerComponent)
    fileSelector?: FileSelectManagerComponent;

    @Input() formulaEditorOpen: boolean = false;
    @Input() templateButtons!: ITemplateButton[];

    @Output() setFormula = new EventEmitter<FormulaEvent>();

    @Output() toggle = new EventEmitter<void>();

    constructor(private el: ElementRef<HTMLElement>) {}

    /**
     * emits setFormula event with given formula
     * @param event event fired
     * @param formula text of formula
     */
    addFormula(event: MouseEvent | TouchEvent, formula: string) {
        event.preventDefault();
        // don't register button press if it was a drag event
        if (this.isDragEvent(event)) {
            return;
        }
        this.setFormula.emit({
            text: formula,
        });
    }

    /**
     * Detects drag based on distance between points.
     * @param event event fired
     */
    isDragEvent(event: MouseEvent | TouchEvent): boolean {
        if (!this.pressStartPos) {
            return false;
        }
        const {x: x1, y: y1} = this.pressStartPos;

        let x2 = x1;
        let y2 = y1;
        if (event instanceof MouseEvent) {
            x2 = event.clientX;
            y2 = event.clientY;
        } else {
            if (event.changedTouches.length === 0) {
                return false;
            }
            x2 = event.changedTouches[0].clientX;
            y2 = event.changedTouches[0].clientY;
        }

        // distance between points in pixels
        const d = Math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2);
        // 10 pixels should be more than natural sway
        return d > 10;
    }

    /**
     * Record the start position when button press starts
     * @param event event fired
     */
    handleMouseDown(event: MouseEvent | TouchEvent) {
        if (event instanceof MouseEvent) {
            this.pressStartPos = {
                x: event.clientX,
                y: event.clientY,
            };
        } else {
            this.pressStartPos = {
                x: event.touches[0].clientX,
                y: event.touches[0].clientY,
            };
        }
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

    /**
     * Render LaTeX in button texts.
     */
    ngAfterViewInit(): void {
        void ParCompiler.processAllMath($(this.el.nativeElement));
    }
}
