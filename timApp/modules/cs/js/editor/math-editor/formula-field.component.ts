/**
 * Formula Editor field for inputting LaTeX math
 * @author Juha Reinikainen
 * @licence MIT
 * @date 24.3.2023
 */

import {
    Component,
    ElementRef,
    EventEmitter,
    Input,
    Output,
    ViewChild,
} from "@angular/core";
import {FormControl} from "@angular/forms";
import type {IMathQuill} from "vendor/mathquill/mathquill";
import type {
    MathFieldMethods,
    MathQuillConfig,
} from "vendor/mathquill/mathquill";

export enum ActiveEditorType {
    Visual = "visual",
    Latex = "latex",
}

/**
 * Edit event wrapper that contains the full input
 * of the active field and its identification number
 */
export type Edit = {
    id: number;
    latex: string;
};

@Component({
    selector: "cs-formula-field",
    template: `
        <div class="formula-container" [class.active-field]="isActive">
            <span 
                    class="visual-input"
                    [class.active-visual-input]="isActive"
                    #visualInput 
                    (keyup.tab)="handleFocus()"
                    (keyup.shift.tab)="handleFocus()"
                    (click)="handleFocus()"
                    (focus)="handleFocus()"
                    (keyup)="handleVisualFocus()">
            </span>

            <textarea name="math-editor-output" #latexInputElement cols="30" 
                      *ngIf="isActive" 
                      rows="{{rows}}"
                      (click)="handleLatexFocus()"
                      (keyup)="handleLatexInput()"
                      [(ngModel)]="latexInput"
                      placeholder="Write LaTeX" i18n-placeholder
                      class="formula-area"
                      (focus)="handleLatexFocus()">
            </textarea>                        
        </div>

    `,
    styleUrls: ["./formula-field.component.scss"],
})
export class FormulaFieldComponent {
    latexInput = "";
    @ViewChild("latexInputElement")
    latexInputElement!: ElementRef<HTMLTextAreaElement>;

    @ViewChild("visualInput") visualInput!: ElementRef<HTMLElement>;

    MQ!: IMathQuill;

    mathField!: MathFieldMethods;

    activeEditor: ActiveEditorType = ActiveEditorType.Visual;

    @Input() id!: number;

    @Input() initialValue!: string;
    @Input()
    get isActive(): boolean {
        return this.active;
    }
    set isActive(value: boolean) {
        this.active = value;
        if (value) {
            setTimeout(() => {
                this.mathField.focus();
            }, 50);
        }
    }
    private active = false;

    @Output() edited = new EventEmitter<Edit>();
    @Output() enter = new EventEmitter<number>();
    @Output() backspace = new EventEmitter<number>();
    @Output() focus = new EventEmitter<Edit>();
    @Output() downArrow = new EventEmitter<number>();
    @Output() upArrow = new EventEmitter<number>();

    rows: number = 2;

    /**
     * sets textarea to have as many rows that are necessary to display
     * its content
     */
    updateTextareaRows() {
        // adjust rows in textarea to match how many are needed
        this.rows = this.latexInput.split("\n").length;
    }

    /**
     * write changes in visual field to latex field if visual field
     * is the one being typed
     * @param field reference to mathField
     */
    editHandler(field: MathFieldMethods) {
        if (this.activeEditor === ActiveEditorType.Visual) {
            const latex = field.latex();
            this.latexInput = latex;
            this.edited.emit({
                latex: this.latexInput,
                id: this.id,
            });
            this.updateTextareaRows();
        }
    }

    async ngAfterViewInit() {
        const mq = (await import("vendor/mathquill/mathquill")).default;
        this.MQ = mq.getInterface(2);
        const elem = this.visualInput.nativeElement;

        const config: MathQuillConfig = {
            spaceBehavesLikeTab: true,
            handlers: {
                edit: (field: MathFieldMethods) => this.editHandler(field),
                enter: (field: MathFieldMethods) => this.enterHandler(field),
                deleteOutOf: (direction, field: MathFieldMethods) => {
                    this.handleDeleteOutOf(direction, field);
                },
                downOutOf: (field: MathFieldMethods) => {
                    this.downArrow.emit(this.id);
                },
                upOutOf: (field: MathFieldMethods) => {
                    this.upArrow.emit(this.id);
                },
            },
        };
        this.mathField = this.MQ.MathField(elem, config);
        this.mathField.latex(this.initialValue);
    }

    /**
     * Enter pressed while not inside environment
     * like \cases
     * @param field current field
     */
    enterHandler(field: MathFieldMethods) {
        this.enterPressed();
    }

    /**
     * Backspace pressed while field is empty
     * @param direction indicates which direction movement happens
     * @param field current field
     */
    handleDeleteOutOf(direction: number, field: MathFieldMethods) {
        this.backspacePressed();
    }

    handleLatexFocus() {
        this.activeEditor = ActiveEditorType.Latex;
    }

    handleVisualFocus() {
        this.activeEditor = ActiveEditorType.Visual;
    }

    /**
     * write changes in latex field to visual field if latex field
     * is the one being typed in.
     */
    handleLatexInput() {
        if (this.activeEditor === ActiveEditorType.Latex) {
            this.mathField.latex(this.latexInput);
            this.edited.emit({
                latex: this.latexInput,
                id: this.id,
            });
            this.updateTextareaRows();
        }
    }

    enterPressed() {
        this.enter.emit(this.id);
    }

    backspacePressed() {
        if (this.latexInput.length === 0) {
            this.backspace.emit(this.id);
        }
    }

    handleFocus() {
        this.activeEditor = ActiveEditorType.Visual;
        this.focus.emit({
            latex: this.latexInput,
            id: this.id,
        });
    }
}
