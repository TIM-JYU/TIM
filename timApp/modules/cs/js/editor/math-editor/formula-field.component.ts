/**
 * Formula Editor field for inputting LaTeX math
 * @author Juha Reinikainen
 * @licence MIT
 * @date 24.3.2023
 */

import type {AfterViewInit} from "@angular/core";
import {
    ChangeDetectorRef,
    Component,
    ElementRef,
    EventEmitter,
    Input,
    Output,
    ViewChild,
} from "@angular/core";
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
 * of the active field and its identification number.
 */
export type Edit = {
    id: number;
    latex: string;
};

export type LineAdd = {
    id: number;
    addBelow: boolean;
};

@Component({
    selector: "cs-formula-field",
    template: `
        <div class="formula-field" [class.active-field]="isActive">
            <div class="input-container">
                <span
                        class="visual-input"
                        [class.active-visual-input]="isActive"
                        #visualInput
                        (keyup.tab)="handleFocus()"
                        (keyup.shift.tab)="handleFocus()"
                        (click)="handleFocus()"
                        (focus)="handleFocus()"
                        (keyup)="handleVisualFocus()"
                        (keydown.control.z)="handleUndo()"
                        (keydown.control.y)="handleRedo()">
                </span>

                <textarea name="math-editor-output" #latexInputElement cols="30"
                          *ngIf="isActive"
                          rows="{{rows}}"
                          (click)="handleLatexFocus()"
                          (keyup)="handleLatexInput()"
                          [(ngModel)]="latexInput"
                          placeholder="Write LaTeX" i18n-placeholder
                          class="formula-area"
                          (focus)="handleLatexFocus()"
                          (keydown.control.z)="handleUndo()"
                          (keydown.control.y)="handleRedo()">
                </textarea>
                <span class="render-error" *ngIf="hasError" i18n>Error in LaTeX code</span>
            </div>

            <div class="formula-field-buttons btn-group btn-group-xs" *ngIf="isActive">
                <button type="button" class="btn btn-default" (click)="handleAddLine(false)" title="Add line above"
                        i18n-title>
                    <span class="glyphicon glyphicon-plus"></span>
                </button>

                <button type="button" class="btn btn-default" (click)="handleRemoveLine()" title="Remove current line"
                        i18n-title>
                    <span class="glyphicon glyphicon-remove"></span>
                </button>
                
                <button type="button" class="btn btn-default" (click)="handleAddLine(true)" title="Add line below"
                        i18n-title>
                    <span class="glyphicon glyphicon-plus"></span>
                </button>
            </div>
        </div>
    `,
    styleUrls: ["./formula-field.component.scss"],
})
export class FormulaFieldComponent implements AfterViewInit {
    latexInput = "";
    rows: number = 2;
    MQ!: IMathQuill;
    undoStack: string[] = [];
    redoStack: string[] = [];
    undoRedoCodes = {
        UNDO: -1,
        REDO: 1,
        NOCHANGE: 0,
    };
    undoRedo = 0;

    mathField!: MathFieldMethods;

    activeEditor: ActiveEditorType = ActiveEditorType.Visual;
    private active = false;
    private error = false;

    @ViewChild("latexInputElement")
    latexInputElement!: ElementRef<HTMLTextAreaElement>;

    @ViewChild("visualInput") visualInput!: ElementRef<HTMLElement>;

    @Output() edited = new EventEmitter<Edit>();
    @Output() enter = new EventEmitter<LineAdd>();
    @Output() backspace = new EventEmitter<number>();
    @Output() focus = new EventEmitter<Edit>();
    @Output() downArrow = new EventEmitter<number>();
    @Output() upArrow = new EventEmitter<number>();
    @Output() delete = new EventEmitter<number>();
    @Output() add = new EventEmitter<LineAdd>();

    @Input() id!: number;

    @Input() initialValue!: string;

    constructor(private cd: ChangeDetectorRef) {}

    @Input()
    get isActive(): boolean {
        return this.active;
    }

    set isActive(value: boolean) {
        this.active = value;
        if (value) {
            if (this.mathField) {
                this.mathField.focus();
            }
        }
    }

    @Input()
    get hasError(): boolean {
        return this.error;
    }

    /**
     * Sets textarea to have as many rows that are necessary to display
     * its content.
     */
    updateTextareaRows() {
        // adjust rows in textarea to match how many are needed
        this.rows = this.latexInput.split("\n").length;
    }

    /**
     * write changes in visual field to LaTeX field if visual field
     * is the one being typed.
     * @param field reference to mathField.
     */
    editHandler(field: MathFieldMethods) {
        if (this.activeEditor === ActiveEditorType.Visual) {
            if (this.undoRedo != this.undoRedoCodes.NOCHANGE) {
                return;
            }
            this.latexInput = field.latex();
            this.edited.emit({
                latex: this.latexInput,
                id: this.id,
            });
            setTimeout(() => this.checkError(), 2);
            this.updateTextareaRows();
            this.updateUndoRedoStack();
        }
    }

    async ngAfterViewInit() {
        const mq = (await import("vendor/mathquill/mathquill")).default;
        this.MQ = mq.getInterface(2);
        const elem = this.visualInput.nativeElement;

        const config: MathQuillConfig = {
            spaceBehavesLikeTab: false,
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
        this.mathField.focus();
    }

    /**
     * Enter pressed while not inside environment
     * like \cases.
     * @param field current field.
     */
    enterHandler(field: MathFieldMethods) {
        this.enterPressed();
    }

    /**
     * Backspace pressed while field is empty.
     * @param direction indicates which direction movement happens.
     * @param field current field.
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
     * Write changes in latex field to visual field if latex field
     * is the one being typed in.
     */
    handleLatexInput() {
        if (this.activeEditor === ActiveEditorType.Latex) {
            this.mathField.latex(this.latexInput);
            this.edited.emit({
                latex: this.latexInput,
                id: this.id,
            });
            setTimeout(() => this.checkError(), 2);
            this.updateTextareaRows();
            this.updateUndoRedoStack();
        }
    }

    enterPressed() {
        this.enter.emit({id: this.id, addBelow: true});
    }

    handleAddLine(addBelow: boolean) {
        this.add.emit({id: this.id, addBelow: addBelow});
    }

    handleRemoveLine() {
        this.delete.emit(this.id);
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

    /**
     * Undo latest change in formula editor.
     */
    handleUndo() {
        if (this.undoStack.length === 1) {
            return;
        }
        this.undoRedo = this.undoRedoCodes.UNDO;
        const temp = this.undoStack.pop();
        if (temp != undefined) {
            this.redoStack.push(temp);
        }
        this.mathField.latex(this.undoStack[this.undoStack.length - 1]);
        this.latexInput = this.undoStack[this.undoStack.length - 1];
        this.updateUndoRedoStack();
    }

    /**
     * Revert last undo in the formula editor.
     */
    handleRedo() {
        if (this.redoStack.length === 0) {
            return;
        }
        this.undoRedo = this.undoRedoCodes.REDO;
        this.mathField.latex(this.redoStack[this.redoStack.length - 1]);
        const temp = this.redoStack.pop();
        if (temp != undefined) {
            this.latexInput = temp;
        }
        this.updateUndoRedoStack();
    }

    /**
     * Save previous change, so it can be restored with undo.
     */
    updateUndoRedoStack() {
        const latex = this.latexInput;
        if (
            this.undoRedo >= this.undoRedoCodes.NOCHANGE &&
            this.undoStack[this.undoStack.length - 1] != latex
        ) {
            if (this.undoRedo === this.undoRedoCodes.NOCHANGE) {
                this.redoStack = [];
            }
            this.undoStack.push(latex);
        }
        setTimeout(() => (this.undoRedo = this.undoRedoCodes.NOCHANGE), 2);
    }

    /**
     * Check if MathQuill produces an error from LaTeX input
     */
    checkError() {
        const fieldValue = this.mathField.latex();
        if (fieldValue.length === 0 && this.latexInput.length > 0) {
            this.error = true;
        } else {
            this.error = false;
        }
    }
}