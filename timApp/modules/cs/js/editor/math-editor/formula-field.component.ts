/**
 * Formula Editor field for inputting LaTeX math
 *
 * @author Daniel Juola
 * @author Juha Reinikainen
 * @license MIT
 * @date 24.3.2023
 */

import type {AfterViewInit, OnDestroy} from "@angular/core";
import {
    Component,
    ElementRef,
    EventEmitter,
    Input,
    Output,
    ViewChild,
} from "@angular/core";
import type {
    IMathQuill,
    MathFieldMethods,
    MathQuillConfig,
} from "vendor/mathquill/mathquill";
import {FormulaPropertyList, FormulaType} from "./formula-types";

/**
 * Specifies which field the typed text should go in.
 */
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

/**
 * wrapper for line add event info.
 */
export type LineAdd = {
    id: number;
    addBelow: boolean;
};

const DEFAULT_ERROR_MESSAGE = $localize`Error in LaTeX code`;
const ERROR_MESSAGE_DELAY = 1000;

@Component({
    selector: "cs-formula-field",
    template: `
        <div class="formula-field" [class.active-field]="isActive">
            <div class="input-container" #inputContainer>
                <span
                        class="visual-input"
                        [class.active-visual-input]="isActive"
                        #visualInput
                        (keyup.tab)="handleFocus()"
                        (keyup.shift.tab)="handleFocus()"
                        (click)="handleFocus()"
                        (focus)="handleFocus()"
                        (touchstart)="handleTouch()"
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
                <span class="placeholder-text" [hidden]="error || !isMathFieldEmpty || !isActive" i18n>Write math</span>
                <span class="render-error" [hidden]="!error || !isActive">{{getErrorMessage()}}</span>
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
export class FormulaFieldComponent implements AfterViewInit, OnDestroy {
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
    error = false;
    latestCorrectInput = "";
    errorTimeoutID = -1;

    @ViewChild("inputContainer") inputContainer!: ElementRef<HTMLElement>;

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

    @Input() formulaType!: FormulaType;

    /**
     * Tell whether the field is active.
     */
    @Input()
    get isActive(): boolean {
        return this.active;
    }

    /**
     * Sets active status of this field
     * also sets focus to visual field.
     * @param value true if should be active false otherwise
     */
    set isActive(value: boolean) {
        this.active = value;
        if (value) {
            if (this.mathField) {
                this.mathField.focus();
            }
        }
    }

    /**
     * Tell whether mathfield has content.
     */
    get isMathFieldEmpty(): boolean {
        if (this.mathField) {
            return this.mathField.latex().length === 0;
        }
        return false;
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
     * Writes changes in visual field to LaTeX field if visual field
     * is the one being typed.
     */
    editHandler() {
        if (this.activeEditor === ActiveEditorType.Visual) {
            if (this.undoRedo != this.undoRedoCodes.NOCHANGE) {
                return;
            }
            // get visual latex code and properties for formula type
            let latexCode = this.mathField.latex();
            const properties = FormulaPropertyList.find(
                (formulaType) => formulaType.type === this.formulaType
            );
            // replace characters if needed
            if (properties) {
                if (properties.typeReplace) {
                    if (latexCode.includes(properties.typeReplace[0])) {
                        // remove typed character
                        this.mathField.keystroke("Backspace");
                        // insert replacement character and update latex code
                        this.mathField.write(properties.typeReplace[1]);
                        latexCode = this.mathField.latex();
                    }
                }
                if (properties.writeReplace) {
                    // update latex code for latex field
                    try {
                        const regex = new RegExp(properties.writeReplace[0]);
                        latexCode = latexCode.replace(
                            regex,
                            properties.writeReplace[1]
                        );
                    } catch (err) {
                        console.log(
                            "invalid RegExp: " + properties.writeReplace[0]
                        );
                        console.log(err);
                    }
                }
            }
            // update latex field and formula editor
            this.latexInput = latexCode;
            this.edited.emit({
                latex: this.latexInput,
                id: this.id,
            });
            this.checkErrors();
            this.updateTextareaRows();
            this.updateUndoRedoStack();
        }
    }

    /**
     * Adds MathQuill CSS to the page if it's not loaded in yet.
     * @private
     */
    private registerMathQuillCss() {
        const mathQuillLink = document.head.querySelector(
            'link[data-type="mathquill"]'
        );
        if (mathQuillLink) {
            return;
        }

        // If not found, create it
        const link = document.createElement("link");
        link.rel = "stylesheet";
        link.href = "/static/scripts/vendor/mathquill/mathquill.css";
        link.dataset.type = "mathquill";

        document.head.appendChild(link);
    }

    /**
     * Loads Mathquill and initializes field.
     */
    async ngAfterViewInit() {
        this.registerMathQuillCss();
        const mq = (await import("vendor/mathquill/mathquill")).default;
        this.MQ = mq.getInterface(2);
        const elem = this.visualInput.nativeElement;

        const config: MathQuillConfig = {
            spaceBehavesLikeTab: false,
            handlers: {
                edit: () => this.editHandler(),
                enter: () => this.enterHandler(),
                deleteOutOf: () => {
                    this.handleDeleteOutOf();
                },
                downOutOf: () => {
                    this.downArrow.emit(this.id);
                },
                upOutOf: () => {
                    this.upArrow.emit(this.id);
                },
            },
        };
        this.mathField = this.MQ.MathField(elem, config);
        this.mathField.latex(this.initialValue);
        this.mathField.focus();
    }

    /**
     * Cancel error check.
     */
    ngOnDestroy(): void {
        if (this.errorTimeoutID !== -1) {
            window.clearTimeout(this.errorTimeoutID);
            this.errorTimeoutID = -1;
        }
    }

    /**
     * Enter pressed while not inside environment
     * like \cases.
     */
    enterHandler() {
        this.enterPressed();
    }

    /**
     * Backspace pressed while field is empty.
     */
    handleDeleteOutOf() {
        this.backspacePressed();
    }

    /**
     * Sets LaTeX textarea as active field.
     */
    handleLatexFocus() {
        this.activeEditor = ActiveEditorType.Latex;
    }

    /**
     * Sets visual field as active field.
     */
    handleVisualFocus() {
        this.activeEditor = ActiveEditorType.Visual;
        if (this.error) {
            this.mathField.latex(this.latestCorrectInput);
        }
    }

    /**
     * Write changes in latex field to visual field if latex field
     * is the one being typed in.
     */
    handleLatexInput() {
        if (this.activeEditor === ActiveEditorType.Latex) {
            let visualInput = this.latexInput;
            // escape characters if needed
            const properties = FormulaPropertyList.find(
                (formulaType) => formulaType.type === this.formulaType
            );
            if (properties) {
                if (properties.editReplace) {
                    try {
                        const regex = new RegExp(properties.editReplace[0]);
                        visualInput = visualInput.replace(
                            regex,
                            properties.editReplace[1]
                        );
                    } catch (err) {
                        console.log(
                            "invalid RegExp: " + properties.editReplace[0]
                        );
                        console.log(err);
                    }
                }
            }
            // update visual field and formula editor
            this.mathField.latex(visualInput);
            this.edited.emit({
                latex: this.latexInput,
                id: this.id,
            });
            this.checkErrors();
            this.updateTextareaRows();
            this.updateUndoRedoStack();
        }
    }

    /**
     * Emit enter event.
     */
    enterPressed() {
        this.enter.emit({id: this.id, addBelow: true});
    }

    /**
     * Emit add line event.
     * @param addBelow if true then add field below else after this field
     */
    handleAddLine(addBelow: boolean) {
        this.add.emit({id: this.id, addBelow: addBelow});
    }

    /**
     * Emit remove line event.
     */
    handleRemoveLine() {
        this.delete.emit(this.id);
    }

    /**
     * Emit backspace event.
     */
    backspacePressed() {
        if (this.latexInput.length === 0) {
            this.backspace.emit(this.id);
        }
    }

    /**
     * Emit focus event and sets active editor to visual field.
     */
    handleFocus() {
        this.activeEditor = ActiveEditorType.Visual;
        if (this.error) {
            this.mathField.latex(this.latestCorrectInput);
        }
        this.focus.emit({
            latex: this.latexInput,
            id: this.id,
        });
    }

    /**
     * Puts focus to visual field
     * in order to show virtual keyboard.
     */
    handleTouch() {
        this.mathField.focus();
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
        window.setTimeout(
            () => (this.undoRedo = this.undoRedoCodes.NOCHANGE),
            2
        );
    }

    /**
     * Makes visual input height match latexInputElement on small screens
     * so error message fits inside element.
     */
    updateVisualInputHeight() {
        if (!this.latexInputElement) {
            return;
        }
        const dir = window
            .getComputedStyle(this.inputContainer.nativeElement)
            .getPropertyValue("flex-direction");
        const textareaHeight = window
            .getComputedStyle(this.latexInputElement.nativeElement)
            .getPropertyValue("height");
        if (dir === "column") {
            this.visualInput.nativeElement.style.minHeight = textareaHeight;
        }
    }

    /**
     * Check if MathQuill produces an error from LaTeX input.
     */
    checkErrors() {
        const hasErrors =
            this.mathField.latex().length === 0 && this.latexInput.length > 0;
        if (!hasErrors) {
            this.error = false;
            this.latestCorrectInput = this.mathField.latex();
        }
        if (this.errorTimeoutID !== -1) {
            window.clearTimeout(this.errorTimeoutID);
        }
        this.errorTimeoutID = window.setTimeout(() => {
            // if this field isn't active anymore latexInput wouldn't exist so
            // no point doing anything.
            if (!this.isActive) {
                return;
            }
            this.error =
                this.mathField.latex().length === 0 &&
                this.latexInput.length > 0;
            if (!this.error) {
                this.latestCorrectInput = this.mathField.latex();
            }
        }, ERROR_MESSAGE_DELAY);

        this.updateVisualInputHeight();
    }

    /**
     * Error message to show when LaTeX is incorrect.
     */
    getErrorMessage() {
        if (this.latestCorrectInput.length === 0) {
            return DEFAULT_ERROR_MESSAGE;
        }
        return this.latestCorrectInput;
    }
}
