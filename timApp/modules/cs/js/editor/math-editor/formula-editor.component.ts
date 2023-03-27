/* eslint no-underscore-dangle: ["error", { "allow": ["_visible"] }] */
/**
 * Formula Editor for inputting LaTeX math
 * @author Juha Reinikainen
 * @licence MIT
 * @date 28.2.2023
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
import {showConfirm} from "tim/ui/showConfirmDialog";
import type {
    IMathQuill,
    MathFieldMethods,
    MathQuillConfig,
} from "vendor/mathquill/mathquill";
import {IEditor} from "../editor";
import formulas from "./latex-commands";

/**
 * Field which has the focus
 */
enum ActiveEditorType {
    Visual = "visual",
    Latex = "latex",
}
enum FormulaType {
    Multi = "multi",
    Inline = "inline",
}

type FormulaTuple = [FormulaType, number, number];

/**
 * OldContent is split into three at cursor location if insert operation is supported
 * if not then just put content to before and after and editing can be empty
 */
type OldContent = {
    before: string;
    editing: string;
    after: string;
};

@Component({
    selector: "cs-formula-editor",
    template: `
        <div [hidden]="!visible" class="formula-editor">
            <div tabindex="0" class="formula-editor-dialog" #formulaEditorDialog>
                <button class="timButton" (click)="setButtonsVisible(buttonsVisible)">{{showFormulasText}}</button>
                <div class="buttons-container" [hidden]="!buttonsVisible" >
                    <button class="symbolButton" *ngFor="let item of formulaArray;" (click)="addFormula(item.text)" 
                     ><img src="{{item.svg}}" alt="{{item.text}}"/></button>
                </div>
                <div class="formula-container">
                    <span class="visual-input" #visualInput></span>

                    <textarea name="math-editor-output" #latexInput cols="30" 
                              rows="{{rows}}"
                              (click)="handleLatexFocus()"
                              (keyup)="handleLatexInput()"
                              [formControl]="latexInputControl"
                              placeholder="Write LaTeX" i18n-placeholder
                              class="formula-area">
                    </textarea>
                </div>

                <div class="formula-button-container">
                    <div class="formula-buttons">
                        <button class="timButton" (click)="handleFormulaOk()" title="Ctrl+s">Ok</button>
                        <button class="timButton" (click)="handleFormulaCancel()" i18n title="Esc">Cancel</button>        
                    </div>

                    <label class="font-weight-normal">
                        <input type="checkbox" [formControl]="isMultilineFormulaControl">
                        <ng-container i18n>Multiline</ng-container>
                    </label>
                </div>
            </div>
        </div>

    `,
    styleUrls: ["./formula-editor.component.scss"],
})
export class FormulaEditorComponent {
    latexInputControl = new FormControl("");
    @ViewChild("latexInput") latexInput!: ElementRef<HTMLTextAreaElement>;

    @ViewChild("visualInput") visualInput!: ElementRef<HTMLElement>;

    @ViewChild("formulaEditorDialog")
    formulaEditorDialog!: ElementRef<HTMLDivElement>;
    MQ!: IMathQuill;

    mathField!: MathFieldMethods;

    activeEditor: ActiveEditorType = ActiveEditorType.Visual;

    oldContent: OldContent = {before: "", editing: "", after: ""};

    @Output() okEvent = new EventEmitter<void>();
    @Output() cancelEvent = new EventEmitter<void>();

    isMultilineFormulaControl = new FormControl(true);

    @Input() editor!: IEditor;

    @Input()
    get visible(): boolean {
        return this._visible;
    }
    set visible(isVis: boolean) {
        this._visible = isVis;
        // became visible so save what was in editor
        if (isVis) {
            this.parseOldContent(this.editor.content);
            const isMulti = this.getInitialMultilineSetting();
            this.isMultilineFormulaControl.setValue(isMulti);
            this.parseEditedFormula();

            // set focus to visual field, timeout is needed for proper timing
            // maybe could find better way
            setTimeout(() => {
                this.mathField.focus();
            }, 0);
        }
    }
    private _visible: boolean = false;

    @Input()
    get currentSymbol(): string {
        return this.buttonSymbol;
    }
    set currentSymbol(value: string) {
        this.buttonSymbol = value;
        if (this.mathField) {
            this.addFormula(value);
        }
    }
    private buttonSymbol: string = "";

    // Array containing default LaTeX-commands for formula buttons
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

    rows: number = 2;

    /**
     * sets textarea to have as many rows that are necessary to display
     * its content
     */
    updateTextareaRows() {
        const latex = this.latexInputControl.value;
        if (latex === null) {
            return;
        }
        // adjust rows in textarea to match how many are needed
        const nrows = latex.split("\n").length;
        this.rows = nrows;
    }

    useExistingParenthesis = false;
    existingParenthesis = ["", ""];
    startCounter = 1;

    constructor() {}

    /**
     * Find the line where cursor is in editor
     */
    getCurrentLine() {
        const cursorPos = this.getCursorLocation();
        const text = this.editor.content;
        let startI = cursorPos;
        let endI = cursorPos;
        if (text[startI] === "\n") {
            startI--;
        }
        while (startI >= 0 && text[startI] !== "\n") {
            startI--;
        }
        while (endI < text.length && text[endI] !== "\n") {
            endI++;
        }
        return text.slice(startI + 1, endI);
    }

    /**
     * Determine whether the user wants to create a multiline or an inline formula
     */
    getInitialMultilineSetting() {
        const currentLine = this.getCurrentLine();
        const isTextInLine = currentLine.trim().length > 0;
        // should be multiline if no real text in line
        return !isTextInLine;
    }

    /**
     * After oldContent is set cursor is between
     * oldContent parts
     */
    getCursorLocation() {
        return this.oldContent.before.length;
    }

    /**
     * Splits string into two parts if possible at cursor location
     * @param str
     */
    parseOldContent(str: string) {
        if (!this.editor.insert) {
            this.oldContent = {
                before: this.editor.content,
                editing: "",
                after: "",
            };
            return;
        }
        const cursorMarker = "│";

        // add cursor character to know where cursor is
        this.editor.insert(cursorMarker);
        // find its index
        const cursorI = this.editor.content.indexOf(cursorMarker);
        // also remove added cursor character from editor
        this.editor.content =
            this.editor.content.slice(0, cursorI) +
            this.editor.content.slice(cursorI + 1);

        this.oldContent = {
            before: this.editor.content.slice(0, cursorI),
            editing: "",
            after: this.editor.content.slice(cursorI),
        };
    }

    /**
     * General method to find $ and $$ syntax parenthesis from a string
     */
    findParenthesisFromString(text: string) {
        let currentIndex = 0;
        let stack = [-1, -1];
        const allParenthesis: FormulaTuple[] = [];

        // count all parenthesis from the beginning
        while (true) {
            // find next $ symbol
            currentIndex = text.indexOf("$", currentIndex);
            // stop if no $ symbol is found
            if (currentIndex < 0) {
                break;
            }
            // skip \$ symbols
            if (text.charAt(currentIndex - 1) === "\\") {
                currentIndex++;
                continue;
            }
            // multiline
            if (text.charAt(currentIndex + 1) === "$") {
                // opening does not exist
                if (stack[1] < 0) {
                    // if possible set opening to current
                    if (text.charAt(currentIndex + 2) !== " ") {
                        stack[1] = currentIndex;
                    }
                    // opening exists
                } else {
                    // if possible set closing to current
                    if (text.charAt(currentIndex - 1) !== " ") {
                        allParenthesis.push([
                            FormulaType.Multi,
                            stack[1],
                            currentIndex + 1,
                        ]);
                        // reset stack
                        stack = [-1, -1];
                    } else {
                        // if possible set opening to current
                        if (text.charAt(currentIndex + 2) !== " ") {
                            stack[1] = currentIndex;
                        }
                    }
                }
                currentIndex += 2;
                // inline
            } else {
                // opening does not exist
                if (stack[0] < 0) {
                    // if possible set opening to current
                    if (text.charAt(currentIndex + 1) !== " ") {
                        stack[0] = currentIndex;
                    }
                    // opening exists
                } else {
                    // if possible set closing to current
                    if (text.charAt(currentIndex - 1) !== " ") {
                        allParenthesis.push([
                            FormulaType.Inline,
                            stack[0],
                            currentIndex,
                        ]);
                        // reset stack
                        stack = [-1, -1];
                    } else {
                        // if possible set opening to current
                        if (text.charAt(currentIndex + 1) !== " ") {
                            stack[0] = currentIndex;
                        }
                    }
                }
                currentIndex++;
            }
        }

        return allParenthesis;
    }

    /**
     * Returns current formula's parenthesis
     * @param allParenthesis array of parethesis indexes and types
     * @param cursorIndex index to check if it is inside parenthesis
     */
    parseCurrentFormula(
        allParenthesis: FormulaTuple[],
        cursorIndex: number
    ): FormulaTuple {
        let leftIndex = -1;
        let rightIndex = -1;
        let isMultiLine = FormulaType.Inline;

        // check if cursor is inside any parenthesis
        for (const [formulaType, startI, endI] of allParenthesis) {
            // parenthesis is completely after cursor
            if (startI > cursorIndex) {
                break;
            }
            // parenthesis is completely before cursor
            if (endI < cursorIndex) {
                continue;
            }
            if (formulaType === FormulaType.Inline) {
                leftIndex = startI;
                rightIndex = endI;
                isMultiLine = FormulaType.Inline;
                break;
            }
            if (formulaType === FormulaType.Multi) {
                leftIndex = startI;
                rightIndex = endI;
                isMultiLine = FormulaType.Multi;
                break;
            }
        }
        return [isMultiLine, leftIndex, rightIndex];
    }

    /**
     * Parses current formula from editor content
     */
    parseEditedFormula() {
        // variables to keep track of editor content
        const text = this.editor.content;
        const allParenthesis = this.findParenthesisFromString(text);
        const currentFormula = this.parseCurrentFormula(
            allParenthesis,
            this.oldContent.before.length
        );

        const leftIndex = currentFormula[1];
        const rightIndex = currentFormula[2];

        // if inside formula, add it to formula editor for modifying
        // otherwise do nothing and keep adding a new formula
        if (leftIndex >= 0 && rightIndex >= 0) {
            this.useExistingParenthesis = true;
            // start editing a multiline formula
            if (currentFormula[0] === FormulaType.Multi) {
                // update old content
                this.oldContent.before = text.slice(0, leftIndex);
                this.oldContent.editing = text.slice(leftIndex, rightIndex + 1);
                this.oldContent.after = text.slice(rightIndex + 1);
                // find formula and its parenthesis
                let formula = this.oldContent.editing.slice(2, -2);
                let trimmed = formula.trimStart();
                let lenDiff = formula.length - trimmed.length;
                this.existingParenthesis[0] = "$$" + formula.slice(0, lenDiff);
                formula = trimmed;

                trimmed = formula.trimEnd();
                lenDiff = formula.length - trimmed.length;
                this.existingParenthesis[1] =
                    "" + formula.slice(formula.length - lenDiff) + "$$";
                formula = trimmed;
                // update formula editor values
                this.isMultilineFormulaControl.setValue(true);
                this.latexInputControl.setValue(formula);
            }
            // start editing an inline formula
            else {
                // update old content
                this.oldContent.before = text.slice(0, leftIndex);
                this.oldContent.editing = text.slice(leftIndex, rightIndex + 1);
                this.oldContent.after = text.slice(rightIndex + 1);
                this.existingParenthesis = ["$", "$"];
                // update formula editor values
                this.isMultilineFormulaControl.setValue(false);
                this.latexInputControl.setValue(
                    this.oldContent.editing.slice(1, -1)
                );
            }
            // update editor views to user
            setTimeout(() => {
                this.handleLatexFocus();
                this.handleLatexInput();
            }, 2);
        }
    }

    /**
     * Switches from existing parenthesis to user chosen
     * parenthesis when editing a formula
     */
    resetUsingParenthesis() {
        if (this.startCounter === 1) {
            this.startCounter--;
            return;
        }
        this.useExistingParenthesis = false;
    }

    /**
     * write changes in visual field to latex field if visual field
     * is the one being typed
     * @param field reference to mathField
     */
    editHandler(field: MathFieldMethods) {
        if (this.activeEditor === ActiveEditorType.Visual) {
            const latex = field.latex();
            this.latexInputControl.setValue(latex);
            this.updateTextareaRows();
            this.updateFormulaToEditor();
        }
    }

    enterHandler(field: MathFieldMethods) {
        // this.handleFormulaOk();
    }

    async loadMathQuill() {
        const elem = this.visualInput.nativeElement;
        elem.addEventListener("click", (_e: MouseEvent) => {
            this.activeEditor = ActiveEditorType.Visual;
        });
        const config: MathQuillConfig = {
            spaceBehavesLikeTab: true,
            handlers: {
                edit: (field: MathFieldMethods) => this.editHandler(field),
                enter: (field: MathFieldMethods) => this.enterHandler(field),
            },
        };
        const mq = (await import("vendor/mathquill/mathquill")).default;
        this.MQ = mq.getInterface(2);

        this.mathField = this.MQ.MathField(elem, config);
    }

    ngAfterViewInit() {
        void this.loadMathQuill();

        this.isMultilineFormulaControl.valueChanges.subscribe((value) => {
            if (this.useExistingParenthesis) {
                this.resetUsingParenthesis();
            }
            this.updateFormulaToEditor();
        });

        this.formulaEditorDialog.nativeElement.addEventListener(
            "keydown",
            (e) => {
                if (e.ctrlKey) {
                    if (e.key === "s") {
                        this.handleFormulaOk();
                        e.stopPropagation();
                        e.preventDefault();
                    }
                } else if (e.key === "Escape") {
                    e.stopPropagation();
                    e.preventDefault();
                    void this.handleFormulaCancel();
                }
            }
        );
    }

    handleLatexFocus() {
        this.activeEditor = ActiveEditorType.Latex;
    }

    /**
     * write changes in latex field to visual field if latex field
     * is the one being typed in.
     */
    handleLatexInput() {
        if (
            this.activeEditor === ActiveEditorType.Latex &&
            this.latexInputControl.value !== null
        ) {
            this.mathField.latex(this.latexInputControl.value);
            this.updateTextareaRows();
            this.updateFormulaToEditor();
        }
    }

    clearFormulaEditor() {
        this.mathField.latex("");
        this.latexInputControl.setValue("");
        this.isMultilineFormulaControl.setValue(true);
        this.startCounter = 1;
    }

    formatLatex(latex: string, isMultiline: boolean): string {
        const wrapSymbol = isMultiline ? "$$" : "$";
        if (latex.length === 0) {
            return "";
        }
        return isMultiline
            ? `${wrapSymbol}\n${latex}\n${wrapSymbol}`
            : `${wrapSymbol}${latex}${wrapSymbol}`;
    }

    /**
     * Updates editor text with current formula text
     */
    updateFormulaToEditor() {
        if (
            this.latexInputControl.value !== null &&
            this.isMultilineFormulaControl.value !== null
        ) {
            const isMultiline = this.isMultilineFormulaControl.value;
            const latex = isMultiline
                ? this.latexInputControl.value
                : this.mathField.latex();
            if (typeof latex === "string") {
                const formulaLatex = this.useExistingParenthesis
                    ? "" +
                      this.existingParenthesis[0] +
                      latex +
                      this.existingParenthesis[1]
                    : this.formatLatex(latex, isMultiline);

                this.editor.content =
                    this.oldContent.before +
                    formulaLatex +
                    this.oldContent.after;
            }
        }
    }

    handleFormulaOk() {
        this.updateFormulaToEditor();
        const finalContent = this.editor.content;
        this.okEvent.emit();
        this.clearFormulaEditor();
        // clearing fields triggers update to editor content
        // rewrite it
        this.editor.content = finalContent;
    }

    async handleFormulaCancel() {
        // content hasn't changed from what it was before opening formula editor
        // so cancel
        if (
            this.oldContent.before +
                this.oldContent.editing +
                this.oldContent.after ===
                this.editor.content ||
            (await showConfirm(
                $localize`Are you sure?`,
                $localize`This will clear the editor.`
            ))
        ) {
            this.editor.content =
                this.oldContent.before +
                this.oldContent.editing +
                this.oldContent.after;
            const finalContent = this.editor.content;
            this.cancelEvent.emit();
            this.clearFormulaEditor();
            // clearing fields triggers update to editor content
            // rewrite it
            this.editor.content = finalContent;
        }
    }

    /**
     * Adds formula to both fields in last known cursor position
     * @param formula LaTeX-formula to be added to fields
     */
    addFormula(formula: string) {
        if (this.activeEditor === ActiveEditorType.Latex) {
            const startPos = this.latexInput.nativeElement.selectionStart;
            const endPos = this.latexInput.nativeElement.selectionEnd;
            const oldValue = this.latexInput.nativeElement.value;
            const newValue =
                oldValue.substring(0, startPos) +
                formula +
                oldValue.substring(endPos, oldValue.length);
            this.latexInputControl.setValue(newValue);
            this.latexInput.nativeElement.selectionStart =
                startPos + newValue.length;
            this.latexInput.nativeElement.selectionEnd =
                endPos + newValue.length;
            this.handleLatexInput();
        } else {
            this.mathField.write(formula);
        }
    }
}
