/**
 * Formula Editor for inputting LaTeX math
 * @author Juha Reinikainen
 * @licence MIT
 * @date 28.2.2023
 */

import type {AfterViewInit} from "@angular/core";
import {
    Component,
    ElementRef,
    EventEmitter,
    Input,
    Output,
    QueryList,
    ViewChild,
    ViewChildren,
    ChangeDetectorRef,
} from "@angular/core";
import {FormControl} from "@angular/forms";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {IEditor} from "../editor";
import formulas from "./latex-commands";

enum FormulaType {
    Multi = "multi",
    Inline = "inline",
}

type FormulaTuple = [FormulaType, number, number];
import type {Edit} from "./formula-field.component";
import {ActiveEditorType} from "./formula-field.component";
import {FormulaFieldComponent} from "./formula-field.component";

/**
 * OldContent is split into three at cursor location if insert operation is supported
 * if not then just put content to before and after and editing can be empty
 */
type OldContent = {
    before: string;
    editing: string;
    after: string;
};

type FieldType = {
    latex: string;
};

/**
 * wrapper for pressed button text
 * Object wrapping is necessary to
 * make angular produce an event for each
 * button press.
 */
type ButtonState = {
    text: string;
};

type StringPair = [string, string];

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

                <div class="fields">
                    <div *ngFor="let field of fields; let i=index;" class="field">
                        <cs-formula-field 
                            [initialValue]="field.latex" 
                            (edited)="handleEdited($event)"
                            (enter)="addField()"
                            (backspace)="removeField()" 
                            (focus)="handleFocus($event)"
                            [isActive]="i === activeFieldsIndex"
                            [id]="i">
                        </cs-formula-field>
                    </div>                    
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
export class FormulaEditorComponent implements AfterViewInit {
    oldContent: OldContent = {before: "", editing: "", after: ""};

    fields!: FieldType[];

    activeFieldsIndex: number = 0;

    cursorLocation: number = -1;

    useExistingParenthesis: boolean = false;

    keepExistingParenthesis: boolean = false;

    existingParenthesis: StringPair = ["", ""];

    @ViewChild("formulaEditorDialog")
    formulaEditorDialog!: ElementRef<HTMLDivElement>;

    @Output() okClose = new EventEmitter<void>();
    @Output() cancelClose = new EventEmitter<void>();

    isMultilineFormulaControl = new FormControl(true);

    @Input() editor!: IEditor;

    @ViewChildren(FormulaFieldComponent)
    fieldComponents!: QueryList<FormulaFieldComponent>;

    @Input()
    get visible(): boolean {
        return this.isVisible;
    }
    set visible(isVis: boolean) {
        this.isVisible = isVis;
        // became visible so save what was in editor
        if (isVis) {
            this.parseOldContent(this.editor.content);
            this.cursorLocation = this.oldContent.before.length;
            const isEditing = this.parseEditedFormula();
            // initialize adding new formula
            if (!isEditing) {
                this.fields = [{latex: ""}];
                this.activeFieldsIndex = 0;
                const isMulti = this.getInitialMultilineSetting();
                this.isMultilineFormulaControl.setValue(isMulti);
            }
        }
    }
    private isVisible = false;

    @Input()
    get currentSymbol(): ButtonState {
        return this.buttonSymbol;
    }
    set currentSymbol(value: ButtonState) {
        this.buttonSymbol = value;
        if (this.fieldComponents !== undefined) {
            this.addFormula(value);
        }
    }

    private buttonSymbol: ButtonState = {text: ""};

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

    /**
     * append new empty field after the current field
     * and sets it as active
     */
    addField() {
        this.fields = [
            ...this.fields.slice(0, this.activeFieldsIndex + 1),
            {latex: ""},
            ...this.fields.slice(this.activeFieldsIndex + 1),
        ];
        this.activeFieldsIndex++;
        this.isMultilineFormulaControl.setValue(this.fields.length > 1);
        this.useExistingParenthesis = false;
    }

    /**
     * removes currently active field
     * sets the previous one as active
     */
    removeField() {
        // don't remove the first field
        if (this.fields.length <= 1) {
            return;
        }
        this.fields = [
            ...this.fields.slice(0, this.activeFieldsIndex),
            ...this.fields.slice(this.activeFieldsIndex + 1),
        ];
        if (this.activeFieldsIndex > 0) {
            this.activeFieldsIndex--;
        }

        this.isMultilineFormulaControl.setValue(this.fields.length > 1);
        this.useExistingParenthesis = false;
    }

    constructor(private cd: ChangeDetectorRef) {}
    handleEdited(res: Edit) {
        this.fields[res.id].latex = res.latex;
        this.updateFormulaToEditor();
        //this.isMultilineFormulaControl.setValue(this.fields.length > 1);
        this.activeFieldsIndex = res.id;
    }

    handleFocus(res: Edit) {
        this.activeFieldsIndex = res.id;
    }

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
        return this.cursorLocation;
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
     * Formulafield component that is being currently edited
     */
    getActiveField() {
        if (this.fieldComponents === undefined) {
            return undefined;
        }
        return this.fieldComponents.find(
            (item, index) => item.id === this.activeFieldsIndex
        );
    }

    /**
     * Parses current formula from editor content
     * if not inside formula then return undefined
     */
    parseCurrentFormula(
        allParenthesis: FormulaTuple[],
        cursorIndex: number
    ): FormulaTuple | undefined {
        // check if cursor is inside any parenthesis
        for (const [formulaType, startI, endI] of allParenthesis) {
            // parenthesis is completely after cursor
            if (startI > cursorIndex) {
                return undefined;
            }
            // parenthesis is completely before cursor
            if (endI < cursorIndex) {
                continue;
            }
            return [formulaType, startI, endI];
        }
        return undefined;
    }

    /**
     * trim character from start and end
     * @param str string to trim
     * @param ch character to trim
     */
    trimCharFromEnd(str: string, ch: string): string {
        let start = 0;
        let end = str.length;
        while (end > start && str[end - 1] === ch) {
            --end;
        }

        return start > 0 || end < str.length ? str.substring(start, end) : str;
    }

    /**
     * splits text into lines of latex
     * @param formula
     */
    getMultilineFormulaLines(formula: string): FieldType[] {
        return formula.split("\n").map((line) => {
            return {latex: this.trimCharFromEnd(line, "\\")};
        });
    }

    /**
     * Sets the active field to the line with cursor
     */
    setLineInFormula() {
        const original = this.editor.content;
        const endI = this.cursorLocation;
        const text =
            original[endI] === "\n"
                ? original.slice(0, endI)
                : original.slice(0, endI + 1);

        const parenthesisNewLine = this.existingParenthesis[0].includes("\n")
            ? 1
            : 0;
        this.activeFieldsIndex =
            text.split("\n").length -
            this.oldContent.before.split("\n").length -
            parenthesisNewLine;
    }

    /**
     * Parses current formula from editor content
     * @return true if editing else false
     */
    parseEditedFormula(): boolean {
        // check if cursor is inside a formula
        const text = this.editor.content;
        const allParenthesis = this.findParenthesisFromString(text);
        const currentFormula = this.parseCurrentFormula(
            allParenthesis,
            this.getCursorLocation()
        );
        // cursor wasn't inside a formula
        if (currentFormula === undefined) {
            return false;
        }

        const leftIndex = currentFormula[1];
        const rightIndex = currentFormula[2];
        // if inside formula, add it to formula editor for editing
        if (leftIndex >= 0 && rightIndex >= 0) {
            this.useExistingParenthesis = true;
            this.keepExistingParenthesis = true;
            // start editing a multiline formula
            if (currentFormula[0] === FormulaType.Multi) {
                // update old content
                this.oldContent.before = text.slice(0, leftIndex);
                this.oldContent.editing = text.slice(leftIndex, rightIndex + 1);
                this.oldContent.after = text.slice(rightIndex + 1);
                // separate formula and parenthesis
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
                // update formula editor content and values
                this.fields = this.getMultilineFormulaLines(formula);
                setTimeout(() => {
                    this.isMultilineFormulaControl.setValue(true);
                    this.setLineInFormula();
                }, 30);
            }
            // start editing an inline formula
            else {
                // update old content and current parenthesis
                this.oldContent.before = text.slice(0, leftIndex);
                this.oldContent.editing = text.slice(leftIndex, rightIndex + 1);
                this.oldContent.after = text.slice(rightIndex + 1);
                this.existingParenthesis = ["$", "$"];
                // update formula editor content and values
                this.fields = [{latex: text.slice(leftIndex + 1, rightIndex)}];
                setTimeout(() => {
                    this.isMultilineFormulaControl.setValue(false);
                }, 30);
            }
            return true;
        }
        return false;
    }

    ngAfterViewInit() {
        this.isMultilineFormulaControl.valueChanges.subscribe((value) => {
            if (!this.keepExistingParenthesis) {
                this.useExistingParenthesis = false;
            }
            this.updateFormulaToEditor();
            this.keepExistingParenthesis = false;
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

    formatLatex(isMultiline: boolean): string | undefined {
        const wrapSymbol = isMultiline ? "$$" : "$";
        if (isMultiline) {
            const latex = this.fields
                .map((field) => field.latex)
                .filter((text) => text.length > 0)
                .join("\\\\\n");
            if (latex.length === 0) {
                return undefined;
            }
            if (this.useExistingParenthesis) {
                return `${this.existingParenthesis[0]}${latex}${this.existingParenthesis[1]}`;
            }
            return `${wrapSymbol}\n${latex}\n${wrapSymbol}`;
        }
        const latex = this.fields[0].latex;
        if (latex.length === 0) {
            return undefined;
        }
        return `${wrapSymbol}${latex}${wrapSymbol}`;
    }

    /**
     * Updates editor text with current formula text
     */
    updateFormulaToEditor() {
        if (this.isMultilineFormulaControl.value !== null) {
            const isMultiline = this.isMultilineFormulaControl.value;
            const formulaLatex = this.formatLatex(isMultiline);
            // If nothing is typed then just show original content
            if (formulaLatex === undefined) {
                this.editor.content =
                    this.oldContent.before + this.oldContent.after;
            } else {
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

        this.okClose.emit();
        this.clearFields();

        this.editor.content = finalContent;
    }

    clearFields() {
        this.fields = [];
        this.useExistingParenthesis = false;
        this.keepExistingParenthesis = false;
        this.cursorLocation = -1;
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
            const finalContent =
                this.oldContent.before +
                this.oldContent.editing +
                this.oldContent.after;

            this.cancelClose.emit();

            this.clearFields();

            this.editor.content = finalContent;
        }
    }

    /**
     * Adds formula to both fields in last known cursor position
     * @param formulaInput LaTeX-formula to be added to fields
     */
    addFormula(formulaInput: ButtonState | string) {
        const formula =
            typeof formulaInput === "string" ? formulaInput : formulaInput.text;
        const activeField = this.fieldComponents.find(
            (item, index) => item.id === this.activeFieldsIndex
        );
        if (activeField === undefined) {
            return;
        }
        if (activeField.activeEditor === ActiveEditorType.Latex) {
            const startPos =
                activeField.latexInput.nativeElement.selectionStart;
            const endPos = activeField.latexInput.nativeElement.selectionEnd;
            const oldValue = activeField.latexInput.nativeElement.value;
            const newValue =
                oldValue.substring(0, startPos) +
                formula +
                oldValue.substring(endPos, oldValue.length);
            activeField.latexInputControl.setValue(newValue);
            activeField.latexInput.nativeElement.selectionStart =
                startPos + newValue.length;
            activeField.latexInput.nativeElement.selectionEnd =
                endPos + newValue.length;
            activeField.handleLatexInput();
        } else {
            activeField.mathField.write(formula);
        }
    }
}
