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
    QueryList,
    ViewChild,
    ViewChildren,
    ChangeDetectorRef,
    ContentChild,
} from "@angular/core";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {CURSOR, IEditor} from "../editor";
import type {ITemplateButton} from "../../csPlugin";
import {FileSelectManagerComponent} from "../../util/file-select";
import type {Edit, LineAdd} from "./formula-field.component";
import {ActiveEditorType} from "./formula-field.component";
import {FormulaFieldComponent} from "./formula-field.component";
import {FormulaEvent} from "./symbol-button-menu.component";

enum FormulaType {
    Multi = "multi",
    Inline = "inline",
}

type FormulaTuple = [FormulaType, number, number];

/**
 * OldContent is split into three at cursor location if insert operation is supported
 * if not then just put content to before and after and editing can be empty.
 */
type OldContent = {
    before: string;
    editing: string;
    after: string;
};

type FieldType = {
    latex: string;
};

type StringPair = [string, string];

type NumPair = [number, number];

@Component({
    selector: "cs-formula-editor",
    template: `
        <div>
            <symbol-button-menu
                    (setFormula)="addFormula($event)"
                    (toggle)="toggleEditor()"
                    [templateButtons]="templateButtons"
                    [formulaEditorOpen]="visible"
            >
                <ng-content></ng-content>
            </symbol-button-menu>            
        </div>
        <div *ngIf="visible" class="formula-editor">
            <div tabindex="0" class="formula-editor-dialog" #formulaEditorDialog (keydown)="handleDialogEvents($event)">

                <div class="fields">
                    <div *ngFor="let field of fields; let i=index;" class="field">
                        <cs-formula-field 
                            [initialValue]="field.latex" 
                            (edited)="handleEdited($event)"
                            (enter)="addField($event)"
                            (backspace)="removeField()" 
                            (focus)="handleFocus($event)"
                            (upArrow)="handleArrowUp($event)"
                            (downArrow)="handleArrowDown($event)"
                            (add)="addField($event)"
                            (delete)="removeField()"
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
                        <input [disabled]="isDisabled" type="checkbox" [(ngModel)]="isMultilineFormula"
                               (ngModelChange)="onMultilineFormulaChange()" 
                               title="Outputs formula in multiple lines" i18n-title>
                        <ng-container i18n>Multiline</ng-container>
                    </label>
                </div>
            </div>
        </div>

    `,
    styleUrls: ["./formula-editor.component.scss"],
})
export class FormulaEditorComponent {
    oldContent: OldContent = {before: "", editing: "", after: ""};

    fields!: FieldType[];

    isMultilineFormula = true;

    activeFieldsIndex: number = 0;

    cursorLocation: number = -1;

    useExistingParenthesis: boolean = false;

    existingParenthesis: StringPair = ["", ""];

    private buttonSymbol: FormulaEvent = {
        text: "",
        command: "",
        useWrite: false,
    };
    private isVisible = false;

    isDisabled = false;

    @ViewChild("formulaEditorDialog")
    formulaEditorDialog!: ElementRef<HTMLDivElement>;

    @ViewChildren(FormulaFieldComponent)
    fieldComponents!: QueryList<FormulaFieldComponent>;

    @ContentChild(FileSelectManagerComponent)
    fileSelector?: FileSelectManagerComponent;

    @Output() okClose = new EventEmitter<number>();
    @Output() cancelClose = new EventEmitter<number>();
    @Output() focusBack = new EventEmitter<void>();
    @Output() toggle = new EventEmitter<void>();

    @Input() templateButtons: ITemplateButton[] = [];

    @Input() editor!: IEditor;

    constructor(private cd: ChangeDetectorRef) {}

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
                this.isMultilineFormula = this.getInitialMultilineSetting();
            }
        }
    }

    @Input()
    get currentSymbol(): FormulaEvent {
        return this.buttonSymbol;
    }

    set currentSymbol(value: FormulaEvent) {
        this.buttonSymbol = value;
        if (this.fieldComponents) {
            this.addFormula(value);
        }
    }

    /**
     * Append new empty field after the current field
     * and sets it as active.
     */
    addField(lineAdd: LineAdd) {
        if (!lineAdd || lineAdd.addBelow) {
            this.fields = [
                ...this.fields.slice(0, this.activeFieldsIndex + 1),
                {latex: ""},
                ...this.fields.slice(this.activeFieldsIndex + 1),
            ];
            this.activeFieldsIndex++;
            this.isMultilineFormula = this.fields.length > 1;
            this.useExistingParenthesis = false;
        } else {
            this.fields = [
                ...this.fields.slice(0, this.activeFieldsIndex),
                {latex: ""},
                ...this.fields.slice(this.activeFieldsIndex),
            ];
            this.isMultilineFormula = this.fields.length > 1;
            this.useExistingParenthesis = false;
        }
        this.isDisabled = true;
    }

    /**
     * Removes currently active field
     * sets the previous one as active.
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

        this.isMultilineFormula = this.fields.length > 1;
        this.isDisabled = this.fields.length > 1;
        this.useExistingParenthesis = false;
        this.updateFormulaToEditor();
    }

    handleEdited(res: Edit) {
        if (res.id < 0 || res.id >= this.fields.length) {
            return;
        }
        this.fields[res.id].latex = res.latex;
        this.updateFormulaToEditor();
        this.activeFieldsIndex = res.id;
    }

    handleFocus(res: Edit) {
        this.activeFieldsIndex = res.id;
    }

    handleArrowDown(id: number) {
        if (this.activeFieldsIndex + 1 < this.fields.length) {
            this.activeFieldsIndex++;
        }
    }

    handleArrowUp(id: number) {
        if (this.activeFieldsIndex > 0) {
            this.activeFieldsIndex--;
        }
    }

    /**
     * Find the line where cursor is in editor.
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
     * Determine whether the user wants to create a multiline or an inline formula.
     */
    getInitialMultilineSetting() {
        const currentLine = this.getCurrentLine();
        const isTextInLine = currentLine.trim().length > 0;
        // should be multiline if no real text in line
        return !isTextInLine;
    }

    /**
     * After oldContent is set cursor is between
     * oldContent parts.
     */
    getCursorLocation() {
        return this.cursorLocation;
    }

    /**
     * Splits string into two parts if possible at cursor location.
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
     * Method to find $ and $$ syntax parenthesis from a string.
     * @param text String where the parenthesis are looked for.
     * @return Array containing types and indexes of parenthesis.
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
     * Formulafield component that is being currently edited.
     */
    getActiveField() {
        if (!this.fieldComponents) {
            return undefined;
        }
        return this.fieldComponents.find(
            (item, index) => item.id === this.activeFieldsIndex
        );
    }

    /**
     * Finds current formula from list of parenthesis.
     * @param allParenthesis List of parenthesis types and indexes.
     * @param cursorIndex Index of the cursor.
     * @return Parenthesis in which the cursor is.
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
     * Trims a specific character from the end of a string.
     * @param str String to be trim.
     * @param ch Character to trim.
     * @return Trimmed string.
     */
    trimCharFromEnd(str: string, ch: string): string {
        const start = 0;
        let end = str.length;
        while (end > start && str[end - 1] === ch) {
            --end;
        }
        return end < str.length ? str.slice(start, end) : str;
    }

    /**
     * Method to find all begin and end syntax matrices from a string.
     * @param formula String where the matrices are looked for.
     * @return Array containing indexes of matrices.
     */
    findMatrixFromString(formula: string): NumPair[] {
        // temporary index variables
        let bIndex = 0;
        let eIndex = 0;
        // temporary stacks for found indexes
        const bStack: number[] = [];
        const eStack: number[] = [];
        // list of found matrices
        const allMatrices: NumPair[] = [];

        while (true) {
            // find next begin and end keywords
            bIndex = formula.indexOf("\\begin", bIndex);
            eIndex = formula.indexOf("\\end", eIndex);
            // stop if no end is found
            if (eIndex < 0) {
                // add possible keywords from stacks to actual list
                if (eStack.length > 0) {
                    allMatrices.push([
                        bStack[bStack.length - 1],
                        eStack[eStack.length - 1],
                    ]);
                }
                break;
            }
            // try to find ends to existing begins, if no more begins are found
            if (bIndex < 0) {
                let i = bStack.length - 1;
                // run loop until all begins have end pairs, or no more ends are found
                while (i >= 0) {
                    eIndex = formula.indexOf("\\end", eIndex);
                    if (eIndex < 0) {
                        break;
                    }
                    eStack.push(eIndex);
                    eIndex += 3;
                    i--;
                }
                // add only the outermost begin and end to list
                allMatrices.push([bStack[i + 1], eStack[eStack.length - 1]]);
                break;
            }
            // begin and end are found, but begin is first. add begin to stack.
            if (bIndex < eIndex) {
                // add possible keywords from stacks to actual list
                if (eStack.length > 0) {
                    allMatrices.push([
                        bStack[bStack.length - 1],
                        eStack[eStack.length - 1],
                    ]);
                    // reset stacks
                    bStack.length = 0;
                    eStack.length = 0;
                }
                bStack.push(bIndex);
                bIndex += 5;
            } else {
                // add end to stack only if it has a begin pair
                if (bStack.length > eStack.length) {
                    // remove inner begin and end from stacks
                    if (eStack.length > 0) {
                        bStack.pop();
                        eStack.shift();
                    }
                    eStack.push(eIndex);
                }
                eIndex += 3;
            }
        }
        // shift indexes from the start of keyword to actual start and end of matrix
        for (const i of allMatrices.keys()) {
            const beginNewLine = formula.lastIndexOf("\n", allMatrices[i][0]);
            if (beginNewLine < 0) {
                allMatrices[i][0] = 0;
            } else {
                allMatrices[i][0] = beginNewLine + 1;
            }
            const endNewLine = formula.indexOf("\n", allMatrices[i][1]);
            if (endNewLine < 0) {
                allMatrices[i][1] = formula.length - 1;
            } else {
                allMatrices[i][1] = endNewLine - 1;
            }
        }
        return allMatrices;
    }

    /**
     * splits text into lines of LaTeX.
     * @param formula
     * @param allMatrices
     */
    getMultilineFormulaLines(
        formula: string,
        allMatrices: NumPair[]
    ): FieldType[] {
        // split all line breaks if no matrices exists
        if (allMatrices.length < 1) {
            return formula.split("\n").map((line) => {
                return {latex: this.trimCharFromEnd(line, "\\")};
            });
        } else {
            let allFields: string[] = [];
            // split and add lines before first matrix
            allFields = allFields.concat(
                formula.slice(0, allMatrices[0][0]).split("\n")
            );
            // add first matrix
            allFields.push(
                formula.slice(allMatrices[0][0], allMatrices[0][1] + 1)
            );
            for (let i = 0; i < allMatrices.length - 1; i++) {
                // split and add lines between matrices
                allFields = allFields.concat(
                    formula
                        .slice(allMatrices[i][1] + 1, allMatrices[i + 1][0])
                        .split("\n")
                );
                // add matrix after the lines
                allFields.push(
                    formula.slice(
                        allMatrices[i + 1][0],
                        allMatrices[i + 1][1] + 1
                    )
                );
            }
            // split and add lines after last matrix
            allFields = allFields.concat(
                formula
                    .slice(
                        allMatrices[allMatrices.length - 1][1] + 1,
                        formula.length
                    )
                    .split("\n")
            );
            // remove empty lines and then return trimmed lines
            allFields = allFields.filter((line) => line.length > 0);
            return allFields.map((line) => {
                return {latex: this.trimCharFromEnd(line, "\\")};
            });
        }
    }

    /**
     * Sets the active field to the line with cursor.
     */
    setMultilineActiveField(fields: FieldType[]) {
        const cursorI = this.cursorLocation;
        const before = this.oldContent.before;
        const beforeAndEditing =
            before.length +
            this.oldContent.editing.length -
            this.existingParenthesis[1].length;
        // if cursor is at end, let active field set automatically to last
        if (cursorI >= beforeAndEditing) {
            return;
        }
        // calculate field index, default value to set is first
        let fieldIndex = 0;
        if (fields.length > 1) {
            let currentText =
                before.length +
                this.existingParenthesis[0].length +
                fields[0].latex.length +
                2;
            while (cursorI > currentText && fieldIndex < fields.length) {
                fieldIndex++;
                currentText += fields[fieldIndex].latex.length + 3;
            }
        }
        // set active field after timeout
        setTimeout(() => {
            this.activeFieldsIndex = fieldIndex;
        }, 70);
    }

    /**
     * Parses current formula from editor content.
     * @return True if editing a formula else false.
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
        if (!currentFormula) {
            return false;
        }

        const leftIndex = currentFormula[1];
        const rightIndex = currentFormula[2];
        // if inside formula, add it to formula editor for editing
        if (leftIndex >= 0 && rightIndex >= 0) {
            this.useExistingParenthesis = true;
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
                const allMatrices = this.findMatrixFromString(formula);
                const allFields = this.getMultilineFormulaLines(
                    formula,
                    allMatrices
                );
                this.fields = allFields;
                this.isMultilineFormula = true;
                this.isDisabled = this.fields.length > 1;
                this.setMultilineActiveField(allFields);
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
                this.isMultilineFormula = false;
                this.isDisabled = false;
            }
            return true;
        }
        return false;
    }

    /**
     * Updates editor text when multiline value changes.
     */
    onMultilineFormulaChange() {
        this.useExistingParenthesis = false;
        this.updateFormulaToEditor();
    }

    /**
     * Handler for keys being pressed, used for shortcuts to save or close the editor.
     */
    handleDialogEvents(e: KeyboardEvent) {
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

    /**
     * Formats formula to LaTeX string.
     * @param isMultiline True if added formula should be multiline, else false.
     * @return Formatted string or undefined if resulting formula is empty
     */
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
        // fields.length should be 1
        const latex = this.fields[0].latex;
        if (latex.length === 0) {
            return undefined;
        }
        return `${wrapSymbol}${latex}${wrapSymbol}`;
    }

    /**
     * Updates editor text with current formula text.
     */
    updateFormulaToEditor() {
        const isMultiline = this.isMultilineFormula;
        const formulaLatex = this.formatLatex(isMultiline);
        // If nothing is typed then just show original content
        if (formulaLatex === undefined) {
            this.editor.content =
                this.oldContent.before + this.oldContent.after;
        } else {
            this.editor.content =
                this.oldContent.before + formulaLatex + this.oldContent.after;
        }
    }

    handleFormulaOk() {
        this.updateFormulaToEditor();
        const finalContent = this.editor.content;

        const isMultiline = this.isMultilineFormula;
        const currentFormulaLength = this.formatLatex(isMultiline)?.length ?? 0;
        // adding new formula
        if (this.oldContent.editing.length === 0) {
            const endPos = this.oldContent.before.length + currentFormulaLength;
            this.okClose.emit(endPos);
        } else {
            // modifying formula
            const endPos = this.oldContent.before.length + currentFormulaLength;
            this.okClose.emit(endPos);
        }
        this.clearFields();

        this.editor.content = finalContent;
    }

    /**
     * Resets the formula editor when it is closed.
     */
    clearFields() {
        this.fields = [];
        this.useExistingParenthesis = false;
        this.cursorLocation = -1;
        this.isDisabled = false;
    }

    async handleFormulaCancel() {
        const oldContent =
            this.oldContent.before +
            this.oldContent.editing +
            this.oldContent.after;
        // content hasn't changed from what it was before opening formula editor
        // so cancel
        if (
            oldContent === this.editor.content ||
            (await showConfirm(
                $localize`Are you sure?`,
                $localize`This will clear the editor.`
            ))
        ) {
            // cancelling creation of a new formula
            if (this.oldContent.editing === "") {
                const endPos = this.oldContent.before.length;
                this.cancelClose.emit(endPos);
            } else {
                // cancelling editing formula
                const endPos =
                    this.oldContent.before.length +
                    this.oldContent.editing.length;
                this.cancelClose.emit(endPos);
            }

            this.clearFields();

            this.editor.content = oldContent;
        }
    }

    /**
     * Moves cursor to where cursor symbol is
     * and deletes the cursor symbol.
     * @param activeField field being edited
     */
    setMathQuillCursor(activeField: FormulaFieldComponent) {
        const span = activeField.visualInput.nativeElement;
        const cursor = CURSOR;
        const children = span.getElementsByTagName("span");
        for (const child of children) {
            // try to pick the correct element to click
            // textContent comparison is not enough to find
            // the unique, correct element to click
            if (
                child.textContent === cursor &&
                child.hasAttribute("mathquill-command-id") &&
                !child.classList.contains("mq-non-leaf")
            ) {
                // clicks at position where cursor is
                child.dispatchEvent(
                    new MouseEvent("mousedown", {
                        bubbles: true,
                    })
                );
                // removes cursor symbol
                activeField.mathField.keystroke("Right Backspace");
                return;
            }
        }

        // put focus to field even if it doesn't have cursor symbol
        activeField.mathField.focus();
    }

    /**
     * Adds formula to both fields in last known cursor position.
     * @param formulaInput LaTeX-formula to be added to fields.
     */
    addFormula(formulaInput: FormulaEvent) {
        const cursorPosition = formulaInput.text.indexOf(CURSOR);
        const formulaWithoutCursor = formulaInput.text.replace(CURSOR, "");

        // write to TIM editor
        if (!this.visible) {
            this.editor.insert?.(formulaInput.text);
            setTimeout(() => {
                this.editor.focus();
            }, 0);
            return;
        }

        const activeField = this.getActiveField();
        if (!activeField) {
            return;
        }

        if (activeField.activeEditor === ActiveEditorType.Latex) {
            const formula = formulaWithoutCursor;

            const startPos =
                activeField.latexInputElement.nativeElement.selectionStart;
            const endPos =
                activeField.latexInputElement.nativeElement.selectionEnd;
            const oldValue = activeField.latexInputElement.nativeElement.value;
            const newValue =
                oldValue.substring(0, startPos) +
                formula +
                oldValue.substring(endPos, oldValue.length);
            activeField.latexInput = newValue;
            activeField.handleLatexInput();
            setTimeout(() => {
                if (cursorPosition !== -1) {
                    activeField.latexInputElement.nativeElement.selectionStart =
                        startPos + cursorPosition;
                    activeField.latexInputElement.nativeElement.selectionEnd =
                        endPos + cursorPosition;
                }
                activeField.latexInputElement.nativeElement.focus();
            }, 0);
        } else {
            activeField.mathField.write(formulaInput.text);

            setTimeout(() => {
                this.setMathQuillCursor(activeField);
            }, 0);
        }
    }

    toggleEditor() {
        this.toggle.emit();
    }
}
