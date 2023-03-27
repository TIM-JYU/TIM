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
} from "@angular/core";
import {FormControl} from "@angular/forms";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {IEditor} from "../editor";
import formulas from "./latex-commands";

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
 * Object wrapping is neccessary to
 * make angular produce a event for each
 * button press.
 */
type ButtonState = {
    text: string;
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

    formulas: string[] = ["\\sqrt{ }", "\\int_{ }^{ }", "\\frac{ }{ }"];

    fields!: FieldType[];

    activeFieldsIndex: number = 0;

    @ViewChild("formulaEditorDialog")
    formulaEditorDialog!: ElementRef<HTMLDivElement>;

    @Output() okEvent = new EventEmitter<void>();
    @Output() cancelEvent = new EventEmitter<void>();

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
            this.fields = [{latex: ""}];
            this.activeFieldsIndex = 0;

            this.parseOldContent(this.editor.content);
            const isMulti = this.getInitialMultilineSetting();
            this.isMultilineFormulaControl.setValue(isMulti);
            this.parseEditedFormula();
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
    }

    handleEdited(res: Edit) {
        this.fields[res.id].latex = res.latex;
        this.activeFieldsIndex = res.id;
        this.updateFormulaToEditor();

        this.isMultilineFormulaControl.setValue(this.fields.length > 1);
    }

    handleFocus(res: Edit) {
        this.fields[res.id].latex = res.latex;
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
        const allParenthesis = [];

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
                        allParenthesis.push([2, stack[1], currentIndex]);
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
                        allParenthesis.push([1, stack[0], currentIndex]);
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
     */
    parseEditedFormula() {
        // variables to keep track of editor content
        let leftIndex = -1;
        let rightIndex = -1;
        let isMultiLine = false;
        const before = this.oldContent.before;
        const text = this.editor.content;
        const allParenthesis = this.findParenthesisFromString(text);
        const activeField = this.getActiveField();
        if (activeField === undefined) {
            return;
        }

        // check if cursor is inside any parenthesis
        for (const entry of allParenthesis) {
            // parenthesis is completely after cursor
            if (entry[1] > before.length) {
                break;
            }
            // parenthesis is completely before cursor
            if (entry[2] < before.length) {
                continue;
            }
            if (entry[0] === 1) {
                leftIndex = entry[1];
                rightIndex = entry[2];
                isMultiLine = false;
                break;
            }
            if (entry[0] === 2) {
                leftIndex = entry[1];
                rightIndex = entry[2] + 1;
                isMultiLine = true;
                break;
            }
        }

        // if inside formula, modify editor content and add current formula to formula editor
        // otherwise do nothing and keep adding a new formula
        if (leftIndex >= 0 && rightIndex >= 0) {
            // start editing a multiline formula
            if (isMultiLine) {
                // update old content
                this.oldContent.before = text.slice(0, leftIndex);
                this.oldContent.editing = text.slice(leftIndex, rightIndex + 1);
                this.oldContent.after = text.slice(rightIndex + 1);
                // update formula editor values
                this.isMultilineFormulaControl.setValue(true);
                activeField.latexInputControl.setValue(
                    this.oldContent.editing.slice(
                        3,
                        this.oldContent.editing.length - 3
                    )
                );
            }
            // start editing an inline formula
            else {
                // update old content
                this.oldContent.before = text.slice(0, leftIndex);
                this.oldContent.editing = text.slice(leftIndex, rightIndex + 1);
                this.oldContent.after = text.slice(rightIndex + 1);
                // update formula editor values
                this.isMultilineFormulaControl.setValue(false);
                activeField.latexInputControl.setValue(
                    this.oldContent.editing.slice(
                        1,
                        this.oldContent.editing.length - 1
                    )
                );
            }
            // update editor views to user
            setTimeout(() => {
                activeField.handleLatexFocus();
                activeField.handleLatexInput();
            }, 2);
        }
    }

    ngAfterViewInit() {
        this.isMultilineFormulaControl.valueChanges.subscribe((value) => {
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

        this.okEvent.emit();
        this.clearFields();

        this.editor.content = finalContent;
    }

    clearFields() {
        this.fields = [];
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

            this.cancelEvent.emit();

            this.clearFields();

            this.editor.content = finalContent;
        }
    }

    /**
     * Adds formula to both fields in last known cursor position
     * @param formula LaTeX-formula to be added to fields
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
