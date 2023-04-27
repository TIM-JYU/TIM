/**
 * Formula Editor for inputting LaTeX math
 *
 * @author Juha Reinikainen
 * @author Daniel Juola
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
import type {StringPair, FormulaTuple} from "./formula-parsing-utils";
import {
    parseOldContent,
    getCurrentLine,
    findMatrixFromString,
    parseEditedFormula,
    getMultilineFormulaLines,
    formatLatex,
} from "./formula-parsing-utils";

/**
 * Describes the type of formula. Type is same for the whole formula.
 */
export enum FormulaType {
    Multi = "multi",
    Inline = "inline",
    Align = "align",
    NotDefined = "undefined",
}

/**
 * OldContent is split into three at cursor location if insert operation is supported
 * if not then just put content to before and after and editing can be empty.
 */
export type OldContent = {
    before: string;
    editing: string;
    after: string;
};

/**
 * Describes content of a field in formula.
 */
export type FieldType = {
    latex: string;
};

@Component({
    selector: "cs-formula-editor",
    template: `
        <div #symbolButtonMenuDiv>
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
                            (upArrow)="handleArrowUp()"
                            (downArrow)="handleArrowDown()"
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
                        <select
                                class="form-control"
                                [(ngModel)]="formulaType"
                                (ngModelChange)="onFormulaTypeChange()"
                        >
                            <option [ngValue]="'inline'" i18n [disabled]="isDisabled">Inline</option>
                            <option [ngValue]="'multi'" i18n>Multiline</option>
                            <option [ngValue]="'align'" i18n>Align</option>
                        </select>
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

    formulaType = FormulaType.Multi;

    activeFieldsIndex: number = 0;

    cursorLocation: number = -1;

    useExistingParenthesis: boolean = false;

    existingParenthesis: StringPair = ["", ""];

    private buttonSymbol: FormulaEvent = {
        text: "",
    };
    private isVisible = false;

    isDisabled = false;

    @ViewChild("formulaEditorDialog")
    formulaEditorDialog!: ElementRef<HTMLDivElement>;

    @ViewChild("symbolButtonMenuDiv")
    symbolButtonMenuDiv!: ElementRef<HTMLDivElement>;

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

    /**
     * Gets whether formula editor is visible or not
     */
    @Input()
    get visible(): boolean {
        return this.isVisible;
    }

    /**
     * Sets visibility status of formula editor and
     * parses formula from editor if cursor was inside a formula in editor
     * and sets initial state for formula editor.
     * @param isVis true if formula editor should be visible else false
     */
    set visible(isVis: boolean) {
        this.isVisible = isVis;
        // became visible so save what was in editor
        if (isVis) {
            this.oldContent = parseOldContent(this.editor);
            this.cursorLocation = this.oldContent.before.length;
            const currentFormula = parseEditedFormula(
                this.editor.content,
                this.cursorLocation
            );
            // initialize adding new formula
            if (
                currentFormula[0] === FormulaType.NotDefined ||
                currentFormula[1] < 0 ||
                currentFormula[2] < 0
            ) {
                this.fields = [{latex: ""}];
                this.activeFieldsIndex = 0;
                this.formulaType = this.getInitialFormulaType();
            } else {
                this.addEditedFormulaToEditor(currentFormula);
                this.symbolButtonMenuDiv.nativeElement.scrollIntoView();
            }
        }
    }

    /**
     * Gets symbol that was pressed.
     */
    @Input()
    get currentSymbol(): FormulaEvent {
        return this.buttonSymbol;
    }

    /**
     * Sets symbol that was pressed and adds it to whether editor is active
     * This approach is used instead of VIewChild to make passing
     * button press events from outside of this component
     * in AngularJs code.
     * @param value formula to add
     */
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
        } else {
            this.fields = [
                ...this.fields.slice(0, this.activeFieldsIndex),
                {latex: ""},
                ...this.fields.slice(this.activeFieldsIndex),
            ];
        }
        if (this.formulaType != FormulaType.Align) {
            this.formulaType =
                this.fields.length > 1 ? FormulaType.Multi : FormulaType.Inline;
        }
        this.useExistingParenthesis = false;
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

        if (this.formulaType != FormulaType.Align) {
            this.formulaType =
                this.fields.length > 1 ? FormulaType.Multi : FormulaType.Inline;
        }
        this.isDisabled = this.fields.length > 1;
        this.useExistingParenthesis = false;
        this.updateFormulaToEditor();
    }

    /**
     * Sets LaTeX content of a field,
     * updates changes to preview and
     * sets edited field as active
     * @param res edit content
     */
    handleEdited(res: Edit) {
        if (res.id < 0 || res.id >= this.fields.length) {
            return;
        }
        this.fields[res.id].latex = res.latex;
        this.updateFormulaToEditor();
        this.activeFieldsIndex = res.id;
    }

    /**
     * Sets active field
     * @param res edit content
     */
    handleFocus(res: Edit) {
        this.activeFieldsIndex = res.id;
    }

    /**
     * Sets active field as one after currently active one if one exists.
     */
    handleArrowDown() {
        if (this.activeFieldsIndex + 1 < this.fields.length) {
            this.activeFieldsIndex++;
        }
    }

    /**
     * Sets active field as one before currently active one if one exists.
     */
    handleArrowUp() {
        if (this.activeFieldsIndex > 0) {
            this.activeFieldsIndex--;
        }
    }

    /**
     * Determine formula type based on current cursor location
     */
    getInitialFormulaType() {
        const currentLine = getCurrentLine(
            this.editor.content,
            this.cursorLocation
        );
        const isEmptyLine = currentLine.trim().length === 0;
        // should be multiline if no real text in line
        return isEmptyLine ? FormulaType.Multi : FormulaType.Inline;
    }

    /**
     * Formulafield component that is being currently edited.
     */
    getActiveField() {
        if (!this.fieldComponents) {
            return undefined;
        }
        return this.fieldComponents.find(
            (item) => item.id === this.activeFieldsIndex
        );
    }

    /**
     * Sets the active field to the line with cursor.
     * @param fields Array containing the LaTeX of the formulas.
     */
    setMultilineActiveField(fields: FieldType[]) {
        const firstConstant = this.formulaType === FormulaType.Align ? 0 : 2;
        const lineConstant = this.formulaType === FormulaType.Align ? 4 : 3;
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
                firstConstant;
            while (cursorI > currentText && fieldIndex < fields.length) {
                fieldIndex++;
                currentText += fields[fieldIndex].latex.length + lineConstant;
            }
        }
        // set active field after timeout
        setTimeout(() => {
            this.activeFieldsIndex = fieldIndex;
        }, 70);
    }

    /**
     * Add the current formula to editor.
     * @param currentFormula Formula to be edited.
     */
    addEditedFormulaToEditor(currentFormula: FormulaTuple) {
        const text = this.editor.content;
        const leftIndex = currentFormula[1];
        const rightIndex = currentFormula[2];
        this.useExistingParenthesis = true;
        // update old content
        this.oldContent.before = text.slice(0, leftIndex);
        this.oldContent.editing = text.slice(leftIndex, rightIndex + 1);
        this.oldContent.after = text.slice(rightIndex + 1);

        let formula = "";
        let trimmed = "";
        let lenDiff = 0;
        let allFields: FieldType[] = [];
        switch (currentFormula[0]) {
            case FormulaType.Multi:
                // separate formula and parenthesis
                formula = this.oldContent.editing.slice(2, -2);
                trimmed = formula.trimStart();
                lenDiff = formula.length - trimmed.length;
                this.existingParenthesis[0] = "$$" + formula.slice(0, lenDiff);
                formula = trimmed;

                trimmed = formula.trimEnd();
                lenDiff = formula.length - trimmed.length;
                this.existingParenthesis[1] =
                    "" + formula.slice(formula.length - lenDiff) + "$$";
                formula = trimmed;
                // update formula editor content and values
                const allMatrices = findMatrixFromString(formula);
                allFields = getMultilineFormulaLines(formula, allMatrices);
                this.fields = allFields;
                this.formulaType = FormulaType.Multi;
                this.setMultilineActiveField(allFields);
                break;
            case FormulaType.Align:
                // separate formula and parenthesis
                formula = this.oldContent.editing.slice(14, -12);
                trimmed = formula.trimStart();
                lenDiff = formula.length - trimmed.length;
                this.existingParenthesis[0] =
                    "\\begin{align*}" + formula.slice(0, lenDiff);
                formula = trimmed;

                trimmed = formula.trimEnd();
                lenDiff = formula.length - trimmed.length;
                this.existingParenthesis[1] =
                    "" +
                    formula.slice(formula.length - lenDiff) +
                    "\\end{align*}";
                formula = trimmed;
                // update formula editor content and values
                allFields = getMultilineFormulaLines(formula, []);
                this.fields = allFields;
                this.formulaType = FormulaType.Align;
                this.setMultilineActiveField(allFields);
                break;
            case FormulaType.Inline:
                this.existingParenthesis = ["$", "$"];
                this.fields = [{latex: text.slice(leftIndex + 1, rightIndex)}];
                this.formulaType = FormulaType.Inline;
                break;
            default:
                throw Error("undefined case " + this.formulaType);
        }
        this.isDisabled = this.fields.length > 1;
    }

    /**
     * Updates editor text when multiline value changes.
     */
    onFormulaTypeChange() {
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
     * Updates editor text with current formula text.
     */
    updateFormulaToEditor() {
        const formulaLatex = formatLatex(
            this.formulaType,
            this.fields,
            this.existingParenthesis,
            this.useExistingParenthesis
        );
        // If nothing is typed then just show original content
        if (formulaLatex === undefined) {
            this.editor.content =
                this.oldContent.before + this.oldContent.after;
        } else {
            this.editor.content =
                this.oldContent.before + formulaLatex + this.oldContent.after;
        }
    }

    /**
     * Sets content of editor to the content
     * of formula editor and emits ok event with cursor location in content.
     */
    handleFormulaOk() {
        this.updateFormulaToEditor();
        const finalContent = this.editor.content;

        const currentFormulaLength =
            formatLatex(
                this.formulaType,
                this.fields,
                this.existingParenthesis,
                this.useExistingParenthesis
            )?.length ?? 0;
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

    /**
     * Handles cancellation of formula editing. Asks for confirmation as it clears editor data.
     */
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
                child.dispatchEvent(
                    new MouseEvent("mouseup", {
                        bubbles: true,
                    })
                );
                // removes cursor symbol
                activeField.mathField.keystroke("Right Backspace");
                activeField.mathField.focus();
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

            activeField.latexInput =
                oldValue.substring(0, startPos) +
                formula +
                oldValue.substring(endPos, oldValue.length);
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

    /**
     * Emit toggle event.
     */
    toggleEditor() {
        this.toggle.emit();
    }
}
