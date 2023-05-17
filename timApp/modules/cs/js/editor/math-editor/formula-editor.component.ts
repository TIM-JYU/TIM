/**
 * Formula Editor for inputting LaTeX math
 *
 * @author Daniel Juola
 * @author Jaakko Palm
 * @author Janne Lahti
 * @author Juha Reinikainen
 * @license MIT
 * @date 28.2.2023
 */

import {
    Component,
    ContentChild,
    ElementRef,
    EventEmitter,
    Input,
    Output,
    QueryList,
    ViewChild,
    ViewChildren,
} from "@angular/core";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {CURSOR, IEditor} from "../editor";
import type {ITemplateButton} from "../../csPlugin";
import {FileSelectManagerComponent} from "../../util/file-select";
import type {Edit, LineAdd} from "./formula-field.component";
import {
    ActiveEditorType,
    FormulaFieldComponent,
} from "./formula-field.component";
import {FormulaEvent} from "./symbol-button-menu.component";
import type {
    FormulaTuple,
    NumPair,
    StringBool,
    StringPair,
    StringTrio,
} from "./formula-parsing-utils";
import {
    findMatrixFromString,
    formatLatex,
    getCurrentLine,
    getMultilineFormulaLines,
    parseEditedFormula,
    parseOldContent,
    checkInnerFormula,
    parseExistingParenthesis,
} from "./formula-parsing-utils";
import type {ReplacePair} from "./formula-types";
import {FormulaPropertyList, FORMULA_TYPES, FormulaType} from "./formula-types";

/**
 * Information about text that was in editor when formula editor was opened.
 * Split into text before formula, text of formula chosen for editing and text after
 * formula.
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
                            [formulaType]="formulaType"
                            [id]="i">
                        </cs-formula-field>
                    </div>
                </div>


                <div class="formula-button-container">
                    <div class="formula-buttons">
                        <button class="timButton" (click)="handleFormulaOk()" i18n title="Ctrl+s">Save</button>
                        <button class="timButton" (click)="handleFormulaCancel()" i18n title="Esc">Cancel</button>
                    </div>

                    <label class="font-weight-normal">
                        <ng-container i18n>LaTeX environment:</ng-container>
                        <select class="form-control"
                                [ngModel]="formulaType"
                                (ngModelChange)="onFormulaTypeChange($event)">
                            <ng-container *ngFor="let type of typeList" >
                                <option *ngIf="type[1]; else elseBlock" [ngValue]="type[0]" [disabled]="isDisabled">{{formulaTypes[type[0]]}}</option>
                                <ng-template #elseBlock>
                                    <option [ngValue]="type[0]">{{formulaTypes[type[0]]}}</option>
                                </ng-template>
                            </ng-container>
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

    typeList!: StringBool[];

    formulaType = FormulaType.Multiline;

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

    formulaTypes = FORMULA_TYPES;

    /**
     * Gets whether formula editor is visible or not.
     */
    @Input()
    get visible(): boolean {
        return this.isVisible;
    }

    /**
     * Sets visibility status of formula editor and parses
     * formula from editor if cursor was inside a formula
     * in editor and sets initial state for formula editor.
     * @param isVis True if formula editor should be visible else false.
     */
    set visible(isVis: boolean) {
        this.isVisible = isVis;
        // became visible so save what was in editor
        if (isVis) {
            this.typeList = FormulaPropertyList.filter(
                (type) => type.type != FormulaType.NotDefined
            ).map((type) => [type.type, type.join.length < 1]);
            this.oldContent = parseOldContent(this.editor);
            this.cursorLocation = this.oldContent.before.length;
            const currentFormula = parseEditedFormula(
                this.editor.content,
                this.cursorLocation
            );
            // currentFormula[1] and [2] are formula begin and end indexes in a text.
            // Index < 0 means no suitable begin or end was found.
            const isNotEditing =
                currentFormula[0] === FormulaType.NotDefined ||
                currentFormula[1] < 0 ||
                currentFormula[2] < 0;
            // initialize adding new formula
            if (isNotEditing) {
                this.fields = [{latex: ""}];
                this.activeFieldsIndex = 0;
                this.formulaType = this.getInitialFormulaType();
            } else {
                // formula editing
                this.addEditedFormulaToEditor(currentFormula);
                // scroll up to button menu so if formula editing
                // was initialized from preview click user
                // doesn't have to manually scroll up
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
     * Sets symbol that was pressed and adds it to whether editor is active.
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
     * Append new empty field before or after the current field and sets it as active.
     * @param lineAdd which field add came from and whether to add before or after that line
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
        // if formula is single line type, switch it to $$ type
        // to support multiple lines
        const singleLineFormulas = FormulaPropertyList.filter(
            (type) => type.join.length < 1
        ).map((type) => type.type);
        if (singleLineFormulas.includes(this.formulaType)) {
            this.formulaType = FormulaType.Multiline;
        }
        this.useExistingParenthesis = false;
        this.isDisabled = true;
    }

    /**
     * Removes currently active field sets the previous one as active.
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

        this.isDisabled = this.fields.length > 1;
        this.useExistingParenthesis = false;
        this.updateFormulaToEditor();
    }

    /**
     * Sets LaTeX content of a field,
     * updates changes to preview and
     * sets edited field as active.
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
     * Sets active field.
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
     * Determine formula type based on current cursor location.
     */
    getInitialFormulaType(): FormulaType {
        const currentLine = getCurrentLine(
            this.editor.content,
            this.cursorLocation
        );
        const isEmptyLine = currentLine.trim().length === 0;
        // should be multiline if adding formula to empty line,
        // and inline if adding formula to line with text.
        return isEmptyLine ? FormulaType.Multiline : FormulaType.Inline;
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
     * When starting to edit, sets the active field to the line with cursor.
     * @param fields Array containing the LaTeX of the formulas.
     */
    setMultilineActiveField(fields: FieldType[]) {
        // Constants depend on number characters between formulas.
        // Number of characters depend on type of the formula.
        const formulaProperties = FormulaPropertyList.find(
            (formulaType) => formulaType.type === this.formulaType
        );
        if (!formulaProperties) {
            return;
        }
        const firstConstant = formulaProperties.activeFieldConstant;
        const lineConstant = formulaProperties.join.length;

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
        // Set active field after timeout. Building formula editor takes a moment.
        // If value is set before finishing, active field will always be the last.
        setTimeout(() => {
            this.activeFieldsIndex = fieldIndex;
        }, 70);
    }

    /**
     * Add the current formula to editor.
     * @param currentFormula Formula to be edited.
     */
    addEditedFormulaToEditor(currentFormula: FormulaTuple) {
        const properties = FormulaPropertyList.find(
            (formulaType) => formulaType.type === currentFormula[0]
        );
        if (!properties) {
            // this should never happen
            throw Error("undefined formula type: " + this.formulaType);
        }
        let usedReplace: ReplacePair = properties.editReplace;
        const text = this.editor.content;
        const start = properties.start.replace(/\n/gm, "");
        const end = properties.end.replace(/\n/gm, "");
        this.useExistingParenthesis = true;
        this.formulaType = properties.type;
        // update old content
        this.oldContent.before = text.slice(0, currentFormula[1]);
        this.oldContent.editing = text.slice(
            currentFormula[1],
            currentFormula[2] + 1
        );
        this.oldContent.after = text.slice(currentFormula[2] + 1);
        // separate formula and parenthesis
        const formulaParts: StringTrio = parseExistingParenthesis(
            this.oldContent.editing,
            start,
            end
        );
        let formula = formulaParts[1];
        this.existingParenthesis[0] = formulaParts[0];
        this.existingParenthesis[1] = formulaParts[2];
        // check if there is a formula inside current formula
        const innerTypes = FormulaPropertyList.filter((formulaType) =>
            properties.inner.includes(formulaType.type)
        );
        for (const innerType of innerTypes) {
            const parts: StringTrio = checkInnerFormula(
                formula,
                innerType.start.replace(/\n/gm, ""),
                innerType.end.replace(/\n/gm, "")
            );
            // update parenthesis, formula type and formula if inner formula was found
            if (parts[0].length > 0 && parts[2].length > 0) {
                formula = parts[1];
                this.existingParenthesis[0] =
                    this.existingParenthesis[0] + parts[0];
                this.existingParenthesis[1] =
                    parts[2] + this.existingParenthesis[1];
                this.formulaType = innerType.type;
                usedReplace = innerType.editReplace;
                break;
            }
        }
        // escape characters if needed
        if (usedReplace) {
            try {
                const regex = new RegExp(usedReplace[0]);
                formula = formula.replace(regex, usedReplace[1]);
            } catch (err) {
                console.log("invalid RegExp: " + usedReplace[0]);
                console.log(err);
            }
        }
        // update formula editor content
        const allMatrices: NumPair[] = findMatrixFromString(formula).map(
            (formulaTuple) => [formulaTuple[1], formulaTuple[2]]
        );
        const allFields = getMultilineFormulaLines(formula, allMatrices);
        this.fields = allFields;
        this.isDisabled = this.fields.length > 1;
        this.setMultilineActiveField(allFields);
    }

    /**
     * Updates editor text when formula type value in drop-down menu changes.
     * @param newType New selected value in the drop-down menu.
     */
    onFormulaTypeChange(newType: FormulaType) {
        // get properties for old formula type
        const prevProperties = FormulaPropertyList.find(
            (formulaType) => formulaType.type === this.formulaType
        );
        // update formula editor values
        this.useExistingParenthesis = false;
        this.formulaType = newType;
        // If needed, replace escaped characters for new formula type.
        // Replace only if previous type had a RegExp to search.
        if (prevProperties && prevProperties.editReplace) {
            // check if new type has a replacement string
            const newProperties = FormulaPropertyList.find(
                (formulaType) => formulaType.type === newType
            );
            // use new replacement string or empty string as default
            let replaceString = "";
            if (newProperties && newProperties.editReplace) {
                replaceString = newProperties.editReplace[1];
            }
            // replace only if new replacement string is not equal to the old one
            if (prevProperties.editReplace[1] != replaceString) {
                try {
                    const regex = new RegExp(prevProperties.editReplace[0]);
                    const newFields: FieldType[] = [];
                    for (const field of this.fields) {
                        const latexContent = field.latex.replace(
                            regex,
                            replaceString
                        );
                        newFields.push({latex: latexContent});
                    }
                    this.fields = newFields;
                } catch (err) {
                    console.log(
                        "invalid RegExp: " + prevProperties.editReplace[0]
                    );
                    console.log(err);
                }
            }
        }
        // update preview
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
            // write formula to TIM editor
            this.editor.content =
                this.oldContent.before + formulaLatex + this.oldContent.after;
        }
    }

    /**
     * Sets content of editor to the content of formula editor
     * and emits ok event with cursor location in content.
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
     * @return true if cancelled or false otherwise
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
                $localize`This will close the formula editor without saving.`
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

            return true;
        }
        return false;
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
            // setTimeout is needed for focus to work
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

            // setTimeout is needed for focus to work
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
