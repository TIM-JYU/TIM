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
import type {
    IMathQuill,
    MathFieldMethods,
    MathQuillConfig,
} from "vendor/mathquill/mathquill";
import {IEditor} from "../editor";

/**
 * Field which has the focus
 */
enum ActiveEditorType {
    Visual = "visual",
    Latex = "latex",
}

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
            <div class="formula-editor-dialog">
                <div class="formula-container">
                    <span class="visual-input" #visualInput></span>
        
                    <textarea name="math-editor-output" #latexInput cols="30" rows="5"
                              (click)="handleLatexFocus()"
                              (keyup)="handleLatexInput()"
                              [formControl]="latexInputControl"
                              placeholder="Write LaTeX" i18n-placeholder>
                    </textarea>                    
                </div>
        
                <div class="formula-button-container">
                    <div class="formula-buttons">
                        <button class="timButton" (click)="handleFormulaOk()">Ok</button>
                        <button class="timButton" (click)="handleFormulaCancel()" i18n>Cancel</button>                                        
                    </div>
        
                    <label class="font-weight-normal">
                        <input type="checkbox" [formControl]="isMultilineFormulaControl"><ng-container i18n>
                        Multiline</ng-container>
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
        }
    }
    private _visible: boolean = false;

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
     * Parses current formula from old content
     */
    parseEditedFormula() {
        let leftIsMultiLine = false;
        let rightIsMultiLine = false;
        let leftIndex = this.oldContent.before.length;
        let rightIndex = 0;

        while (leftIndex >= 0) {
            leftIsMultiLine = false;
            leftIndex = this.oldContent.before.lastIndexOf("$", leftIndex - 1);
            if (this.oldContent.before.charAt(leftIndex + 1) !== " ") {
                if (this.oldContent.before.charAt(leftIndex - 1) === "$") {
                    leftIsMultiLine = true;
                }
                break;
            }
        }

        while (rightIndex >= 0) {
            rightIsMultiLine = false;
            rightIndex = this.oldContent.after.indexOf("$", rightIndex + 1);
            if (this.oldContent.after.charAt(rightIndex - 1) !== " ") {
                if (this.oldContent.after.charAt(rightIndex + 1) === "$") {
                    rightIsMultiLine = true;
                }
                break;
            }
        }

        if (leftIndex >= 0 && rightIndex >= 0) {
            if (leftIsMultiLine && rightIsMultiLine) {
                this.oldContent.editing =
                    this.oldContent.before.slice(leftIndex - 1) +
                    this.oldContent.after.slice(0, rightIndex + 2);
                this.oldContent.before = this.oldContent.before.slice(
                    0,
                    leftIndex - 1
                );
                this.oldContent.after = this.oldContent.after.slice(
                    rightIndex + 2
                );
                this.isMultilineFormulaControl.setValue(true);
                this.latexInputControl.setValue(
                    this.oldContent.editing.slice(
                        3,
                        this.oldContent.editing.length - 3
                    )
                );
            } else {
                this.oldContent.editing =
                    this.oldContent.before.slice(leftIndex) +
                    this.oldContent.after.slice(0, rightIndex + 1);
                this.oldContent.before = this.oldContent.before.slice(
                    0,
                    leftIndex
                );
                this.oldContent.after = this.oldContent.after.slice(
                    rightIndex + 1
                );
                this.isMultilineFormulaControl.setValue(false);
                this.latexInputControl.setValue(
                    this.oldContent.editing.slice(
                        1,
                        this.oldContent.editing.length - 1
                    )
                );
            }
            this.handleLatexFocus();
            this.handleLatexInput();
        }
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
            this.updateFormulaToEditor();
        }
    }

    enterHandler(field: MathFieldMethods) {
        this.handleFormulaOk();
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
            this.updateFormulaToEditor();
        });
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
            this.updateFormulaToEditor();
        }
    }

    clearFields() {
        this.mathField.latex("");
        this.latexInputControl.setValue("");
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
                const formulaLatex = this.formatLatex(latex, isMultiline);

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
        // clearing fields triggers update to editor content
        // rewrite it
        this.editor.content = finalContent;
    }

    handleFormulaCancel() {
        // content hasn't changed from what it was before opening formula editor
        // so cancel
        if (
            this.oldContent.before +
                this.oldContent.editing +
                this.oldContent.after ===
                this.editor.content ||
            confirm($localize`Are you sure? Cancel will clear the editor.`)
        ) {
            this.editor.content =
                this.oldContent.before +
                this.oldContent.editing +
                this.oldContent.after;
            const finalContent = this.editor.content;
            this.cancelEvent.emit();
            this.clearFields();
            // clearing fields triggers update to editor content
            // rewrite it
            this.editor.content = finalContent;
        }
    }
}
