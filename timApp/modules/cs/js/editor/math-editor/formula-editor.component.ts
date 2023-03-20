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
            this.oldContent = this.parseOldContent(this.editor.content);
            this.checkIfEditing();
        }
    }
    private _visible: boolean = false;

    constructor() {}

    /**
     * Finds cursor location from editor
     * @return index in editor.content or -1 if editor.insert not defined
     */
    findCursorLocation(): number {
        if (!this.editor.insert) {
            return -1;
        }
        const cursorMarker = "│";

        // add cursor character to know where cursor is
        this.editor.insert(cursorMarker);
        // find its index
        const index = this.editor.content.indexOf(cursorMarker);
        // rewind changes to editor content
        const before = this.editor.content.slice(0, index);
        const after = this.editor.content.slice(index + 1);
        this.editor.content = before + after;

        return index;
    }

    /**
     * Splits string into three parts if possible at cursor location
     * @param str
     */
    parseOldContent(str: string): OldContent {
        const cursorI = this.findCursorLocation();
        if (cursorI === -1) {
            return {
                before: this.editor.content,
                editing: "",
                after: "",
            };
        }

        // form and return the old content
        const result = {
            before: this.editor.content.slice(0, cursorI),
            editing: "",
            after: this.editor.content.slice(cursorI),
        };

        return result;
    }

    checkIfEditing() {
        const before = this.oldContent.before;
        const after = this.oldContent.after;

        // TODO: check if inside formula or not. check also if formula is singleline or multiline.

        // set formula editor values if editing existing formula
        const insideFormula = false;
        if (insideFormula) {
            this.isMultilineFormulaControl.setValue(false);
            this.latexInputControl.setValue("\\frac{pi}{4}");
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
