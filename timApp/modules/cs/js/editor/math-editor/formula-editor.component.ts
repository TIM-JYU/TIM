/**
 * Formula Editor for inputting LaTeX math
 * @author Juha Reinikainen
 * @licence MIT
 * @date 28.2.2023
 */

import type {OnInit} from "@angular/core";
import {Component, ElementRef, Input, ViewChild} from "@angular/core";
import {FormControl} from "@angular/forms";
import type {
    IMathQuill,
    MathFieldMethods,
    MathQuillConfig,
} from "vendor/mathquill/mathquill";
import type {IEditor} from "../editor";
import {AceEditorComponent} from "../ace";

/**
 * Field which has the focus
 */
enum ActiveEditorType {
    Visual = "visual",
    Latex = "latex",
}

@Component({
    selector: "cs-formula-editor",
    template: `
        <div class="formula-editor">
            <div class="formula-container">
                <span #visualInput></span>
    
                <textarea name="math-editor-output" #latexInput cols="30" rows="10"
                          (click)="handleLatexFocus()"
                          (keyup)="handleLatexInput()"
                          [formControl]="latexInputControl"
                            placeholder="write LaTeX">
                </textarea>                    
            </div>

            <div class="formula-button-container">
                <div class="formula-buttons">
                    <button class="timButton" (click)="handleFormulaOk()">Ok</button>
                    <button class="timButton">Cancel</button>                                        
                </div>

                <label class="font-weight-normal">
                    <input type="checkbox" [formControl]="isMultilineFormulaControl">
                    Multiline
                </label>
            </div>
        </div>
    `,
    styleUrls: ["./formula-editor.component.scss"],
})
export class FormulaEditorComponent implements OnInit, IEditor {
    latexInputControl = new FormControl("");
    @ViewChild("latexInput") latexInput!: ElementRef<HTMLTextAreaElement>;

    @ViewChild("visualInput") visualInput!: ElementRef<HTMLElement>;
    MQ!: IMathQuill;

    mathField!: MathFieldMethods;

    activeEditor: ActiveEditorType = ActiveEditorType.Visual;

    content: string = "";

    @ViewChild("aceEditor") aceEditor!: AceEditorComponent;

    @Input() handleOk!: (formulaLatex: string, isMultiline: boolean) => void;

    isMultilineFormulaControl = new FormControl(true);

    constructor() {}

    ngOnInit(): void {}

    focus(): void {}

    editHandler(field: any) {
        // write changes in visual field to latex field if visual field
        // was the one modified
        if (this.activeEditor === ActiveEditorType.Visual) {
            const latex = field.latex();
            this.latexInputControl.setValue(latex);
        }
    }

    enterHandler(field: any) {
        console.log("enter");
    }

    async loadMathQuill() {
        const elem = this.visualInput.nativeElement;
        elem.addEventListener("click", (e: MouseEvent) => {
            this.activeEditor = ActiveEditorType.Visual;
        });
        const config: MathQuillConfig = {
            spaceBehavesLikeTab: true,
            handlers: {
                edit: (field: any) => this.editHandler(field),
                enter: (field: any) => this.enterHandler(field),
            },
        };
        const mq = (await import("vendor/mathquill/mathquill")).default;
        this.MQ = mq.getInterface(2);

        this.mathField = this.MQ.MathField(elem, config);
    }

    ngAfterViewInit() {
        void this.loadMathQuill();
    }

    handleLatexFocus() {
        this.activeEditor = ActiveEditorType.Latex;
    }

    handleLatexInput() {
        // write changes in latex field to visual field if latex field
        // was the one modified
        if (this.latexInputControl.value) {
            this.mathField.latex(this.latexInputControl.value);
        }
    }

    handleFormulaOk() {
        if (
            this.mathField.latex &&
            this.isMultilineFormulaControl.value !== null
        ) {
            const isMultiline = this.isMultilineFormulaControl.value;
            this.handleOk(this.mathField.latex(), isMultiline);
        }
    }

    setReadOnly(b: boolean): void {}
}
