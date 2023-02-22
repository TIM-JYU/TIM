/**
 * Math Editor for inputting LaTeX math
 * @author Juha Reinikainen
 * @licence MIT
 * @date 20.2.2023
 */

import type {OnInit} from "@angular/core";
import {Component, ElementRef, ViewChild} from "@angular/core";
import type {IEditor} from "./editor";
import {
    IMathQuill,
    MathFieldMethods,
    MathQuillConfig,
} from "../../../../static/scripts/vendor/mathquill/mathquill";
import {FormControl} from "@angular/forms";

/**
 * Field which has the focus
 */
enum ActiveEditorType {
    Visual = "visual",
    Latex = "latex",
}

@Component({
    selector: "cs-math-editor",
    template: `
        <div class="math-editor-container">
            <div class="math-editor-input">
                <h2>Input</h2>
                <span #visualInput></span>
            </div>

            <div class="math-editor-output-container">
                <h2>Latex</h2>
                <textarea name="math-editor-output" #latexInput cols="30" rows="10"
                          (click)="handleLatexFocus()"
                          (keyup)="handleLatexInput()"
                          [formControl]="latexInputControl">
            </textarea>
            </div>
        </div>
    `,
    styleUrls: ["./math-editor.component.scss"],
})
export class MathEditorComponent implements OnInit, IEditor {
    latexInputControl = new FormControl("");
    @ViewChild("latexInput") latexInput!: ElementRef<HTMLTextAreaElement>;

    @ViewChild("visualInput") visualInput!: ElementRef<HTMLElement>;
    MQ!: IMathQuill;

    mathField!: MathFieldMethods;

    activeEditor: ActiveEditorType = ActiveEditorType.Visual;

    content: string = "";

    constructor() {}

    ngOnInit(): void {}

    focus(): void {}

    editHandler(field: any) {
        // write changes in visual field to latex field if visual field
        //was the one modified
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
        //was the one modified
        if (this.latexInputControl.value) {
            this.mathField.latex(this.latexInputControl.value);
        }
    }

    setReadOnly(b: boolean): void {}
}
