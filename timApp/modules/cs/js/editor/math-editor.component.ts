import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";
import type {IEditor} from "./editor";

@Component({
    selector: "cs-math-editor",
    template: `
    <p>
      abitti-editor works!
    </p>
  `,
    styleUrls: [
        "./math-editor.component.scss",
        "../../../../static/scripts/vendor/mathquill/mathquill.css",
    ],
})
export class MathEditorComponent implements OnInit, IEditor {
    constructor() {}

    ngOnInit(): void {}

    content: string = "";

    focus(): void {}

    async loadMathQuill() {
        const mq = (await import("vendor/mathquill/mathquill")).default;
        mq.getInterface(2).StaticMath(document.getElementById("math")!);
    }

    setReadOnly(b: boolean): void {}
}
