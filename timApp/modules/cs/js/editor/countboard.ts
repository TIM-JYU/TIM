import {
    Component,
    Input,
} from "@angular/core";

import {countLines, countWords} from "../util";

interface ICountLimit {
    show?: boolean;
    min?: number;
    max?: number;
    text?: string;
}

interface ICountOptions {
    preventSave?: boolean;
    tooManyWord?: string;
    tooFewWord?: string;
    lines?: ICountLimit;
    words?: ICountLimit;
    chars?: ICountLimit;
}

@Component({
    selector: "cs-count-board", // TODO: styling
    template: `
        <div *ngIf="options_" class="csPluginCountItems">
            <span *ngIf="lines">Lines: <span>{{lines}}</span></span>
            <span *ngIf="words">Words: <span>{{words}}</span></span>
            <span *ngIf="chars">Chars: <span>{{chars}}</span></span>
        </div>    
        <div *ngIf="countError" class="csPluginCountError">
            <p>{{countError}}</p>
        </div>`,
})
export class CountBoardComponent {
    options_?: ICountOptions;
    items: number = 0;
    lines: number = 0;
    words: number = 0;
    chars: number = 0;
    countError: string = "";
    preventSave: boolean = false;
    
    count(str: string) {
        if (!this.options_) { return; }
        this.chars = str.length;
        this.lines = countLines(str);
        this.words = countWords(str);
        this.countError = "";
        this.countError += this.checkCountLimits(this.options_.lines, this.lines, "lines");
        this.countError += this.checkCountLimits(this.options_.words, this.words, "words");
        this.countError += this.checkCountLimits(this.options_.chars, this.chars, "chars");
        this.preventSave = !!this.options_?.preventSave && this.countError !== "";
    }

    checkCountLimits(limits: ICountLimit | undefined, count: number, countType: string): string {
        if (!limits) { return ""; }
        const tooFew = this.options_?.tooFewWord ?? "Too few";
        const tooMany = this.options_?.tooManyWord ?? "Too Many";
        const cType = limits.text ?? countType;
        if (limits.min && count < limits.min) { return " " + tooFew + " " + cType + ", min: " + limits.min + "."; }
        if (limits.max && count > limits.max) { return " " + tooMany + " " + cType + ", max: " + limits.max + "."; }
        return "";
    }
    
    @Input()
    set options(options: ICountOptions | undefined) {
        this.options_ = options;
    }
}