/* eslint no-underscore-dangle: ["error", { "allow": ["options_"] }] */
import {Component, Input} from "@angular/core";

import {countLines, countWords} from "../util/util";

interface ICountLimit {
    show?: boolean;
    min?: number;
    max?: number;
    text?: string;
    preventSave?: boolean;
    tooManyWord?: string;
    tooFewWord?: string;
    name?: string;
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
            <span *ngIf="nameLines">{{nameLines}}: <span>{{lines}}</span><br></span>
            <span *ngIf="nameWords">{{nameWords}}: <span>{{words}}</span><br></span>
            <span *ngIf="nameChars">{{nameChars}}: <span>{{chars}}</span></span>
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
    nameChars: string = "";
    nameWords: string = "";
    nameLines: string = "";
    space: string = "<br>";

    count(str: string) {
        if (!this.options_) {
            return;
        }
        this.chars = str.length;
        this.lines = countLines(str);
        this.words = countWords(str);
        this.countError = "";
        if (this.nameLines) {
            this.countError += this.checkCountLimits(
                this.options_.lines,
                this.lines,
                $localize`lines`
            );
        }

        if (this.nameWords) {
            this.countError += this.checkCountLimits(
                this.options_.words,
                this.words,
                $localize`words`
            );
        }
        if (this.nameChars) {
            this.countError += this.checkCountLimits(
                this.options_.chars,
                this.chars,
                $localize`chars`
            );
        }
        this.preventSave =
            !!this.options_?.preventSave && this.countError !== "";
    }

    checkCountLimits(
        limits: ICountLimit | undefined,
        count: number,
        countType: string
    ): string {
        if (!limits) {
            return "";
        }
        const cType = limits.text ?? (limits.name ?? countType).toLowerCase();
        const tooFew =
            limits.tooFewWord ??
            (this.options_?.tooFewWord ?? $localize`Too few`) +
                " " +
                cType +
                ", " +
                $localize`min:` +
                " " +
                limits.min +
                ".";
        const tooMany =
            limits.tooManyWord ??
            (this.options_?.tooManyWord ?? $localize`Too many`) +
                " " +
                cType +
                ", " +
                $localize`max:` +
                " " +
                limits.max +
                ".";
        if (limits.min && count < limits.min) {
            return " " + tooFew;
        }
        if (limits.max && count > limits.max) {
            return " " + tooMany;
        }
        return "";
    }

    getName(limits: ICountLimit | undefined, name: string): string {
        if (!limits) {
            return "";
        }
        if (limits.show === false) {
            return "";
        }
        if (limits.name) {
            return limits.name;
        }
        if (limits.text) {
            let s = limits.text;
            s = s[0].toUpperCase() + s.slice(1);
            return s;
        }
        return name;
    }

    @Input()
    set options(options: ICountOptions | undefined) {
        this.options_ = options;
        if (!this.options_) {
            return;
        }
        this.nameChars = this.getName(this.options_.chars, $localize`Chars`);
        this.nameWords = this.getName(this.options_.words, $localize`Words`);
        this.nameLines = this.getName(this.options_.lines, $localize`Lines`);
    }
}
