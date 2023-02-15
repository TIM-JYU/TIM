/* eslint no-underscore-dangle: ["error", { "allow": ["content_"] }] */
import {EventEmitter} from "@angular/core";
import {Output} from "@angular/core";
import type {SimpleChanges} from "@angular/core";
import {ElementRef, ViewChild, Component, Input} from "@angular/core";
import type {IEditor, IParsonsHtmlLine} from "./editor";

@Component({
    selector: "cs-parsons-editor",
    template: `<div #area class="no-popup-menu"></div>`,
})
export class ParsonsEditorComponent implements IEditor {
    private parson?: {
        join: (str: string) => string;
        clear: () => void;
        check: (str: string, correct?: number[], styles?: string[]) => string;
    };
    private content_?: string;
    @Input() shuffle: boolean = false;
    @Input() maxcheck?: number;
    @Input() shuffleHost?: boolean;
    @Input() notordermatters: boolean = false;
    @Input() base: string = "";
    @Input() styleWords: string = "";
    @Input() parsonsHTML?: IParsonsHtmlLine[];
    @Input() words: boolean = false;
    @ViewChild("area") area!: ElementRef;
    @Output("change") private contentChange: EventEmitter<{
        content: string;
        init: boolean;
    }> = new EventEmitter(true);
    ngOnChanges(changes: SimpleChanges) {
        if (this.parson) {
            // TODO: something smarter than recreating the whole thing on changes
            this.content = this.content; // creates a new parson
        }
    }

    focus() {}

    ngAfterViewInit() {
        this.createParsons(this.content);
    }

    setReadOnly(b: boolean) {
        // TODO: implement readonly
    }

    get content(): string {
        return this.content_ ?? this.base;
    }
    set content(str: string) {
        this.parson?.clear();

        this.createParsons(str);
    }

    async createParsons(content: string) {
        const csp = await import("../cs-parsons/csparsons");
        const parson = new csp.CsParsonsWidget({
            sortable: this.area.nativeElement as Element,
            words: this.words,
            minWidth: "40px",
            shuffle: this.shuffle,
            styleWords: this.styleWords,
            parsonsHTML: this.parsonsHTML,
            maxcheck: this.maxcheck,
            shuffleHost: this.shuffleHost,
            notordermatters: this.notordermatters,
            onChange: (p) => {
                this.contentChanged(p.join("\n"));
            },
        });
        parson.init(this.base, content);
        parson.show();
        this.parson = parson;
        this.content_ = parson.join("\n");
        this.contentChange.emit({content: this.content_, init: true});
    }

    contentChanged(content: string) {
        this.content_ = content;
        this.contentChange.emit({content, init: false});
    }

    check(): string {
        if (this.shuffleHost) {
            return "";
        }
        return this.parson?.check(this.content) ?? "";
    }

    checkHost(correct?: number[], styles?: string[]): string {
        return this.parson?.check(this.content, correct, styles) ?? "";
    }
}
