/* eslint no-underscore-dangle: ["error", { "allow": ["content_"] }] */
import {EventEmitter} from "@angular/core";
import {Output} from "@angular/core";
import type {SimpleChanges} from "@angular/core";
import {ElementRef, ViewChild, Component, Input} from "@angular/core";
import {ICsParsonsOptions} from "../cs-parsons/csparsons";
import type {IEditor} from "./editor";

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
    @Input() parsonsOptions?: ICsParsonsOptions;
    @Input() base: string = "";

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
        if (!this.parsonsOptions) {
            this.parsonsOptions = {};
        }
        this.parsonsOptions.onChange = (p) => {
            this.contentChanged(p.join("\n"));
        };
        this.parsonsOptions.sortable = this.area.nativeElement as Element;
        const parson = new csp.CsParsonsWidget(this.parsonsOptions);
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
        if (this.parsonsOptions?.shuffleHost) {
            return "";
        }
        return this.parson?.check(this.content) ?? "";
    }

    checkHost(correct?: number[], styles?: string[]): string {
        return this.parson?.check(this.content, correct, styles) ?? "";
    }
}
