/* eslint no-underscore-dangle: ["error", { "allow": ["content_"] }] */
import {
    ElementRef,
    ViewChild,
    Component,
    Input,
    SimpleChanges,
} from "@angular/core";
import {IEditor} from "./editor";

@Component({
    selector: "cs-parsons-editor",
    template: `<div #area class="no-popup-menu"></div>`,
})
export class ParsonsEditorComponent implements IEditor {
    private parson?: {
        join: (str: string) => string;
        clear: () => void;
        check: (str: string) => string;
    };
    private content_?: string;
    @Input() shuffle: boolean = false;
    @Input() maxcheck?: number;
    @Input() notordermatters: boolean = false;
    @Input() base: string = "";
    @Input() styleWords: string = "";
    @Input() words: boolean = false;
    @ViewChild("area") area!: ElementRef;

    ngOnChanges(changes: SimpleChanges) {
        if (this.parson) {
            // TODO: something smarter than recreating the whole thing on changes
            this.content = this.content; // creates a new parson
        }
    }

    ngAfterViewInit() {
        this.createParsons(this.content);
    }

    get content(): string {
        return this.content_ ?? this.base;
    }
    set content(str: string) {
        this.parson?.clear();

        this.createParsons(str);
    }

    async createParsons(content: string) {
        this.content_ = content;

        const csp = await import("../cs-parsons/csparsons");
        const parson = new csp.CsParsonsWidget({
            sortable: this.area.nativeElement as Element,
            words: this.words,
            minWidth: "40px",
            shuffle: this.shuffle,
            styleWords: this.styleWords,
            maxcheck: this.maxcheck,
            notordermatters: this.notordermatters,
            onChange: (p) => {
                this.content_ = p.join("\n");
            },
        });
        parson.init(this.base, this.content_);
        parson.show();
        this.parson = parson;
    }

    check(): string {
        return this.parson?.check(this.content) ?? "";
    }
}
