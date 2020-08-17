/* eslint no-underscore-dangle: ["error", { "allow": ["content_"] }] */
import $ from "jquery";
import {
    ElementRef,
    ViewChild,
    Component,
    Input,
    SimpleChanges,
    ChangeDetectorRef,
} from "@angular/core";
import {wrapText} from "tim/document/editing/utils";
import {countChars} from "../util/util";
import {IEditor} from "./editor";

@Component({
    selector: "cs-normal-editor",
    template: `
        <textarea #area class="csRunArea csEditArea no-popup-menu"
                [rows]="rows"
                [(ngModel)]="content"
                [placeholder]="placeholder"
                [disabled]="disabled">
        </textarea>`,
})
export class NormalEditorComponent implements IEditor {

    private content_: string = "";
    rows: number = 1;
    @Input() minRows: number = 1;
    @Input() maxRows: number = 100;
    @Input() placeholder: string = "";
    @Input() disabled: boolean = false;
    @ViewChild("area") private area!: ElementRef;

    constructor(private cdr: ChangeDetectorRef) { }

    get content(): string {
        return this.content_;
    }

    set content(str: string) {
        this.content_ = str;
        this.rows = countChars(this.content, "\n") + 1;
        this.checkRowBounds();
        this.cdr.detectChanges();
    }

    ngOnChanges(changes: SimpleChanges) {
        this.checkRowBounds();
    }

    checkRowBounds() {
        if (this.rows < this.minRows) {
            this.rows = this.minRows;
        } else if (this.maxRows != -1 && this.rows > this.maxRows) {
            this.rows = this.maxRows;
        }
    }

    ngAfterViewInit() {
        $(this.area.nativeElement).on("keydown", (event) => { // TODO: check that this gets disabled when not used
            if (event.which === 9) { // tab key
                event.preventDefault();
                if (event.shiftKey) {
                    return;
                }
                this.insert("    ");
                return;
            }
        });
    }

    insert(str: string, strPos?: number): void {
        const txtarea = this.area.nativeElement as HTMLTextAreaElement;
        const scrollPos = txtarea.scrollTop;
        strPos = strPos ?? txtarea.selectionStart ?? 0;

        const cont = this.content;
        this.content = cont.slice(0, strPos) + str + cont.slice(strPos);

        strPos = strPos + str.length;
        if (txtarea.selectionStart) {
            txtarea.selectionStart = strPos;
            txtarea.selectionEnd = strPos;
            txtarea.focus();
        }
        txtarea.scrollTop = scrollPos;
    }

    doWrap(wrap: number) {
        const r = wrapText(this.content, wrap);
        if (!r.modified) { return; }

        const element = this.area.nativeElement as HTMLTextAreaElement;
        const start = element.selectionStart;

        this.content = r.s;
        element.value = r.s;
        element.selectionStart = start;
        element.selectionEnd = start;
    }
}
