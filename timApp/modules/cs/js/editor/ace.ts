/* eslint no-underscore-dangle: ["error", { "allow": ["content_", "minRows_", "maxRows_", "languageMode_"] }] */
/* eslint-disable @typescript-eslint/tslint/config -- decorators cause issues on setters */
import $ from "jquery";
import {Ace} from "ace-builds/src-noconflict/ace";
import {
    ElementRef,
    ViewChild,
    Component,
    Input,
} from "@angular/core";
import {wrapText} from "tim/document/editing/utils";
import {IEditor} from "./editor";

type IAceEditor = Ace.Editor;

@Component({
    selector: "cs-ace-editor",
    template: `
        <pre #area style="width: 100%;" [style.display]="null">
        </pre>`,
})
export class AceEditorComponent implements IEditor {
    private aceEditor!: IAceEditor;
    private languageMode_: string = "text";
    private minRows_: number = 1;
    private maxRows_: number = 100;
    private content_?: string;
    @ViewChild("area") area!: ElementRef;
    @Input() placeholder: string = ""; // TODO: make this work

    @Input()
    set languageMode(lang: string) {
        this.languageMode_ = lang;
        this.aceEditor?.getSession().setMode("ace/mode/" + lang);
    }

    @Input()
    set minRows(rows: number) {
        this.minRows_ = rows;
        this.aceEditor?.setOption("minLines", rows);
    }
    @Input()
    set maxRows(rows: number) {
        this.maxRows_ = rows;
        this.aceEditor?.setOption("maxLines", rows);
    }

    async ngAfterViewInit() {
        const ace = (await import("tim/editor/ace")).ace;

        const editor = ace.edit(this.area.nativeElement);
        this.aceEditor = editor;

        const session = editor.getSession();
        session.setUndoManager(new ace.UndoManager());
        session.setMode("ace/mode/" + this.languageMode_);

        editor.setOptions({
            enableBasicAutocompletion: true,
            enableLiveAutocompletion: false,
            enableSnippets: true,
            minLines: this.minRows_,
            maxLines: this.maxRows_,
            // showTokenInfo: true
        });
        editor.setFontSize(15);
        if ($(this.area.nativeElement).parents(".reveal").length > 0) {
            editor.setFontSize(25);
        }
        editor.getSession().setUseWorker(false); // syntax check away
        editor.renderer.setScrollMargin(12, 12, 0, 0);
        this.content = this.content_ ?? "";
        this.content_ = undefined;
    }

    get content(): string {
        return this.aceEditor?.getValue() ?? this.content_;
    }
    set content(str: string) {
        if (this.aceEditor) {
            this.aceEditor.setValue(str, 1);
        } else {
            this.content_ = str;
        }
    }

    insert(str: string, strPos?: number): void {
        const sess = this.aceEditor.getSession();
        let cursor;
        if (strPos) {
            cursor = sess.getDocument().indexToPosition(strPos, 0);
        } else {
            cursor = this.aceEditor.getCursorPosition();
        }
        sess.insert(cursor, str); // TODO: might have to move cursor
    }

    doWrap(wrap: number) {
        const r = wrapText(this.content, wrap);
        if (!r.modified) { return; }

        const editor = this.aceEditor;
        const sess = editor.getSession();
        let cursor = editor.getCursorPosition();
        const index = sess.getDocument().positionToIndex(cursor, 0);
        this.content = r.s;
        cursor = sess.getDocument().indexToPosition(index, 0);
        editor.selection.moveCursorToPosition(cursor);
        editor.selection.clearSelection();
    }
}
