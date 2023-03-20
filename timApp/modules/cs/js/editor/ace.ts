/* eslint no-underscore-dangle: ["error", { "allow": ["content_", "minRows_", "maxRows_", "languageMode_", "disabled_"] }] */
import $ from "jquery";
import type {Ace} from "ace-builds/src-noconflict/ace";
import {Component, ElementRef, Input, ViewChild} from "@angular/core";
import {wrapText} from "tim/document/editing/utils";
import type {IEditor} from "./editor";
import {CURSOR} from "./editor";

type IAceEditor = Ace.Editor;

@Component({
    selector: "cs-ace-editor",
    template: `
        <pre #area style="width: 100%;" [style.display]="null">
        </pre>`,
})
export class AceEditorComponent implements IEditor {
    private aceEditor?: IAceEditor;
    private languageMode_: string = "text";
    private minRows_: number = 1;
    private maxRows_: number = 100;
    private content_?: string;
    private editorreadonly: boolean = false;
    private disabled_: boolean = false;
    formulaFunction = function () {};
    @ViewChild("area") area!: ElementRef;
    @Input() placeholder: string = ""; // TODO: make this work

    @Input()
    set languageMode(lang: string) {
        this.languageMode_ = lang;
        this.aceEditor?.getSession().setMode("ace/mode/" + lang);
    }

    updateDisabled() {
        if (this.aceEditor) {
            if (this.disabled) {
                this.aceEditor.setOptions({
                    readOnly: true,
                    highlightActiveLine: false,
                    highlightGutterLine: false,
                });
                (
                    this.aceEditor.renderer.$cursorLayer as unknown as {
                        element: HTMLElement;
                    }
                ).element.style.opacity = "0";
            } else {
                this.aceEditor.setOptions({
                    readOnly: false,
                    highlightActiveLine: true,
                    highlightGutterLine: true,
                });
                (
                    this.aceEditor.renderer.$cursorLayer as unknown as {
                        element: HTMLElement;
                    }
                ).element.style.opacity = "1";
            }
        }
        this.aceEditor?.setOption("readOnly", this.disabled);
    }

    focus() {
        this.aceEditor?.focus();
    }

    @Input()
    set disabled(d: boolean) {
        this.disabled_ = d;
        this.updateDisabled();
    }

    get disabled() {
        return this.disabled_;
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

    setReadOnly(b: boolean) {
        this.disabled = b;
        this.editorreadonly = b;
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
        this.updateDisabled();
        this.content = this.content_ ?? "";
        this.content_ = undefined;

        this.aceEditor?.commands.addCommand({
            name: "toggleCommentLines",
            bindKey: {
                win: "Ctrl-'",
                mac: "Command-'",
            },
            exec: () => {
                this.aceEditor?.toggleCommentLines();
            },
        });
        this.aceEditor?.commands.addCommand({
            name: "addFormula",
            bindKey: {
                win: "Ctrl-E",
                mac: "Command-E",
            },
            exec: () => {
                this.formulaFunction();
            },
        });
    }

    get content(): string {
        return this.aceEditor?.getValue() ?? this.content_ ?? "";
    }

    set content(str: string) {
        if (this.aceEditor) {
            this.aceEditor.setValue(str, 1);
        } else {
            this.content_ = str;
        }
    }

    insert(str: string, strPos?: number): void {
        if (!this.aceEditor) {
            return;
        }
        const sess = this.aceEditor.getSession();
        let cursor;
        let back = -1;
        const ci = str.indexOf(CURSOR); // check if there is a cursor marker
        if (ci >= 0) {
            str = str.replace(CURSOR, "");
            back = str.length - ci;
        }
        if (strPos) {
            cursor = sess.getDocument().indexToPosition(strPos, 0);
            sess.insert(cursor, str); // TODO: might have to move cursor
        } else {
            sess.replace(sess.selection.getRange(), str);
            if (back > 0) {
                this.aceEditor.navigateLeft(back);
            }
        }
    }

    doWrap(wrap: number) {
        if (!this.aceEditor) {
            return;
        }

        const r = wrapText(this.content, wrap);
        if (!r.modified) {
            return;
        }

        const editor = this.aceEditor;
        const sess = editor.getSession();
        let cursor = editor.getCursorPosition();
        const index = sess.getDocument().positionToIndex(cursor, 0);
        this.content = r.s;
        cursor = sess.getDocument().indexToPosition(index, 0);
        editor.selection.moveCursorToPosition(cursor);
        editor.selection.clearSelection();
    }
    addFormulaEditorOpenHandler(cb: () => void): void {
        this.formulaFunction = cb;
    }
}
