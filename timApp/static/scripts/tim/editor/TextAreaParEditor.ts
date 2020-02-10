import {wrapText} from "tim/document/editing/utils";
import {
    KEY_1,
    KEY_2,
    KEY_3,
    KEY_4,
    KEY_5,
    KEY_B,
    KEY_ENTER,
    KEY_I,
    KEY_O,
    KEY_S,
    KEY_TAB,
    KEY_Y,
} from "../util/keycodes";
import {$log, $timeout} from "../util/ngimport";
import {BaseParEditor, CURSOR, focusAfter, IEditorCallbacks, SelectionRange} from "./BaseParEditor";

export class TextAreaParEditor extends BaseParEditor {
    public editor: JQuery;
    private editorElement: HTMLTextAreaElement;

    constructor(editor: JQuery, callbacks: IEditorCallbacks) {
        super(editor, callbacks);
        this.editor = editor;
        this.editorElement = editor.get(0) as HTMLTextAreaElement;
        this.editor.keydown((e) => {
            if (e.ctrlKey) {
                if (e.keyCode === KEY_S) {
                    this.callbacks.saveClicked();
                    e.preventDefault();
                } else if (e.keyCode === KEY_B) {
                    this.surroundClicked("**", "**");
                    e.preventDefault();
                } else if (e.keyCode === KEY_I) {
                    this.italicSurroundClicked();
                    e.preventDefault();
                } else if (e.altKey) {
                    if (e.keyCode === KEY_O) {
                        this.codeBlockClicked();
                        e.preventDefault();
                    }
                } else if (e.keyCode === KEY_O) {
                    this.surroundClicked("`", "`");
                    e.preventDefault();
                } else if (e.keyCode === KEY_Y) {
                    this.commentClicked();
                    e.preventDefault();
                } else if (e.keyCode === KEY_1) {
                    this.headerClicked("#");
                    e.preventDefault();
                } else if (e.keyCode === KEY_2) {
                    this.headerClicked("##");
                    e.preventDefault();
                } else if (e.keyCode === KEY_3) {
                    this.headerClicked("###");
                    e.preventDefault();
                } else if (e.keyCode === KEY_4) {
                    this.headerClicked("####");
                    e.preventDefault();
                } else if (e.keyCode === KEY_5) {
                    this.headerClicked("#####");
                    e.preventDefault();
                } else if (e.keyCode === KEY_ENTER) {
                    this.endLineClicked();
                    e.preventDefault();
                }
            } else if (e.keyCode === KEY_TAB) {
                const outdent = e.shiftKey;
                this.indent(outdent);
                e.preventDefault();
            } else if (e.shiftKey) {
                if (e.keyCode === KEY_ENTER) {
                    this.paragraphClicked();
                    e.preventDefault();
                }
            }
            this.checkWrap();
        });
    }

    // Navigation
    @focusAfter
    undoClicked() {
        document.execCommand("undo", false, undefined);
    }

    @focusAfter
    redoClicked() {
        document.execCommand("redo", false, undefined);
    }

    scrollToCaret() {
        const editor = this.editorElement;
        const text = this.getEditorText();
        const lineHeight = parseInt(this.editor.css("line-height"), 10);
        const height = this.editor.height();
        const currentLine = text.substr(0, editor.selectionStart).split("\n").length;
        const currentScroll = this.editor.scrollTop();
        if (!currentScroll || !height) {
            return;
        }
        const currentLineY = currentLine * lineHeight;
        if (currentLineY > currentScroll + height) {
            this.editor.scrollTop(currentLineY - height);
        } else if ((currentLineY - lineHeight) < currentScroll) {
            this.editor.scrollTop(currentLineY - lineHeight);
        }
    }

    @focusAfter
    leftClicked() {
        const editor = this.editorElement;
        editor.selectionStart = editor.selectionEnd -= 1;
        this.scrollToCaret();
    }

    @focusAfter
    rightClicked() {
        const editor = this.editorElement;
        editor.selectionStart = editor.selectionEnd += 1;
        this.scrollToCaret();
    }

    @focusAfter
    upClicked() {
        const editor = this.editorElement;
        let pos = editor.selectionEnd;
        const prevLine = editor.value.lastIndexOf("\n", pos);
        const TwoBLine = editor.value.lastIndexOf("\n", prevLine - 1);
        if (prevLine === -1) {
            return;
        }
        pos = pos - prevLine;
        editor.selectionStart = editor.selectionEnd = TwoBLine + pos;
        this.scrollToCaret();
    }

    @focusAfter
    downClicked() {
        const editor = this.editorElement;
        let pos = editor.selectionEnd;
        const prevLine = editor.value.lastIndexOf("\n", pos);
        const nextLine = editor.value.indexOf("\n", pos + 1);
        if (nextLine === -1) {
            return;
        }
        pos = pos - prevLine;
        editor.selectionStart = editor.selectionEnd = nextLine + pos;
        this.scrollToCaret();
    }

    @focusAfter
    homeClicked() {
        const editor = this.editorElement;
        editor.selectionEnd = editor.selectionStart =
            editor.value.lastIndexOf("\n", editor.selectionEnd - 1) + 1;
        this.scrollToCaret();
    }

    @focusAfter
    endClicked() {
        const editor = this.editorElement;
        const pos = editor.selectionEnd;
        let i = editor.value.indexOf("\n", pos);
        if (i === -1) {
            i = editor.value.length;
        }
        editor.selectionStart = editor.selectionEnd = i;
        this.scrollToCaret();
    }

    @focusAfter
    topClicked() {
        const editor = this.editorElement;
        editor.selectionStart = editor.selectionEnd = 0;
        this.editor.scrollTop(0);
    }

    @focusAfter
    bottomClicked() {
        const editor = this.editorElement;
        editor.selectionStart = editor.selectionEnd = editor.value.length;
        this.editor.scrollTop(this.editor[0].scrollHeight);
    }

    @focusAfter
    insertClicked() {
        const input = document.getElementById("teksti") as HTMLInputElement;
        input.addEventListener("keypress", () => {
            const s = input.selectionStart;
            if (s == null) {
                return;
            }
            input.value = input.value.substr(0, s) + input.value.substr(s + 1);
            input.selectionEnd = s;
        }, false);
    }

    indent(outdent = false) {
        const tab = "    ";
        const tablength = tab.length;
        const selection = this.editor.getSelection();
        const pos = selection.start;
        const value = this.getEditorText();

        if (selection.text !== "") {
            let tempStart = selection.start;
            while (tempStart--) {
                if (value.charAt(tempStart) === "\n") {
                    selection.start = tempStart + 1;
                    break;
                }
            }

            let toIndent = value.substring(selection.start, selection.end);
            const lines = toIndent.split("\n");

            if (outdent) {
                for (let i = 0; i < lines.length; i++) {
                    if (lines[i].startsWith(tab)) {
                        lines[i] = lines[i].substring(tablength);
                    }
                }
                toIndent = lines.join("\n");
                this.editor.setSelection(selection.start, selection.end);
                this.editor.replaceSelectedText(toIndent);
                this.editor.setSelection(selection.start, selection.start + toIndent.length);
            } else {
                for (let j = 0; j < lines.length; j++) {
                    lines[j] = tab + lines[j];
                }
                toIndent = lines.join("\n");
                this.editor.setSelection(selection.start, selection.end);
                this.editor.replaceSelectedText(toIndent);
                this.editor.setSelection(selection.start, selection.start + toIndent.length);
            }

        } else {
            const left = value.substring(0, pos);
            const right = value.substring(pos);
            let edited = left + tab + right;

            if (outdent) {
                if (value.substring(pos - tablength, pos) === tab) {
                    edited = value.substring(0, pos - tablength) + right;
                    this.editor.val(edited);
                    this.editor.setSelection(pos - tablength, pos - tablength);
                }
            } else {
                this.editor.val(edited);
                this.editor.setSelection(pos + tablength, pos + tablength);
            }
        }
    }

    // Navigation
    // Style
    @focusAfter
    indentClicked() {
        this.indent();
    }

    @focusAfter
    outdentClicked() {
        this.indent(true);
    }

    @focusAfter
    surroundClicked(before: string, after: string, func?: () => boolean) {
        if (this.editor.getSelection().text === "") {
            this.selectWord();
        }
        const surrounded = (func) ? func() : this.surroundedBy(before, after);
        if (surrounded) {
            const selection = this.editor.getSelection();
            const word = selection.text;
            const start = selection.start - before.length;
            const end = selection.end + after.length;
            this.editor.setSelection(start, end);
            this.editor.replaceSelectedText(word, "select");
        } else {
            this.editor.surroundSelectedText(before, after, "select");
        }
    }

    @focusAfter
    codeBlockClicked() {
        this.editor.surroundSelectedText("```\n", "\n```", "select");
    }

    @focusAfter
    headerClicked(head: string) {
        const selection = this.selectLine(true);
        const end = selection.end;
        let line = this.editor.getSelection().text;
        let original = 0;
        while (line.startsWith("#")) {
            original++;
            line = line.substr(1);
        }
        line = line.trim();
        const headAndSpace = head + " ";
        this.editor.replaceSelectedText(headAndSpace + line);
        const n = (headAndSpace.length - original);
        this.editor.setSelection(end + n, end + n);
    }

    selectWord() {
        const nonASCIISingleCaseWordChar = /[\u00df\u0587\u0590-\u05f4\u0600-\u06ff\u3040-\u309f\u30a0-\u30ff\u3400-\u4db5\u4e00-\u9fcc\uac00-\ud7af]/;
        const isWordCharBasic = (ch: string) => {
            return /\w/.test(ch) || ch > "\x80" &&
                (ch.toUpperCase() !== ch.toLowerCase() || nonASCIISingleCaseWordChar.test(ch)) && !/\s/.test(ch);
        };
        const selection = this.editor.getSelection();
        const doc = this.getEditorText();
        const coords = this.selectLine(false);
        const line = doc.substring(coords.start, coords.end);
        const linestart = coords.start;
        const lineend = coords.end;
        if (line) {
            let tempStart = selection.start;
            while (tempStart > linestart && isWordCharBasic(doc.charAt(tempStart - 1))) {
                tempStart--;
            }
            let tempEnd = selection.start;
            while (tempEnd < lineend && isWordCharBasic(doc.charAt(tempEnd))) {
                tempEnd++;
            }
            if (tempStart !== tempEnd) {
                this.editor.setSelection(tempStart, tempEnd);
                return true;
            }
        }
        return false;
    }

    surroundedBy(before: string, after: string): boolean {
        const value = this.getEditorText();
        const selection = this.editor.getSelection();
        const word = value.substring(selection.start - before.length, selection.end + after.length);
        return (word.startsWith(before) && word.endsWith(after));
    }

    // Style
    // Insert
    /**
     * @param descDefault Placeholder for description
     * @param linkDefault Placeholder for link address
     * @param isImage true, if link is an image
     */
    @focusAfter
    linkClicked(descDefault: string, linkDefault: string, isImage: boolean) {
        const image = (isImage) ? "!" : "";
        // this.editor.replaceSelectedText(image + "[" + descDefault + "](" + linkDefault + ")");
        this.replaceSelected(image + "[", descDefault, "](", linkDefault, ")");
    }

    selectLine(select: boolean) {
        const selection = this.editor.getSelection();
        const value = this.getEditorText();
        let tempStart = selection.start;
        while (tempStart > 0) {
            tempStart--;
            if (value.charAt(tempStart) === "\n" || tempStart < 0) {
                tempStart += 1;
                break;
            }
        }
        let tempEnd = selection.start;
        while (tempEnd < value.length) {
            if (value.charAt(tempEnd) === "\n" || tempEnd >= value.length) {
                break;
            }
            tempEnd++;
        }
        if (select) {
            this.editor.setSelection(tempStart, tempEnd);
        }
        return {start: tempStart, end: tempEnd};
    }

    @focusAfter
    listClicked() {
        this.selectLine(true);
        this.editor.replaceSelectedText("- " + this.editor.getSelection().text);
    }

    @focusAfter
    insertTemplate(text: string) {
        const ci = text.indexOf(CURSOR);
        if (ci >= 0) { text = text.slice(0, ci) + text.slice(ci + 1); }
        const pluginnamehere = "PLUGINNAMEHERE";
        const searchEndIndex = this.editor.getSelection().start;
        this.editor.replaceSelectedText(text);
        const searchStartIndex = this.editor.getSelection().start;
        const index = this.getEditorText().lastIndexOf(pluginnamehere, searchStartIndex);
        if (index > searchEndIndex) {
            this.editor.setSelection(index, index + pluginnamehere.length);
        }
        if (ci >= 0) {
            this.editor.setSelection(searchEndIndex + ci);
        }
    }

    editorStartsWith(text: string) {
        return this.getEditorText().startsWith(text);
    }

    changeValue(attributes: string[], text: string) {
        const sel = this.editor.getSelection();
        const t = this.getEditorText();
        if (t.length === 0) {
            return;
        }
        let st = sel.start;
        if (t[st] === "\n") {
            st--;
        }
        let b = t.lastIndexOf("\n", st);
        let e = t.indexOf("\n", st);
        if (b < 0) {
            b = 0;
        } else {
            b++;
        }
        if (e < 0) {
            e = t.length;
        }
        if (e <= b) {
            return;
        }

        let line = t.substring(b, e);
        for (const a of attributes) {
            const ma = line.match(" *" + a);
            if (ma) {
                line = ma[0] + " " + text;
                this.editor.setSelection(b, e);
                this.editor.replaceSelectedText(line);
                break;
            }
        }
    }

    @focusAfter
    ruleClicked() {
        this.endClicked();
        this.editor.replaceSelectedText("\n#-\n---\n#-\n");
    }

    @focusAfter
    paragraphClicked() {
        this.endClicked();
        this.editor.replaceSelectedText("\n#-\n");
    }

    /*
     * Creates a comment section of selected text, comment block or comments the cursor line
     */
    @focusAfter
    commentClicked() {
        const editor = this.editorElement;
        // If text is selected surround selected text with comment brackets
        if (editor.selectionEnd - editor.selectionStart === 0) {
            const pos = editor.selectionEnd;
            const endOfLastLine = editor.value.lastIndexOf("\n", pos - 1);
            // If the cursor is in the beginning of a line, make the whole line a comment
            if (pos - endOfLastLine === 1) {
                this.selectLine(true);
            }
        }
        this.surroundClicked("{!!!", "!!!}");
    }

    @focusAfter
    endLineClicked() {
        const editor = this.editorElement;
        const selection = this.editor.getSelection();
        const value = this.getEditorText();
        const pos = selection.start;
        this.selectLine(true);
        const lineToBreak = this.editor.getSelection().text;
        let toKeepInLine;
        if (lineToBreak.length > 0) {
            toKeepInLine = value.substring(editor.selectionStart, pos);
        } else {
            toKeepInLine = "";
        }
        let toNextLine;
        if ((editor.selectionEnd - pos) > 0) {
            toNextLine = value.substring(pos, editor.selectionEnd);
        } else {
            toNextLine = "";
        }
        toNextLine = toNextLine.trim();
        this.editor.replaceSelectedText(toKeepInLine + "\\\n" + toNextLine);
        this.endClicked();
    }

    @focusAfter
    pageBreakClicked() {
        const editor = this.editorElement;
        const selection = this.editor.getSelection();
        const value = this.getEditorText();
        const cursor = selection.start;
        this.selectLine(true);

        const lineToBreak = this.editor.getSelection().text;
        let toKeepInLine;

        if (lineToBreak.length > 0) {
            toKeepInLine = value.substring(editor.selectionStart, cursor) + "\n";
        } else {
            toKeepInLine = "";
        }
        let toNextLine;
        if ((editor.selectionEnd - cursor) > 0) {
            toNextLine = value.substring(cursor, editor.selectionEnd);
        } else {
            toNextLine = "";
        }
        toNextLine = toNextLine.trim();

        const breakline = "\n#- {.printpagebreak}\n#-\n";

        this.editor.replaceSelectedText(toKeepInLine + breakline + "\n" + toNextLine);
    }

    // Insert
    // Special characters
    @focusAfter
    charClicked($event: Event, char?: string) {
        if (!$event.target) {
            return;
        }
        let character = $($event.target).text();
        $log.info(char);
        if (typeof char !== "undefined") {
            character = char;
        }
        this.editor.replaceSelectedText(character);
    }

    @focusAfter
    // Special characters
    // TEX
    indexClicked() {
        this.editor.surroundSelectedText("_{", "}", "select");
    }

    @focusAfter
    powerClicked() {
        this.editor.surroundSelectedText("^{", "}", "select");
    }

    @focusAfter
    squareClicked() {
        this.editor.surroundSelectedText("\\sqrt {", "}", "select");
    }

    getEditorText(): string {
        return this.editor.val() as string;
    }

    setEditorText(text: string) {
        this.editor.val(text);
    }

    getPosition(): SelectionRange {
        return [this.editorElement.selectionStart, this.editorElement.selectionEnd];
    }

    setPosition([start, end]: SelectionRange) {
        const editor = this.editorElement;
        editor.selectionStart = start;
        editor.selectionEnd = end;
        this.scrollToCaret();
    }

    forceWrap(force: boolean) {
        let n = this.getWrapValue();
        if (!n) { return; }
        if (n < 0) { n = -n; }
        const text = this.getEditorText();
        if (!force) {
            if (text.includes("```")) { return; }
            if (text.includes("|")) { return; }
        }
        const r = wrapText(text, n);
        if (!r.modified) { return; }
        const editor = this.editorElement;
        const start = editor.selectionStart;

        // $scope.setEditorText(r.s);

        // editor.select();
        // $timeout(() => {
        $(this.editor).val(r.s);
        $timeout(() => {
            editor.selectionStart = start;
            editor.selectionEnd = start;
        });
        // });
    }

    replaceSelected(begin: string, descDefault: string, mid: string, seltext: string, end: string) {
        if (this.editor.getSelection().text === "") {
            if (this.selectWord()) {
                descDefault = this.editor.getSelection().text;
            }
        } else {
            descDefault = this.editor.getSelection().text;
        }
        this.editor.replaceSelectedText(begin + descDefault + mid + seltext + end);
        const pos = this.editor.getSelection().start - end.length;
        this.editor.setSelection(pos - seltext.length, pos);
    }

    styleClicked(descDefault: string, styleDefault: string) {
        // this.editor.replaceSelectedText("[" + descDefault + "]{." + styleDefault + "}");
        this.replaceSelected("[", descDefault, "]{.", styleDefault, "}");
    }
}
