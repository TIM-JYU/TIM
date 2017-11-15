import {$log, $timeout} from "../ngimport";
import {BaseParEditor, focusAfter, IEditorCallbacks} from "./BaseParEditor";
import {wrapText} from "../controllers/view/editing";

export class TextAreaParEditor extends BaseParEditor {
    public editor: JQuery;
    private editorElement: HTMLTextAreaElement;

    constructor(editor: JQuery, callbacks: IEditorCallbacks) {
        super(editor, callbacks);
        this.editor = editor;
        this.editorElement = editor.get(0) as HTMLTextAreaElement;
        this.editor.keydown((e) => {
            if (e.ctrlKey) {
                if (e.keyCode === 83) {
                    this.callbacks.saveClicked();
                    e.preventDefault();
                } else if (e.keyCode === 66) {
                    this.surroundClicked("**", "**");
                    e.preventDefault();
                } else if (e.keyCode === 73) {
                    this.italicSurroundClicked();
                    e.preventDefault();
                } else if (e.altKey) {
                    if (e.keyCode === 79) {
                        this.codeBlockClicked();
                        e.preventDefault();
                    }
                } else if (e.keyCode === 79) {
                    this.surroundClicked("`", "`");
                    e.preventDefault();
                } else if (e.keyCode === 89) {
                    this.commentClicked();
                    e.preventDefault();
                } else if (e.keyCode === 49) {
                    this.headerClicked("#");
                    e.preventDefault();
                } else if (e.keyCode === 50) {
                    this.headerClicked("##");
                    e.preventDefault();
                } else if (e.keyCode === 51) {
                    this.headerClicked("###");
                    e.preventDefault();
                } else if (e.keyCode === 52) {
                    this.headerClicked("####");
                    e.preventDefault();
                } else if (e.keyCode === 13) {
                    this.endLineClicked();
                    e.preventDefault();
                }
            } else if (e.keyCode === 9) {
                const outdent = e.shiftKey;
                this.indent(outdent);
                e.preventDefault();
            } else if (e.shiftKey) {
                if (e.keyCode === 13) {
                    this.paragraphClicked();
                    e.preventDefault();
                }
            }
            this.checkWrap();
        });
    }

    //Navigation
    @focusAfter
    undoClicked() {
        document.execCommand("undo", false, null);
    }

    @focusAfter
    redoClicked() {
        document.execCommand("redo", false, null);
    }

    scrollToCaret() {
        const editor = this.editorElement;
        const text = this.editor.val();
        const lineHeight = parseInt(this.editor.css("line-height"));
        const height = this.editor.height();
        const currentLine = text.substr(0, editor.selectionStart).split("\n").length;
        const currentScroll = this.editor.scrollTop();
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
        this.editor.scrollTop(this.editor.prop("scrollHeight"));
    }

    @focusAfter
    insertClicked() {
        const input = document.getElementById("teksti") as HTMLInputElement;
        input.addEventListener("keypress", () => {
            const s = input.selectionStart;
            input.value = input.value.substr(0, s) + input.value.substr(s + 1);
            input.selectionEnd = s;
        }, false);
    }

    indent(outdent = false) {
        const tab = "    ";
        const tablength = tab.length;
        const selection = this.editor.getSelection();
        const pos = selection.start;
        const value = this.editor.val();

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
                    if (lines[i].substring(0, tablength) === tab) {
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

    //Navigation
    //Style
    @focusAfter
    indentClicked() {
        this.indent();
    }

    @focusAfter
    outdentClicked() {
        this.indent(true);
    }

    @focusAfter
    surroundClicked(before, after, func = null) {
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
    headerClicked(head) {
        const selection = this.selectLine(true);
        const tempEnd = selection.end;
        let line = this.editor.getSelection().text;
        let original = 0;
        while (line.charAt(0) === "#") {
            original++;
            line = line.substr(1);
        }
        line = line.trim();
        this.editor.replaceSelectedText(head + " " + line);
        this.editor.setSelection(tempEnd + (head.length - original), tempEnd + (head.length - original));
    }

    selectWord() {
        const nonASCIISingleCaseWordChar = /[\u00df\u0587\u0590-\u05f4\u0600-\u06ff\u3040-\u309f\u30a0-\u30ff\u3400-\u4db5\u4e00-\u9fcc\uac00-\ud7af]/;
        const isWordCharBasic = (ch) => {
            return /\w/.test(ch) || ch > "\x80" &&
                (ch.toUpperCase() !== ch.toLowerCase() || nonASCIISingleCaseWordChar.test(ch)) && !/\s/.test(ch);
        };
        const selection = this.editor.getSelection();
        const doc = this.editor.val();
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
        const value = this.editor.val();
        const selection = this.editor.getSelection();
        const word = value.substring(selection.start - before.length, selection.end + after.length);
        return (word.indexOf(before) === 0 && word.lastIndexOf(after) === (word.length - after.length));
    }

    //Style
    //Insert
    /**
     * @param descDefault Placeholder for description
     * @param linkDefault Placeholder for link address
     * @param isImage true, if link is an image
     */
    @focusAfter
    linkClicked(descDefault, linkDefault, isImage) {
        const image = (isImage) ? "!" : "";
        if (this.editor.getSelection().text === "") {
            if (this.selectWord()) {
                descDefault = this.editor.getSelection().text;
            }
        } else {
            descDefault = this.editor.getSelection().text;
        }
        this.editor.replaceSelectedText(image + "[" + descDefault + "](" + linkDefault + ")");
    }

    selectLine(select) {
        const selection = this.editor.getSelection();
        const value = this.editor.val();
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
    insertTemplate(text) {
        const pluginnamehere = "PLUGINNAMEHERE";
        const searchEndIndex = this.editor.getSelection().start;
        this.editor.replaceSelectedText(text);
        const searchStartIndex = this.editor.getSelection().start;
        const index = this.editor.val().lastIndexOf(pluginnamehere, searchStartIndex);
        if (index > searchEndIndex) {
            this.editor.setSelection(index, index + pluginnamehere.length);
        }
    }

    editorStartsWith(text) {
        return this.editor.val().startsWith(text);
    }

    changeValue(attributes, text) {
        const sel = this.editor.getSelection();
        const t = this.editor.val();
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
        for (let i = 0; i < attributes.length; i++) {
            const ma = line.match(" *" + attributes[i]);
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
        const value = this.editor.val();
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
        var editor = this.editorElement;
        var selection = this.editor.getSelection();
        var value = this.editor.val();
        var cursor = selection.start;
        this.selectLine(true);

        var lineToBreak = this.editor.getSelection().text;
        var toKeepInLine;

        if (lineToBreak.length > 0) {
            toKeepInLine = value.substring(editor.selectionStart, cursor) + "\n";
        } else {
            toKeepInLine = "";
        }
        var toNextLine;
        if ((editor.selectionEnd - cursor) > 0) {
            toNextLine = value.substring(cursor, editor.selectionEnd);
        } else {
            toNextLine = "";
        }
        toNextLine = toNextLine.trim();

        var breakline = '\n#-{print="false"}\n<div id="CSSpagebreak"><p>!================!Page Break!================!</p></div>\n#-\n';

        this.editor.replaceSelectedText(toKeepInLine + breakline + "\n" + toNextLine);
    }

    //Insert
    //Special characters
    @focusAfter
    charClicked($event, char) {
        let character = $($event.target).text();
        $log.info(char);
        if (typeof char !== "undefined") {
            character = char;
        }
        this.editor.replaceSelectedText(character);
    }

    @focusAfter
    //Special characters
    //TEX
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
        return this.editor.val();
    }

    setEditorText(text: string) {
        this.editor.val(text);
    }

    setPosition(pos) {
        const editor = this.editor.get(0) as HTMLTextAreaElement;
        editor.selectionStart = editor.selectionEnd = pos;
        this.scrollToCaret();
    }

    forceWrap(force: boolean) {
        var n = this.getWrapValue();
        if (!n) return;
        if (n < 0) n = -n;
        var text = this.getEditorText();
        if (!force) {
            if (text.indexOf("```") >= 0) return;
            if (text.indexOf("|") >= 0) return;
        }
        var r = wrapText(text, n);
        if (!r.modified) return;
        let editor = this.editorElement;
        var start = editor.selectionStart;

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
}
