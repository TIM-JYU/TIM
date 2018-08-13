import {wrapText} from "../document/editing/editing";
import {$log, $timeout} from "../util/ngimport";
import {IAceEditor} from "./ace-types";
import {BaseParEditor, CURSOR, focusAfter, IEditorCallbacks} from "./BaseParEditor";

interface ISnippetManager {
    insertSnippet(editor: AceAjax.Editor, text: string): void;
}

export class AceParEditor extends BaseParEditor {
    public editor: IAceEditor;
    private snippetManager: ISnippetManager;

    constructor(ace: AceAjax.Ace, editor: AceAjax.Editor, callbacks: IEditorCallbacks, mode: string = "ace/mode/markdown") {
        super(editor as IAceEditor, callbacks);
        this.editor = editor as IAceEditor;
        this.snippetManager = ace.require("ace/snippets").snippetManager;
        const line = editor.renderer.lineHeight;
        const containertop = $(".editorContainer").position().top;
        const height = ($(window).innerHeight() || 700) - containertop;
        const max = Math.floor((height / 2) / line);

        this.editor.$blockScrolling = Infinity;
        this.editor.renderer.setPadding(10);
        this.editor.renderer.setScrollMargin(2, 2, 2, 40);
        this.editor.renderer.setVScrollBarAlwaysVisible(true);
        this.editor.getSession().setMode(mode);
        this.editor.getSession().setUseWrapMode(false);
        this.editor.getSession().setWrapLimitRange(0, 79);
        this.editor.setOptions({
            maxLines: max,
            minLines: 5,
            autoScrollEditorIntoView: true,
            hScrollBarAlwaysVisible: false,
            vScrollBarAlwaysVisible: false,
            enableBasicAutocompletion: true,
            enableLiveAutocompletion: true,
        });

        this.editor.commands.addCommand({
            name: "saveFile",
            bindKey: {
                win: "Ctrl-S",
                mac: "Command-S",
                sender: "editor|cli",
            },
            exec: () => {
                this.callbacks.saveClicked();
            },
        });
        this.editor.commands.addCommand({
            name: "bold",
            bindKey: {
                win: "Ctrl-B",
                mac: "Command-B",
                sender: "editor|cli",
            },
            exec: () => {
                this.surroundClicked("**", "**");
            },
        });
        this.editor.commands.addCommand({
            name: "italic",
            bindKey: {
                win: "Ctrl-I",
                mac: "Command-I",
                sender: "editor|cli",
            },
            exec: () => {
                this.surroundClicked("*", "*", () => this.surroundedByItalic());
            },
        });
        this.editor.commands.addCommand({
            name: "code",
            bindKey: {
                win: "Ctrl-O",
                mac: "Command-O",
                sender: "editor|cli",
            },
            exec: () => {
                this.surroundClicked("`", "`");
            },
        });
        this.editor.commands.addCommand({
            name: "codeBlock",
            bindKey: {
                win: "Ctrl-Alt-O",
                mac: "Command-Alt-O",
                sender: "editor|cli",
            },
            exec: () => {
                this.codeBlockClicked();
            },
        });
        this.editor.commands.addCommand({
            name: "h1",
            bindKey: {
                win: "Ctrl-1",
                mac: "Command-1",
                sender: "editor|cli",
            },
            exec: () => {
                this.headerClicked("#");
            },
        });
        this.editor.commands.addCommand({
            name: "h2",
            bindKey: {
                win: "Ctrl-2",
                mac: "Command-2",
                sender: "editor|cli",
            },
            exec: () => {
                this.headerClicked("##");
            },
        });
        this.editor.commands.addCommand({
            name: "h3",
            bindKey: {
                win: "Ctrl-3",
                mac: "Command-3",
                sender: "editor|cli",
            },
            exec: () => {
                this.headerClicked("###");
            },
        });
        this.editor.commands.addCommand({
            name: "h4",
            bindKey: {
                win: "Ctrl-4",
                mac: "Command-4",
                sender: "editor|cli",
            },
            exec: () => {
                this.headerClicked("####");
            },
        });
        this.editor.commands.addCommand({
            name: "endLine",
            bindKey: {
                win: "Ctrl-Enter",
                mac: "Command-Enter",
                sender: "editor|cli",
            },
            exec: () => {
                this.endLineClicked();
            },
        });
        this.editor.commands.addCommand({
            name: "insertParagraph",
            bindKey: {
                win: "Shift-Enter",
                mac: "Shift-Enter",
                sender: "editor|cli",
            },
            exec: () => {
                this.paragraphClicked();
            },
        });
        this.editor.commands.addCommand({
            name: "commentBlock",
            bindKey: {
                win: "Ctrl-Y",
                mac: "Command-Y",
                sender: "editor|cli",
            },
            exec: () => {
                this.commentClicked();
            },
        });
        this.editor.commands.addCommand({
            name: "pageBreak",
            bindKey: {
                win: "Ctrl-M",
                mac: "Command-M",
                sender: "editor|cli",
            },
            exec: () => {
                this.pageBreakClicked();
            },
        });
        this.editor.keyBinding.addKeyboardHandler(
            () => {
                this.checkWrap();
            }, null,
        );
    }

    // Navigation

    @focusAfter
    undoClicked() {
        this.editor.undo();
    }

    @focusAfter
    redoClicked() {
        this.editor.redo();
    }

    @focusAfter
    pageBreakClicked() {
        const cursor = this.editor.getCursorPosition();
        const line = this.editor.session.getLine(cursor.row);
        const range = this.editor.getSelection().getRange();
        range.start.column = 0;
        range.end.column = line.length;

        let toKeepInLine;
        if (line.length > 0) {
            toKeepInLine = line.substring(0, cursor.column) + "\n";
        } else {
            toKeepInLine = "";
        }
        let toNextLine;
        if ((line.length - cursor.column) > 0) {
            toNextLine = line.substring(cursor.column, line.length);
        } else {
            toNextLine = "";
        }
        toNextLine = toNextLine.trim();

        const breakline = '\n#-{print="false"}\n<div id="CSSpagebreak"><p>!================!Page Break!================!</p></div>\n#-\n';

        this.editor.selection.setRange(range, false);
        this.editor.insert(toKeepInLine + breakline + "\n" + toNextLine);
    }

    gotoCursor() {
        const firstrow = this.editor.renderer.getFirstFullyVisibleRow();
        const lastrow = this.editor.renderer.getLastFullyVisibleRow();
        const cursor = this.editor.getCursorPosition();
        if (cursor.row < firstrow) {
            this.editor.renderer.scrollToLine(cursor.row, false, true, () => {
            });
        } else if (cursor.row > lastrow) {
            this.editor.renderer.scrollToLine(cursor.row - (lastrow - firstrow), false, true, () => {
            });
        }
    }

    @focusAfter
    leftClicked() {
        this.editor.navigateLeft(1);
        this.gotoCursor();
    }

    @focusAfter
    rightClicked() {
        this.editor.navigateRight(1);
        this.gotoCursor();
    }

    @focusAfter
    upClicked() {
        this.editor.navigateUp(1);
        this.gotoCursor();
    }

    @focusAfter
    downClicked() {
        this.editor.navigateDown(1);
        this.gotoCursor();
    }

    @focusAfter
    homeClicked() {
        this.editor.navigateLineStart();
        this.gotoCursor();
    }

    @focusAfter
    endClicked() {
        this.editor.navigateLineEnd();
        this.gotoCursor();
    }

    @focusAfter
    topClicked() {
        this.editor.navigateFileStart();
        this.gotoCursor();
    }

    @focusAfter
    bottomClicked() {
        this.editor.navigateFileEnd();
        this.gotoCursor();
    }

    @focusAfter
    insertClicked() {
        this.editor.setOverwrite(!this.editor.getOverwrite());
    }

    // Navigation
    // Style
    @focusAfter
    indentClicked() {
        this.editor.indent();
    }

    @focusAfter
    outdentClicked() {
        this.editor.blockOutdent();
    }

    @focusAfter
    surroundClicked(before: string, after: string, func?: () => boolean) {
        if ((this.editor.session.getTextRange(this.editor.getSelectionRange()) === "")) {
            this.selectWord();
        }
        const text = this.editor.session.getTextRange(this.editor.getSelectionRange());
        const surrounded = (func) ? func() : this.surroundedBy(before, after);
        if (surrounded) {
            const range = this.editor.getSelectionRange();
            range.start.column -= before.length;
            range.end.column += after.length;
            this.editor.selection.setRange(range, false);
            this.snippetManager.insertSnippet(this.editor, "${0:" + text + "}");
        } else {
            this.snippetManager.insertSnippet(this.editor, before + "${0:$SELECTION}" + after);
        }
    }

    selectWord() {
        const cursor = this.editor.getCursorPosition();
        const wordrange = this.editor.getSession().getAWordRange(cursor.row, cursor.column);
        const word = (this.editor.session.getTextRange(wordrange));
        if (/^\s*$/.test(word)) {
            return false;
        }
        const wordtrim = word.trim();
        const difference = word.length - wordtrim.length;
        wordrange.end.column -= difference;
        this.editor.selection.setRange(wordrange, false);
        return true;
    }

    surroundedBy(before: string, after: string): boolean {
        const range = this.editor.getSelectionRange();
        range.start.column -= before.length;
        range.end.column += after.length;
        const word = (this.editor.session.getTextRange(range));
        return (word.indexOf(before) === 0 && word.lastIndexOf(after) === (word.length - after.length));
    }

    @focusAfter
    codeBlockClicked() {
        this.snippetManager.insertSnippet(this.editor, "```\n${0:$SELECTION}\n```");
    }

    @focusAfter
    headerClicked(head: string) {
        const cursor = this.editor.getCursorPosition();
        let line = this.editor.session.getLine(cursor.row);
        const range = this.editor.getSelection().getRange();
        range.start.column = 0;
        range.end.column = line.length;
        while (line.charAt(0) === "#") {
            line = line.substr(1);
        }
        line = line.trim();
        this.editor.selection.setRange(range, false);
        this.editor.insert(head + " " + line);
    }

    // Style
    // Insert
    /**
     * @param descDefault Placeholder for description
     * @param styleDefault Placeholder for link address
     */
    @focusAfter
    styleClicked(descDefault: string, styleDefault: string) {
        if ((this.editor.session.getTextRange(this.editor.getSelectionRange()) === "")) {
            if (this.selectWord()) {
                descDefault = this.editor.session.getTextRange(this.editor.getSelectionRange());
            }
        } else {
            descDefault = this.editor.session.getTextRange(this.editor.getSelectionRange());
        }
        this.snippetManager.insertSnippet(this.editor, "[" + descDefault + "]{.${0:" + styleDefault + "}}");
    }

    /**
     * @param descDefault Placeholder for description
     * @param linkDefault Placeholder for link address
     * @param isImage true, if link is an image
     */
    @focusAfter
    linkClicked(descDefault: string, linkDefault: string, isImage: boolean) {
        const image = (isImage) ? "!" : "";
        if ((this.editor.session.getTextRange(this.editor.getSelectionRange()) === "")) {
            if (this.selectWord()) {
                descDefault = this.editor.session.getTextRange(this.editor.getSelectionRange());
            }
        } else {
            descDefault = this.editor.session.getTextRange(this.editor.getSelectionRange());
        }
        this.snippetManager.insertSnippet(this.editor, image + "[" + descDefault + "](${0:" + linkDefault + "})");
    }

    @focusAfter
    listClicked() {
        this.snippetManager.insertSnippet(this.editor, "- ${0:$SELECTION}");
    }

    @focusAfter
    paragraphClicked() {
        this.editor.navigateLineEnd();
        this.snippetManager.insertSnippet(this.editor, "\n#-\n");
    }

    @focusAfter
    endLineClicked() {
        const pos = this.editor.getCursorPosition();
        const line = this.editor.session.getLine(pos.row);
        const range = this.editor.getSelection().getRange();
        range.start.column = 0;
        range.end.column = line.length;
        let toKeepInLine;
        if (line.length > 0) {
            toKeepInLine = line.substring(0, pos.column);
        } else {
            toKeepInLine = "";
        }
        let toNextLine;
        if ((line.length - pos.column) > 0) {
            toNextLine = line.substring(pos.column, line.length);
        } else {
            toNextLine = "";
        }
        toNextLine = toNextLine.trim();
        this.editor.selection.setRange(range, false);
        this.editor.insert(toKeepInLine + "\\" + "\n" + toNextLine);
    }

    @focusAfter
    insertTemplate(text: string) {
        const ci = text.indexOf(CURSOR);
        if (ci >= 0) { text = text.slice(0, ci) + text.slice(ci + 1); }
        const range = this.editor.getSelectionRange();
        const start = range.start;
        this.snippetManager.insertSnippet(this.editor, text);
        const line = this.editor.session.getLine(start.row);
        const pluginnamehere = "PLUGINNAMEHERE";
        const index = line.lastIndexOf(pluginnamehere);
        if (index > -1) {
            range.start.column = index;
            range.end.row = start.row;
            range.end.column = index + pluginnamehere.length;
            this.editor.selection.setRange(range, false);
        }
        if (ci >= 0) {
            const pos = this.editor.session.doc.positionToIndex(start, 0);
            const r = this.editor.session.doc.indexToPosition(pos + ci, 0);
            range.start = r;
            range.end = r;
            this.editor.selection.setRange(range, false);
        }
    }

    editorStartsWith(text: string) {
        return this.editor.session.getLine(0).startsWith(text);
    }

    editorGetRow(i: number) {
        return this.editor.session.getLine(i);
    }

    changeValue(attributes: string[], text: string) {
        const pos = this.editor.getCursorPosition();
        let line = this.editor.session.getLine(pos.row);
        for (let i = 0; i < attributes.length; i++) {
            const ma = line.match(" *" + attributes[i]);
            if (ma) {
                const len = line.length;
                line = ma[0] + " " + text;
                const range = this.editor.getSelectionRange();
                range.start.column = 0;
                range.end.column = len + 1;
                this.editor.session.replace(range, line);
                break;
            }
        }

    }

    @focusAfter
    ruleClicked() {
        this.editor.navigateLineEnd();
        this.snippetManager.insertSnippet(this.editor, "\n#-\n---\n#-\n");
    }

    /*
     * Creates a comment section of selected text, comment block or comments the cursor line
     */
    @focusAfter
    commentClicked() {
        const selection = this.editor.getSelection();
        const range = selection.getRange();
        const pos = this.editor.getCursorPosition();
        // If cursor is at the start of a line and there is no selection
        if (pos.column === 0 && (range.start.row === range.end.row && range.start.column === range.end.column)) {
            this.editor.selection.selectLine();
        } else {
            // If there is nothing but a comment block in line erase it
            range.start.column -= 4;
            range.end.column += 4;
            const text = this.editor.session.getTextRange(range);
            if (text === "{!!!!!!}") {
                this.editor.selection.setRange(range, false);
                this.snippetManager.insertSnippet(this.editor, "");
                return;
            }
        }
        this.surroundClicked("{!!!", "!!!}");
    }

    // Insert
    // Special characters
    @focusAfter
    charClicked($event: Event, char: string | undefined) {
        if (!$event.target) {
            return;
        }
        let character = $($event.target).text();
        $log.info(char);
        if (typeof char !== "undefined") {
            character = char;
        }
        this.editor.insert(character);
    }

    // Special characters
    // TEX
    @focusAfter
    texClicked() {
        this.snippetManager.insertSnippet(this.editor, "$${0:$SELECTION}$");
    }

    @focusAfter
    texBlockClicked() {
        this.snippetManager.insertSnippet(this.editor, "$$${0:$SELECTION}$$");
    }

    @focusAfter
    indexClicked() {
        this.snippetManager.insertSnippet(this.editor, "_{${0:$SELECTION}}");
    }

    @focusAfter
    powerClicked() {
        this.snippetManager.insertSnippet(this.editor, "^{${0:$SELECTION}}");
    }

    @focusAfter
    squareClicked() {
        this.snippetManager.insertSnippet(this.editor, "\\sqrt{${0:$SELECTION}}");
    }

    getEditorText(): string {
        return this.editor.getSession().getValue();
    }

    setEditorText(text: string) {
        this.editor.getSession().setValue(text);
    }

    setPosition(pos: number) {
        const range = this.editor.session.doc.indexToPosition(pos, 0);
        this.editor.moveCursorTo(range.row, range.column); // TODO: find a way to move to postion
        this.gotoCursor();
    }

    forceWrap(force: boolean) {
        let n = this.getWrapValue();
        if (!n) { return; }
        if (n < 0) { n = -n; }
        const text = this.getEditorText();
        if (!force) {
            if (text.indexOf("```") >= 0) { return; }
            if (text.indexOf("|") >= 0) { return; }
        }
        const r = wrapText(text, n);
        if (!r.modified) { return; }
        const editor = this.editor;
        let cursor = editor.selection.getCursor();
        const index = editor.session.doc.positionToIndex(cursor, 0);
        const range = editor.getSelection().getRange(); // new Range(0,0, 10000,1000);// $scope.editor.session.doc.indexToPosition(100000);
        range.start.row = 0; // TODO: easier way to find full range
        range.end.row = 1000;
        range.start.column = 0;
        range.end.column = 1000;
        // $scope.setEditorText(r.s); // not good, undo does not work
        editor.session.replace(range, r.s);
        $timeout(() => {
            cursor = editor.session.doc.indexToPosition(index, 0);
            editor.selection.moveCursorToPosition(cursor);
            editor.selection.clearSelection();
        });
    }

    setAutoCompletion(enable: boolean) {
        this.editor.setOptions({enableLiveAutocompletion: enable});
    }
}
