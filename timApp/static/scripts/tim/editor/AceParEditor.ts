import {Ace} from "ace-builds/src-noconflict/ace";
import {wrapText} from "tim/document/editing/utils";
import type {IAce} from "tim/editor/ace";
import {$log} from "tim/util/ngimport";
import type {IEditorCallbacks, SelectionRange} from "tim/editor/BaseParEditor";
import {
    BaseParEditor,
    CURSOR,
    EditorType,
    focusAfter,
} from "tim/editor/BaseParEditor";
import type {IEditor} from "../../../../modules/cs/js/editor/editor";
import AceAjax = Ace;
import IAceEditor = Ace.Editor;

interface ISnippetManager {
    insertSnippet(editor: AceAjax.Editor, text: string): void;
}

type EditorElementEventHandler<TType extends string> = JQuery.TypeEventHandler<
    HTMLElement,
    undefined,
    HTMLElement,
    HTMLElement,
    TType
>;

export class AceParEditor extends BaseParEditor implements IEditor {
    public editor: IAceEditor;
    private snippetManager: ISnippetManager;
    private ace: IAce;
    type: EditorType.Ace = EditorType.Ace;
    formulaFunction = function () {};
    constructor(
        ace: IAce,
        editor: AceAjax.Editor,
        callbacks: IEditorCallbacks,
        mode: string = "ace/mode/markdown"
    ) {
        super(editor, callbacks);
        this.editor = editor;
        this.ace = ace;
        const snippetModule = ace.require("ace/snippets") as {
            snippetManager: ISnippetManager;
        };
        this.snippetManager = snippetModule.snippetManager;
        const line = editor.renderer.lineHeight;
        const containertop = $(".editorContainer").position().top;
        const height = ($(window).innerHeight() ?? 700) - containertop;
        const max = Math.floor(height / 2 / line);

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
            hScrollBarAlwaysVisible: true,
            // This needs to be true so that horizontal scrolling is possible when Difference in original document is
            // next to the editor. The horizontal scrolling doesn't work properly when editor is resized to be narrower
            // than the text in it (doesn't scroll all the way to the right)); seems to be an issue with the Ace editor.
            vScrollBarAlwaysVisible: false,
            enableBasicAutocompletion: true,
            enableLiveAutocompletion: true,
        });

        this.editor.commands.addCommand({
            name: "saveFile",
            bindKey: {
                win: "Ctrl-S",
                mac: "Command-S",
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
            },
            exec: () => {
                this.codeBlockClicked();
            },
        });
        this.editor.commands.addCommand({
            name: "texBlockInsert",
            bindKey: {
                win: "Ctrl-Alt-B",
                mac: "Command-Alt-B",
            },
            exec: () => {
                this.texBlockInsertClicked();
            },
        });
        this.editor.commands.addCommand({
            name: "h1",
            bindKey: {
                win: "Ctrl-1",
                mac: "Command-1",
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
            },
            exec: () => {
                this.headerClicked("####");
            },
        });
        this.editor.commands.addCommand({
            name: "h5",
            bindKey: {
                win: "Ctrl-5",
                mac: "Command-5",
            },
            exec: () => {
                this.headerClicked("#####");
            },
        });
        this.editor.commands.addCommand({
            name: "endLine",
            bindKey: {
                win: "Ctrl-Enter",
                mac: "Command-Enter",
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
            },
            exec: () => {
                this.pageBreakClicked();
            },
        });
        this.editor.commands.addCommand({
            name: "toggleCommentLines",
            bindKey: {
                win: "Ctrl-'",
                mac: "Command-'",
            },
            exec: () => {
                this.editor.toggleCommentLines();
            },
        });
        this.editor.commands.addCommand({
            name: "addFormula",
            bindKey: {
                win: "Ctrl-E",
                mac: "Command-E",
            },
            exec: () => {
                this.formulaFunction();
            },
        });
        this.editor.keyBinding.setKeyboardHandler({
            handleKeyboard: () => {
                this.checkWrap();
            },
        });
    }

    /**
     * Add an event listener to the editor container.
     *
     * @param events Events to listen to. Follows jQuery format.
     * @param handler Handler for the events.
     */
    addContainerEventListener<TType extends string>(
        events: TType,
        handler: EditorElementEventHandler<TType>
    ) {
        $(this.editor.container).on(events, handler);
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
        if (line.length - cursor.column > 0) {
            toNextLine = line.substring(cursor.column, line.length);
        } else {
            toNextLine = "";
        }
        toNextLine = toNextLine.trim();

        const breakline = "\n#- {.printpagebreak}\n#-\n";

        this.editor.selection.setRange(range, false);
        this.editor.insert(toKeepInLine + breakline + "\n" + toNextLine);
    }

    gotoCursor() {
        const firstrow = this.editor.renderer.getFirstFullyVisibleRow();
        const lastrow = this.editor.renderer.getLastFullyVisibleRow();
        const cursor = this.editor.getCursorPosition();
        if (cursor.row < firstrow) {
            this.editor.renderer.scrollToLine(
                cursor.row,
                false,
                true,
                () => {}
            );
        } else if (cursor.row > lastrow) {
            this.editor.renderer.scrollToLine(
                cursor.row - (lastrow - firstrow),
                false,
                true,
                () => {}
            );
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
        if (
            this.editor.session.getTextRange(
                this.editor.getSelectionRange()
            ) === ""
        ) {
            this.selectWord();
        }
        const text = this.editor.session.getTextRange(
            this.editor.getSelectionRange()
        );
        const surrounded = func ? func() : this.surroundedBy(before, after);
        if (surrounded) {
            const range = this.editor.getSelectionRange();
            range.start.column -= before.length;
            range.end.column += after.length;
            this.editor.selection.setRange(range, false);
            this.snippetManager.insertSnippet(this.editor, "${0:" + text + "}");
        } else {
            this.snippetManager.insertSnippet(
                this.editor,
                before + "${0:$SELECTION}" + after
            );
        }
    }

    selectWord() {
        const cursor = this.editor.getCursorPosition();
        const wordrange = this.editor
            .getSession()
            .getAWordRange(cursor.row, cursor.column);
        const word = this.editor.session.getTextRange(wordrange);
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
        const word = this.editor.session.getTextRange(range);
        return word.startsWith(before) && word.endsWith(after);
    }

    @focusAfter
    codeBlockClicked() {
        this.snippetManager.insertSnippet(
            this.editor,
            "```\n${0:$SELECTION}\n```"
        );
    }

    @focusAfter
    headerClicked(head: string) {
        const cursor = this.editor.getCursorPosition();
        let line = this.editor.session.getLine(cursor.row);
        const range = this.editor.getSelection().getRange();
        range.start.column = 0;
        range.end.column = line.length;
        while (line.startsWith("#")) {
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
        if (
            this.editor.session.getTextRange(
                this.editor.getSelectionRange()
            ) === ""
        ) {
            if (this.selectWord()) {
                descDefault = this.editor.session.getTextRange(
                    this.editor.getSelectionRange()
                );
            }
        } else {
            descDefault = this.editor.session.getTextRange(
                this.editor.getSelectionRange()
            );
        }
        descDefault = this.escapeDollarSign(descDefault);
        this.snippetManager.insertSnippet(
            this.editor,
            "[" + descDefault + "]{.${0:" + styleDefault + "}}"
        );
    }

    /**
     * Escapes $ characters in given string.
     * @param descDefault the string that needs its $ characters escaped
     * @return the given string with its $ characters escaped
     */
    escapeDollarSign(descDefault: string) {
        // $ is a special symbol in TextMate/Sublime Text snippets and should be escaped
        // https://forum.sublimetext.com/t/escaping-dollar-sign-in-snippets/31078
        descDefault = descDefault.replace(/\$/g, "\\$");
        return descDefault;
    }

    /**
     * Checks for a selection that can be sent to be translated.
     * @return the selected text range
     */
    checkTranslationSelection() {
        return this.editor.session.getTextRange(
            this.editor.getSelectionRange()
        );
    }

    /**
     * Replaces the selected text with the translation in editor.
     * @param descDefault the translation that will replace original selected text
     */
    replaceTranslation(descDefault: string) {
        descDefault = this.escapeDollarSign(descDefault);
        this.snippetManager.insertSnippet(this.editor, descDefault);
    }

    /**
     * @param descDefault Placeholder for description
     * @param linkDefault Placeholder for link address
     * @param isImage true, if link is an image
     */
    @focusAfter
    linkClicked(descDefault: string, linkDefault: string, isImage: boolean) {
        const image = isImage ? "!" : "";
        if (
            this.editor.session.getTextRange(
                this.editor.getSelectionRange()
            ) === ""
        ) {
            if (this.selectWord()) {
                descDefault = this.editor.session.getTextRange(
                    this.editor.getSelectionRange()
                );
            }
        } else {
            descDefault = this.editor.session.getTextRange(
                this.editor.getSelectionRange()
            );
        }
        this.snippetManager.insertSnippet(
            this.editor,
            image + "[" + descDefault + "](${0:" + linkDefault + "})"
        );
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
        if (line.length - pos.column > 0) {
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
        if (ci >= 0) {
            text = text.slice(0, ci) + text.slice(ci + 1);
        }
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
            const pos = this.editor.session
                .getDocument()
                .positionToIndex(start, 0);
            const r = this.editor.session
                .getDocument()
                .indexToPosition(pos + ci, 0);
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
        for (const a of attributes) {
            const ma = line.match(" *" + a);
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
    ruleClicked(sub: boolean) {
        this.editor.navigateLineEnd();
        const slideSep = sub
            ? '\n#-{slide_break="sub"}\n---\n#-\n'
            : "\n#-\n---\n#-\n";
        this.snippetManager.insertSnippet(this.editor, slideSep);
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
        if (
            pos.column === 0 &&
            range.start.row === range.end.row &&
            range.start.column === range.end.column
        ) {
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
    charClicked($event: Event, char?: string) {
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
    texBlockInsertClicked() {
        this.snippetManager.insertSnippet(
            this.editor,
            "$$\n\\begin{aligned}\n" +
                "${0:$SELECTION}\n" +
                "\\end{aligned}\n$$"
        );
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
        this.snippetManager.insertSnippet(
            this.editor,
            "\\sqrt{${0:$SELECTION}}"
        );
    }

    getEditorText(): string {
        return this.editor.getSession().getValue();
    }

    setEditorText(text: string) {
        this.editor.getSession().setValue(text);
    }

    getPosition(): SelectionRange {
        const r = this.editor.selection.getRange();
        return [
            this.editor.session.getDocument().positionToIndex(r.start, 0),
            this.editor.session.getDocument().positionToIndex(r.end, 0),
        ];
    }

    setPosition([start, end]: SelectionRange) {
        const range = this.editor.session
            .getDocument()
            .indexToPosition(start, 0);
        const range2 = this.editor.session
            .getDocument()
            .indexToPosition(end, 0);
        const Range = this.ace.Range;
        this.editor.selection.setRange(Range.fromPoints(range, range2), false);
        this.gotoCursor();
    }

    replaceSelectedText(s: string) {
        this.editor.getSession().replace(this.editor.getSelectionRange(), s);
    }

    forceWrap(force: boolean) {
        let n = this.getWrapValue();
        if (!n) {
            return;
        }
        if (n < 0) {
            n = -n;
        }
        const text = this.getEditorText();
        if (!force) {
            if (text.includes("```")) {
                return;
            }
            if (text.includes("|")) {
                return;
            }
        }
        const r = wrapText(text, n);
        if (!r.modified) {
            return;
        }
        const editor = this.editor;
        let cursor = editor.selection.getCursor();
        const sess = editor.getSession();
        const index = sess.getDocument().positionToIndex(cursor, 0);
        sess.setValue(r.s);
        cursor = sess.getDocument().indexToPosition(index, 0);
        editor.selection.moveCursorToPosition(cursor);
        editor.selection.clearSelection();
    }

    setAutoCompletion(enable: boolean) {
        this.editor.setOptions({enableLiveAutocompletion: enable});
    }

    get content(): string {
        return this.getEditorText();
    }
    set content(value: string) {
        this.setEditorText(value);
    }

    insert(str: string): void {
        this.editor.insert(str);
    }

    setReadOnly(b: boolean): void {
        this.editor.setReadOnly(b);
    }

    addFormulaEditorOpenHandler(cb: () => void): void {
        this.formulaFunction = cb;
    }
}
