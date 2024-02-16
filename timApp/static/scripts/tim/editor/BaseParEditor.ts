import type {Ace as Ace_} from "ace-builds/src-noconflict/ace";
import {$timeout} from "tim/util/ngimport";
import type {AceParEditor} from "tim/editor/AceParEditor";
import type {TextAreaParEditor} from "tim/editor/TextAreaParEditor";
type IAceEditor = Ace_.Editor;

export function focusAfter(
    target: TextAreaParEditor | AceParEditor,
    key: string,
    descriptor: PropertyDescriptor
) {
    const originalMethod = descriptor.value as (...args: unknown[]) => void;
    descriptor.value = function (this: BaseParEditor, ...args: unknown[]) {
        originalMethod.apply(this, args);
        this.callbacks.wrapFn();
    };
}

export interface IEditorCallbacks {
    wrapFn: () => void;
    saveClicked: () => void;
    getWrapValue: () => number;
}

export type SelectionRange = [number, number];

export enum EditorType {
    Ace,
    Textarea,
}

export abstract class BaseParEditor {
    public callbacks: IEditorCallbacks;
    protected editor: IAceEditor | JQuery;
    public abstract type: EditorType;

    protected constructor(
        editor: IAceEditor | JQuery,
        callbacks: IEditorCallbacks
    ) {
        this.editor = editor;
        this.callbacks = callbacks;
    }

    public focus() {
        this.editor.focus();
    }

    public abstract forceWrap(force: boolean): void;

    public getWrapValue(): number {
        return this.callbacks.getWrapValue();
    }

    public abstract pageBreakClicked(): void;

    public abstract surroundClicked(
        before: string,
        after: string,
        func?: () => boolean
    ): void;

    public italicSurroundClicked() {
        this.surroundClicked("*", "*", () => this.surroundedByItalic());
    }

    public abstract surroundedBy(before: string, after: string): boolean;

    public abstract getPosition(): SelectionRange;

    public abstract getEditorText(): string;

    public abstract setPosition(pos: SelectionRange): void;

    public surroundedByItalic() {
        return (
            (this.surroundedBy("*", "*") && !this.surroundedBy("**", "**")) ||
            this.surroundedBy("***", "***")
        );
    }

    public getCurrentLinePosAndLine(): [number, string] {
        const s = this.getEditorText();
        const pos = this.getPosition();
        const start = pos[0];
        let i1 = start - 1;
        let i2 = start;
        while (i1 >= 0 && s[i1] != "\n") {
            i1--;
        }
        while (i2 < s.length && s[i2] != "\n") {
            i2++;
        }
        return [i1 + 1, s.substring(i1 + 1, i2)];
    }

    checkWrap() {
        if (this.getWrapValue() <= 0) {
            return;
        }
        $timeout(() => {
            // time to let new char happen
            this.forceWrap(false);
        });
    }

    formatBlockTemplate(text: string): string {
        const pos = this.getPosition();
        const isBlock = text.startsWith("```");
        if (
            isBlock &&
            pos[0] == pos[1] &&
            pos[0] > 0 &&
            this.getEditorText()[pos[0] - 1] != "\n"
        ) {
            text = "\n" + text;
        }
        return text;
    }
}

export const CURSOR = "⁞";
