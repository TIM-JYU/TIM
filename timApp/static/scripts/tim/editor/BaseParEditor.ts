import {Ace as Ace_} from "ace-builds/src-noconflict/ace";
import {$timeout} from "../util/ngimport";
import {AceParEditor} from "./AceParEditor";
import {TextAreaParEditor} from "./TextAreaParEditor";
import IAceEditor = Ace_.Editor;

export function focusAfter(target: TextAreaParEditor | AceParEditor, key: string, descriptor: PropertyDescriptor) {
    const originalMethod = descriptor.value as (...args: unknown[]) => void;
    descriptor.value = function(this: BaseParEditor, ...args: unknown[]) {
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

    constructor(editor: IAceEditor | JQuery, callbacks: IEditorCallbacks) {
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

    public abstract surroundClicked(before: string, after: string, func?: () => boolean): void;

    public italicSurroundClicked() {
        this.surroundClicked("*", "*", () => this.surroundedByItalic());
    }

    public abstract surroundedBy(before: string, after: string): boolean;

    public abstract getPosition(): SelectionRange;

    public abstract setPosition(pos: SelectionRange): void;

    public surroundedByItalic() {
        return (this.surroundedBy("*", "*") && !this.surroundedBy("**", "**")) || this.surroundedBy("***", "***");
    }

    checkWrap() {
        if (this.getWrapValue() <= 0) { return; }
        $timeout(() => { // time to let new char happend
            this.forceWrap(false);
        });
    }
}

export const CURSOR = "⁞";
