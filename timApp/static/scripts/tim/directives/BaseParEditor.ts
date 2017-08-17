import {IAceEditor} from "../ace-types";

export function focusAfter(target: BaseParEditor, key: string, descriptor: PropertyDescriptor) {
    const originalMethod = descriptor.value;
    descriptor.value = function(this: BaseParEditor, ...args: any[]) {
        originalMethod.apply(this, args);
        this.wrapFn();
    };
}

export abstract class BaseParEditor {
    public wrapFn: () => void;
    public saveClicked: () => void;
    protected editor: IAceEditor | JQuery;

    constructor(editor: IAceEditor | JQuery, wrapFn: () => void, saveClicked: () => void) {
        this.editor = editor;
        this.wrapFn = wrapFn;
        this.saveClicked = saveClicked;
    }

    public focus() {
        this.editor.focus();
    }

    public abstract pageBreakClicked();

    public abstract surroundClicked(before: string, after: string, func?);

    public italicSurroundClicked() {
        this.surroundClicked("*", "*", () => this.surroundedByItalic());
    }

    public abstract surroundedBy(before: string, after: string): boolean;

    public surroundedByItalic() {
        return (this.surroundedBy("*", "*") && !this.surroundedBy("**", "**")) || this.surroundedBy("***", "***");
    }
}
