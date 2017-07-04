import {IAceEditor} from "../ace-types";

export function focusAfter(target: BaseParEditor, key: string, descriptor: PropertyDescriptor) {
    const originalMethod = descriptor.value;
    descriptor.value = function(this: BaseParEditor, ...args: any[]) {
        originalMethod.apply(this, args);
        this.wrapFn();
    };
}

export class BaseParEditor {
    public wrapFn: () => void;
    protected editor: IAceEditor | JQuery;

    constructor(editor: IAceEditor | JQuery, wrapFn: () => void) {
        this.editor = editor;
        this.wrapFn = wrapFn;
    }

    public focus() {
        this.editor.focus();
    }
}
