import type {PareditorComponent} from "tim/editor/pareditor.component";

let currentEditor: PareditorComponent | undefined;

export function editorChangeValue(attributes: string[], text: string) {
    if (!currentEditor) {
        return;
    }
    currentEditor.getEditor()!.changeValue(attributes, text);
}

export function setCurrentEditor(e: PareditorComponent | undefined) {
    currentEditor = e;
}

export function getCurrentEditor() {
    return currentEditor;
}
