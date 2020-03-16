import {PareditorController} from "tim/editor/pareditor";

let currentEditor: PareditorController | undefined;

export function editorChangeValue(attributes: string[], text: string) {
    if (!currentEditor) {
        return;
    }
    currentEditor.getEditor()!.changeValue(attributes, text);
}

export function setCurrentEditor(e: PareditorController | undefined) {
    currentEditor = e;
}

export function getCurrentEditor() {
    return currentEditor;
}
