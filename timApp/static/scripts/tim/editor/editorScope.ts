import {AceParEditor} from "./AceParEditor";
import {TextAreaParEditor} from "./TextAreaParEditor";

let currentEditorScope: AceParEditor | TextAreaParEditor | undefined;

export function editorChangeValue(attributes: string[], text: string) {
    if (!currentEditorScope) {
        return;
    }
    currentEditorScope.changeValue(attributes, text);
}

export function setEditorScope(scope: AceParEditor | TextAreaParEditor | undefined) {
    currentEditorScope = scope;
}
