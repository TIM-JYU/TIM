import {AceParEditor} from "./directives/AceParEditor";
import {TextAreaParEditor} from "./directives/TextAreaParEditor";

let currentEditorScope: AceParEditor | TextAreaParEditor | null = null;

export function editorChangeValue(attributes: string[], text: string) {
    if (!currentEditorScope) {
        return;
    }
    currentEditorScope.changeValue(attributes, text);
}

export function setEditorScope(scope: AceParEditor | TextAreaParEditor | null) {
    currentEditorScope = scope;
}
