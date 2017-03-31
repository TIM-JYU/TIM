let currentEditorScope = null;
export function editorChangeValue(attributes, text) {
    if (!currentEditorScope) {
        return;
    }
    currentEditorScope.changeValue(attributes, text);
}

export function setEditorScope(scope) {
    currentEditorScope = scope;
}
