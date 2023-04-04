/**
 * Utility functions for formula editor
 *
 * @author Juha Reinikainen
 * @licence MIT
 * @data 4.4.2023
 */

import type {IEditor} from "../editor";

/**
 * Gets latex from preview
 * @param event
 */
function getLatexFromPreview(event: MouseEvent): string | undefined {
    const endParent = document.querySelector(".csrunPreview");
    // probably not inside preview
    if (!endParent) {
        return;
    }

    // try to find root of formula
    let current = event.target;
    if (!current || !(current instanceof Element)) {
        return;
    }

    while (current !== endParent && current instanceof Element) {
        if (current.classList.contains("math")) {
            console.log(current);
            const annotation = current.querySelector("annotation");
            if (!annotation || !annotation.textContent) {
                return;
            }
            const latex = annotation.textContent.trim();
            return latex;
        }
        current = current.parentElement;
    }
}

/**
 * Moves cursor inside clicked formula in preview in editor
 * and opens formula editor.
 * @param event mouse click event
 * @param editor editor containing latex that was rendered to preview
 */
export function selectFormulaFromPreview(event: MouseEvent, editor: IEditor) {
    const latex = getLatexFromPreview(event);
    if (!latex) {
        return;
    }
    const i = editor.content.indexOf(latex);
    console.log(JSON.stringify(latex), JSON.stringify(editor.content), i);
    editor.focus();
    editor.moveCursorToContentIndex?.(i);
}
