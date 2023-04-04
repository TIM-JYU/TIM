/**
 * Utility functions for formula editor
 *
 * @author Juha Reinikainen
 * @licence MIT
 * @data 4.4.2023
 */

import type {IEditor} from "../editor";

/**
 * Moves cursor inside clicked formula in preview in editor
 * and opens formula editor.
 * @param event mouse click event
 */
export function selectFormulaFromPreview(event: MouseEvent, editor: IEditor) {
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
            console.log(latex);
            return;
        }
        current = current.parentElement;
    }
    console.log("not found");
}
