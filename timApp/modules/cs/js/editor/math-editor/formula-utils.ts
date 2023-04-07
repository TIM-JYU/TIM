/**
 * Utility functions for formula editor
 *
 * @author Juha Reinikainen
 * @licence MIT
 * @data 4.4.2023
 */

import type {IEditor} from "../editor";

/**
 * preview formula is inside parent .math class element
 * and contains latex as a string
 */
type PreviewFormula = {
    latex: string;
    element: Element;
};

/**
 * Gets latex from preview
 * @param event
 */
function getLatexFromPreview(event: MouseEvent): PreviewFormula | undefined {
    const endParent = document.querySelector(".csrunPreview");
    // probably not inside preview
    if (!endParent) {
        return undefined;
    }

    // try to find root of formula
    let current = event.target;
    if (!current || !(current instanceof Element)) {
        return undefined;
    }

    // traverse parents until element with math class is found
    // or until .csrunPreview which indicates we probably weren't inside any formula
    while (current !== endParent && current instanceof Element) {
        if (current.classList.contains("math")) {
            const annotation = current.querySelector("annotation");
            if (!annotation || !annotation.textContent) {
                return undefined;
            }
            const latex = annotation.textContent.trim();
            return {latex: latex, element: current};
        }
        current = current.parentElement;
    }
    return undefined;
}

/**
 * Moves past formulas before this and returns where
 * to start looking for clicked formula
 * @param clicked clicked formula in preview
 * @param editor editor containing formulas
 * @return index in editor.content or -1 if couldn't find
 */
function movePastFormulasBeforeClicked(
    clicked: PreviewFormula,
    editor: IEditor
): number {
    const previewElement = document.querySelector(".csrunPreview");
    // probably not inside preview
    if (!previewElement) {
        return -1;
    }
    let index = 0;
    const content = editor.content;

    for (const mathElem of previewElement.querySelectorAll(".math")) {
        // stop when clicked element is reached
        if (mathElem === clicked.element) {
            return index;
        }
        const annotation = mathElem.querySelector("annotation");
        // probably shouldn't happen
        if (!annotation || !annotation.textContent) {
            continue;
        }
        const latex = annotation.textContent.trim();
        const nextIndex = content.indexOf(latex, index);
        if (nextIndex === -1) {
            return -1;
        }
        index = nextIndex + latex.length;
    }
    return index;
}

/**
 * Moves cursor inside clicked formula in preview in editor
 * @param event mouse click event
 * @param editor editor containing latex that was rendered to preview
 * @return whether the formula was found and cursor was moved to it
 */
export function selectFormulaFromPreview(event: MouseEvent, editor: IEditor) {
    const clickedPreviewFormula = getLatexFromPreview(event);
    if (!clickedPreviewFormula) {
        return false;
    }
    const startI = movePastFormulasBeforeClicked(clickedPreviewFormula, editor);
    if (startI === -1) {
        return false;
    }
    const i = editor.content.indexOf(clickedPreviewFormula.latex, startI);
    if (i === -1) {
        return false;
    }
    if (!editor.moveCursorToContentIndex) {
        return false;
    }
    editor.moveCursorToContentIndex(i);
    return true;
}
