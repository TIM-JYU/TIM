import {Paragraph} from "tim/document/structure/paragraph";
import {DocumentPart} from "tim/document/structure/documentPart";
import $ from "jquery";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import {Area} from "tim/document/structure/area";
import {EditType} from "tim/document/editing/edittypes";
import {maybeDeref} from "tim/document/structure/maybeDeref";

/**
 * A {@link Paragraph} that is associated with a context.
 *
 * The context can be any {@link DocumentPart}.
 *
 * If the Paragraph does not have any particular context (i.e. it is not inside any {@link Area} and does not
 * come from a {@link ReferenceParagraph}), then the context is just the same as the wrapped
 * {@link Paragraph} itself.
 */
export class ParContext {
    isHelp = false as const;

    constructor(public par: Paragraph, public context: DocumentPart) {}

    private toJQuery() {
        return $(this.par.htmlElement);
    }

    /**
     * Returns the corresponding {@link Paragraph} object without dereferencing {@link ReferenceParagraph}.
     */
    get originalPar() {
        const c = this.context;
        if (c instanceof Paragraph) {
            return c; // same as this.par
        } else if (c instanceof ReferenceParagraph) {
            return c.original;
        } else {
            // inside non-reference area
            return this.par;
        }
    }

    offset() {
        return this.toJQuery().offset();
    }

    hasClass(s: string) {
        return this.par.htmlElement.classList.contains(s);
    }

    isActionable() {
        return !this.isInPreview() && !this.preamble;
    }

    isInPreview() {
        return this.toJQuery().parents(".previewcontent").length > 0;
    }

    textContent() {
        return this.getContent().textContent;
    }

    equals(other: ParContext) {
        return this.par.htmlElement === other.par.htmlElement;
    }

    getNotesContainer() {
        const n = this.par.htmlElement.getElementsByClassName("notes");
        if (n.length === 0) {
            const newContainer = document.createElement("div");
            newContainer.classList.add("notes");
            this.par.htmlElement.appendChild(newContainer);
            return newContainer;
        }
        return n[0];
    }

    addClass(...c: string[]) {
        this.par.htmlElement.classList.add(...c);
    }

    removeClass(...c: string[]) {
        this.par.htmlElement.classList.remove(...c);
    }

    isReference() {
        return this.context instanceof ReferenceParagraph;
    }

    get preamble() {
        return this.originalPar.preamblePath;
    }

    getReadline() {
        return this.par.htmlElement.querySelector(".readline");
    }

    getContent() {
        return this.par.htmlElement.querySelector(".parContent")!;
    }

    isSetting() {
        return this.par.isSetting();
    }

    /**
     * Returns whether this paragraph is safe to delete without breaking the document structure.
     */
    isDeletableOnItsOwn() {
        const d = maybeDeref(this.context);
        if (d instanceof Area) {
            if (this.context instanceof ReferenceParagraph) {
                return false;
            }
            if (d.isStartOrEnd(this.par)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns the {@link HTMLElement} that can be used for inserting a new {@link DocumentPart}
     * before/after this `ParContext`.
     *
     * @param editType Whether the edit action is adding above or below this `ParContext`.
     */
    getElementForInsert(editType: EditType.AddAbove | EditType.AddBelow) {
        const d = maybeDeref(this.context);
        if (d instanceof Area) {
            if (d.isStartOrEnd(this.par)) {
                if (
                    d.startPar.par.equals(this.par) &&
                    editType === EditType.AddAbove
                ) {
                    if (d.collapse) {
                        return d.startPar.par.htmlElement;
                    } else {
                        return d.getAreaContainer();
                    }
                } else if (
                    d.endPar.par.equals(this.par) &&
                    editType === EditType.AddBelow
                ) {
                    return d.getAreaContainer();
                }
            }
        }
        return this.par.htmlElement;
    }

    getJsonForServer() {
        return {
            curr: this.par.getJsonForServer(),
            orig: this.originalPar.getJsonForServer(),
        };
    }
}
