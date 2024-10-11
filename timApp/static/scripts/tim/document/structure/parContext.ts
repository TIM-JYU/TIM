import type {Paragraph} from "tim/document/structure/paragraph";
import $ from "jquery";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import {Area} from "tim/document/structure/area";
import {EditType} from "tim/document/editing/edittypes";
import {getContextualAreaInfo} from "tim/document/structure/areaContext";
import {vctrlInstance} from "tim/document/viewctrlinstance";

function getOutermostRef(par: Paragraph) {
    let ref;
    let curr = par.parent;
    while (curr) {
        if (curr instanceof ReferenceParagraph) {
            ref = curr;
        }
        curr = curr.parent;
    }
    return ref;
}

/**
 * TODO remove this class. This is just a wrapper for {@link Paragraph}.
 */
export class ParContext {
    isHelp = false as const;

    constructor(public par: Paragraph) {}

    private toJQuery() {
        return $(this.par.htmlElement);
    }

    get readLineCtx() {
        let p = this.par;
        if (
            vctrlInstance?.isTranslation() &&
            this.par.parent instanceof ReferenceParagraph
        ) {
            p = this.par.parent.original;
        }
        return new ParContext(p);
    }

    /**
     * Returns the corresponding {@link Paragraph} object without dereferencing {@link ReferenceParagraph}.
     */
    get originalPar() {
        // Special case: with translations, we want to resolve only the immediate reference
        // This way, the server handled resolving the diff between the original untranslated ref and this ref
        if (
            vctrlInstance?.isTranslation() &&
            this.par.parent instanceof ReferenceParagraph
        ) {
            return this.par.parent.original;
        }

        const ref = getOutermostRef(this.par);

        if (ref) {
            return ref.original;
        } else {
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
        return (
            this.toJQuery().parents(".previewcontent").length > 0 ||
            this.toJQuery().parents(".previeworiginalcontent").length > 0
        );
    }

    textContent() {
        const c = this.getContent();
        return c !== null ? c.textContent : "";
    }

    equals(other: ParContext) {
        return this.par.htmlElement === other.par.htmlElement;
    }

    getNotesContainer() {
        const n = this.par.htmlElement.getElementsByClassName("notes");
        if (n.length === 0) {
            const newContainer = document.createElement("div");
            newContainer.classList.add("notes");
            const target =
                this.par.htmlElement.querySelector(
                    ".parContent"
                )?.parentElement;
            target?.appendChild(newContainer);
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
        return getOutermostRef(this.par) instanceof ReferenceParagraph;
    }

    get preamble() {
        return this.originalPar.preamblePath;
    }

    getReadline() {
        return this.par.htmlElement.querySelector(".readline");
    }

    getReadSectionMark() {
        return this.par.htmlElement.querySelector(".readsection");
    }

    getContent() {
        return this.par.htmlElement.querySelector(".parContent")!;
    }

    getAuthorInfo() {
        const e = this.par.htmlElement.querySelector(".authorinfo");
        if (!e) {
            return null;
        }

        const usernameList = e.getAttribute("data-usernames");
        if (!usernameList) {
            return null;
        }

        return {
            usernames: usernameList.split(",").map((s) => s.trim()),
        };
    }

    isSetting() {
        return this.par.isSetting();
    }

    /**
     * Returns whether this paragraph is safe to delete without breaking the document structure.
     */
    isDeletableOnItsOwn() {
        const {areasAfterRef} = getContextualAreaInfo(this);
        if (areasAfterRef.length > 0) {
            return false;
        }
        const d = this.par.parent;
        if (d instanceof Area) {
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
        const d = this.par.parent;
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
