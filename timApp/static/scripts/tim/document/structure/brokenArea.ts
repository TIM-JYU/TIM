import {Paragraph} from "tim/document/structure/paragraph";
import $ from "jquery";
import {DocumentPart, IDocumentPart} from "tim/document/structure/documentPart";
import {NotRemovableHTMLElement} from "tim/document/structure/notRemovableElement";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import {DerefOption} from "tim/document/structure/derefOption";
import {enumAreaPars} from "tim/document/structure/enumAreaPars";

/**
 * Represents an {@link Area} that is in some way broken. For example, `area_end` attribute could be missing.
 *
 * Currently, the main use case is a temporary workaround for the fact that nested areas are not handled
 * properly (implementing nested areas requires some server changes too, but possibly not very much).
 *
 * For example, if the document has two nested areas like:
 *
 * ```
 * a1
 *   ..pars..
 *   a2
 *     ..pars..
 *   a2_end
 *   ..pars..
 * a1_end
 * ```
 *
 * then the HTML structure will contain two top-level instances of `a1`:
 *
 * * the first instance lacks area end paragraph
 * * the second instance lacks area start paragraph
 *
 */
export class BrokenArea implements IDocumentPart {
    htmlElement: NotRemovableHTMLElement;

    constructor(
        public readonly areaname: string,
        htmlElement: HTMLElement,
        readonly inner: Array<Paragraph | ReferenceParagraph<Paragraph>>,
        public readonly reason: string
    ) {
        this.htmlElement = htmlElement as NotRemovableHTMLElement;
    }

    nextInHtml() {
        return this.htmlElement.nextElementSibling;
    }

    equals(other: DocumentPart) {
        if (!(other instanceof BrokenArea)) {
            return false;
        }
        return this.htmlElement === other.htmlElement;
    }

    remove() {
        $(this.htmlElement).remove();
    }

    *enumPars(d: DerefOption) {
        yield* enumAreaPars(this, d);
    }

    getFirstOrigPar(): Paragraph | undefined {
        const x = this.enumPars(DerefOption.NoDeref).next();
        if (!x.done) {
            return x.value;
        }
    }

    getSinglePar(el: Element, d: DerefOption) {
        for (const p of this.enumPars(d)) {
            if (p.htmlElement === el) {
                return p;
            }
        }
    }

    toString() {
        return `BrokenArea(${
            this.areaname
        }, ${this.inner.length.toString()} inner pars, ${this.reason})`;
    }
}
