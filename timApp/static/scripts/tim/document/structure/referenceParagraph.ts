import {Paragraph} from "tim/document/structure/paragraph";
import {Area} from "tim/document/structure/area";
import {DocumentPart, IDocumentPart} from "tim/document/structure/documentPart";
import {DerefOption} from "tim/document/structure/derefOption";

/**
 * A kind of {@link DocumentPart} that references another {@link Paragraph} or an {@link Area}.
 *
 * @property original The original {@link Paragraph} that refers to a {@link Paragraph} or an {@link Area}.
 * @property target The referred {@link DocumentPart}.
 */
export class ReferenceParagraph<T extends Paragraph | Area>
    implements IDocumentPart {
    constructor(public original: Paragraph, public target: T) {}

    equals(other: DocumentPart) {
        if (!(other instanceof ReferenceParagraph)) {
            return false;
        }
        return this.original.htmlElement === other.original.htmlElement;
    }

    nextInHtml() {
        return this.target.nextInHtml();
    }

    getSinglePar(el: Element) {
        return this.target.getSinglePar(el);
    }

    getFirstOrigPar() {
        return this.original;
    }

    *enumPars(d: DerefOption) {
        if (d === DerefOption.Deref) {
            yield* this.target.enumPars(d);
        } else {
            yield this.original;
        }
    }

    toString() {
        return `RefPar(${this.original.toString()} -> ${this.target.toString()})`;
    }

    remove() {
        this.target.remove();
    }
}
