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
    public parent?: Area | ReferenceParagraph<Paragraph | Area>;
    constructor(
        public readonly original: Paragraph,
        public readonly target: T
    ) {}

    equals(other: DocumentPart) {
        if (!(other instanceof ReferenceParagraph)) {
            return false;
        }
        return this.original.htmlElement === other.original.htmlElement;
    }

    nextInHtml() {
        return this.target.nextInHtml();
    }

    getSinglePar(el: Element, d: DerefOption) {
        return d === DerefOption.Deref
            ? this.target.getSinglePar(el, d)
            : this.original;
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

    toShortString() {
        return `RefPar(${this.original.id})`;
    }

    format(pad: string) {
        let s =
            pad +
            `RefPar, parent: ${this.parent?.toShortString() ?? "undefined"}\n`;
        s += `${pad}  ${this.original.toString()} ->\n`;
        s += `${this.target.format(pad + "  ")}`;
        return s;
    }

    remove() {
        this.target.remove();
    }
}
