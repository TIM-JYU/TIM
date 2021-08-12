import {Paragraph} from "tim/document/structure/paragraph";
import {Area} from "tim/document/structure/area";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import {NotRemovableElement} from "tim/document/structure/notRemovableElement";
import {DerefOption} from "tim/document/structure/derefOption";

/**
 * Represents the various possible parts of a document.
 */
export type DocumentPart =
    | Paragraph
    | Area
    | ReferenceParagraph<Paragraph | Area>;

export interface IDocumentPart {
    /**
     * Returns whether this `IDocumentPart` is the same as the given one.
     * @param other The other {@link DocumentPart} to compare against.
     */
    equals(other: DocumentPart): boolean;

    /**
     * This is essentially a shortcut method for calling {@link enumPars} with the
     * argument {@link DerefOption.NoDeref} and getting the first result.
     */
    getFirstOrigPar(): Paragraph;

    /**
     * Returns a {@link Paragraph} inside this `IDocumentPart` that corresponds to the given {@link Element}.
     * @param el The {@link Element} to match against.
     * @param d Specifies the behavior of how {@link ReferenceParagraph}s are iterated. See {@link DerefOption}.
     */
    getSinglePar(el: Element, d: DerefOption): Paragraph | undefined;

    /**
     * Gets the element in DOM tree that follows this `IDocumentPart`.
     */
    nextInHtml(): NotRemovableElement | null | undefined;

    /**
     * Removes this `IDocumentPart` from the DOM.
     */
    remove(): void;

    /**
     * Enumerates the {@link Paragraph}s in this `IDocumentPart`.
     * @param d Specifies the behavior of how {@link ReferenceParagraph}s are iterated. See {@link DerefOption}.
     */
    enumPars(d: DerefOption): Generator<Paragraph>;

    /**
     * Converts this `IDocumentPart` to a (possibly multiline) string.
     * @param pad The padding to use.
     */
    format(pad: string): string;
}
