import type {Paragraph} from "tim/document/structure/paragraph";
import type {Area} from "tim/document/structure/area";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import type {DocumentPart} from "tim/document/structure/documentPart";

/**
 * If the provided {@link DocPart} is a {@link ReferenceParagraph}, returns
 * its target {@link Paragraph}. Otherwise, returns the argument unchanged.
 * @param ctx The {@link DocPart} to transform.
 */
export function maybeDeref(ctx: DocumentPart): Paragraph | Area {
    return ctx instanceof ReferenceParagraph ? ctx.target : ctx;
}
