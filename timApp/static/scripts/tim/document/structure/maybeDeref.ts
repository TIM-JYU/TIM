import {DocumentPart} from "tim/document/structure/documentPart";
import {Paragraph} from "tim/document/structure/paragraph";
import {Area} from "tim/document/structure/area";
import {BrokenArea} from "tim/document/structure/brokenArea";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";

/**
 * If the provided {@link DocumentPart} is a {@link ReferenceParagraph}, returns
 * its target {@link Paragraph}. Otherwise, returns the argument unchanged.
 * @param ctx The {@link DocumentPart} to transform.
 */
export function maybeDeref(ctx: DocumentPart): Paragraph | Area | BrokenArea {
    return ctx instanceof ReferenceParagraph ? ctx.target : ctx;
}
