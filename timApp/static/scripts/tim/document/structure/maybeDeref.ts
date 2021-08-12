import {Paragraph} from "tim/document/structure/paragraph";
import {Area} from "tim/document/structure/area";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";

/**
 * If the provided {@link DocPart} is a {@link ReferenceParagraph}, returns
 * its target {@link Paragraph}. Otherwise, returns the argument unchanged.
 * @param ctx The {@link DocPart} to transform.
 */
export function maybeDeref<
    DerefTarget extends Area | Paragraph,
    DocPart extends Paragraph | Area | ReferenceParagraph<DerefTarget>
>(
    ctx: DocPart
): DocPart extends Paragraph | ReferenceParagraph<Paragraph>
    ? Paragraph
    : DocPart extends Paragraph | ReferenceParagraph<Paragraph | Area>
    ? Paragraph | Area
    : Area {
    return ctx instanceof ReferenceParagraph ? ctx.target : ctx;
}
