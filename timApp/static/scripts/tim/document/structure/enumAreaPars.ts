import {Area} from "tim/document/structure/area";
import {DerefOption} from "tim/document/structure/derefOption";
import {Paragraph} from "tim/document/structure/paragraph";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";

export function* enumAreaPars(
    area: Area,
    d: DerefOption
): Generator<Paragraph> {
    for (const i of area.inner) {
        if (i instanceof Paragraph) {
            yield i;
        } else if (i instanceof ReferenceParagraph) {
            if (d === DerefOption.NoDeref) {
                yield i.original;
            } else {
                yield* i.target.enumPars(d);
            }
        } else {
            yield* i.enumPars(d);
        }
    }
}
