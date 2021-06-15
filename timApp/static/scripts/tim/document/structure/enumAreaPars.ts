import {BrokenArea} from "tim/document/structure/brokenArea";
import {Area} from "tim/document/structure/area";
import {DerefOption} from "tim/document/structure/derefOption";
import {Paragraph} from "tim/document/structure/paragraph";

export function* enumAreaPars(
    area: BrokenArea | Area,
    d: DerefOption | DerefOption.Deref
) {
    for (const i of area.inner) {
        if (i instanceof Paragraph) {
            yield i;
        } else {
            if (d === DerefOption.NoDeref) {
                yield i.original;
            } else {
                yield* i.target.enumPars(d);
            }
        }
    }
}
