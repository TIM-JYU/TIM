import {ParContext} from "tim/document/structure/parContext";
import {Area} from "tim/document/structure/area";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import {Paragraph} from "tim/document/structure/paragraph";

/**
 * Possible cases of how a {@link Paragraph} may relate to an {@link Area}.
 */
export enum ParAreaInclusionKind {
    // The paragraph is outside the area, or there was no area to begin with.
    Outside,
    // The paragraph is the start paragraph of the area (having the "area" attribute).
    IsStart,
    // The paragraph is the end paragraph of the area (having the "area_end" attribute).
    IsEnd,
    // The paragraph is inside the area (and is not the start or end paragraph of it).
    Inside,
}

/**
 * Gets information about the surrounding areas and references concerning the given paragraph.
 *
 * As an example, consider the following document structure:
 *
 * ```
 * par
 * area 1
 *   par
 *   par
 *   area 2
 *     par  <-- case 1
 *     ref
 *       area 3
 *         par
 *         par
 *         area 4
 *           par  <-- case 2
 *           par
 *         par
 *     par
 *   par
 *   ref   <-- let's call this refX
 *     par <-- case 3
 * par
 * ```
 *
 * In case 1, we would return:
 *
 * * areasBeforeRef == [area 1, area 2]
 * * areasAfterRef == []
 * * refAreaInclusion == Outside
 * * refPar == undefined
 *
 * In case 2:
 *
 * * areasBeforeRef == [area 1, area 2]
 * * areasAfterRef == [area 3, area 4]
 * * refAreaInclusion == Inside
 * * refPar == undefined
 *
 * In case 3:
 *
 * * areasBeforeRef == [area 1]
 * * areasAfterRef == []
 * * refAreaInclusion == Outside
 * * refPar == refX
 *
 * So, the first area reference (counting starts from outermost) will be the splitpoint of the two arrays.
 *
 * If the innermost DocumentPart in the context is a ReferenceParagraph<Paragraph>, it is returned as refPar.
 *
 * @param par The paragraph for which to get the info.
 */
export function getContextualAreaInfo(par: ParContext) {
    const ctxs = [];
    let p: Area | ReferenceParagraph<Area | Paragraph> | undefined =
        par.par.parent;
    while (p !== undefined) {
        ctxs.push(p);
        p = p.parent;
    }
    ctxs.reverse();
    let refAreaInclusion = ParAreaInclusionKind.Outside;
    const areasBeforeRef = [];
    const areasAfterRef = [];
    let refEncountered = false;
    let refPar;
    for (const ctx of ctxs) {
        if (refPar) {
            throw Error("there shouldn't have been anything after refPar");
        }
        if (ctx instanceof ReferenceParagraph) {
            refEncountered = true;
            if (ctx.target instanceof Area) {
                // The area will be registered on next iteration; no need to do anything here.
            } else {
                // TODO: Should be enough to just assign refPar = ctx, but then TypeScript
                //  doesn't narrow it to ReferenceParagraph<Paragraph>.
                refPar = new ReferenceParagraph(ctx.original, ctx.target);
            }
        } else {
            if (refEncountered) {
                areasAfterRef.push(ctx);
                if (areasAfterRef.length > 1) {
                    refAreaInclusion = ParAreaInclusionKind.Inside;
                } else if (ctx.startPar.par.equals(par.par)) {
                    refAreaInclusion = ParAreaInclusionKind.IsStart;
                } else if (ctx.endPar.par.equals(par.par)) {
                    refAreaInclusion = ParAreaInclusionKind.IsEnd;
                } else {
                    refAreaInclusion = ParAreaInclusionKind.Inside;
                }
            } else {
                areasBeforeRef.push(ctx);
            }
        }
    }
    return {
        areasBeforeRef,
        areasAfterRef,
        refAreaInclusion,
        refPar,
    };
}
