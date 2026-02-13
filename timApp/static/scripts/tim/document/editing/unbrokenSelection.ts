import {ParSelection} from "tim/document/editing/parSelection";
import $ from "jquery";
import {Area} from "tim/document/structure/area";
import {ParContext} from "tim/document/structure/parContext";
import {getActiveDocument} from "tim/document/activedocument";
import {getContextualAreaInfo} from "tim/document/structure/areaContext";

/**
 * Represents a "not broken" contiguous selection of paragraphs.
 *
 * It is possible for the user to select a "broken" selection of paragraphs.
 * For example, the selection may include an end of some area but not the corresponding start.
 *
 * We will automatically extend it to an "unbroken" one so that each area start par has a corresponding end
 * in the selection.
 */
export class UnbrokenSelection extends ParSelection {
    private readonly areaStart: Area | undefined;
    private readonly areaEnd: Area | undefined;
    public forceIds: boolean = false;

    private constructor(start: ParContext | Area, end: ParContext | Area) {
        super(
            start instanceof ParContext
                ? start
                : new ParContext(start.startPar.par),
            end instanceof ParContext ? end : new ParContext(end.endPar.par)
        );
        this.areaStart = start instanceof Area ? start : undefined;
        this.areaEnd = end instanceof Area ? end : undefined;
    }

    remove() {
        $(this.removeAllButFirst()).remove();
    }

    removeAllButFirst() {
        let curr = this.areaStart ?? this.start.par;
        const todelete = [];
        const last = this.areaEnd ?? this.end.par;
        const doc = getActiveDocument();
        while (!curr.equals(last)) {
            // We know there should be at least one more element because curr != last.
            const next = curr.nextInHtml()!;

            const par = doc.getParMap().get(next);
            if (par) {
                curr = par[1];
                if (
                    curr.parent instanceof Area &&
                    curr.parent.collapse &&
                    curr.parent.isStartOrEnd(curr)
                ) {
                    // The par might be the start par of a collapsible area,
                    // in which case the next DocumentPart we're looking for is the area.
                    curr = curr.parent;
                }
                todelete.push(curr);
            } else {
                // If parMap didn't have an entry, this must be an uncollapsible area.
                const area = doc.getUncollapsibleAreaMap().get(next)!;
                todelete.push(area);
                curr = area;
            }
        }
        for (const d of todelete) {
            d.remove();
        }
        const firstctx = this.areaStart ?? this.start.par;
        if (firstctx instanceof Area) {
            const ac = firstctx.getAreaContainer();
            if (firstctx.collapse) {
                $(ac).remove();
                return firstctx.startPar.par.htmlElement;
            } else {
                return ac;
            }
        }
        return firstctx.htmlElement;
    }

    static explicit(par: ParContext) {
        return new UnbrokenSelection(par, par);
    }

    /**
     * Returns a new UnbrokenSelection such that
     *
     * 1) the new selection is not broken, and
     * 2) the old selection is contained by the new selection, and
     * 3) there is no smaller selection that would satisfy the above two conditions.
     *
     * We will assume that the current document as a whole is not broken, i.e. all areas are ended appropriately etc.
     */
    static minimal(s: ParSelection): UnbrokenSelection {
        const startInfo = getContextualAreaInfo(s.start);
        const endInfo = getContextualAreaInfo(s.end);

        // Find the first area (starting from innermost) that contains both start and end.
        let innermostCommonArea;
        const endInfoAll = [
            ...endInfo.areasBeforeRef,
            ...endInfo.areasAfterRef,
        ];
        let eInd = -1;
        const startInfoAll = [
            ...startInfo.areasBeforeRef,
            ...startInfo.areasAfterRef,
        ];
        let sInd = startInfoAll.length;
        for (let i = startInfoAll.length - 1; i >= 0; i--) {
            const x = startInfoAll[i];
            sInd = i;
            eInd = endInfoAll.findIndex((a) => a.equals(x));
            if (eInd >= 0) {
                innermostCommonArea = endInfoAll[eInd];
                break;
            }
        }
        if (!innermostCommonArea) {
            sInd = -1;
        }

        // If we're inside an area reference, extend the selection to cover the full area.
        // Currently it's not possible to edit individual area paragraphs through an area reference,
        // so it's better not to deceive the user.
        if (innermostCommonArea && eInd >= endInfo.areasBeforeRef.length) {
            return new UnbrokenSelection(
                endInfo.areasAfterRef[0],
                endInfo.areasAfterRef[0]
            );
        }
        if (
            innermostCommonArea &&
            (innermostCommonArea.isStartOrEnd(s.start.par) ||
                innermostCommonArea.isStartOrEnd(s.end.par))
        ) {
            // Here we have a selection inside a single area, and we have selected the start and/or end paragraph
            // of the area. In that case, we must extend the selection to cover the whole area.
            return new UnbrokenSelection(
                innermostCommonArea,
                innermostCommonArea
            );
        }

        // Widen types to include undefined to match reality.
        const start = startInfoAll[sInd + 1] as Area | undefined;
        const end = endInfoAll[eInd + 1] as Area | undefined;
        return new UnbrokenSelection(
            start ? start : s.start,
            end ? end : s.end
        );
    }
}

export function getExplicitSelection(par: ParContext) {
    return UnbrokenSelection.explicit(par);
}

export function getMinimalUnbrokenSelection(
    first: ParContext,
    last: ParContext
) {
    return UnbrokenSelection.minimal(new ParSelection(first, last));
}
