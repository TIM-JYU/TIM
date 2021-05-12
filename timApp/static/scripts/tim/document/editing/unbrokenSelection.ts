import {ParSelection} from "tim/document/editing/parSelection";
import $ from "jquery";
import {Area} from "tim/document/structure/area";
import {ParContext} from "tim/document/structure/parContext";
import {maybeDeref} from "tim/document/structure/maybeDeref";

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
    private constructor(
        public readonly start: ParContext,
        public readonly end: ParContext
    ) {
        super(start, end);
    }

    getMinimalUnbrokenSelection() {
        return this;
    }

    remove() {
        $(this.removeAllButFirst()).remove();
    }

    removeAllButFirst() {
        let curr = this.start;
        const todelete = [];
        let last = this.start.context;
        for (curr of this.iter()) {
            if (!curr.context.equals(last)) {
                todelete.push(curr.context);
                last = curr.context;
            }
        }
        for (const d of todelete) {
            d.remove();
        }
        const firstctx = maybeDeref(this.start.context);
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
        const areaStack = [];
        let missingStart;
        for (const curr of s.iter()) {
            const d = maybeDeref(curr.context);
            if (d instanceof Area) {
                if (d.startPar.par.equals(curr.par)) {
                    areaStack.push(d);
                } else if (d.endPar.par.equals(curr.par)) {
                    const a = areaStack.pop();
                    if (!a) {
                        missingStart = d;
                    }
                }
            }
        }
        const start = missingStart
            ? new ParContext(missingStart.startPar.par, missingStart)
            : s.start;
        const missingEnd: Area | undefined = areaStack[0];
        const end = missingEnd
            ? new ParContext(missingEnd.endPar.par, missingEnd)
            : s.end;
        return new UnbrokenSelection(start, end);
    }
}

export function getMinimalUnbrokenSelection(
    first: ParContext,
    last: ParContext
) {
    return UnbrokenSelection.minimal(new ParSelection(first, last));
}
