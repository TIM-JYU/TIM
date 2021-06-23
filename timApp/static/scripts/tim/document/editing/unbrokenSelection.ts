import {ParSelection} from "tim/document/editing/parSelection";
import $ from "jquery";
import {Area} from "tim/document/structure/area";
import {ParContext} from "tim/document/structure/parContext";
import {maybeDeref} from "tim/document/structure/maybeDeref";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";

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
            // If the selection start paragraph is not equal to the area start paragraph, it means
            // that the selection is inside one area. In that case, we don't
            // want to touch the whole area container, but only the selected
            // paragraphs.
            if (!firstctx.startPar.par.equals(this.start.par)) {
                const it = this.iter();

                // Don't remove the first one; we return that one.
                it.next();

                for (const p of it) {
                    p.par.remove();
                }
                return this.start.par.htmlElement;
            }
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
        let start = missingStart
            ? new ParContext(missingStart.startPar.par, missingStart)
            : s.start;
        const missingEnd: Area | undefined = areaStack[0];
        let end = missingEnd
            ? new ParContext(missingEnd.endPar.par, missingEnd)
            : s.end;

        // If we're inside an area reference, extend the selection to cover the full area.
        // Currently it's not possible to edit individual area paragraphs through an area reference,
        // so it's better not to deceive the user.
        if (
            start.context instanceof ReferenceParagraph &&
            start.context.target instanceof Area
        ) {
            start = new ParContext(
                start.context.target.startPar.par,
                start.context
            );
        }
        if (
            end.context instanceof ReferenceParagraph &&
            end.context.target instanceof Area
        ) {
            end = new ParContext(end.context.target.endPar.par, end.context);
        }
        return new UnbrokenSelection(start, end);
    }
}

export function getMinimalUnbrokenSelection(
    first: ParContext,
    last: ParContext
) {
    return UnbrokenSelection.minimal(new ParSelection(first, last));
}
