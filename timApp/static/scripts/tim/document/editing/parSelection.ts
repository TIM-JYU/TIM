import {ParContext} from "tim/document/structure/parContext";
import {nextParContext} from "tim/document/structure/iteration";

const maxIterations = 20000;

/**
 * A contiguous collection of paragraphs. The end is included in the selection.
 */
export class ParSelection {
    constructor(
        public readonly start: ParContext,
        public readonly end: ParContext
    ) {}

    addClass(c: string) {
        for (const p of this.iter()) {
            p.addClass(c);
        }
    }

    next() {
        return nextParContext(this.end);
    }

    hasMultiple() {
        return !this.start.equals(this.end);
    }

    removeClass(c: string) {
        for (const p of this.iter()) {
            p.removeClass(c);
        }
    }

    *iter() {
        let curr: ParContext | undefined = this.start;
        let iter = 0;
        while (curr) {
            iter++;
            if (iter > maxIterations) {
                throw Error(
                    `Possible infinite loop in ParSelection.iter(): exceeded ${maxIterations} iterations.`
                );
            }
            yield curr;
            if (curr.equals(this.end)) {
                return;
            }
            curr = nextParContext(curr);
        }
    }
}
