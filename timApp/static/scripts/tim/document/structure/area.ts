import {Paragraph} from "tim/document/structure/paragraph";
import $ from "jquery";
import {CollapseControls} from "tim/document/structure/collapseControls";
import {DocumentPart, IDocumentPart} from "tim/document/structure/documentPart";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import {DerefOption} from "tim/document/structure/derefOption";
import {enumAreaPars} from "tim/document/structure/enumAreaPars";

enum AreaBoundary {
    Start,
    End,
}

export abstract class AreaPar {
    abstract kind: AreaBoundary;

    constructor(public par: Paragraph, public areaname: string) {}
}

export class AreaStartPar extends AreaPar {
    kind = AreaBoundary.Start;
}

export class AreaEndPar extends AreaPar {
    kind = AreaBoundary.End;
}

/**
 * A named section in a document.
 */
export class Area implements IDocumentPart {
    public readonly areaname: string;

    constructor(
        public readonly startPar: AreaStartPar,
        public readonly endPar: AreaEndPar,
        // TODO: This should be DocumentPart[] as soon as nested areas are supported.
        public readonly inner: Array<Paragraph | ReferenceParagraph<Paragraph>>,
        public readonly collapse: CollapseControls | undefined
    ) {
        if (startPar.areaname !== endPar.areaname) {
            throw Error(
                `Area names don't match: ${startPar.areaname} and ${endPar.areaname}`
            );
        }
        this.areaname = startPar.areaname;
    }

    equals(other: DocumentPart) {
        if (!(other instanceof Area)) {
            return false;
        }
        return this.startPar.par.htmlElement === other.startPar.par.htmlElement;
    }

    nextInHtml() {
        if (this.collapse) {
            return this.startPar.par.htmlElement.nextElementSibling
                ?.nextElementSibling;
        } else {
            return this.startPar.par.htmlElement.parentElement?.parentElement
                ?.nextElementSibling;
        }
    }

    getSinglePar(el: Element, d: DerefOption) {
        for (const p of this.enumPars(d)) {
            if (p.htmlElement === el) {
                return p;
            }
        }
    }

    getFirstOrigPar() {
        return this.startPar.par;
    }

    *enumInnerPars(d: DerefOption) {
        yield* enumAreaPars(this, d);
    }

    *enumPars(d: DerefOption) {
        yield this.startPar.par;
        yield* this.enumInnerPars(d);
        yield this.endPar.par;
    }

    toString() {
        return `Area(${this.areaname} ${this.startPar.par.toString()}...${
            this.inner.length
        } inner pars...${this.endPar.par.toString()})`;
    }

    getAreaContainer() {
        return this.endPar.par.htmlElement.parentElement!.parentElement!;
    }

    remove() {
        this.startPar.par.remove(); // If collapsible area, startPar is outside the area container.
        $(this.getAreaContainer()).remove();
    }

    isStartOrEnd(par: Paragraph) {
        return this.startPar.par.equals(par) || this.endPar.par.equals(par);
    }
}
