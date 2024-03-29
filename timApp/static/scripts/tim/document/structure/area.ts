import type {Paragraph} from "tim/document/structure/paragraph";
import $ from "jquery";
import type {CollapseControls} from "tim/document/structure/collapseControls";
import type {
    DocumentPart,
    IDocumentPart,
} from "tim/document/structure/documentPart";
import type {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import type {DerefOption} from "tim/document/structure/derefOption";
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
    public parent: undefined | Area | ReferenceParagraph<Area>;
    constructor(
        public readonly startPar: AreaStartPar,
        public readonly endPar: AreaEndPar,
        public readonly inner: DocumentPart[],
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
        return this.getAreaContainer().nextElementSibling;
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

    toShortString() {
        return `Area(${this.areaname})`;
    }

    format(pad: string): string {
        let s = pad;
        s += `Area(${this.areaname}), parent: ${
            this.parent?.toShortString() ?? "undefined"
        }`;
        return [this.startPar.par, ...this.inner, this.endPar.par].reduce(
            (p, c) => p + "\n" + c.format(pad + "  "),
            s
        );
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
