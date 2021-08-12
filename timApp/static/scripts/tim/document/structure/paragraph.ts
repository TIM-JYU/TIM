import $ from "jquery";
import {DocumentPart, IDocumentPart} from "tim/document/structure/documentPart";
import {DerefOption} from "tim/document/structure/derefOption";
import {NotRemovableHTMLElement} from "tim/document/structure/notRemovableElement";
import {Area} from "tim/document/structure/area";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";

export function getEditLine(el: HTMLElement) {
    const r = el.querySelector(".editline");
    if (!r) {
        throw Error("par has no editline");
    }
    return r;
}

/**
 * The basic "building block" of a document.
 */
export class Paragraph implements IDocumentPart {
    public htmlElement: NotRemovableHTMLElement;
    public parent: Area | undefined | ReferenceParagraph<Area | Paragraph>;
    constructor(
        public id: string,
        public hash: string,
        public docId: number,
        public attrs: Record<string, string>,
        htmlElement: HTMLElement,
        public preamblePath: string | undefined
    ) {
        this.htmlElement = htmlElement as NotRemovableHTMLElement;
    }

    nextInHtml() {
        return this.htmlElement.nextElementSibling;
    }

    getSinglePar(el: Element) {
        if (el === this.htmlElement) {
            return this;
        }
    }

    equals(other: DocumentPart) {
        if (!(other instanceof Paragraph)) {
            return false;
        }
        return this.htmlElement === other.htmlElement;
    }

    isSetting() {
        return this.attrs.settings !== undefined;
    }

    isTranslation() {
        return this.attrs.r === "tr";
    }

    getFirstOrigPar() {
        return this;
    }

    *enumPars(d: DerefOption) {
        yield this;
    }

    toString() {
        let s = `Par(${this.id} ${this.docId}`;
        // s += ` ${this.hash}`;
        if (Object.keys(this.attrs).length > 0) {
            // Remove quotes from JSON for better readability.
            s += ` ${JSON.stringify(this.attrs).replace(/"/g, "")}`;
        }
        if (this.preamblePath) {
            s += ` ${this.preamblePath}`;
        }
        return `${s})`;
    }

    format(pad: string) {
        return `${pad + this.toString()}, parent: ${
            this.parent?.toShortString() ?? "undefined"
        }`;
    }

    remove() {
        $(this.htmlElement).remove();
    }

    getEditLine() {
        return getEditLine(this.htmlElement);
    }

    getJsonForServer() {
        return {
            doc_id: this.docId,
            par_id: this.id,
        };
    }
}
