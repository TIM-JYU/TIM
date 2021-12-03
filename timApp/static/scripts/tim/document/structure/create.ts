import {HelpPar} from "tim/document/structure/helpPar";
import {ParContext} from "tim/document/structure/parContext";
import {getActiveDocument} from "tim/document/activedocument";
import {TimDocument} from "tim/document/timDocument";

export function findParentPar(p: JQuery<HTMLElement>) {
    if (!p.hasClass("par")) {
        p = p.parents(".par");
    }
    return p[0];
}

export function fromParents(p: JQuery) {
    return createParContext(findParentPar(p));
}

export function createParContext(el: Element) {
    const c = tryCreateParContextOrHelp(el);
    if (c instanceof ParContext) {
        return c;
    }
    if (c instanceof HelpPar) {
        throw Error("Unexpected HelpPar");
    }
    throw Error("Failed to create ParContext");
}

function getDocumentForElement(el: Element) {
    const root = getParContainerElem()!;
    if (root.contains(el)) {
        return getActiveDocument();
    }
    // If the element wasn't in the main content area, this must be in preview.
    const d = new TimDocument($(el).parents(".previewcontent")[0]);
    d.buildSections();
    return d;
}

export function createParContextOrHelp(el: Element) {
    return tryCreateParContextOrHelp(el)!;
}

export function tryCreateParContextOrHelp(el: Element) {
    const doc = getDocumentForElement(el);
    if (el.id === "HELP_PAR") {
        return new HelpPar(el as HTMLElement);
    }

    const par = doc.getParMap().get(el);
    return par ? new ParContext(par[1]) : undefined;
}

export function getParContainerElem() {
    return document.getElementById("pars")!;
}
