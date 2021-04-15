import {Paragraph} from "tim/document/structure/paragraph";
import {ParContext} from "tim/document/structure/parContext";
import {maybeDeref} from "tim/document/structure/maybeDeref";
import {
    createParContext,
    fromHtmlElement,
    getParContainerElem,
} from "tim/document/structure/parsing";
import {DocumentPart} from "tim/document/structure/documentPart";
import {DerefOption} from "tim/document/structure/derefOption";

function isBottomContainer(e: Element) {
    return e.classList.contains("addBottomContainer");
}

export function nextParContext(par: ParContext) {
    let nh = par.context.nextInHtml();
    const d = maybeDeref(par.context);
    if (d instanceof Paragraph) {
        nh = d.nextInHtml();
    } else {
        let found = false;
        for (const p of d.enumPars()) {
            if (found) {
                return new ParContext(p, par.context);
            }
            if (p.equals(par.par)) {
                found = true;
            }
        }
        if (!found) {
            throw Error("par not found from its own area?");
        }
        // This was the area end paragraph; just get the next after the area.
        nh = d.nextInHtml();
    }
    if (nh && !isBottomContainer(nh)) {
        return createParContext(nh);
    }
}

export function* enumDocParts(): Generator<DocumentPart> {
    const parContainer = getParContainerElem();
    if (!parContainer) {
        throw Error("pars div not found");
    }
    let currelem = parContainer.firstElementChild;
    while (currelem && !isBottomContainer(currelem)) {
        if (!(currelem instanceof HTMLElement)) {
            throw Error("currelem not HTMLElement");
        }
        if (currelem.id === "HELP_PAR") {
            currelem = currelem.nextElementSibling;
            continue;
        }
        const par = fromHtmlElement(currelem);
        yield par;
        currelem = par.nextInHtml() ?? null;
    }
}

export function* enumPars(d: DerefOption) {
    for (const p of enumDocParts()) {
        yield* p.enumPars(d);
    }
}

export function* enumParCtxts() {
    for (const p of enumDocParts()) {
        for (const x of p.enumPars(DerefOption.Deref)) {
            yield new ParContext(x, p);
        }
    }
}
