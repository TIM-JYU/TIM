import {ParContext} from "tim/document/structure/parContext";
import {enumDocParts, PreambleIteration} from "tim/document/structure/parsing";
import {DerefOption} from "tim/document/structure/derefOption";
import {getActiveDocument} from "tim/document/activedocument";
import {getParContainerElem} from "tim/document/structure/create";

export function nextParContext(par: ParContext) {
    const next = getActiveDocument().getParMap().get(par.par.htmlElement)![2];
    return next ? new ParContext(next) : undefined;
}

export function* enumPars(d: DerefOption) {
    for (const p of enumDocParts(
        PreambleIteration.Include,
        getParContainerElem()
    )) {
        yield* p.enumPars(d);
    }
}
