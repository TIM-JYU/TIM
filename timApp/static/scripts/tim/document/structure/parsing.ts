import {maybeDeref} from "tim/document/structure/maybeDeref";
import {Area, AreaEndPar, AreaStartPar} from "tim/document/structure/area";
import {Result} from "tim/util/utils";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import {CollapseControls} from "tim/document/structure/collapseControls";
import * as t from "io-ts";
import {Paragraph} from "tim/document/structure/paragraph";
import {documentglobals} from "tim/util/globals";
import {DocumentPart} from "tim/document/structure/documentPart";

function getAttr(el: Element, attrName: string) {
    const attr = el.getAttribute(attrName);
    if (attr === null) {
        console.log(el);
        throw Error(err(`missing ${attrName} attribute`));
    }
    return attr;
}

function err(msg: string) {
    return `Element is not a valid paragraph (${msg}).`;
}

function parseAttrs(attrs: string) {
    let parsed;
    try {
        parsed = JSON.parse(attrs);
    } catch {
        throw Error(err(`invalid attrs, JSON parse failed`));
    }
    if (!t.record(t.string, t.unknown).is(parsed)) {
        throw Error(err(`invalid JSON attr, not an object`));
    }
    // attrs contains a list of classes (if the paragraph has at least 1 class) but we don't need it in TS.
    delete parsed.classes;
    if (!t.record(t.string, t.string).is(parsed)) {
        throw Error(err(`invalid JSON attr, not an object of strings`));
    }
    return parsed;
}

/**
 * Constructs a {@link DocumentPart} from the given HTML element.
 * @param el An element that is a direct child of the #pars container.
 */
export function fromHtmlElement(el: HTMLElement) {
    const ucarea = tryParseUncollapsibleArea(el);
    if (ucarea.ok) {
        return ucarea.result;
    }
    const carea = tryParseCollapsibleArea(el);
    if (carea.ok) {
        return carea.result;
    }

    const {par, original} = parseParagraph(el);
    if (original) {
        const refpar = new ReferenceParagraph(original, par);
        par.parent = refpar;
        return refpar;
    }
    return par;
}

function tryParseCollapsibleArea(
    el: HTMLElement
): Result<Area | ReferenceParagraph<Area>, string> {
    if (!el.classList.contains("par")) {
        return {ok: false, result: "missing par class"};
    }
    const areaname = el.getAttribute("data-area");
    if (!areaname) {
        return {ok: false, result: "expected data-area"};
    }
    const x = el.nextElementSibling;
    if (!x) {
        return {ok: false, result: "area container missing"};
    }
    const content = x.firstElementChild;
    if (!content || !(content instanceof HTMLElement)) {
        return {ok: false, result: "area container content missing"};
    }
    let contentpars = getAreaContent(content);
    const ret = maybeFixAreaContent(contentpars);
    contentpars = ret.contentpars;
    const last = maybeDeref(contentpars[contentpars.length - 1]);
    if (!(last instanceof Paragraph)) {
        return {ok: false, result: ""};
    }
    const p = parseParagraph(el);
    const start = new AreaStartPar(p.par, areaname);
    const end = new AreaEndPar(last, areaname);
    const inner = contentpars.slice(0, contentpars.length - 1);
    const area = new Area(start, end, inner, new CollapseControls(start));
    p.par.parent = area;
    for (const c of contentpars) {
        c.parent = area;
    }
    let result: ReferenceParagraph<Area> | Area;
    if (p.original) {
        result = new ReferenceParagraph(p.original, area);
        area.parent = result;
    } else {
        result = area;
    }
    return {ok: true, result: result};
}

function parseParagraph(e: HTMLElement) {
    let original = null;
    const refattrs = e.getAttribute("ref-attrs");
    const parOrOrig = new Paragraph(
        e.id,
        getAttr(e, "t"),
        documentglobals().curr_item.id,
        parseAttrs(getAttr(e, "attrs")),
        e,
        e.getAttribute("data-from-preamble") ?? undefined
    );
    let par;
    if (refattrs) {
        const refparsed = parseAttrs(refattrs);
        const target = new Paragraph(
            getAttr(e, "ref-id"),
            getAttr(e, "ref-t"),
            parseInt(getAttr(e, "ref-doc-id"), 10),
            refparsed,
            e,
            e.getAttribute("data-from-preamble") ?? undefined
        );
        original = parOrOrig;
        par = target;
    } else {
        par = parOrOrig;
    }
    return {par, original};
}

export function isBottomContainer(e: Element) {
    return e.classList.contains("addBottomContainer");
}

export enum PreambleIteration {
    Exclude,
    Include,
}

export function* enumDocParts(
    pi: PreambleIteration,
    parContainer: HTMLElement
): Generator<DocumentPart> {
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
        if (
            !par.getFirstOrigPar()?.preamblePath ||
            pi === PreambleIteration.Include
        ) {
            yield par;
        }
        currelem = par.nextInHtml() ?? null;
    }
}

function getAreaContent(areacontent: HTMLElement) {
    return Array.from(enumDocParts(PreambleIteration.Include, areacontent));
}

/**
 * Right now, the HTML structure sent by the server doesn't properly differentiate between these cases:
 *
 * * a reference to an area
 * * an area with full of reference pars
 *
 * Both of these cases look similar in HTML. So, for now, we have to figure out which one of these is the case for the
 * given contentpars. We do that by checking whether all the contentpars are ReferenceParagraphs and whether they all
 * share the same doc and paragraph ids in the original paragraph.
 *
 * This function fixes most of the common cases but not more complicated ones - it is necessary to fix the server
 * to send proper information.
 *
 * For example, the following case does not work properly currently:
 *
 * ```
 * doc 1      doc2         doc3
 * ----------------------------
 * refpar --> area
 *              refpar --> area
 *                           par
 * ```
 *
 * When doc 1 is viewed, the HTML looks as if there were only the first reference (doc 1 --> doc 2).
 *
 * @param contentpars The area content to inspect.
 */
function maybeFixAreaContent(contentpars: DocumentPart[]) {
    const originalIdSet = new Set(
        contentpars.map(
            (c) =>
                c instanceof ReferenceParagraph &&
                `${c.original.id}-${c.original.docId}`
        )
    );
    let orig;
    if (
        originalIdSet.size === 1 &&
        originalIdSet.values().next().value !== false
    ) {
        orig = (contentpars[0] as ReferenceParagraph<Area | Paragraph>)
            .original;
        contentpars = contentpars.map((c) => maybeDeref(c));
    }
    return {contentpars, orig};
}

function tryParseUncollapsibleArea(
    el: HTMLElement
): Result<Area | ReferenceParagraph<Area>, string> {
    if (el.classList.contains("par")) {
        return {ok: false, result: "unexpected par class"};
    }
    if (!el.classList.contains("area")) {
        return {ok: false, result: "expected area class"};
    }

    let areaname = null;
    for (const c of el.classList) {
        if (c.startsWith("area_")) {
            areaname = c.substring(5);
        }
    }
    if (!areaname) {
        return {ok: false, result: "area name missing"};
    }
    const areacontent = el.firstElementChild;
    if (!areacontent || !(areacontent instanceof HTMLElement)) {
        return {ok: false, result: "areacontent missing"};
    }
    let contentpars = getAreaContent(areacontent);
    if (contentpars.length < 2) {
        throw Error("uncollapsible area cannot have less than 2 content pars");
    }
    const ret = maybeFixAreaContent(contentpars);
    contentpars = ret.contentpars;
    const orig = ret.orig;
    const first = maybeDeref(contentpars[0]);
    const last = maybeDeref(contentpars[contentpars.length - 1]);
    if (!(first instanceof Paragraph)) {
        return {ok: false, result: ""};
    }
    if (!(last instanceof Paragraph)) {
        return {ok: false, result: ""};
    }
    if (first.attrs.area !== areaname) {
        throw Error(
            `area name mismatch 1: ${first.attrs.area} !== ${areaname}`
        );
    }
    if (last.attrs.area_end !== areaname) {
        throw Error("area name mismatch 2");
    }
    const start = new AreaStartPar(first, areaname);
    const end = new AreaEndPar(last, areaname);
    const inner = contentpars.slice(1, contentpars.length - 1);
    const area = new Area(start, end, inner, undefined);
    for (const p of contentpars) {
        p.parent = area;
    }
    let result: ReferenceParagraph<Area> | Area;
    if (orig) {
        result = new ReferenceParagraph(orig, area);
        area.parent = result;
    } else {
        result = area;
    }
    return {ok: true, result: result};
}
