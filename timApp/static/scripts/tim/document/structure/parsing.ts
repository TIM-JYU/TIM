import {maybeDeref} from "tim/document/structure/maybeDeref";
import {BrokenArea} from "tim/document/structure/brokenArea";
import {Area, AreaEndPar, AreaStartPar} from "tim/document/structure/area";
import {HelpPar} from "tim/document/structure/helpPar";
import {ParContext} from "tim/document/structure/parContext";
import {Result} from "tim/util/utils";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import {CollapseControls} from "tim/document/structure/collapseControls";
import * as t from "io-ts";
import {Paragraph} from "tim/document/structure/paragraph";
import {documentglobals} from "tim/util/globals";

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

function getContext(el: Element) {
    let rootChild = el;
    while (true) {
        const p = rootChild.parentElement;
        if (!p) {
            throw Error("root paragraph container not found");
        }
        // The main content area of the document has "pars" id, but in editor preview,
        // the div has "paragraphs" class.
        if (p.id === "pars" || p.classList.contains("paragraphs")) {
            break;
        }
        rootChild = p;
    }
    const prevsib = rootChild.previousElementSibling;
    if (
        prevsib &&
        (prevsib.classList.contains("areaexpand") ||
            prevsib.classList.contains("areacollapse"))
    ) {
        rootChild = prevsib;
    }
    if (!(rootChild instanceof HTMLElement)) {
        throw Error("root was not HTMLElement");
    }
    return fromHtmlElement(rootChild);
}

export function fromParents(p: JQuery) {
    if (!p.hasClass("par")) {
        p = p.parents(".par");
    }
    return createParContextOrHelp(p[0]);
}

export function createParContext(el: Element) {
    const c = createParContextOrHelp(el);
    if (c instanceof ParContext) {
        return c;
    }
    throw Error("unexpected help par");
}

export function createParContextOrHelp(el: Element) {
    const ctx = getContext(el);
    let s = ctx.getSinglePar(el);
    if (!s) {
        const target = maybeDeref(ctx);
        if (target instanceof BrokenArea) {
            s = maybeDeref(target.inner[0]);
        } else if (target instanceof Area && !target.collapse) {
            s = target.startPar.par;
        } else {
            throw Error("didn't find corresponding single par");
        }
    }
    if (s.id === "HELP_PAR") {
        return new HelpPar(s);
    }
    return new ParContext(s, ctx);
}

export function createParContextNoPreview(el: Element) {
    const ctx = createParContextOrHelp(el);
    if (!ctx.isHelp && !ctx.isInPreview()) {
        return ctx;
    }
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
        return new ReferenceParagraph(original, par);
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
    const contentpars = getAreaContent(content);
    const p = parseParagraph(el);
    const start = new AreaStartPar(p.par, areaname);
    const end = new AreaEndPar(
        maybeDeref(contentpars[contentpars.length - 1]),
        areaname
    );
    const inner = contentpars.slice(0, contentpars.length - 1);
    const area = new Area(start, end, inner, new CollapseControls(start));
    const result = p.original ? new ReferenceParagraph(p.original, area) : area;
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

function getAreaContent(areacontent: HTMLElement) {
    const contentpars = [];
    for (const e of areacontent.children) {
        if (!(e instanceof HTMLElement)) {
            throw Error("not a HTML element");
        }
        const ret = parseParagraph(e);
        contentpars.push(
            ret.original
                ? new ReferenceParagraph(ret.original, ret.par)
                : ret.par
        );
    }
    return contentpars;
}

function tryParseUncollapsibleArea(
    el: HTMLElement
): Result<Area | ReferenceParagraph<Area> | BrokenArea, string> {
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
    const contentpars = getAreaContent(areacontent);
    // TODO: Nested areas are not currently supported. Server sends broken area pieces in that case.
    //  We still want to be able to handle the document, so we return a BrokenArea.
    if (contentpars.length < 2) {
        return {
            ok: true,
            result: new BrokenArea(
                areaname,
                el,
                contentpars,
                "too few contentpars"
            ),
        };
    }
    const fcontent = contentpars[0];
    const first = maybeDeref(fcontent);
    if (first.attrs.area !== areaname) {
        return {
            ok: true,
            result: new BrokenArea(
                areaname,
                el,
                contentpars,
                "area name not found or mismatch"
            ),
        };
    }
    const last = maybeDeref(contentpars[contentpars.length - 1]);
    if (last.attrs.area_end !== areaname) {
        return {
            ok: true,
            result: new BrokenArea(
                areaname,
                el,
                contentpars,
                "area_end name not found or mismatch"
            ),
        };
    }
    const start = new AreaStartPar(first, areaname);
    const end = new AreaEndPar(last, areaname);
    const inner = contentpars.slice(1, contentpars.length - 1);
    const area = new Area(start, end, inner, undefined);
    const result =
        fcontent instanceof ReferenceParagraph
            ? new ReferenceParagraph(fcontent.original, area)
            : area;
    return {ok: true, result: result};
}

export function getParContainerElem() {
    return document.getElementById("pars");
}
