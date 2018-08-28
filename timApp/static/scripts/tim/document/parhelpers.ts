import angular from "angular";
import $ from "jquery";
import {getActiveDocument} from "tim/document/document";
import {IItem} from "../item/IItem";

export type Paragraph = JQuery;
export type Paragraphs = JQuery;
export type Area = JQuery;
export type ParOrArea = JQuery;

export function createNewPar(): Paragraph {
    return $("<div>", {class: "par new", id: "NEW_PAR", attrs: "{}"})
        .append($("<div>", {class: "parContent"}).html("New paragraph"));
}

export function getParId($par: Paragraph | null | undefined): string | undefined {
    if ($par == null || $par.length === 0 || !$par.hasClass("par")) {
        return;
    }
    return $par.attr("id");
}

export function getParAttributes($par: Paragraph): {[attr: string]: string} {
    const attrs = $par.attr("attrs");
    if (!attrs) {
        return {};
    }
    return JSON.parse(attrs);
}

export function getAreaId($area: Paragraph) {
    if (!$area.hasClass("area")) {
        return null;
    }

    return $area.attr("data-name");
}

export function getAreaDocId($area: Paragraph) {
    if (!$area.hasClass("area")) {
        return null;
    }

    return $area.attr("data-doc-id");
}

export function getFirstPar($parOrArea: Paragraph): Paragraph | null {
    if ($parOrArea.length > 1) {
        return getFirstPar($parOrArea.first());
    }
    if ($parOrArea.hasClass("area")) {
        return $parOrArea.find(".par").first();
    }
    if ($parOrArea.hasClass("par")) {
        return $parOrArea;
    }

    return null;
}

export function getLastPar($parOrArea: Paragraph): Paragraph | null {
    if ($parOrArea.length > 1) {
        return getLastPar($parOrArea.last());
    }
    if ($parOrArea.hasClass("area")) {
        return $parOrArea.find(".par").last();
    }
    if ($parOrArea.hasClass("par")) {
        return $parOrArea;
    }

    return null;
}

export function getFirstParId($parOrArea: Paragraph) {
    return getParId(getFirstPar($parOrArea));
}

export function getLastParId($parOrArea: Paragraph) {
    return getParId(getLastPar($parOrArea));
}

export function getNextPar($par: Paragraph) {
    const $next = $par.next();
    if ($next.hasClass("par")) {
        return $next;
    }
    if ($next.hasClass("area")) {
        const $content = $next.children(".areaContent");
        if ($content.length > 0) {
            const $firstpar = $content.children(".par:first");
            if ($firstpar.length > 0) {
                return $firstpar;
            }
        }
    }
    return null;
}

export function getElementByParId(id: string) {
    return $("#" + id);
}

export function getElementByParHash(t: string) {
    return $("[t='" + t + "']");
}

export function getRefAttrs($par: Paragraph) {
    return {
        "ref-id": $par.attr("ref-id"),
        "ref-t": $par.attr("ref-t"),
        "ref-doc-id": $par.attr("ref-doc-id"),
        "ref-attrs": JSON.parse($par.attr("ref-attrs") || "{}"),
    };
}

export function forEachParagraph(func: (this: HTMLElement, index: number, element: HTMLElement) => void | false) {
    $(".paragraphs .par").each(func);
}

export function getElementByRefId(ref: string) {
    return $(".par[ref-id='" + ref + "']");
}

export function isReference($par: Paragraph) {
    return angular.isDefined($par.attr("ref-id"));
}

export function getParIndex($par: Paragraph) {
    const parId = getParId($par);
    const $pars = $(".par");
    let realIndex = 0;

    if ($par.find(".parContent").not(":empty").length === 0) {
        return null;
    }

    for (let i = 0; i < $pars.length; i++) {
        const $node = $pars.eq(i);
        if ($node.find(".parContent").not(":empty").length === 0) {
            continue;
        }

        if (getParId($node) === parId) {
            // $log.info('getParIndex(' + parId + ') = ' + realIndex);
            return realIndex;
        }

        realIndex++;
    }
    return null;
}

export function getArea(area: string): Area {
    return $(".area_" + area);
}

export function getPars($parFirst: Paragraph, $parLast: Paragraph): JQuery {
    const pars = [];
    let next: Paragraph | null = $parFirst;
    while (next != null) {
        pars.push(next);
        if (next.is($parLast)) {
            break;
        }
        next = getNextPar(next);
    }
    return pars.reduce((previousValue, currentValue, currentIndex, array) => previousValue.add(currentValue));
}

export function dereferencePar($par: Paragraph) {
    if ($par.length === 0 || !$par.hasClass("par")) {
        return null;
    }
    if ($par.attr("ref-id") && $par.attr("ref-doc-id")) {
        return [$par.attr("ref-doc-id"), $par.attr("ref-id")];
    }
    const doc = getActiveDocument();
    return [doc.id, $par.attr("id")];
}

/**
 * Adds an element to the paragraph margin.
 * @method addElementToParagraphMargin
 * @param par - Paragraph where the element will be added
 * @param el - Element to add
 */
export function addElementToParagraphMargin(par: Element, el: Element) {
    const container = par.getElementsByClassName("notes");
    if (container.length > 0) {
        container[0].appendChild(el);
    } else {
        const newContainer = document.createElement("div");
        newContainer.classList.add("notes");
        newContainer.appendChild(el);
        par.appendChild(newContainer);
    }
}

export function isSettingsPar(par: Paragraph) {
    return getParAttributes(par).settings != null;
}

export function isPreamble($par: Paragraph) {
    return $par.hasClass("preamble");
}

export function getPreambleDocId($par: Paragraph) {
    return $par.attr("data-from-preamble");
}

export function isActionablePar($par: Paragraph) {
    const isPreview = $par.parents(".previewcontent").length > 0;
    const preamble = isPreamble($par);
    return !(isPreview || preamble);
}

export function canEditPar(item: IItem, $par: JQuery) {
    const attrs = getParAttributes($par);
    const right = item.rights[attrs.edit];
    if (right == null) {
        return item.rights.editable;
    }
    return right;
}

/**
 * Get paragraph from element or its parent, if it exists.
 * @param {Element} e
 * @returns {Paragraph | undefined}
 */
export function findElementParagraph(e: Element): Paragraph | undefined {
    if ($(e).hasClass(".par")) {
        return $(e);
    } else {
        return $(e).parents(".par");
    }
}

export const EDITOR_CLASS = "editorArea";
export const EDITOR_CLASS_DOT = "." + EDITOR_CLASS;
