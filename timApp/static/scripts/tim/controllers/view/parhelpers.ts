import angular from "angular";
import $ from "jquery";
import {getActiveDocument} from "./document";

export function createNewPar(): JQuery {
    return $("<div>", {class: "par new", id: "NEW_PAR", attrs: "{}"})
        .append($("<div>", {class: "parContent"}).html("New paragraph"));
}

export function getParId($par: JQuery): string | null {
    if ($par === null || $par.length === 0 || !$par.hasClass("par")) {
        return null;
    }
    return $par.attr("id");
}

export function getParAttributes($par: JQuery) {
    return JSON.parse($par.attr("attrs"));
}

export function getAreaId($area: JQuery) {
    if (!$area.hasClass("area")) {
        return null;
    }

    return $area.attr("data-name");
}

export function getAreaDocId($area: JQuery) {
    if (!$area.hasClass("area")) {
        return null;
    }

    return $area.attr("data-doc-id");
}

export function getFirstPar($parOrArea: JQuery) {
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

export function getLastPar($parOrArea: JQuery) {
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

export function getFirstParId($parOrArea: JQuery) {
    return getParId(getFirstPar($parOrArea));
}

export function getLastParId($parOrArea: JQuery) {
    return getParId(getLastPar($parOrArea));
}

export function getNextPar($par: JQuery) {
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

export function getRefAttrs($par: JQuery) {
    return {
        "ref-id": $par.attr("ref-id"),
        "ref-t": $par.attr("ref-t"),
        "ref-doc-id": $par.attr("ref-doc-id"),
        "ref-attrs": JSON.parse($par.attr("ref-attrs") || "{}"),
    };
}

export function forEachParagraph(func) {
    $(".paragraphs .par").each(func);
}

export function getElementByRefId(ref) {
    return $(".par[ref-id='" + ref + "']");
}

export function isReference($par: JQuery) {
    return angular.isDefined($par.attr("ref-id"));
}

export function getParIndex($par: JQuery) {
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
            //$log.info('getParIndex(' + parId + ') = ' + realIndex);
            return realIndex;
        }

        realIndex++;
    }
}

export function getArea(area: string) {
    return $(".area_" + area);
}

export function getPars($parFirst: JQuery, $parLast: JQuery) {
    const pars = [$parFirst];
    let $par = $parFirst;
    let $next = $par.next();
    let i = 1000000;

    while (i > 0) {
        if ($next.length === 0) {
            $par = $par.parent();
            $next = $par.next();
            if ($par.prop("tagName").toLowerCase() === "html") {
                break;
            }

            continue;
        }

        if ($next.hasClass("area")) {
            $next = $next.children(".areaContent").children().first();
            continue;
        }

        if ($next.hasClass("par") && getParIndex($next) !== null) {
            pars.push($next);
            if ($next.is($parLast)) {
                break;
            }
        }

        $par = $next;
        $next = $par.next();
        i -= 1;
    }

    return $(pars).map(function() {
        return this.toArray();
    });
}

export function dereferencePar($par: JQuery) {
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

export function isSettings(text) {
    return text.startsWith('``` {settings=""}');
}

export function isPreamble($par: JQuery) {
    return $par.hasClass("preamble");
}

export function getPreambleDocId($par: JQuery) {
    return $par.attr("data-from-preamble");
}

export function isActionablePar($par: JQuery) {
    const isPreview = $par.parents(".previewcontent").length > 0;
    const preamble = isPreamble($par);
    return !(isPreview || preamble);
}

export function canEditPar(item, $par: JQuery) {
    const attrs = getParAttributes($par);
    const right = item.rights[attrs.edit];
    if (right === undefined) {
        return true;
    }
    return right;
}

export const EDITOR_CLASS = "editorArea";
export const EDITOR_CLASS_DOT = "." + EDITOR_CLASS;
