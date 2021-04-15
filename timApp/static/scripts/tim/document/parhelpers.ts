import $ from "jquery";
import {Paragraph} from "tim/document/structure/paragraph";
import {Area} from "tim/document/structure/area";
import {ParContext} from "tim/document/structure/parContext";
import {maybeDeref} from "tim/document/structure/maybeDeref";
import {getParContainerElem} from "tim/document/structure/parsing";
import {enumParCtxts} from "tim/document/structure/iteration";
import {IItem} from "../item/IItem";
import {RightNames} from "../user/IRights";
import {isInViewport} from "../util/utils";

export function getElementByParId(id: string) {
    return $("#" + id);
}

/**
 * Adds an element to the paragraph margin.
 * @param par - Paragraph where the element will be added
 * @param el - Element to add
 */
export function addElementToParagraphMargin(par: ParContext, el: Element) {
    const container = par.getNotesContainer();
    container.appendChild(el);
}

export function canEditPar(item: IItem, par: ParContext) {
    const edit = par.par.attrs.edit;
    if (!RightNames.is(edit)) {
        return item.rights.editable;
    }
    return item.rights[edit];
}

export function canSeeSource(item: IItem, par: ParContext) {
    if (item.rights.copy) {
        return true;
    }
    const attrs = par.par.attrs;
    return !(attrs.plugin || attrs.defaultplugin);
}

export const EDITOR_CLASS = "editorArea";
export const EDITOR_CLASS_DOT = "." + EDITOR_CLASS;

/**
 * Save currently viewed paragraph hash to browser history to make the browser
 * come back there when returning to the document.
 */
export function saveCurrentScreenPar() {
    if (!getParContainerElem()) {
        return;
    }
    let par;
    for (const p of enumParCtxts()) {
        if (!isInViewport(p.par.htmlElement)) {
            continue;
        }
        const d = maybeDeref(p.context);
        if (d instanceof Area && d.collapse?.isCollapsed()) {
            continue;
        }
        if (!p.par.preamblePath) {
            par = p;
            break;
        }
    }
    if (par) {
        // Don't replace if the hash is going to stay the same.
        let hash = getParAnchor(par.originalPar);
        if (hash == null) {
            hash = "";
        }
        if (location.hash !== hash) {
            const url = `${location.protocol}//${location.host}${location.pathname}${location.search}${hash}`;
            window.history.replaceState(undefined, document.title, url);
        }
    }
}

/**
 * Get paragraph header hash if available, otherwise use the id as hash.
 */
export function getParAnchor(par: Paragraph) {
    const headerlink = par.htmlElement.querySelector(".headerlink");
    if (headerlink) {
        return headerlink.getAttribute("href");
    }
    return `#${par.id}`;
}

export function getCitePar(docId: number, par: ParContext) {
    return `#- {rd="${docId}" rl="no" rp="${par.originalPar.id}"}`;
}
