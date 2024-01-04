import $ from "jquery";
import type {Paragraph} from "tim/document/structure/paragraph";
import {Area} from "tim/document/structure/area";
import type {ParContext} from "tim/document/structure/parContext";
import {enumPars} from "tim/document/structure/iteration";
import {DerefOption} from "tim/document/structure/derefOption";
import {getParContainerElem} from "tim/document/structure/create";
import type {IItem} from "tim/item/IItem";
import {RightNames} from "tim/user/IRights";
import {isInViewport} from "tim/util/utils";
import {isDocumentGlobals, someglobals} from "tim/util/globals";
import {Users} from "tim/user/userService";

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
    let res: boolean;
    if (!RightNames.is(edit)) {
        res = item.rights.editable;
    } else {
        res = item.rights[edit];
    }

    const globals = someglobals();
    if (isDocumentGlobals(globals) && globals.docSettings.parAuthorOnlyEdit) {
        const authorInfo = par.getAuthorInfo();
        const userName = Users.getCurrent().name;
        if (!item.rights.owner && !authorInfo?.usernames?.includes(userName)) {
            res = false;
        }
    }

    return res;
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
    for (const p of enumPars(DerefOption.NoDeref)) {
        if (!isInViewport(p.htmlElement, true)) {
            continue;
        }
        const d = p.parent;
        if (d instanceof Area && d.collapse?.isCollapsed()) {
            continue;
        }
        if (!p.preamblePath) {
            par = p;
            break;
        }
    }
    if (par) {
        // Don't replace if the hash is going to stay the same.
        let hash = getParAnchor(par);
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
    const headerlink = par.htmlElement.querySelector(".headerlink > a");
    if (headerlink) {
        return headerlink.getAttribute("href");
    }
    return `#${par.id}`;
}

export function getCitePar(docId: number, par: ParContext) {
    return `#- {rd="${docId}" rl="no" rp="${par.originalPar.id}"}`;
}
