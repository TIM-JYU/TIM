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

/**
 * Checks if the user can edit the paragraph based on the
 * edit attribute and document settings.
 * User can edit if
 *   - he is owner of the document
 *   - in edit attribute there is a group that the user is in
 *   - in edit attribute there is a right that the user has
 *   - document is set to parAuthorOnlyEdit and the user
 *     is the author of the paragraph
 * Else: Even the user has normal edit right, if the document is set to
 * parAuthorOnlyEdit and the user is not the author of the paragraph,
 * he can't edit.
 * @param item - The item containing the paragraph
 * @param par - The paragraph context to check
 * @returns true if the user can edit the paragraph, false otherwise
 */
export function canEditPar(item: IItem, par: ParContext) {
    if (item.rights.owner) {
        return true; // Owner is always allowed to edit
    }
    let res: boolean = false;
    const editRaw = par.par.attrs.edit ?? "";
    const user = Users.getCurrent();
    res = item.rights.editable;
    if (editRaw !== "") {
        const edits = editRaw
            .split(",")
            .map((s) => s.trim())
            .filter((s) => s.length > 0);

        const knownEdits: string[] = [];
        const groupEdits: string[] = [];
        for (const e of edits) {
            if (RightNames.is(e) || e === "view" || e === "edit") {
                knownEdits.push(e);
            } else {
                groupEdits.push(e);
            }
        }
        const groups = user.groups;
        for (const edit of groupEdits) {
            // groups are checked first, because if the user is in a group
            // that has edit rights,
            // they should be able to edit even if they don't have the right directly
            if (groups.some((g) => g.name === edit)) {
                return true;
            }
        }

        // if there are no group edits, check the known edits
        for (const edit of knownEdits) {
            if (RightNames.is(edit)) {
                res = item.rights[edit];
            }
            if (edit === "view") {
                // if the user can view, he has view right
                res = true;
            }
            if (edit === "edit" && item.rights.editable) {
                // if the user can edit, he has edit right
                res = true;
            }
            if (res) {
                return true;
            }
        }
        res = false;
    }

    // If the document is set to parAuthorOnlyEdit,
    // only the author of the paragraph can edit it.
    // The author can edit even there is no edit right,
    // but if the user is not the author, they can't edit
    // even if they have the edit right.
    const globals = someglobals();
    if (isDocumentGlobals(globals) && globals.docSettings.parAuthorOnlyEdit) {
        const authorInfo = par.getAuthorInfo();
        if (!authorInfo) {
            return res;
        }
        if (authorInfo?.usernames?.includes(user.name)) {
            return true;
        }
        if (!item.rights.owner) {
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
