import type {IPromise} from "angular";
import $ from "jquery";
import moment from "moment";
import {getActiveDocument} from "tim/document/activedocument";
import {documentglobals, genericglobals} from "tim/util/globals";
import {
    diffDialog,
    setDiffDialog,
    showDiffDialog,
} from "tim/document/showDiffDialog";
import type {ParContext} from "tim/document/structure/parContext";
import {
    createParContextOrHelp,
    findParentPar,
    fromParents,
    tryCreateParContextOrHelp,
} from "tim/document/structure/create";
import type {HelpPar} from "tim/document/structure/helpPar";
import type {IItem} from "tim/item/IItem";
import {Users} from "tim/user/userService";
import {$http, $log, $timeout} from "tim/util/ngimport";
import type {IOkResponse} from "tim/util/utils";
import {
    getViewName,
    isInViewport,
    isIOS,
    markPageDirty,
    posToRelative,
    to,
    to2,
} from "tim/util/utils";
import type {EditPosition} from "tim/document/editing/edittypes";
import {EditType} from "tim/document/editing/edittypes";
import type {OnClickArg} from "tim/document/eventhandlers";
import {onClick, onMouseOver, onMouseOverOut} from "tim/document/eventhandlers";
import {canSeeSource} from "tim/document/parhelpers";

export const readClasses = {
    1: "screen",
    2: "hover",
    3: "click",
    4: "read",
};

export enum ReadingType {
    OnScreen = 1,
    HoverPar = 2,
    ClickPar = 3,
    ClickRed = 4,
}

function isAlreadyRead(par: ParContext, readingType: ReadingType) {
    const readClassName = readClasses[readingType];
    const readline = par.getReadline();
    if (!readline) {
        return true;
    }
    if (!readline.classList.contains(readClassName)) {
        return false;
    }
    const lastReadTime = moment(readline.getAttribute(`time-${readClassName}`));
    const timeSinceLastRead = moment.duration(moment().diff(lastReadTime));
    return timeSinceLastRead < getActiveDocument().readExpiry();
}

export async function markParRead(par: ParContext, readingType: ReadingType) {
    const readClassName = readClasses[readingType];
    if (isAlreadyRead(par, readingType)) {
        return;
    }

    // If the document has cache enabled, other readingtypes would invalidate the cache very often,
    // so we won't post other types of readmarks.
    if (
        documentglobals().docSettings.cache &&
        readingType !== ReadingType.ClickRed
    ) {
        return;
    }

    const [docId, parId] = [par.par.docId, par.par.id];
    const readline = par.getReadline();
    if (!readline) {
        return;
    }
    readline.classList.add(readClassName);
    readline.setAttribute(`time-${readClassName}`, moment().toISOString());
    if (!Users.isLoggedIn() || !docId) {
        return;
    }
    const r = await to($http.put(`/read/${docId}/${parId}/${readingType}`, {}));
    if (!r.ok) {
        readline.classList.remove(readClassName);
        return;
    }
    readline.classList.remove(readClassName + "-modified");
    if (readingType === ReadingType.ClickRed) {
        markPageDirty();
        getActiveDocument().refreshSectionReadMarks();
    }
}

async function markParsRead(pars: ParContext[], item: IItem) {
    const parIds = pars
        .map((par) => par.readLineCtx)
        .filter((e) => !isAlreadyRead(e, ReadingType.ClickRed))
        .map((e) => {
            return [e.par.docId, e.par.id];
        });
    pars.forEach((p) =>
        p.getReadline()?.classList.add(readClasses[ReadingType.ClickRed])
    );
    const r = await to(
        $http.put(
            "/read/" + item.id + "/" + "null" + "/" + ReadingType.ClickRed,
            {pars: parIds}
        )
    );
    if (!r.ok) {
        $log.error("Could not save the read markings");
        return;
    }
    markPageDirty();
}

let readPromise: IPromise<unknown> | null = null;
let readingParId: string | undefined;

function queueParagraphForReading() {
    // Filter out yet unprocessed paragraphs because section builder might not yet been run
    const isRead = (
        e: HelpPar | ParContext | undefined
    ): e is HelpPar | ParContext =>
        e !== undefined && !e.isHelp && !isAlreadyRead(e, ReadingType.OnScreen);
    const visiblePars = $(".par:not(.preamble)")
        .filter((i, e) => isInViewport(e, true))
        .toArray()
        .map((e) => tryCreateParContextOrHelp(e))
        .filter(isRead);
    if (visiblePars.length === 0) {
        return;
    }
    const parToRead = visiblePars[0];
    if (parToRead.isHelp) {
        return;
    }
    const parId = parToRead.originalPar.id;

    if (readPromise != null && readingParId !== parId) {
        $timeout.cancel(readPromise);
    } else if (readingParId === parId) {
        return;
    }
    readingParId = parId;
    const numWords =
        parToRead
            .textContent()
            ?.trim()
            .split(/[\s\n]+/).length ?? 1;
    readPromise = $timeout(async () => {
        await markParRead(parToRead, ReadingType.OnScreen);
        queueParagraphForReading();
    }, 300 * numWords);
}

async function handleSeeChanges(elem: JQuery, e: OnClickArg) {
    const par = fromParents(elem).readLineCtx;
    const [id, blockId, t] = [par.par.docId, par.par.id, par.par.hash];
    const parData = await to(
        $http.get<Array<{par_hash: string}>>(`/read/${id}/${blockId}`)
    );
    if (!parData.ok) {
    } else {
        const oldb = $http.get<{text: string}>("/getBlock", {
            params: {
                doc_id: id,
                par_hash: parData.result.data[0].par_hash,
                par_id: blockId,
            },
        });
        const newb = $http.get<{text: string}>("/getBlock", {
            params: {
                doc_id: id,
                par_hash: t,
                par_id: blockId,
            },
        });
        const oldr = await to(oldb);
        const newbr = await to(newb);
        if (oldr.ok && newbr.ok) {
            const mi = await showDiffDialog({
                left: oldr.result.data.text,
                right: newbr.result.data.text,
                title: "Changes",
                showToolbar: true,
            });
            if (diffDialog) {
                diffDialog.close();
            }
            setDiffDialog(mi);
            await to2(mi.result);
            setDiffDialog(undefined);
        }
    }
}

async function readlineHandler(elem: JQuery, e: OnClickArg) {
    if ((e.target as HTMLElement).tagName === "BUTTON") {
        return;
    }
    const par = createParContextOrHelp(findParentPar(elem));
    if (par.isHelp) {
        return;
    }
    await markParRead(par.readLineCtx, ReadingType.ClickRed);
}

export async function initReadings(item: IItem) {
    onClick(".readline > button", handleSeeChanges);
    onClick(
        ".readline",
        readlineHandler,
        false,
        (e) => e.classList.contains("read-modified") && isIOS()
    );
    onMouseOver(".readline.read-modified", (p, e) => {
        const ev = e.originalEvent as MouseEvent | TouchEvent;
        const pos = posToRelative(p[0], ev);
        const children = p.children();

        // Workaround: The document might have been edited just now, so creating context may fail
        // if getActiveDocument().rebuildSections() has not yet been called.
        const par = tryCreateParContextOrHelp(findParentPar(p));
        if (!par || par.isHelp) {
            return;
        }
        if (children.length === 0 && canSeeSource(item, par)) {
            const x = document.createElement("button");
            x.classList.add("timButton", "btn-xs");
            x.title = "See changes";
            x.textContent = "Changes";
            x.style.top = pos.y + "px";
            p.append(x);
        } else if (children.length > 0) {
            // children[0].style.top = pos.y + "px";
        }
    });

    $(window).scroll(queueParagraphForReading);

    queueParagraphForReading();

    onClick(".readsection", function readSectionHandler($readsection, e) {
        const doc = getActiveDocument();
        const par = fromParents($readsection).readLineCtx;
        const pars = doc.getSectionFor(par.par);
        if (!pars) {
            return;
        }
        markParsRead(pars, item);
        // Go through pars since areas can get their own section markers
        pars.forEach((p) => p.getReadSectionMark()?.remove());
    });

    onMouseOverOut(
        "#pars .par:not('.preamble')",
        function mouseOverHandler($this, e, select) {
            const el = $this[0];

            // Workaround: The document might have been edited just now, so creating context may fail
            // if getActiveDocument().rebuildSections() has not yet been called.
            const ctx = tryCreateParContextOrHelp(el);
            if (select && ctx && !ctx.isHelp) {
                markParRead(ctx, ReadingType.HoverPar);
            }
        }
    );

    if (Users.isLoggedIn() && genericglobals().bookmarks != null) {
        await $timeout(10000);
        await to(
            $http.post("/bookmarks/markLastRead/" + item.id, {
                view: getViewName(),
            })
        );
    }
}

export async function handleUnread(pos: EditPosition) {
    if (pos.type !== EditType.Edit && pos.type !== EditType.CommentAction) {
        return;
    }
    const first = pos.type === EditType.Edit ? pos.pars.start : pos.par;
    const [docid, parid] = [first.par.docId, first.par.id];
    const result = await to(
        $http.put<IOkResponse & {latest?: unknown}>(
            `/unread/${docid}/${parid}`,
            {}
        )
    );
    if (!result.ok) {
        return;
    }
    const rline = first.getReadline();
    if (!rline) {
        return;
    }
    rline.classList.remove("read", "read-modified");
    if (result.result.data.latest) {
        rline.classList.add("read-modified");
    }
    getActiveDocument().refreshSectionReadMarks();
}
