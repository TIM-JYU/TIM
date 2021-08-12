import {IPromise} from "angular";
import $ from "jquery";
import moment from "moment";
import {getActiveDocument} from "tim/document/activedocument";
import {documentglobals, genericglobals} from "tim/util/globals";
import {
    diffDialog,
    setDiffDialog,
    showDiffDialog,
} from "tim/document/showDiffDialog";
import {ParContext} from "tim/document/structure/parContext";
import {
    createParContextOrHelp,
    findParentPar,
    fromParents,
    tryCreateParContextOrHelp,
} from "tim/document/structure/create";
import {IItem} from "../item/IItem";
import {Users} from "../user/userService";
import {$http, $log, $timeout} from "../util/ngimport";
import {
    getViewName,
    IOkResponse,
    isInViewport,
    markPageDirty,
    posToRelative,
    to,
} from "../util/utils";
import {EditPosition, EditType} from "./editing/edittypes";
import {
    onClick,
    OnClickArg,
    onMouseOver,
    onMouseOverOut,
} from "./eventhandlers";
import {canSeeSource} from "./parhelpers";

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
    const visiblePars = $(".par:not('.preamble')")
        .filter((i, e) => isInViewport(e))
        .toArray()
        .map((e) => createParContextOrHelp(e))
        .filter((e) => !e.isHelp && isAlreadyRead(e, ReadingType.OnScreen));
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
    const par = fromParents(elem);
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
            await to(mi.result);
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
    await markParRead(par, ReadingType.ClickRed);
}

export async function initReadings(item: IItem) {
    onClick(".readline > button", handleSeeChanges);
    onClick(".readline", readlineHandler);
    onMouseOver(".readline.read-modified", (p, e) => {
        const ev = e.originalEvent as MouseEvent | TouchEvent;
        const pos = posToRelative(p[0], ev);
        const children = p.children();
        const par = createParContextOrHelp(findParentPar(p));
        if (par.isHelp) {
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
        const par = fromParents($readsection);
        const pars = doc.getSections().get(par.par.htmlElement);
        if (!pars) {
            return;
        }
        markParsRead(pars, item);
        $readsection.remove();
    });

    onMouseOverOut("#pars .par:not('.preamble')", function mouseOverHandler(
        $this,
        e,
        select
    ) {
        const el = $this[0];
        const ctx = tryCreateParContextOrHelp(el);
        if (select && ctx && !ctx.isHelp) {
            markParRead(ctx, ReadingType.HoverPar);
        }
    });

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
