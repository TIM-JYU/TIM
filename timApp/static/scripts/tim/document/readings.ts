import {IPromise} from "angular";
import $ from "jquery";
import moment from "moment";
import {getActiveDocument} from "tim/document/activedocument";
import {genericglobals} from "tim/util/globals";
import {
    diffDialog,
    setDiffDialog,
    showDiffDialog,
} from "tim/document/showDiffDialog";
import {IItem} from "../item/IItem";
import {showMessageDialog} from "../ui/dialog";
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
import {onClick, onMouseOver, onMouseOverOut} from "./eventhandlers";
import {canSeeSource, dereferencePar, getArea, getParId} from "./parhelpers";

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

function isAlreadyRead(readline: JQuery, readingType: ReadingType) {
    const readClassName = readClasses[readingType];
    if (!readline.hasClass(readClassName)) {
        return false;
    }
    const lastReadTime = moment(readline.attr(`time-${readClassName}`));
    const timeSinceLastRead = moment.duration(moment().diff(lastReadTime));
    return timeSinceLastRead < getActiveDocument().readExpiry();
}

export async function markParRead(par: JQuery, readingType: ReadingType) {
    const readline = par.find(".readline");
    if (readline.length === 0) {
        return;
    }
    const readClassName = readClasses[readingType];
    if (isAlreadyRead(readline, readingType)) {
        return;
    }

    // If the paragraph is only a preview, ignore it.
    if (
        par.parents(".previewcontent").length > 0 ||
        par.parents(".csrunPreview").length > 0
    ) {
        return;
    }

    const d = dereferencePar(par);
    if (!d) {
        return;
    }
    const [docId, parId] = d;
    if (parId === "NEW_PAR" || !parId || parId === "HELP_PAR") {
        return;
    }
    readline.addClass(readClassName);
    readline.attr(`time-${readClassName}`, moment().toISOString());
    if (!Users.isLoggedIn() || !docId) {
        return;
    }
    const r = await to($http.put(`/read/${docId}/${parId}/${readingType}`, {}));
    if (!r.ok) {
        readline.removeClass(readClassName);
        return;
    }
    readline.removeClass(readClassName + "-modified");
    if (readingType === ReadingType.ClickRed) {
        markPageDirty();
        getActiveDocument().refreshSectionReadMarks();
    }
}

async function markParsRead($pars: JQuery) {
    const parIds = $pars
        .toArray()
        .map((e) => $(e))
        .filter(
            (e) => !isAlreadyRead(e.find(".readline"), ReadingType.ClickRed)
        )
        .map((e) => {
            const d = dereferencePar(e)!;
            return [d[0], d[1]];
        });
    $pars.find(".readline").addClass(readClasses[ReadingType.ClickRed]);
    const doc = getActiveDocument();
    const r = await to(
        $http.put(
            "/read/" + doc.getId() + "/" + "null" + "/" + ReadingType.ClickRed,
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
        .find(".readline")
        .not((i, e) => isAlreadyRead($(e), ReadingType.OnScreen));
    const parToRead = visiblePars.first().parents(".par");
    const parId = getParId(parToRead);

    if (readPromise != null && readingParId !== parId) {
        $timeout.cancel(readPromise);
    } else if (readingParId === parId) {
        return;
    }

    if (parToRead.length === 0) {
        return;
    }
    readingParId = parId;
    const numWords = parToRead
        .find(".parContent")
        .text()
        .trim()
        .split(/[\s\n]+/).length;
    readPromise = $timeout(async () => {
        await markParRead(parToRead, ReadingType.OnScreen);
        queueParagraphForReading();
    }, 300 * numWords);
}

async function handleSeeChanges(elem: JQuery, e: JQuery.Event) {
    const par = elem.parents(".par");
    const derefData = dereferencePar(par);
    if (!derefData) {
        return;
    }
    const [id, blockId, t] = derefData;
    if (!id || !blockId) {
        return;
    }
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

async function readlineHandler(elem: JQuery, e: JQuery.MouseEventBase) {
    if ((e.target as HTMLElement).tagName === "BUTTON") {
        return;
    }
    await markParRead(elem.parents(".par"), ReadingType.ClickRed);
}

export async function initReadings(item: IItem) {
    onClick(".readline > button", handleSeeChanges);
    onClick(".readline", readlineHandler);
    onMouseOver(".readline.read-modified", (p, e) => {
        const ev = e.originalEvent as MouseEvent | TouchEvent;
        const pos = posToRelative(p[0], ev);
        const children = p.children();
        if (children.length === 0 && canSeeSource(item, p.parents(".par"))) {
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

    onClick(".areareadline", function areareadlineHandler($this, e) {
        const oldClass = $this.attr("class") ?? null;
        $this.attr("class", "readline read");

        if (!Users.isLoggedIn()) {
            return true;
        }

        // Collapsible area
        const areaId = $this.parent().attr("data-area");
        if (!areaId) {
            return;
        }
        $log.info($this);

        (async () => {
            const r = await to(
                $http.put("/read/" + item.id + "/" + areaId, {})
            );
            if (r.ok) {
                getArea(areaId)
                    .find(".readline")
                    .attr("class", "areareadline read");
                markPageDirty();
            } else {
                await showMessageDialog("Could not save the read marking.");
                $this.attr("class", oldClass);
            }
        })();

        return false;
    });

    $(window).scroll(queueParagraphForReading);

    queueParagraphForReading();

    onClick(".readsection", function readSectionHandler($readsection, e) {
        const doc = getActiveDocument();
        const par = $readsection.parents(".par");
        const parId = getParId(par);
        if (par.length === 0 || !parId) {
            void showMessageDialog("Unable to mark this section as read");
            return;
        }
        const pars = doc.getSections().get(parId);
        if (!pars) {
            return;
        }
        markParsRead($(pars.map((p: JQuery) => p[0])));
        $readsection.remove();
    });

    onMouseOverOut(".par:not('.preamble')", function mouseOverHandler(
        $this,
        e,
        select
    ) {
        if (select) {
            markParRead($this, ReadingType.HoverPar);
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
    if (pos.type !== EditType.Edit) {
        return;
    }
    const first = pos.pars.first();
    const [docid, parid] = dereferencePar(first)!;
    const result = await to(
        $http.put<IOkResponse & {latest?: unknown}>(
            `/unread/${docid!}/${parid!}`,
            {}
        )
    );
    if (!result.ok) {
        return;
    }
    const rline = first.find(".readline");
    rline.removeClass("read read-modified");
    if (result.result.data.latest) {
        rline.addClass("read-modified");
    }
    getActiveDocument().refreshSectionReadMarks();
}
