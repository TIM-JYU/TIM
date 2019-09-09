import {IPromise} from "angular";
import $ from "jquery";
import moment from "moment";
import {getActiveDocument} from "tim/document/document";
import {IItem} from "../item/IItem";
import {showMessageDialog} from "../ui/dialog";
import {Users} from "../user/userService";
import {$http, $log, $timeout} from "../util/ngimport";
import {IOkResponse, isInViewport, markPageDirty, posToRelative, to} from "../util/utils";
import {showDiffDialog} from "./diffDialog";
import {EditPosition, EditType} from "./editing/editing";
import {IExtraData} from "./editing/edittypes";
import {onClick, onMouseOver, onMouseOverOut} from "./eventhandlers";
import {canSeeSource, dereferencePar, getArea, getParId, getRefAttrs, isReference} from "./parhelpers";
import {vctrlInstance, ViewCtrl} from "./viewctrl";

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
    const readClassName = readClasses[readingType];
    if (isAlreadyRead(readline, readingType)) {
        return;
    }

    // If the paragraph is only a preview, ignore it.
    if (par.parents(".previewcontent").length > 0 || par.parents(".csrunPreview").length > 0) {
        return;
    }
    const parId = getParId(par);
    if (parId === "NEW_PAR" || !parId || parId === "HELP_PAR") {
        return;
    }
    readline.addClass(readClassName);
    readline.attr(`time-${readClassName}`, moment().toISOString());
    let data = {};
    if (isReference(par)) {
        data = getRefAttrs(par);
    }
    if (!Users.isLoggedIn()) {
        return;
    }
    const r = await to($http.put(`/read/${getActiveDocument().getId()}/${parId}/${readingType}`, data));
    if (!r.ok) {
        $log.error("Could not save the read marking for paragraph " + parId);
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
    const parIds = $pars.map((i, e) => {
        return getParId($(e));
    }).get();
    $pars.find(".readline").addClass(readClasses[ReadingType.ClickRed]);
    const doc = getActiveDocument();
    const r = await to($http.put("/read/" + doc.getId() + "/" + "null" + "/" + ReadingType.ClickRed, {pars: parIds}));
    if (!r.ok) {
        $log.error("Could not save the read markings");
        return;
    }
    markPageDirty();
}

async function markAllAsRead() {
    const doc = getActiveDocument();
    const r = await to($http.put("/read/" + doc.getId(), {}));
    if (!r.ok) {
        await showMessageDialog("Could not mark the document as read.");
        return;
    }
    $(".readline").attr("class", "readline read");
}

let readPromise: IPromise<unknown> | null = null;
let readingParId: string | undefined;

function queueParagraphForReading() {
    //noinspection CssInvalidPseudoSelector
    const visiblePars = $(".par:not('.preamble')").filter((i, e) => isInViewport(e)).find(".readline").not((i, e) => isAlreadyRead($(e), ReadingType.OnScreen));
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
    const numWords = parToRead.find(".parContent").text().trim().split(/[\s\n]+/).length;
    readPromise = $timeout((async () => {
        await markParRead(parToRead, ReadingType.OnScreen);
        queueParagraphForReading();
    }), 300 * numWords);
}

async function handleSeeChanges(elem: JQuery, e: JQuery.Event) {
    const par = elem.parents(".par");
    const [id, blockId, t] = dereferencePar(par);
    const parData = await to($http.get<Array<{par_hash: string}>>(`/read/${id}/${blockId}`));
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
            });
            if (vctrlInstance) {
                if (vctrlInstance.diffDialog) {
                    vctrlInstance.diffDialog.close();
                }
                vctrlInstance.diffDialog = await mi.dialogInstance.promise;
                await to(mi.result);
                vctrlInstance.diffDialog = undefined;
            }
        }
    }
}

async function readlineHandler(elem: JQuery, e: JQuery.Event) {
    if ((e.target as HTMLElement).tagName === "BUTTON") {
        return;
    }
    markParRead(elem.parents(".par"), ReadingType.ClickRed);
}

export async function initReadings(sc: ViewCtrl) {
    onClick(".readline > button", handleSeeChanges);
    onClick(".readline", readlineHandler);
    onMouseOver(".readline.read-modified", (p, e) => {
        const ev = e.originalEvent as MouseEvent | TouchEvent;
        const pos = posToRelative(p[0], ev);
        const children = p.children();
        if (children.length === 0 && canSeeSource(sc.item, p.parents(".par"))) {
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
        const oldClass = $this.attr("class") || null;
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
            const r = await to($http.put("/read/" + sc.docId + "/" + areaId, {}));
            if (r.ok) {
                getArea(areaId).find(".readline").attr("class", "areareadline read");
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

    onMouseOverOut(".par:not('.preamble')", function mouseOverHandler($this, e, select) {
        if (select) {
            markParRead($this, ReadingType.HoverPar);
        }
    });

    if (Users.isLoggedIn()) {
        await $timeout(10000);
        await $http.post("/bookmarks/markLastRead/" + sc.docId, {});
    }
}

export async function handleUnread(item: IItem, extraData: IExtraData, pos: EditPosition) {
    if (pos.type !== EditType.Edit) {
        return;
    }
    const result = await $http.put<IOkResponse & {latest?: unknown}>(`/unread/${item.id}/${extraData.par}`, {});
    const rline = pos.pars.first().find(".readline");
    rline.removeClass("read read-modified");
    if (result.data.latest) {
        rline.addClass("read-modified");
    }
    getActiveDocument().refreshSectionReadMarks();
}
