import {IPromise} from "angular";
import $ from "jquery";
import moment from "moment";
import {getActiveDocument} from "tim/document/document";
import {IItem} from "../item/IItem";
import {showMessageDialog} from "../ui/dialog";
import {Users} from "../user/userService";
import {$http, $log, $timeout, $window} from "../util/ngimport";
import {isInViewport, markPageDirty, to} from "../util/utils";
import {EditPosition, EditType} from "./editing/editing";
import {IExtraData} from "./editing/edittypes";
import {onClick, onMouseOverOut} from "./eventhandlers";
import {getArea, getParId, getRefAttrs, isReference} from "./parhelpers";
import {ViewCtrl} from "./viewctrl";

export const readClasses = {
    1: "screen",
    2: "hover",
    3: "click",
    4: "read",
};

export enum readingTypes {
    onScreen = 1,
    hoverPar = 2,
    clickPar = 3,
    clickRed = 4,
}

function isAlreadyRead(readline: JQuery, readingType: readingTypes) {
    const readClassName = readClasses[readingType];
    if (!readline.hasClass(readClassName)) {
        return false;
    }
    const lastReadTime = moment(readline.attr(`time-${readClassName}`));
    const timeSinceLastRead = moment.duration(moment().diff(lastReadTime));
    return timeSinceLastRead < getActiveDocument().readExpiry();
}

export async function markParRead($par: JQuery, readingType: readingTypes) {
    const $readline = $par.find(".readline");
    const readClassName = readClasses[readingType];
    if (isAlreadyRead($readline, readingType)) {
        return;
    }

    // If the paragraph is only a preview, ignore it.
    if ($par.parents(".previewcontent").length > 0 || $par.parents(".csrunPreview").length > 0) {
        return;
    }
    const parId = getParId($par);
    if (parId === "NEW_PAR" || !parId || parId === "HELP_PAR") {
        return;
    }
    $readline.addClass(readClassName);
    $readline.attr(`time-${readClassName}`, moment().toISOString());
    let data = {};
    if (isReference($par)) {
        data = getRefAttrs($par);
    }
    if (!Users.isLoggedIn()) {
        return;
    }
    const r = await to($http.put(`/read/${getActiveDocument().id}/${parId}/${readingType}`, data));
    if (!r.ok) {
        $log.error("Could not save the read marking for paragraph " + parId);
        $readline.removeClass(readClassName);
        return;
    }
    $readline.removeClass(readClassName + "-modified");
    if (readingType === readingTypes.clickRed) {
        markPageDirty();
        getActiveDocument().refreshSectionReadMarks();
    }
}

async function markParsRead($pars: JQuery) {
    const parIds = $pars.map((i, e) => {
        return getParId($(e));
    }).get();
    $pars.find(".readline").addClass(readClasses[readingTypes.clickRed]);
    const doc = getActiveDocument();
    const r = await to($http.put("/read/" + doc.id + "/" + "null" + "/" + readingTypes.clickRed, {pars: parIds}));
    if (!r.ok) {
        $log.error("Could not save the read markings");
        return;
    }
    markPageDirty();
}

async function markAllAsRead() {
    const doc = getActiveDocument();
    const r = await to($http.put("/read/" + doc.id, {}));
    if (!r.ok) {
        await showMessageDialog("Could not mark the document as read.");
        return;
    }
    $(".readline").attr("class", "readline read");
}

let readPromise: IPromise<any> | null = null;
let readingParId: string | undefined;

function queueParagraphForReading() {
    //noinspection CssInvalidPseudoSelector
    const visiblePars = $(".par:not('.preamble'):onScreen").find(".readline").not((i, e) => isAlreadyRead($(e), readingTypes.onScreen));
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
        await markParRead(parToRead, readingTypes.onScreen);
        queueParagraphForReading();
    }) as any, 300 * numWords);
}

function readlineHandler($this: JQuery, e: Event) {
    markParRead($this.parents(".par"), readingTypes.clickRed);
    return true;
}

export async function initReadings(sc: ViewCtrl) {
    onClick(".readline", readlineHandler);

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

    ($ as any).expr[":"].onScreen = isInViewport;

    $($window).scroll(queueParagraphForReading);

    queueParagraphForReading();

    onClick(".readsection", function readSectionHandler($readsection, e) {
        const doc = getActiveDocument();
        const $par = $readsection.parents(".par");
        const parId = getParId($par);
        if ($par.length === 0 || !parId) {
            void showMessageDialog("Unable to mark this section as read");
            return;
        }
        const $pars = doc.sections[parId];
        markParsRead($($pars.map((p: JQuery) => p[0])));
        $readsection.remove();
    });

    onMouseOverOut(".par:not('.preamble')", function mouseOverHandler($this, e, select) {
        if (select) {
            markParRead($this, readingTypes.hoverPar);
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
    await $http.put(`/unread/${item.id}/${extraData.par}`, {});
    pos.pars.first().find(".readline").removeClass("read read-modified");
    getActiveDocument().refreshSectionReadMarks();
}
