import $ from "jquery";
import {$http, $log, $timeout, $window} from "../../ngimport";
import {getArea, getParId, getRefAttrs, isReference} from "./parhelpers";
import {markPageDirty} from "../../utils";
import {Users} from "../../services/userService";
import {getActiveDocument} from "./document";
import {onClick, onMouseOverOut} from "./eventhandlers";

export const readClasses = {
    1: "screen",
    2: "hover",
    3: "click",
    4: "read",
};

export const readingTypes = {
    onScreen: 1,
    hoverPar: 2,
    clickPar: 3,
    clickRed: 4,
};

export async function markParRead($par: JQuery, readingType: number) {
    const $readline = $par.find(".readline");
    const readClassName = readClasses[readingType];
    if ($readline.hasClass(readClassName)) {
        return;
    }

    // If the paragraph is only a preview, ignore it.
    if ($par.parents(".previewcontent").length > 0 || $par.parents(".csrunPreview").length > 0) {
        return;
    }
    const parId = getParId($par);
    if (parId === "NEW_PAR" || parId === null || parId === "HELP_PAR") {
        return;
    }
    $readline.addClass(readClassName);
    let data = {};
    if (isReference($par)) {
        data = getRefAttrs($par);
    }
    if (!Users.isLoggedIn()) return;
    try {
        await $http.put("/read/" + getActiveDocument().id + "/" + parId + "/" + readingType, data);
    } catch (e) {
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
    try {
        await $http.put("/read/" + doc.id + "/" + "null" + "/" + readingTypes.clickRed, {pars: parIds});
    } catch (e) {
        $log.error("Could not save the read markings");
        return;
    }
    markPageDirty();
}

async function markAllAsRead() {
    const doc = getActiveDocument();
    try {
        await $http.put("/read/" + doc.id, {});
    } catch (e) {
        $window.alert("Could not mark the document as read.");
        return;
    }
    $(".readline").attr("class", "readline read");
}

let readPromise = null;
let readingParId = null;

function queueParagraphForReading() {
    //noinspection CssInvalidPseudoSelector
    const visiblePars = $(".par:onScreen").find(".readline").not("." + readClasses[readingTypes.onScreen]);
    const parToRead = visiblePars.first().parents(".par");
    const parId = getParId(parToRead);

    if (readPromise !== null && readingParId !== parId) {
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

function readlineHandler($this: JQuery, e) {
    markParRead($this.parents(".par"), readingTypes.clickRed);
    return true;
}

export function initReadings(sc) {
    onClick(".readline", readlineHandler);

    onClick(".areareadline", function areareadlineHandler($this, e) {
        const oldClass = $this.attr("class");
        $this.attr("class", "readline read");

        if (!Users.isLoggedIn()) {
            return true;
        }

        // Collapsible area
        const areaId = $this.parent().attr("data-area");
        $log.info($this);

        $http.put("/read/" + sc.docId + "/" + areaId, {})
            .then((response) => {
                getArea(areaId).find(".readline").attr("class", "areareadline read");
                markPageDirty();
            }, () => {
                $window.alert("Could not save the read marking.");
                $this.attr("class", oldClass);
            });

        return false;
    });

    $.expr[":"].onScreen = function onScreenHandler(el) {
        const rect = el.getBoundingClientRect();

        return (
            rect.top >= 0 &&
            rect.left >= 0 &&
            rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
            rect.right <= (window.innerWidth || document.documentElement.clientWidth)
        );
    };

    $($window).scroll(queueParagraphForReading);

    queueParagraphForReading();

    onClick(".readsection", function readSectionHandler($readsection, e) {
        const doc = getActiveDocument();
        const $par = $readsection.parents(".par");
        const $pars = doc.sections[getParId($par)];
        if ($par.length === 0) {
            $window.alert("Unable to mark this section as read");
            return;
        }
        markParsRead($pars);
        $readsection.remove();
    });

    onMouseOverOut(".par", function mouseOverHandler($this, e, select) {
        if (select) {
            markParRead($this, readingTypes.hoverPar);
        }
    });

    if (Users.isLoggedIn()) {
        $timeout(() => {
            $http.post("/bookmarks/markLastRead/" + sc.docId, {}).then(() => {
                // all ok
            }, () => {
                $log.error("Failed to mark document as last read");
            });
        }, 10000);
    }
}
