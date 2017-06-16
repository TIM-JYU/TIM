import $ from "jquery";
import {$document, $log} from "../../ngimport";
import {dist} from "../../utils";
import {EDITOR_CLASS_DOT} from "./parhelpers";

function fixPageCoords(e) {
    if (!("pageX" in e) || (e.pageX === 0 && e.pageY === 0)) {
        e.pageX = e.originalEvent.touches[0].pageX;
        e.pageY = e.originalEvent.touches[0].pageY;
    }
    return e;
}

export function onClick(className, func, overrideModalCheck = false) {
    let downEvent = null;
    let downCoords = null;
    let lastDownEvent = null;
    let lastclicktime = -1;

    $document.on("mousedown touchstart", className, function(e) {
        if (!overrideModalCheck && ($(".actionButtons").length > 0 || $(EDITOR_CLASS_DOT).length > 0)) {
            // Disable while there are modal gui elements
            return;
        }
        if (lastDownEvent && lastDownEvent.type !== e.type && new Date().getTime() - lastclicktime < 500) {
            // This is to prevent chaotic behavior from both mouseDown and touchStart
            // events happening at the same coordinates
            $log.info("Ignoring event:");
            $log.info(e);
            return;
        }

        downEvent = fixPageCoords(e);
        lastDownEvent = downEvent;
        downCoords = {left: downEvent.pageX, top: downEvent.pageY};
        lastclicktime = new Date().getTime();
    });
    $document.on("mousemove touchmove", className, function(e) {
        if (downEvent === null) {
            return;
        }

        const e2 = fixPageCoords(e);
        if (dist(downCoords, {left: e2.pageX, top: e2.pageY}) > 10) {
            // Moved too far away, cancel the event
            downEvent = null;
        }
    });
    $document.on("touchcancel", className, function(e) {
        downEvent = null;
    });
    // it is wrong to register both events at the same time; see https://stackoverflow.com/questions/8503453
    const isIOS = ((/iphone|ipad/gi).test(navigator.appVersion));
    const eventName = isIOS ? "touchend" : "mouseup";
    $document.on(eventName, className, function(e) {
        if (downEvent !== null) {
            if (func($(this), downEvent)) {
                //e.preventDefault();
                //e.stopPropagation();
            }
            downEvent = null;
        }
    });
}

export function onMouseOver(className, func) {
    $document.on("mouseover", className, function(e) {
        if (func($(this), fixPageCoords(e))) {
            e.preventDefault();
            e.stopPropagation();
        }
    });
}

export function onMouseOut(className, func) {
    $document.on("mouseout", className, function(e) {
        if (func($(this), fixPageCoords(e))) {
            e.preventDefault();
            e.stopPropagation();
        }
    });
}

export function onMouseOverOut(className, func) {
    // A combination function with a third parameter
    // true when over, false when out

    $document.on("mouseover", className, function(e) {
        if (func($(this), fixPageCoords(e), true)) {
            e.preventDefault();
        }
    });

    $document.on("mouseout", className, function(e) {
        if (func($(this), fixPageCoords(e), false)) {
            e.preventDefault();
        }
    });
}
