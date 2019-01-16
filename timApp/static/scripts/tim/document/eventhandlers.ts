import $ from "jquery";
import {$document, $log} from "../util/ngimport";
import {Coords, dist} from "../util/utils";
import {EDITOR_CLASS_DOT} from "./parhelpers";

function fixPageCoords(e: JQueryEventObject) {
    if (!("pageX" in e) || (e.pageX === 0 && e.pageY === 0)) {
        const originalEvent = e.originalEvent;
        if (originalEvent instanceof TouchEvent) {
            e.pageX = originalEvent.touches[0].pageX;
            e.pageY = originalEvent.touches[0].pageY;
        }
    }
    return e;
}

export function onClick(className: string,
                        func: (obj: JQuery, e: JQueryEventObject) => unknown,
                        overrideModalCheck = false) {
    let downEvent: JQueryEventObject | undefined;
    let downCoords: Coords | undefined;
    let lastDownEvent: JQueryEventObject | undefined;
    let lastclicktime: number | undefined;

    $document.on("mousedown touchstart", className, (e: JQueryEventObject) => {
        if (!overrideModalCheck && ($(".actionButtons").length > 0 || $(EDITOR_CLASS_DOT).length > 0)) {
            // Disable while there are modal gui elements
            return;
        }
        if (lastDownEvent &&
            lastclicktime &&
            lastDownEvent.type !== e.type &&
            new Date().getTime() - lastclicktime < 500) {
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
    $document.on("mousemove touchmove", className, (e: JQueryEventObject) => {
        if (downEvent == null) {
            return;
        }

        const e2 = fixPageCoords(e);
        if (!downCoords) {
            return;
        }
        if (dist(downCoords, {left: e2.pageX, top: e2.pageY}) > 10) {
            // Moved too far away, cancel the event
            downEvent = undefined;
        }
    });
    $document.on("touchcancel", className, (e: JQueryEventObject) => {
        downEvent = undefined;
    });
    // it is wrong to register both events at the same time; see https://stackoverflow.com/questions/8503453
    const isIOS = ((/iphone|ipad/gi).test(navigator.appVersion));
    const eventName = isIOS ? "touchend" : "mouseup";
    $document.on(eventName, className, function(e) {
        if (downEvent != null) {
            func($(this) as any, downEvent);
            downEvent = undefined;
        }
    });
}

export function onMouseOver(className: string, func: MouseFn) {
    $document.on("mouseover", className, function(e: JQueryEventObject) {
        func($(this) as any, fixPageCoords(e));
    });
}

export function onMouseOut(className: string, func: MouseFn) {
    $document.on("mouseout", className, function(e: JQueryEventObject) {
        func($(this) as any, fixPageCoords(e));
    });
}

export type MouseFn = (p: JQuery, e: JQueryEventObject) => void;
export type MouseOverOutFn = (p: JQuery, e: JQueryEventObject, over: boolean) => void;

export function onMouseOverOut(className: string, func: MouseOverOutFn) {
    // A combination function with a third parameter
    // true when over, false when out

    $document.on("mouseover", className, function(e: JQueryEventObject) {
        func($(this) as any, fixPageCoords(e), true);
    });

    $document.on("mouseout", className, function(e: JQueryEventObject) {
        func($(this) as any, fixPageCoords(e), false);
    });
}
