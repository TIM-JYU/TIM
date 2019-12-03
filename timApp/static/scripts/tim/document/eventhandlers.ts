import $ from "jquery";
import {KEY_ENTER} from "../util/keycodes";
import {$document, $log} from "../util/ngimport";
import {Coords, dist, isTouchEvent} from "../util/utils";
import {EDITOR_CLASS_DOT} from "./parhelpers";

function fixPageCoords(e: JQuery.MouseEventBase) {
    if (e.pageX === 0 && e.pageY === 0) {
        const originalEvent = e.originalEvent;
        if (originalEvent && isTouchEvent(originalEvent)) {
            e.pageX = originalEvent.touches[0].pageX;
            e.pageY = originalEvent.touches[0].pageY;
        }
    }
    return e;
}

export function onClick(className: string,
                        func: (obj: JQuery, e: JQuery.MouseEventBase) => unknown,
                        overrideModalCheck = false) {
    let downEvent: JQuery.MouseEventBase | undefined;
    let downCoords: Coords | undefined;
    let lastDownEvent: JQuery.Event | undefined;
    let lastclicktime: number | undefined;

    $(document).on("mousedown", className, (e) => { // touchstart
        if (!overrideModalCheck && ($(EDITOR_CLASS_DOT).length > 0)) {
            // Disable while there are modal gui elements
            return;
        }
        if (lastDownEvent &&
            lastclicktime &&
            lastDownEvent.type !== e.type &&
            new Date().getTime() - lastclicktime < 500) {
            // This is to prevent chaotic behavior from both mouseDown and touchStart
            // events happening at the same coordinates
            $log.info("Ignoring event:", e);
            return;
        }

        downEvent = fixPageCoords(e);
        lastDownEvent = downEvent;
        downCoords = {left: downEvent.pageX, top: downEvent.pageY};
        lastclicktime = new Date().getTime();
    });
    $document.on("mousemove", className, (e) => { // touchmove
        if (downEvent == null) {
            return;
        }

        const e2 = fixPageCoords(e);
        if (!downCoords || e2.pageX == null || e2.pageY == null) {
            return;
        }
        if (dist(downCoords, {left: e2.pageX, top: e2.pageY}) > 10) {
            // Moved too far away, cancel the event
            downEvent = undefined;
        }
    });
    $document.on("touchcancel", className, (e) => {
        downEvent = undefined;
    });
    // it is wrong to register both events at the same time; see https://stackoverflow.com/questions/8503453
    const isIOS = ((/iphone|ipad/gi).test(navigator.appVersion));
    const eventName = isIOS ? "touchend" : "mouseup";
    $document.on(eventName, className, (e) => {
        if (downEvent != null) {
            func($(e.currentTarget), downEvent);
            downEvent = undefined;
        }
    });

    // When the element has focus, allow clicking by pressing Enter.
    $(document).on("keypress", className, (e) => {
        const kbE = e.originalEvent;
        if (!kbE || kbE.keyCode != KEY_ENTER) {
            return;
        }
        $(e.currentTarget).trigger("click");
        // func($(e.currentTarget), ne);
    });
}

export function onMouseOver(className: string, func: MouseFn) {
    $document.on("mouseover", className, (e) => { // touchstart
        func($(e.currentTarget), fixPageCoords(e));
    });
}

export function onMouseOut(className: string, func: MouseFn) {
    $document.on("mouseout", className, (e) => {
        func($(e.currentTarget), fixPageCoords(e));
    });
}

export type MouseFn = (p: JQuery, e: JQuery.MouseEventBase) => void;
export type MouseOverOutFn = (p: JQuery, e: JQuery.MouseEventBase, over: boolean) => void;

export function onMouseOverOut(className: string, func: MouseOverOutFn) {
    // A combination function with a third parameter
    // true when over, false when out

    $document.on("mouseover", className, (e) => {
        func($(e.currentTarget), fixPageCoords(e), true);
    });

    $document.on("mouseout", className, (e) => {
        func($(e.currentTarget), fixPageCoords(e), false);
    });
}
