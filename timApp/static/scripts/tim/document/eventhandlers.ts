import $ from "jquery";
import {KEY_ENTER} from "../util/keycodes";
import {$document, $log} from "../util/ngimport";
import {Coords, dist, isIOS, isTouchEvent} from "../util/utils";
import {EDITOR_CLASS_DOT} from "./parhelpers";

function fixPageCoords(
    e: JQuery.MouseEventBase | JQuery.TouchEventBase
): JQuery.MouseEventBase {
    if (e.pageX === 0 && e.pageY === 0) {
        const originalEvent = e.originalEvent;
        if (originalEvent && isTouchEvent(originalEvent)) {
            e.pageX = originalEvent.touches[0].pageX;
            e.pageY = originalEvent.touches[0].pageY;
        }
    }
    return e as JQuery.MouseEventBase;
}

export interface OnClickArg {
    target: Element;
    originalEvent: MouseEvent;
    pageX: number;
    pageY: number;
    type: string;
}

export function onClick(
    className: string,
    func: (obj: JQuery, e: OnClickArg) => unknown,
    overrideModalCheck = false,
    needsDoubleClick: ((obj: Element) => boolean) | undefined = undefined
) {
    let downEvent: JQuery.MouseEventBase | undefined;
    let downCoords: Coords | undefined;
    let lastDownEvent: JQuery.Event | undefined;
    let lastclicktime: number | undefined;
    let lastClickElement: Element | undefined;

    // iOS will not trigger mousedown unless the element is actually clickable
    // See https://stackoverflow.com/a/3303839
    const startEvent = isIOS() ? "touchstart" : "mousedown";

    // Reset last click if it wasn't on the same element
    $(document).on(startEvent, (e) => {
        // jQuery doesn't properly type the target
        const target = e.target as unknown as Element;
        if (lastClickElement !== target) {
            lastClickElement = undefined;
        }
    });

    $(document).on(startEvent, className, (e) => {
        // touchstart
        if (!overrideModalCheck && $(EDITOR_CLASS_DOT).length > 0) {
            // Disable while there are modal gui elements
            return;
        }
        if (
            lastDownEvent &&
            lastclicktime &&
            lastDownEvent.type !== e.type &&
            new Date().getTime() - lastclicktime < 500
        ) {
            // This is to prevent chaotic behavior from both mouseDown and touchStart
            // events happening at the same coordinates
            $log.info("Ignoring event:", e);
            return;
        }
        if (lastClickElement !== e.target && needsDoubleClick?.(e.target)) {
            lastClickElement = e.target;
            return;
        }

        downEvent = fixPageCoords(e);
        lastDownEvent = downEvent;
        downCoords = {left: downEvent.pageX, top: downEvent.pageY};
        lastclicktime = new Date().getTime();
    });
    $document.on("mousemove", className, (e) => {
        // touchmove
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
            lastClickElement = undefined;
        }
    });
    $document.on("touchcancel", className, (e) => {
        downEvent = undefined;
        lastClickElement = undefined;
    });
    // it is wrong to register both events at the same time; see https://stackoverflow.com/questions/8503453
    const endEvent = isIOS() ? "touchend" : "mouseup";
    $document.on(endEvent, className, (e) => {
        if (downEvent?.originalEvent) {
            func($(e.currentTarget), {
                originalEvent: downEvent.originalEvent,
                target: downEvent.target,
                pageX: downEvent.pageX,
                pageY: downEvent.pageY,
                type: downEvent.type,
            });
            downEvent = undefined;
            lastClickElement = undefined;
        }
    });
    if (className != "html") {
        // When the element has focus, allow clicking by pressing Enter.
        $(document).on("keypress", className, (e) => {
            const kbE = e.originalEvent;
            if (!kbE || kbE.keyCode != KEY_ENTER) {
                return;
            }
            const t = e.target as HTMLElement;
            const rect = t.getBoundingClientRect();
            func($(e.currentTarget), {
                type: "click",
                target: e.target,
                originalEvent: new MouseEvent("click", {
                    clientX: rect.x,
                    clientY: rect.y,
                }),
                pageX: rect.x,
                pageY: rect.y,
            });
        });
    }
}

export function onMouseOver(className: string, func: MouseFn) {
    $document.on("mouseover", className, (e) => {
        // touchstart
        func($(e.currentTarget), fixPageCoords(e));
    });
}

export function onMouseOut(className: string, func: MouseFn) {
    $document.on("mouseout", className, (e) => {
        func($(e.currentTarget), fixPageCoords(e));
    });
}

export type MouseFn = (p: JQuery, e: JQuery.MouseEventBase) => void;
export type MouseOverOutFn = (
    p: JQuery,
    e: JQuery.MouseEventBase,
    over: boolean
) => void;

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
