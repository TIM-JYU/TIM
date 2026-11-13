/**
 * Touch support for the mouse based jQuery UI widgets (such as sortable).
 *
 * jQuery UI handles only mouse events. This module turns touch events into
 * simulated mouse events for every widget that is built on $.ui.mouse.
 *
 * Based on jQuery UI Touch Punch 0.2.3,
 * Copyright 2011-2014 Dave Furfero, dual licensed under the MIT or GPL Version 2 licenses.
 */
// The underscore names are jQuery UI's own widget methods.
/* eslint-disable no-underscore-dangle */
import $ from "jquery";
import "jquery-ui/ui/version";
import "jquery-ui/ui/widget";
import "jquery-ui/ui/widgets/mouse";

interface IMouseWidget {
    element: JQuery;
    touchPunchMoved?: boolean;
    _mouseInit(): void;
    _mouseDestroy(): void;
    _mouseCapture(event: Touch): boolean;
}

type TouchJQueryEvent = JQuery.TriggeredEvent<
    HTMLElement,
    undefined,
    HTMLElement,
    HTMLElement
>;

const EVENT_NAMESPACE = ".timTouchPunch";

// True while some widget is handling a touch, so that nested widgets do not react to the same touch.
let touchHandled = false;

function simulateMouseEvent(event: TouchJQueryEvent, type: string) {
    const original = event.originalEvent;
    if (!(original instanceof TouchEvent) || original.touches.length > 1) {
        // Ignore multi-touch gestures such as pinch zoom.
        return;
    }
    event.preventDefault();
    const touch = original.changedTouches[0];
    event.target.dispatchEvent(
        new MouseEvent(type, {
            bubbles: true,
            cancelable: true,
            view: window,
            detail: 1,
            screenX: touch.screenX,
            screenY: touch.screenY,
            clientX: touch.clientX,
            clientY: touch.clientY,
            button: 0,
        })
    );
}

function install() {
    if (!("ontouchend" in document)) {
        return;
    }
    const mouseProto = (
        $ as unknown as {ui: {mouse: {prototype: IMouseWidget}}}
    ).ui.mouse.prototype;
    const origMouseInit = mouseProto._mouseInit;
    const origMouseDestroy = mouseProto._mouseDestroy;

    mouseProto._mouseInit = function (this: IMouseWidget) {
        this.element
            .on("touchstart" + EVENT_NAMESPACE, (e) => {
                const original = e.originalEvent;
                if (
                    touchHandled ||
                    !(original instanceof TouchEvent) ||
                    !this._mouseCapture(original.changedTouches[0])
                ) {
                    return;
                }
                touchHandled = true;
                this.touchPunchMoved = false;
                simulateMouseEvent(e, "mouseover");
                simulateMouseEvent(e, "mousemove");
                simulateMouseEvent(e, "mousedown");
            })
            .on("touchmove" + EVENT_NAMESPACE, (e) => {
                if (!touchHandled) {
                    return;
                }
                this.touchPunchMoved = true;
                simulateMouseEvent(e, "mousemove");
            })
            .on("touchend" + EVENT_NAMESPACE, (e) => {
                if (!touchHandled) {
                    return;
                }
                simulateMouseEvent(e, "mouseup");
                simulateMouseEvent(e, "mouseout");
                if (!this.touchPunchMoved) {
                    // The touch did not move, so it was a tap.
                    simulateMouseEvent(e, "click");
                }
                touchHandled = false;
            });
        origMouseInit.call(this);
    };

    mouseProto._mouseDestroy = function (this: IMouseWidget) {
        this.element.off(EVENT_NAMESPACE);
        origMouseDestroy.call(this);
    };
}

install();
