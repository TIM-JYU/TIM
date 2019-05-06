import angular, {IHttpResponse, IPromise} from "angular";
import * as t from "io-ts";
import moment from "moment";
import {$timeout} from "./ngimport";

export function checkBindings(controller: any, bindings: {[name: string]: string}) {
    for (const k of Object.keys(bindings)) {
        if (!bindings[k].startsWith("?")) {
            if (controller[k] == null) {
                throw new Error(`Binding is undefined: ${k}`);
            }
        }
    }
}

const blacklist = new Set(["name", "title"]);

// adapted from http://aboutcode.net/2013/07/27/json-date-parsing-angularjs.html
export function convertDateStringsToMoments(input: {[index: string]: any}): void {
    if (input == null || typeof input !== "object") {
        return;
    }

    for (const [key, value] of Object.entries(input)) {
        if (typeof value === "string") {
            // ignore strings and keys that are most likely not intended to be interpreted as timestamps
            if (!blacklist.has(key)) {
                const m = moment(value, moment.ISO_8601, true);
                if (m.isValid()) {
                    input[key] = m;
                }
            }
        } else {
            convertDateStringsToMoments(value);
        }
    }
}

export function angularWait(): IPromise<void> {
    return $timeout(() => {
    }, 100);
}

export function assertIsText(n: Node): n is Text {
    if (n.nodeType !== Node.TEXT_NODE) {
        throw new Error("Expected a Text node, got " + n);
    }
    return true;
}

export function stringOrNull(x: {toString: () => string}): string {
    if (x != null) {
        return x.toString();
    }
    return "null";
}

export function checkIfElement(x: any): x is Element {
    return typeof ((x as any).hasAttribute) === "function";
}

/**
 * Check if element is in view
 * @param el - Element to check
 * @returns {boolean} true if in view
 */
export function isInViewport(el: Element) {
    const rect = el.getBoundingClientRect();
    if (!document.documentElement) return true;
    return (
        rect.top >= 0 &&
        rect.left >= 0 &&
        rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
        rect.right <= (window.innerWidth || document.documentElement.clientWidth)
    );
}

/**
 * Scroll window to the given element.
 * @param element - Element to scroll to.
 */
export function scrollToElement(element: Element) {
    if (!!element && element.scrollIntoView) {
        element.scrollIntoView();
    }
}

/**
 * Gets the parent element of the given element.
 * @param element - Element whose parent is queried for
 * @returns {Element} Element parent
 */
export function getElementParent(element: Node): Element | null {
    /*
     if (typeof element.parentElement !== "undefined")
     return element.parentElement;
     */
    if (!element) {
        return null;
    }
    const parent = element.parentNode;
    if (!parent) {
        return null;
    }
    if (checkIfElement(parent)) {
        return parent;
    }

    return getElementParent(parent);
}

export interface Coords {
    left: number;
    top: number;
}

export function dist(coords1: Coords, coords2: Coords) {
    return Math.sqrt(Math.pow(coords2.left - coords1.left, 2) + Math.pow(coords2.top - coords1.top, 2));
}

export function markPageDirty() {
    const e = angular.element("#page_is_dirty");
    e.val("1");
}

export function markPageNotDirty() {
    const e = angular.element("#page_is_dirty");
    e.val("0");
}

export function isPageDirty() {
    const e = angular.element("#page_is_dirty");
    return e.val() === "1";
}

export function getURLParameter(sParam: string): string | undefined {
    const sPageURL = window.location.search.substring(1);
    const sURLVariables = sPageURL.split("&");
    for (const urlvar of sURLVariables) {
        const sParameterName = urlvar.split("=");
        if (sParameterName[0] === sParam) {
            return decodeURIComponent(sParameterName[1]);
        }
    }
    return undefined;
}

/**
 * Marks seemingly unused imports as used so that TypeScript compiler won't optimize them out when compiling.
 *
 * For example, timApp module needs ngSanitize, but it is specified as a string reference in the angular.module(...)
 * call, which is why TypeScript compiler cannot see the connection to the import statement. See app.ts.
 *
 * @param modules The modules to mark as used.
 */
export function markAsUsed(...modules: any[]) {
    // no need to do anything here
}

export function printJson(data: any, desc: string = "") {
    console.error(desc, JSON.stringify(data));
}

export function clone<T>(obj: T): T {
    return JSON.parse(JSON.stringify(obj));
}

type Success<T> = {ok: true, result: T};
type Failure<T> = {ok: false, result: T};
export type Result<T, U> = Success<T> | Failure<U>;

/**
 * Wraps the given promise so that it always gets fulfilled.
 * Adapted from await-to-js library: https://github.com/scopsy/await-to-js
 * @param promise Promise to wrap.
 * @returns A promise that resolves to either a success or error.
 */
export function to<T, U = {data: {error: string}}>(promise: IPromise<T>): IPromise<Result<T, U>> {
    return promise
        .then<Success<T>>((data: T) => ({ok: true, result: data}))
        .catch<Failure<U>>((err) => {
            return {ok: false, result: err};
        });
}

export function assertNotNull(obj: any) {
    if (obj == null) {
        throw new Error("object was unexpectedly null or undefined");
    }
}

export interface IBounds {
    left: number;
    top: number;
    right: number;
    bottom: number;
}

export interface ISize {
    width: number;
    height: number;
}

export type OptionalBinding = "<?" | "@?" | "=?";

export type MandatoryBinding = "<" | "@" | "=" | "&";

export type BindingType = MandatoryBinding | OptionalBinding;

export type Binding<T, Type extends BindingType> = T;

export type Require<T> = Binding<T, "<">;

function getVisualViewport() {
    const w = window as any;
    if (!w.visualViewport) {
        return undefined;
    }
    return w.visualViewport as {
        height: number,
        offsetLeft: number,
        offsetTop: number,
        onresize: null,
        onscroll: null,
        pageLeft: number,
        pageTop: number,
        scale: number,
        width: number,
    };
}

function fixClientRectForVisualViewport(r: ClientRect | DOMRect) {
    const vw = getVisualViewport();
    if (!vw) {
        return r;
    }
    const newLeft = r.left - vw.offsetLeft;
    const newTop = r.top - vw.offsetTop;
    return {
        bottom: newTop + r.height,
        height: r.height,
        left: newLeft,
        right: newLeft + r.width,
        top: newTop,
        width: r.width,
    };
}

export function getVisualBoundingRect(el: Element) {
    return fixClientRectForVisualViewport(el.getBoundingClientRect());
}

export function getOutOffsetFully(el: Element): IBounds {
    const rect = getVisualBoundingRect(el);
    const bounds = {left: 0, top: 0, right: 0, bottom: 0};
    if (rect.top < 0) {
        bounds.top = rect.top;
    }
    const {width, height} = getViewPortSize();
    if (rect.bottom > height) {
        bounds.bottom = height - rect.bottom;
    }
    if (rect.right > width) {
        bounds.right = width - rect.right;
    }
    if (rect.left < 0) {
        bounds.left = rect.left;
    }
    return bounds;
}

export function getOutOffsetVisible(el: Element) {
    const minVisiblePixels = 20;
    const rect = getVisualBoundingRect(el);
    const bounds = {left: 0, top: 0, right: 0, bottom: 0};
    if (rect.top < 0) {
        bounds.top = rect.top;
    }
    const {width, height} = getViewPortSize();
    if (rect.top > height - minVisiblePixels) {
        bounds.bottom = height - rect.bottom + rect.height - minVisiblePixels;
    }
    if (rect.left > width - minVisiblePixels) {
        bounds.right = width - rect.right + rect.width - minVisiblePixels;
    }
    if (rect.left + rect.width < minVisiblePixels) {
        bounds.left = rect.left + rect.width - minVisiblePixels;
    }
    return bounds;
}

export const empty = () => {
};

// adapted from https://stackoverflow.com/a/11744120
export function getViewPortSize() {
    const vw = getVisualViewport();
    if (vw) {
        return {width: vw.width, height: vw.height};
    }
    const w = window,
        d = document,
        e = d.documentElement,
        g = d.getElementsByTagName('body')[0];
    // documentElement.client{Width,Height} excludes scrollbars, so it works best.
    if (!e) return {width: 1024, height: 768};
    const width = e.clientWidth,
        height = e.clientHeight;
    return {width, height};
}

export function isSmallScreen() {
    return getViewPortSize().width < 1200;
}

export function isMobileDevice() {
    const touch = typeof ("ontouchstart" in window || navigator.msMaxTouchPoints) !== "undefined";
    return touch && isSmallScreen();
}

export function escapeRegExp(str: string) {
    return str.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
}

export function debugTextToHeader(s: string) {
  var para = document.createElement("p");
  var node = document.createTextNode(s);
  para.appendChild(node);
  var element = document.getElementById("header");
  if ( element )
    element.appendChild(para);
}

export function getPageXY(e: JQuery.Event) {
    if (!(
        "pageX" in e) || (
        e.pageX == 0 && e.pageY == 0)) {
        const originalEvent = e.originalEvent as any;
        if (originalEvent.touches.length) {
            return {
                X: originalEvent.touches[0].pageX,
                Y: originalEvent.touches[0].pageY,
            };
        }
        if (originalEvent.changedTouches.length) {
            return {
                X: originalEvent.changedTouches[0].pageX,
                Y: originalEvent.changedTouches[0].pageY,
            };
        }
        // return null;
    }

    return {X: e.pageX, Y: e.pageY};
}

export function setStorage(key: string, value: any) {
    const s = JSON.stringify(value);
    window.localStorage.setItem(key, s);
}

export function getStorage(key: string) {
    const s = window.localStorage.getItem(key);
    if (!s) {
        return s;
    }
    return JSON.parse(s);
}

// from https://stackoverflow.com/a/1026087
export function capitalizeFirstLetter(s: string) {
    if (s.length === 0) {
        return s;
    }
    return s.charAt(0).toUpperCase() + s.slice(1);
}

export type ToReturn<T, U = {data: {error: string}}> = IPromise<Result<IHttpResponse<T>, U>>;
export const ToReturn = Promise;

export function injectStyle(url: string) {
    const links = document.getElementsByTagName("link");
    for (let i = 0; i < links.length; i++) {
        if (links[i].getAttribute("href") === url) {
            return;
        }
    }
    const head = document.getElementsByTagName("head")[0];
    const link = document.createElement("link");
    link.setAttribute("rel", "stylesheet");
    link.setAttribute("href", url);
    head.appendChild(link);
}

export function fixDefExport<T>(o: {default: T}) {
    return o as any as T;
}

export interface IOkResponse {
    status: "ok";
}

export function numOrStringToNumber(s: number | string) {
    if (typeof s === "number") {
        return s;
    }
    return parseFloat(s);
}

export function valueOr<T extends {}, K extends T>(v: T | undefined | null, def: K): T {
    return v != null ? v : def;
}

export function valueDefu(s: string | undefined | null, def: string): string {
    if (s === undefined) return def;
    if (s === null) return "";
    return s;
}

export const StringArray = t.array(t.string);
export const ModuleArray = t.array(t.type({name: t.string, requires: StringArray}));

export type MouseOrTouch = MouseEvent | Touch;

export function isTouchEvent(e: MouseOrTouch | TouchEvent): e is TouchEvent {
    return (window as any).TouchEvent && e instanceof TouchEvent;
}

export function posToRelative(e: Element, p: MouseOrTouch | TouchEvent) {
    const rect = e.getBoundingClientRect();
    let posX;
    let posY;
    if (!isTouchEvent(p)) {
        posX = p.clientX;
        posY = p.clientY;
    } else {
        posX = p.touches[0].clientX;
        posY = p.touches[0].clientY;
    }

    return {
        x: posX - rect.left,
        y: posY - rect.top,
    };
}
