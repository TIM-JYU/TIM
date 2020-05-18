import angular, {IHttpResponse, IPromise} from "angular";
import * as t from "io-ts";
import moment from "moment";
import {AbstractControl, ValidatorFn} from "@angular/forms";
import {IGroup} from "../user/IUser";
import {$rootScope, $timeout} from "./ngimport";

const blacklist = new Set(["name", "title", "completionDate"]);
const UnknownRecord = t.record(t.string, t.unknown);

export const defaultErrorMessage = "Syntax error or no reply from server?";
export const defaultTimeout = 20000;


// adapted from http://aboutcode.net/2013/07/27/json-date-parsing-angularjs.html
export function convertDateStringsToMoments(input: unknown): unknown {
    if (!UnknownRecord.is(input)) {
        return input;
    }

    let converted: Record<string, unknown> | Array<unknown>;
    if (Array.isArray(input)) {
        converted = [];
        for (const value of input) {
            if (typeof value === "string") {
                const m = moment(value, moment.ISO_8601, true);
                if (m.isValid()) {
                    converted.push(m);
                } else {
                    converted.push(value);
                }
            } else {
                converted.push(convertDateStringsToMoments(value));
            }
        }
    } else {
        converted = {};
        for (const [key, value] of Object.entries(input)) {
            if (typeof value === "string") {
                // ignore strings and keys that are most likely not intended to be interpreted as timestamps
                if (!blacklist.has(key)) {
                    const m = moment(value, moment.ISO_8601, true);
                    if (m.isValid()) {
                        converted[key] = m;
                    } else {
                        converted[key] = value;
                    }
                } else {
                    converted[key] = value;
                }
            } else {
                converted[key] = convertDateStringsToMoments(value);
            }
        }
    }
    return converted;
}

export function angularWait(): IPromise<void> {
    return $timeout(() => {
    }, 100);
}

export function assertIsText(n: Node): n is Text {
    if (n.nodeType !== Node.TEXT_NODE) {
        throw new Error("Expected a Text node, got " + typeof n);
    }
    return true;
}

export function stringOrNull(x: { toString: () => string }): string {
    if (x != null) {
        return x.toString();
    }
    return "null";
}

export function checkIfElement(x: Node): x is Element {
    return x instanceof Element;
}

/**
 * Check if element is in view
 * @param el - Element to check
 * @returns {boolean} true if in view
 */
export function isInViewport(el: Element) {
    const rect = el.getBoundingClientRect();
    if (!document.documentElement) {
        return true;
    }
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
 * Scroll element in to view inside parent.
 * See https://stackoverflow.com/questions/6215779/scroll-if-element-is-not-visible
 * @param helement html element to scroll
 * @param hparent html parent wehre to scroll
 * @param marginleft margins for each direction, these are tried to keep front of scroll dircetion
 * @param margintop
 * @param marginright
 * @param marginbottom
 */
export function scrollToViewInsideParent(helement: HTMLElement, hparent: HTMLElement,
                                         marginleft: number, margintop: number,
                                         marginright: number, marginbottom: number) {
    const element = $(helement);
    if (element == null) {
        return false;
    }
    const parent = $(hparent);
    if (parent == null) {
        return false;
    }
    const elementOffset = element.offset();
    if (elementOffset == null) {
        return false;
    }
    const parentScrollTop = parent.scrollTop();
    if (parentScrollTop == null) {
        return false;
    }
    const parentScrollLeft = parent.scrollLeft();
    if (parentScrollLeft == null) {
        return false;
    }
    const parentInnerHeight = parent.innerHeight();
    if (parentInnerHeight == null) {
        return false;
    }
    const parentInnerWidth = parent.innerWidth();
    if (parentInnerWidth == null) {
        return false;
    }
    const parentOffset = parent.offset();
    if (parentOffset == null) {
        return false;
    }
    const height = element.innerHeight();
    if (!height) {
        return;
    }
    const width = element.innerWidth();
    if (!width) {
        return;
    }

    const topdy = parentOffset.top - elementOffset.top + margintop;
    const bottomdy = (elementOffset.top + height) - (parentOffset.top + parentInnerHeight) + marginbottom;
    const leftdx = parentOffset.left - elementOffset.left + marginleft;
    const rightdx = (elementOffset.left + width) - (parentOffset.left + parentInnerWidth) + marginright;

    if (topdy >= 0) {
        parent.scrollTop(parentScrollTop - topdy);
    }
    if (bottomdy >= 0) {
        parent.scrollTop(parentScrollTop + bottomdy);
    }
    if (leftdx >= 0) {
        parent.scrollLeft(parentScrollLeft - leftdx);
    }
    if (rightdx >= 0) {
        parent.scrollLeft(parentScrollLeft + rightdx);
    }
    return true;

    /*
    const offset = elementOffset.top + parentScrollTop;
    const offsetEnd = offset + height;

    const visibleAreaStart = parent.scrollTop();
    if ( visibleAreaStart == null ) { return false; }

    const visibleAreaEnd = visibleAreaStart + parentInnerHeight;

    if ( elementOffset.top != 0 ) { return true; }

    if (offset - height < visibleAreaStart) {
        parent.animate({scrollTop: offset - height}, 600);
        return false;
    } else if (offsetEnd > visibleAreaEnd) {
        parent.animate({scrollTop: parentScrollTop + offsetEnd - visibleAreaEnd }, 600);
        return false;

    }
    return true;
    */
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

export function getUrlParams() {
    return new URLSearchParams(document.location.search);
}

export function getURLParameter(sParam: string): string | undefined {
    const params = getUrlParams();
    return params.get(sParam) ?? undefined;
}

/**
 * Marks seemingly unused imports as used so that TypeScript compiler won't optimize them out when compiling.
 *
 * For example, timApp module needs ngSanitize, but it is specified as a string reference in the angular.module(...)
 * call, which is why TypeScript compiler cannot see the connection to the import statement. See app.ts.
 *
 * @param modules The modules to mark as used.
 */
export function markAsUsed(...modules: unknown[]) {
    // no need to do anything here
}

export function printJson(data: unknown, desc: string = "") {
    console.error(desc, JSON.stringify(data));
}

export function clone<T>(obj: T): T {
    return JSON.parse(JSON.stringify(obj)) as T;
}

interface Success<T> {
    ok: true;
    result: T;
}

interface Failure<T> {
    ok: false;
    result: T;
}

export type Result<T, U> = Success<T> | Failure<U>;

/**
 * Wraps the given promise so that it always gets fulfilled.
 * Additionally, calls $rootScope.$applyAsync() to force AngularJS update. We can't override global Promise object
 * because new Angular overrides it.
 * Adapted from await-to-js library: https://github.com/scopsy/await-to-js
 * @param promise Promise to wrap.
 * @returns A promise that resolves to either a success or error.
 */
export function to<T, U = { data: { error: string } }>(promise: Promise<T> | IPromise<T>): Promise<Result<T, U>> {
    return refreshAngularJS(to2<T, U>(promise as Promise<T>));
}

/**
 * Same as "to" function, but meant to be called from new Angular.
 * @param promise
 */
export function to2<T, U = { error: { error: string } }>(promise: Promise<T>): Promise<Result<T, U>> {
    return promise.then<Success<T>>((data: T) => ({ok: true, result: data})).catch<Failure<U>>((err) => {
        return {ok: false, result: err as U};
    });
}

export function refreshAngularJS<T>(p: Promise<T> | IPromise<T>): Promise<T> {
    return (p as Promise<T>).finally(() => {
        $rootScope.$applyAsync();
    });
}

export function assertNotNull(obj: unknown) {
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

interface ExtendedWindow extends Window {
    visualViewport?: {
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
    TouchEvent?: unknown;
}

function getVisualViewport() {
    const w = window as ExtendedWindow;
    if (!w.visualViewport) {
        return undefined;
    }
    return w.visualViewport;
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
    const w = window;
    const d = document;
    const e = d.documentElement;
    const g = d.getElementsByTagName("body")[0];
    // documentElement.client{Width,Height} excludes scrollbars, so it works best.
    if (!e) {
        return {width: 1024, height: 768};
    }
    const width = e.clientWidth;
    const height = e.clientHeight;
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
    const para = document.createElement("p");
    const node = document.createTextNode(s);
    para.appendChild(node);
    const element = document.getElementById("header");
    if (element) {
        element.appendChild(para);
    }
}

function getTouchCoords(e: TouchEvent) {
    if (e.touches.length) {
        return {
            X: e.touches[0].pageX,
            Y: e.touches[0].pageY,
        };
    }
    if (e.changedTouches.length) {
        return {
            X: e.changedTouches[0].pageX,
            Y: e.changedTouches[0].pageY,
        };
    }
    return {X: 0, Y: 0};
}

export function getPageXY(e: JQuery.MouseEventBase | JQuery.TouchEventBase | MouseEvent | TouchEvent) {
    if (e instanceof MouseEvent) {
        return {X: e.pageX, Y: e.pageY};
    } else if (isTouchEvent(e)) {
        return getTouchCoords(e);
    }
    if (e.pageX == 0 && e.pageY == 0) {
        if (!windowAsAny().TouchEvent) {
            return {X: e.pageX, Y: e.pageY};
        }
        const o = e.originalEvent;
        if (!o || !(isTouchEvent(o))) {
            return {X: e.pageX, Y: e.pageY};
        }
        return getTouchCoords(o);
    }
    if (e.pageX === undefined || e.pageY === undefined) {
        return {X: 0, Y: 0};
    }

    return {X: e.pageX, Y: e.pageY};
}

export function setStorage(key: string, value: unknown) {
    const s = JSON.stringify(value);
    window.localStorage.setItem(key, s);
}

export function getStorage(key: string): unknown {
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

export type ToReturn<T, U = { data: { error: string } }> = Promise<Result<IHttpResponse<T>, U>>;
export const ToReturn = Promise;

export function injectStyle(url: string) {
    const links = document.getElementsByTagName("link");
    for (const l of links) {
        if (l.getAttribute("href") === url) {
            return;
        }
    }
    const head = document.getElementsByTagName("head")[0];
    const link = document.createElement("link");
    link.setAttribute("rel", "stylesheet");
    link.setAttribute("href", url);
    head.appendChild(link);
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
    if (s === undefined) {
        return def;
    }
    if (s === null) {
        return "";
    }
    return s;
}

export const StringArray = t.array(t.string);
export const ModuleArray = t.array(t.type({name: t.string, requires: StringArray}));
export const StringDict = t.record(t.string, t.string);
export const StringUnknownDict = t.record(t.string, t.unknown);
export const StringOrNumber = t.union([t.string, t.number]);

export type MouseOrTouch = MouseEvent | Touch;

export function isTouchEvent(e: MouseOrTouch | TouchEvent | Event | JQuery.MouseEventBase | JQuery.TouchEventBase): e is TouchEvent {
    return (window as ExtendedWindow).TouchEvent && e instanceof TouchEvent;
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

let copyHelperElement: HTMLTextAreaElement | undefined;

export function getClipboardHelper(): HTMLTextAreaElement {
    let e1 = copyHelperElement;  // prevent extra creating and deleting
    if (e1) {
        return e1;
    }
    e1 = document.createElement("textarea");
    e1.setAttribute("readonly", "");
    // e1.style.position = 'absolute';
    e1.style.position = "fixed"; // fixed seems better for FF and Edge so not to jump to end
    // e1.style.left = '-9999px';
    e1.style.top = "-9999px";
    document.body.appendChild(e1);
    // document.body.removeChild(el);
    copyHelperElement = e1;
    return e1;
}

export function copyToClipboard(s: string) {
    const e1 = getClipboardHelper();
    e1.value = s;
    const isIOS = navigator.userAgent.match(/ipad|ipod|iphone/i);
    if (isIOS) {
        // e1.contentEditable = true;
        e1.readOnly = true;
        const range = document.createRange();
        range.selectNodeContents(e1);
        const sel = window.getSelection();
        if (sel) {
            sel.removeAllRanges();
            sel.addRange(range);
        }
        e1.setSelectionRange(0, 999999);
    } else {
        e1.select();
    }
    document.execCommand("copy");
}

export const dateFormat = "D.M.YYYY HH:mm:ss";

export function getCookie(name: string) {
    const a = `; ${document.cookie}`.match(`;\\s*${name}=([^;]+)`);
    return a ? a[1] : undefined;
}

export function getGroupDesc(group: IGroup) {
    return group.personal_user ? group.personal_user.real_name + " (" + group.name + ")" : group.name;
}

export function windowAsAny() {
    return window as unknown as Record<string, unknown>;
}

export function createValidator(validityChecker: (s: string) => boolean, name: string): ValidatorFn {
    return (control: AbstractControl) => {
        const viewValue = control.value;
        if (typeof viewValue !== "string") {
            return null;
        }
        const valid = validityChecker(viewValue);
        return valid ? null : {[name]: {value: viewValue}};
    };
}

export function isFirefox() {
    return getBrowserKind() == BrowserKind.Firefox;
}

export function isIE() {
    return getBrowserKind() == BrowserKind.IE;
}

/**
 * On Firefox, we need max-content in timTable to get rid of the horizontal scrollbar.
 * On the other hand, max-content does not work well in Chrome because it makes some columns too wide.
 */
export function maxContentOrFitContent() {
    return isFirefox() ? "max-content" : "fit-content";
}

enum BrowserKind {
    Chrome,
    Safari,
    IE,
    Firefox,
    Unknown,
}

function getBrowserKind() {
    const userAgent = navigator.userAgent.toLowerCase();
    if (userAgent.includes("chrome")) {
        return BrowserKind.Chrome;
    } else if (userAgent.includes("safari")) {
        return BrowserKind.Safari;
    } else if (userAgent.includes("msie")) {
        return BrowserKind.IE;
    } else if (userAgent.includes("firefox")) {
        return BrowserKind.Firefox;
    }
    if (userAgent.includes("trident/")) {
        return BrowserKind.IE;
    }
    return BrowserKind.Unknown;
}

export function log(s: string) {
    console.log(s);
}

export function truncate(text: string, max: number) {
    if (text.length > max) {
        text = text.substring(0, max - 1) + "...";
    }
    return text;
}

export function parseIframeopts(iframeopts: string) {
    const parse = Range.prototype.createContextualFragment.bind(document.createRange());
    const parsed = parse(`<div ${iframeopts}></div>`);
    let sandbox = "";
    for (const c of parsed.firstElementChild!.attributes) {
        if (c.name === "sandbox") {
            sandbox = c.value;
        }
        // TODO: Handle possible other iframe options.
    }
    return {sandbox};
}

export function seconds2Time(seconds: number) {
    const h = Math.floor(seconds / 3600);
    const m = Math.floor((seconds - h * 3600) / 60);
    const s = seconds - (h * 3600) - (m * 60);
    return {hours: h, minutes: m, seconds: s};
}

export function secondsToHHMMSS(time: number) {
    if (!time) {
        return "";
    }
    const {hours, minutes, seconds} = seconds2Time(time);
    const pad = (tt: number) => `${tt < 10 ? "0" : ""}${tt}`;
    return `${pad(hours)}:${pad(minutes)}:${pad(seconds)}`;
}
