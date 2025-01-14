import type {IHttpResponse, IPromise} from "angular";
import angular from "angular";
import type {Props} from "io-ts";
import * as t from "io-ts";
import moment from "moment";
import type {AbstractControl, ValidatorFn} from "@angular/forms";
import humanizeDuration from "humanize-duration";
import {isLeft} from "fp-ts/lib/Either";
import type {Pos} from "tim/ui/pos";
import {either} from "fp-ts/Either";
import type {Observable} from "rxjs";
import {lastValueFrom} from "rxjs";
import {HttpErrorResponse, HttpParams} from "@angular/common/http";
import type {IGroup} from "tim/user/IUser";
import {$rootScope, $timeout} from "tim/util/ngimport";

const blacklist = new Set([
    "name",
    "title",
    "completionDate",
    "message",
    "rows", // question data
]);
export const UnknownRecord = t.record(t.string, t.unknown);
const UnknownRecordOrArray = t.union([UnknownRecord, t.array(t.unknown)]);

export const defaultErrorMessage = "Syntax error or no reply from server?";
export const defaultWuffMessage = $localize`Something went wrong. TIM admins have been notified about the issue.`;
export const defaultTimeout = 20000;

const timestampFormats = [
    "YYYY-MM-DDTHH:mm:ss.SSSSSSZ",
    "YYYY-MM-DDTHH:mm:ssZ",
];

// adapted from http://aboutcode.net/2013/07/27/json-date-parsing-angularjs.html
export function convertDateStringsToMoments(input: unknown): unknown {
    if (!UnknownRecordOrArray.is(input)) {
        return input;
    }

    let converted: Record<string, unknown> | Array<unknown>;
    if (Array.isArray(input)) {
        converted = [];
        for (const value of input) {
            if (typeof value === "string") {
                const m = moment(value, timestampFormats, true);
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
            if (!blacklist.has(key)) {
                if (typeof value === "string") {
                    const m = moment(value, timestampFormats, true);
                    if (m.isValid()) {
                        converted[key] = m;
                    } else {
                        converted[key] = value;
                    }
                } else {
                    converted[key] = convertDateStringsToMoments(value);
                }
            } else {
                converted[key] = value;
            }
        }
    }
    return converted;
}

export function angularWait(): IPromise<void> {
    return $timeout(() => {}, 100);
}

export function isText(n: Node): n is Text {
    return n.nodeType === Node.TEXT_NODE;
}

export function stringOrNull(x: {toString: () => string}): string {
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
 * @param allowOverflow - If true, consider overflowing element as in viewport
 * @returns {boolean} true if in view
 */
export function isInViewport(el: Element, allowOverflow = false) {
    const rect = el.getBoundingClientRect();
    if (!document.documentElement) {
        return true;
    }
    // Some elements are not visible
    if (rect.width == 0 && rect.height == 0 && rect.x == 0 && rect.y == 0) {
        return false;
    }
    const h = window.innerHeight || document.documentElement.clientHeight;
    const w = window.innerWidth || document.documentElement.clientWidth;

    // Check if element is fully inside the viewport
    const fullyInside =
        rect.top >= 0 && rect.left >= 0 && rect.bottom <= h && rect.right <= w;
    if (fullyInside) {
        return true;
    }

    // Check if element fills in the viewport in full in one of the axes
    return (
        allowOverflow &&
        ((rect.top <= 0 && rect.bottom >= h) ||
            (rect.left <= 0 && rect.right >= h))
    );
}

/**
 * Scroll window to the given element.
 * @param element - Element to scroll to.
 */
export function scrollToElement(element?: Element) {
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
export function scrollToViewInsideParent(
    helement: HTMLElement,
    hparent: HTMLElement,
    marginleft: number,
    margintop: number,
    marginright: number,
    marginbottom: number
) {
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
    const bottomdy =
        elementOffset.top +
        height -
        (parentOffset.top + parentInnerHeight) +
        marginbottom;
    const leftdx = parentOffset.left - elementOffset.left + marginleft;
    const rightdx =
        elementOffset.left +
        width -
        (parentOffset.left + parentInnerWidth) +
        marginright;

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
    return Math.sqrt(
        Math.pow(coords2.left - coords1.left, 2) +
            Math.pow(coords2.top - coords1.top, 2)
    );
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

export function getUrlParamsJSON() {
    const params = getUrlParams();
    return Object.fromEntries(params.entries());
}

export function getURLParameter(sParam: string): string | undefined {
    const params = getUrlParams();
    return params.get(sParam) ?? undefined;
}

export function getUrlHttpParams() {
    return new HttpParams({
        fromString: window.location.search.replace("?", "&"),
    });
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

export interface Failure<T> {
    ok: false;
    result: T;
}

export type Result<T, U> = Success<T> | Failure<U>;

type AngularJSError = {data: {error: string}; status?: number};
export type AngularError = {error: {error: string}; status?: number};

export function mapSuccess<T, U, M>(
    r: Result<T, U>,
    f: (p: T) => M
): Result<M, U> {
    if (!r.ok) {
        return r;
    }
    return {ok: true, result: f(r.result)};
}

/**
 * Wraps the given promise so that it always gets fulfilled.
 * Additionally, calls $rootScope.$applyAsync() to force AngularJS update. We can't override global Promise object
 * because new Angular overrides it.
 * Adapted from await-to-js library: https://github.com/scopsy/await-to-js
 * @param promise Promise to wrap.
 * @returns A promise that resolves to either a success or error.
 */
export function to<T, U = AngularJSError>(
    promise: Promise<T> | IPromise<T>
): Promise<Result<T, U>> {
    return refreshAngularJS(to2<T, U>(promise as Promise<T>));
}

/**
 * Same as "to" function, but meant to be called from new Angular.
 * @param promise
 */
export function to2<T, U = AngularError>(
    promise: Promise<T>
): Promise<Result<T, U>> {
    return promise
        .then<Success<T>>((data: T) => ({ok: true, result: data}))
        .catch<Failure<U>>((err) => {
            return {ok: false, result: err as U};
        });
}

export function toPromise<T, U = AngularError>(
    observable: Observable<T>
): Promise<Result<T, U>> {
    return lastValueFrom(observable)
        .then<Success<T>>((data: T) => ({ok: true, result: data}))
        .catch<Failure<U>>((err) => {
            if (err instanceof HttpErrorResponse) {
                return {
                    ok: false,
                    result: {
                        error: err.error ?? err.message,
                        status: err.status,
                    } as U,
                };
            }
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

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export type Binding<T, Type extends BindingType> = T;

export type Require<T> = Binding<T, "<">;

interface ExtendedWindow extends Window {
    TouchEvent?: unknown;
}

function getVisualViewport(): VisualViewport | null {
    return window.visualViewport;
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
    const minVisiblePixels = 50;
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

export const empty = () => {};

// adapted from https://stackoverflow.com/a/11744120
export function getViewPortSize() {
    const vw = getVisualViewport();
    if (vw) {
        return {width: vw.width, height: vw.height};
    }
    const d = document;
    const e = d.documentElement;
    // documentElement.client{Width,Height} excludes scrollbars, so it works best.
    if (!e) {
        return {width: 1024, height: 768};
    }
    const width = e.clientWidth;
    const height = e.clientHeight;
    return {width, height};
}

export function isMobileDevice() {
    const touch =
        window.ontouchstart !== undefined || navigator.maxTouchPoints > 0;
    return touch && isScreenSizeOrLower("md");
}

// TIM also defines four device-* DIVs which can be also used to determine the active media type,
// but in some cases screen size info is needed even before the DOM has been drawn fully.
// For this reason, we define the media sizes manually here -- it's unlikely they'll change in a while.
const MEDIA_SIZES = {
    xs: 768,
    sm: 992,
    md: 1281, // Note: this has been customized. Bootstrap default is 1200.
};

export function isScreenSizeOrLower(size: keyof typeof MEDIA_SIZES) {
    return getViewPortSize().width < MEDIA_SIZES[size];
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

function getTouchCoords(e: TouchEvent): Pos {
    if (e.touches.length) {
        return {
            x: e.touches[0].pageX,
            y: e.touches[0].pageY,
        };
    }
    if (e.changedTouches.length) {
        return {
            x: e.changedTouches[0].pageX,
            y: e.changedTouches[0].pageY,
        };
    }
    return {x: 0, y: 0};
}

export function getPageXY(
    e: JQuery.MouseEventBase | JQuery.TouchEventBase | MouseEvent | TouchEvent
): Pos {
    if (e instanceof MouseEvent) {
        return {x: e.pageX, y: e.pageY};
    } else if (isTouchEvent(e)) {
        return getTouchCoords(e);
    }
    if (e.pageX == 0 && e.pageY == 0) {
        if (!windowAsAny().TouchEvent) {
            return {x: e.pageX, y: e.pageY};
        }
        const o = e.originalEvent;
        if (!o || !isTouchEvent(o)) {
            return {x: e.pageX, y: e.pageY};
        }
        return getTouchCoords(o);
    }
    if (e.pageX === undefined || e.pageY === undefined) {
        return {x: 0, y: 0};
    }

    return {x: e.pageX, y: e.pageY};
}

export function setStorage(key: string, value: unknown) {
    const s = JSON.stringify(value);
    window.localStorage.setItem(key, s);
}

export function getStorage(key: string): unknown {
    const s = window.localStorage.getItem(key);
    if (s === null) {
        return s;
    }
    try {
        return JSON.parse(s);
    } catch (e) {
        return null;
    }
}

export function getTypedStorage<T, O = T>(
    key: string,
    decoder: t.Type<T, O>
): T | undefined {
    const r = decoder.decode(getStorage(key));
    if (isLeft(r)) {
        return undefined;
    } else {
        return r.right;
    }
}

export class TimStorage<T, O = T> {
    constructor(private key: string, private codec: t.Type<T, O>) {}

    set(value: T) {
        setStorage(this.key, this.codec.encode(value));
    }

    get() {
        return getTypedStorage(this.key, this.codec);
    }
}

// from https://stackoverflow.com/a/1026087
export function capitalizeFirstLetter<T extends string>(s: T): Capitalize<T> {
    if (s.length === 0) {
        return s as Capitalize<T>;
    }
    return (s.charAt(0).toUpperCase() + s.slice(1)) as Capitalize<T>;
}

export type ToReturn<T, U = {data: {error: string}}> = Promise<
    Result<IHttpResponse<T>, U>
>;
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
    return parseFloat(s.replace(",", "."));
}

export function valueOr<
    T extends Record<string, unknown> | string | number | boolean,
    K extends T
>(v: T | undefined | null, def: K): T {
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
export const ModuleArray = t.array(
    t.type({name: t.string, requires: StringArray})
);
export const StringOrNumber = t.union([t.string, t.number]);

export type MouseOrTouch = MouseEvent | Touch;

export function touchEventToTouch(event: TouchEvent) {
    return event.touches[0] || event.changedTouches[0];
}

export function isTouchEvent(
    e:
        | MouseOrTouch
        | TouchEvent
        | Event
        | JQuery.MouseEventBase
        | JQuery.TouchEventBase
): e is TouchEvent {
    return !!(window as ExtendedWindow).TouchEvent && e instanceof TouchEvent;
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

const iDevices = new Set([
    "iPad Simulator",
    "iPhone Simulator",
    "iPod Simulator",
    "iPad",
    "iPhone",
    "iPod",
]);

export function isIOS() {
    // navigator.platform is legacy but still the only option is some browsers
    if (!!navigator.platform) {
        return iDevices.has(navigator.platform);
    }

    // New iPads can report MacIntel as platform
    // https://stackoverflow.com/a/58979271
    return !!(
        navigator.userAgent.match(/ipad|ipod|iphone/i) ||
        (navigator.platform === "MacIntel" && navigator.maxTouchPoints > 2)
    );
}

export function isSafari() {
    return (
        navigator.userAgent.includes("Safari") &&
        !navigator.userAgent.includes("Chrome")
    );
}

export function isFirefox() {
    return getBrowserKind() == BrowserKind.Firefox;
}

export function isIE() {
    return getBrowserKind() == BrowserKind.IE;
}

export function copyToClipboard(s: string) {
    void navigator.clipboard.writeText(s);
}

export const dateFormat = "D.M.YYYY HH:mm:ss";

export function getCookie(name: string) {
    const a = `; ${document.cookie}`.match(`;\\s*${name}=([^;]+)`);
    return a ? a[1] : undefined;
}

export function getGroupDesc(group: IGroup) {
    return group.personal_user
        ? group.personal_user.real_name + " (" + group.name + ")"
        : group.name;
}

export function windowAsAny() {
    return window as unknown as Record<string, unknown>;
}

export function createValidator(
    validityChecker: (s: string) => boolean,
    name: string
): ValidatorFn {
    return (control: AbstractControl) => {
        const viewValue = control.value;
        if (typeof viewValue !== "string") {
            return null;
        }
        const valid = validityChecker(viewValue);
        return valid ? null : {[name]: {value: viewValue}};
    };
}

/**
 * On Firefox, we need max-content in timTable to get rid of the horizontal scrollbar.
 * On the other hand, max-content does not work well in Chrome because it makes some columns too wide.
 */
export function maxContentOrFitContent() {
    return isFirefox() ? "max-content" : "fit-content";
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

export function parseIframeopts(iframeopts: string, framesrc: string) {
    const parse = Range.prototype.createContextualFragment.bind(
        document.createRange()
    );
    const parsed = parse(`<div ${iframeopts}></div>`);
    let sandbox = "";
    let allow = null;
    for (const c of parsed.firstElementChild?.attributes ?? []) {
        if (c.name === "sandbox") {
            sandbox = c.value;
        }
        if (c.name === "allow") {
            allow = c.value;
        }
        // TODO: Handle possible other iframe options.
    }
    let canAllowScriptsAndSameOrigin = false;
    try {
        const u = new URL(framesrc, location.origin);
        canAllowScriptsAndSameOrigin = location.origin !== u.origin;
    } catch {}
    if (
        sandbox.includes("allow-scripts") &&
        sandbox.includes("allow-same-origin") &&
        !canAllowScriptsAndSameOrigin
    ) {
        sandbox = sandbox.replace("allow-same-origin", "");
        console.warn(
            "Disallowed unsafe sandbox value (allow-scripts allow-same-origin); removed allow-same-origin."
        );
    }
    return {sandbox, allow};
}

export function seconds2Time(seconds: number) {
    const d = moment.duration(seconds, "s");
    return {hours: d.hours(), minutes: d.minutes(), seconds: d.seconds()};
}

const pad = (tt: number) => `${tt < 10 ? "0" : ""}${tt}`;

export function secondsToHHMMSS(
    time: number,
    displayUnits: humanizeDuration.Unit[]
) {
    const d = seconds2Time(time);
    const hours = d.hours;
    let minutes = d.minutes;
    let seconds = d.seconds;
    let s = "";
    let shouldPadSeconds = false;
    if (displayUnits.includes("h")) {
        s += pad(hours) + ":";
        shouldPadSeconds = true;
    } else {
        minutes += hours * 60;
    }
    if (displayUnits.includes("m")) {
        s += pad(minutes) + ":";
        shouldPadSeconds = true;
    } else {
        seconds += minutes * 60;
    }
    s += shouldPadSeconds ? pad(seconds) : seconds;
    return s;
}

/**
 * Formats a string in form "Text {i}" where `i` is the index of the item to insert.
 * @param s Format string
 * @param fmt Values to insert in place of templates "{i}"
 */
export function formatString(s: string, ...fmt: string[]) {
    return fmt.reduce((str, val, i) => str.replace(`{${i}}`, val), s);
}

export function templateString(
    s: string,
    fmtVariables: Record<string, unknown>
) {
    return Object.entries(fmtVariables).reduce(
        (str, [key, val]) => str.replace(`{${key}}`, "" + val),
        s
    );
}

export function getViewName() {
    return document.location.pathname.split("/")[1];
}

export function secondsToShortTime(
    secs: number,
    displayUnits: humanizeDuration.Unit[] = [],
    locale: string = "en"
) {
    const SECS_IN_DAY = 24 * 60 * 60;
    let prefix = "";
    if (secs > SECS_IN_DAY) {
        const rem = secs % SECS_IN_DAY;
        prefix =
            humanizeDuration((secs - rem) * 1000, {
                units: ["d"],
                round: true,
                language: locale,
            }) + " + ";
        secs = rem;
    }
    return `${prefix}${secondsToHHMMSS(secs, displayUnits)}`;
}

export function timeout(ms?: number) {
    return new Promise((resolve) => window.setTimeout(resolve, ms));
}

/**
 * Shortcut function for defining a codec with mandatory and optional properties.
 * @param mandatory The mandatory properties.
 * @param optional The optional properties.
 */
export function mandatoryAndOptional<T extends Props, U extends Props>(
    mandatory: T,
    optional: U
) {
    return t.intersection([t.type(mandatory), t.partial(optional)]);
}

export const DateFromString = new t.Type<Date, string, unknown>(
    "DateFromString",
    (u): u is Date => u instanceof Date,
    (u, c) =>
        either.chain(t.string.validate(u, c), (s) => {
            const d = new Date(s);
            return isNaN(d.getTime()) ? t.failure(u, c) : t.success(d);
        }),
    (a) => a.toISOString()
);

export const MomentFromString = new t.Type<moment.Moment, string, unknown>(
    "DateFromString",
    (u): u is moment.Moment => moment.isMoment(u),
    (u, c) =>
        either.chain(t.string.validate(u, c), (s) => {
            const d = moment.utc(s);
            return d.isValid() ? t.success(d) : t.failure(u, c);
        }),
    (a) => a.toISOString()
);

export const MomentDurationFromString = new t.Type<
    moment.Duration,
    string,
    unknown
>(
    "DateFromString",
    (u): u is moment.Duration => moment.isDuration(u),
    (u, c) =>
        either.chain(t.string.validate(u, c), (s) => {
            const d = moment.duration(s);
            return d.isValid() ? t.success(d) : t.failure(u, c);
        }),
    (a) => a.toISOString()
);

/**
 * Given the list of strings, shorten them to the shortest unique prefix.
 *
 * For example ["Adam", "David", "Addy", "Bob"] => ["Ada", "D", "Add", "B"]
 *
 * @param strs Strings to shorten and to keep unique.
 */
export function shortestUniquePrefixes(strs: string[]): string[] {
    const result = [];
    for (const str of strs) {
        let prefixLength = 0;
        while (
            prefixLength < str.length &&
            strs.some(
                (s) => s != str && s.startsWith(str.substring(0, prefixLength))
            )
        ) {
            prefixLength++;
        }
        result.push(str.substring(0, prefixLength));
    }
    return result;
}

export function closest(arr: number[], val: number): number {
    return arr.reduce((prev, curr) =>
        Math.abs(curr - val) < Math.abs(prev - val) ? curr : prev
    );
}

export function formatNumberCode(
    numCode: string,
    splitEvery: number,
    targetLength?: number,
    separator: string = "-"
) {
    targetLength = targetLength ?? numCode.length;
    let newCode = "";
    for (let i = 0; i < targetLength; i++) {
        if (i > 0 && i % splitEvery === 0) {
            newCode += separator;
        }
        newCode += numCode[i] || " ";
    }
    return newCode;
}

export function replaceStyle(
    styleOrigin: "user-prefs-style" | "document-style" | "document-settings",
    toPath: string
) {
    const el = document.querySelector(
        `link[rel="stylesheet"][data-style-origin="${styleOrigin}"]`
    );
    if (!el) {
        return;
    }
    const newPath = `/${toPath}`;
    if (el.getAttribute("href") == newPath) {
        return;
    }
    el.setAttribute("href", newPath);
}

/**
 * Splits the given string into items. The items are separated by newlines, commas, or semicolons.
 * Any leading dashes are removed from the items.
 *
 * @param itemsString String to split into items.
 * @returns Array of items.
 */
export function splitItems(itemsString: string): string[] {
    return itemsString
        .split("\n")
        .flatMap((n) => n.split(/[,;]/))
        .map((n) => n.replace(/^ *- */, "").trim());
}
