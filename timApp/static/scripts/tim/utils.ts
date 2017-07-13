import angular, {IPromise} from "angular";
import $ from "jquery";
import sessionsettings from "tim/session";
import {$log, $timeout} from "./ngimport";
import moment from "moment";

export function checkBindings(controller: any, bindings: {[name: string]: string}) {
    for (const k of Object.keys(bindings)) {
        if (!bindings[k].startsWith("?")) {
            if (controller[k] === undefined) {
                throw new Error(`Binding is undefined: ${k}`);
            }
        }
    }
}

// adapted from http://aboutcode.net/2013/07/27/json-date-parsing-angularjs.html
export function convertDateStringsToMoments(input): void {
    if (input === null || typeof input !== "object") {
        return;
    }

    for (const key of Object.keys(input)) {
        const value = input[key];
        if (typeof value === "string") {
            const m = moment(value, moment.ISO_8601, true);
            if (m.isValid()) {
                input[key] = m;
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
    if (x !== null && x !== undefined) {
        return x.toString();
    }
    return "null";
}

export function checkIfElement(x: any): x is Element {
    return typeof ((x as any).hasAttribute) === "function";
}

/**
 * Scroll window to the given element.
 * @method scrollToElement
 * @param element - Element to scroll to.
 */
export function scrollToElement(element) {
    if (!!element && element.scrollIntoView) {
        element.scrollIntoView();
    }
}

/**
 * Gets the parent element of the given element.
 * @method getElementParent
 * @param element - Element whose parent is queried for
 * @returns {Element} Element parent
 */
export function getElementParent(element: Node): Element {
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

    getElementParent(parent);
}

export function dist(coords1, coords2) {
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

export function GetURLParameter(sParam: string): string | null {
    const sPageURL = window.location.search.substring(1);
    const sURLVariables = sPageURL.split("&");
    for (const urlvar of sURLVariables) {
        const sParameterName = urlvar.split("=");
        if (sParameterName[0] === sParam) {
            return decodeURIComponent(sParameterName[1]);
        }
    }
    return null;
}

export function setsetting(setting, value) {
    $.ajax({
        type: "POST",
        url: "/sessionsetting/" + setting + "/" + value,
        success(data) {
            $log.info(data);
            sessionsettings[setting] = value;
        },
        error() {
            $log.info("Could not set setting.");
        },
    });
}

export function applyMixins(derivedCtor: any, baseCtors: any[]) {
    baseCtors.forEach((baseCtor) => {
        Object.getOwnPropertyNames(baseCtor.prototype).forEach((name) => {
            derivedCtor.prototype[name] = baseCtor.prototype[name];
        });
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
export function markAsUsed(...modules: any[]) {
    // no need to do anything here
}
