import angular from "angular";
import $ from "jquery";
import sessionsettings from "tim/session";


/**
 * Check if element is in view
 * @method isInViewport
 * @param element - Element to check
 * @returns {boolean} true if in view
 */
export function isInViewport(element) {
    var elementTop = $(element).offset().top;
    var elementBottom = elementTop + $(element).outerHeight();
    var viewportTop = $(window).scrollTop();
    var viewportBottom = viewportTop + $(window).height();
    return elementBottom > viewportTop && elementTop < viewportBottom;
};

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
export function getElementParent(element) {
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
    if (typeof parent.tagName !== "undefined") {
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

export function GetURLParameter(sParam: string) {
    const sPageURL = window.location.search.substring(1);
    const sURLVariables = sPageURL.split("&");
    for (let i = 0; i < sURLVariables.length; i++) {
        const sParameterName = sURLVariables[i].split("=");
        if (sParameterName[0] == sParam) {
            return decodeURIComponent(sParameterName[1]);
        }
    }
}

export function setsetting(setting, value) {
    $.ajax({
        type: "POST",
        url: "/sessionsetting/" + setting + "/" + value,
        success(data) {
            console.log(data);
            sessionsettings[setting] = value;
        },
        error() {
            console.log("Could not set setting.");
        },
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
