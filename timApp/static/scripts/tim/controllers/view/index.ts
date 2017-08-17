import $ from "jquery";
import {addEvents} from "tim/marktree";
import {timLogTime} from "tim/timTiming";
import {$document, $http, $log, $timeout, $window} from "../../ngimport";
import {getActiveDocument} from "./document";

function totext(str) {
    if (str.indexOf("{") > 0) {
        return str.substring(0, str.indexOf("{")).trim();
    }
    return str;
}

function tolink(str) {
    if (str.indexOf("{") >= 0 && str.indexOf("}") > 0) {
        const ob = str.indexOf("{");
        const cb = str.indexOf("}");
        return str.substring(ob + 1, cb);
    }
    return "#" + str.replace(/^(\d)+(\.\d+)*\.? /, "").trim().replace(/ +/g, "-").toLowerCase();
}

function findIndexLevel(str) {
    for (let i = 0; i < str.length; i++) {
        if (str.charAt(i) !== "#") {
            return i;
        }
    }

    return 0;
}

function invertState(state) {
    if (state === "exp") {
        return "col";
    }
    if (state === "col") {
        return "exp";
    }
    return state;
}

function clearSelection() {
    const doc = $document as any;
    if (doc.selection) {
        doc.selection.empty();
    }
    else if ($window.getSelection) {
        $window.getSelection().removeAllRanges();
    }
}

function invertStateClearSelection(event, state) {
    if (event.which !== 1) {
        // Listen only to the left mouse button
        return state;
    }

    const newState = invertState(state);
    if (newState !== state) {
        clearSelection();
    }
    return newState;
}

async function getIndex() {
    timLogTime("getindex", "view");
    const doc = getActiveDocument();
    try {
        var response = await $http.get<any>("/index/" + doc.id);
    } catch (e) {
        $log.error("Could not get index");
        return;
    }
    timLogTime("getindex succ", "view");
    if (response.data.empty) {
    } else {
        const indexElement = $(".index-sidebar .sideBarContainer");
        $(indexElement).html(response.data);
    }
    timLogTime("getindex done", "view");
}

export function initIndex() {
    // call marktree.js initialization function so that TOC clicking works
    addEvents();
    $timeout(() => {
        const indexHeadings = $("#menuTabs").find(".subexp .exp");
        const subHeadings = indexHeadings.find("ul.sub li.basic");
        if (indexHeadings.length === 1 || indexHeadings.length + subHeadings.length < 40) {
            indexHeadings.attr("class", "col");
            indexHeadings.children(".sub").attr("class", "subexp");
        }
    });
}
