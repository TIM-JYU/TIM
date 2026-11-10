import $ from "jquery";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import type {
    AnswerBrowserData,
    IAnswerSaveEvent,
} from "tim/answer/answer-browser.component";

export function handleAnswerResponse(
    taskIdFull: string,
    params: IAnswerSaveEvent
) {
    const parts = taskIdFull.split(".");
    const docId = parseInt(parts[0], 10);
    const taskName = parts[1];
    const taskId = docId + "." + taskName;
    const v = vctrlInstance;
    if (!v) {
        throw new Error("ViewCtrl was undefined");
    }
    const ab = v.getAnswerBrowser(taskId);
    if (ab) {
        ab.registerNewAnswer(params);
    }
}

export function prepareAnswerRequest(taskIdFull: string, url: string) {
    const parts = taskIdFull.split(".");
    const isFull = parts.length === 3;
    const docId = parseInt(parts[0], 10);
    const taskName = parts[1];
    const taskId = docId + "." + taskName;
    const v = vctrlInstance;
    if (!v) {
        throw new Error("ViewCtrl was undefined");
    }
    const d: {
        abData?: AnswerBrowserData;
        ref_from?: {docId: number; par?: string};
    } = {};
    if (taskName !== "") {
        const ab = v.getAnswerBrowser(taskId);
        if (ab) {
            d.abData = ab.getBrowserData();
        }
    }
    let e;
    if (isFull) {
        e = document.getElementById(taskIdFull);
    } else {
        e = document.querySelector(`[id^='${CSS.escape(taskIdFull)}.']`);
    }

    if (e) {
        const par = $(e).parents(".par");
        d.ref_from = {docId: v.item.id, par: par.attr("id")};
    }
    return {
        url: url + window.location.search, // make urlmacros possible, must know the page url
        data: d,
    };
}

export function getTaskIdIfAnswerUrl(url: string): string | undefined {
    const fullUrl = new URL(url, window.location.origin);
    const re = /\/[^/]+\/([^/]+)\/answer$/;
    const match = re.exec(fullUrl.pathname);
    if (match) {
        return decodeURIComponent(match[1]);
    }
    return undefined;
}
