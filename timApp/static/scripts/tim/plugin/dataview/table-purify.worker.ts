/// <reference lib="webworker" />

/**
 * Custom WebWorker that sanitizes given rows of a DataView. This is one of the primary methods to speed up TimTable
 * loading, as sanitizing initial data takes up to 50% of the loading time.
 *
 * Because web workers are isolated from the main thread, no DOM API is available, in which case
 * using DOMPurify is impossible. As such, this web worker uses sanitize-html instead which is initially
 * designed for running in isolated environments with no access to DOM (like NodeJS, for instance).
 *
 * Web workers are bundled separately from main app chunks, so sanitize-html is only bundled as part of this web worker.
 * That means that the penalty of downloading an extra purifier comes only when tables are used with virtual scrolling
 * enabled.
 */

import * as sanitizeHtml from 'sanitize-html';

interface PurifyData {
    row: number;
    data: string[];
}

addEventListener('message', ({data}: { data: PurifyData }) => {
    postMessage({
        data: data.data.map(c => sanitizeHtml(c)),
        row: data.row
    } as PurifyData);
});
