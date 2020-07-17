/// <reference lib="webworker" />

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
