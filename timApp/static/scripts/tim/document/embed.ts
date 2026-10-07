/**
 * Embed mode (?embed=true): the document is shown inside an <iframe> on an external page.
 *
 * The host page cannot read the height of a cross-origin frame, so the embedded page
 * reports its content height with postMessage. It also reports saved answers so that
 * the host can show a status. Messages have the shape
 *
 *     {type: "tim-embed", event: "resize", height: number}
 *     {type: "tim-embed", event: "answer-saved", taskId: string, points: number | null}
 *
 * and are only sent to the parent origin when it is TIM's own origin or one of the
 * origins listed in the EMBED_ALLOWED_ORIGINS config option (the same list that is
 * used for the Content-Security-Policy frame-ancestors header).
 */

export interface IEmbedResizeMessage {
    type: "tim-embed";
    event: "resize";
    height: number;
}

export interface IEmbedAnswerSavedMessage {
    type: "tim-embed";
    event: "answer-saved";
    taskId: string;
    points: number | null;
}

export type EmbedMessage = IEmbedResizeMessage | IEmbedAnswerSavedMessage;

let embedTargetOrigin: string | undefined;
let embedActive = false;
let lastHeight = -1;

/**
 * Returns the origin of the page that embeds this one, or undefined if it cannot be determined.
 */
export function getEmbedParentOrigin(): string | undefined {
    const ancestors = window.location.ancestorOrigins;
    if (ancestors && ancestors.length > 0) {
        return ancestors[0];
    }
    if (document.referrer) {
        try {
            return new URL(document.referrer).origin;
        } catch {
            return undefined;
        }
    }
    return undefined;
}

/**
 * Picks the postMessage targetOrigin: the parent origin if it is allowed, otherwise undefined
 * (in which case nothing is posted).
 */
export function resolveEmbedTargetOrigin(
    allowedOrigins: string[],
    parentOrigin = getEmbedParentOrigin()
): string | undefined {
    if (!parentOrigin) {
        return undefined;
    }
    if (
        parentOrigin === window.location.origin ||
        allowedOrigins.includes(parentOrigin)
    ) {
        return parentOrigin;
    }
    return undefined;
}

export function isEmbedMode() {
    return embedActive;
}

export function postEmbedMessage(msg: EmbedMessage) {
    if (!embedActive || !embedTargetOrigin || window.parent === window) {
        return;
    }
    window.parent.postMessage(msg, embedTargetOrigin);
}

function getContentHeight() {
    // The root element is sized by its content (html/body have no fixed height in embed mode),
    // so this shrinks as well as grows, unlike scrollHeight which never goes below the viewport.
    return Math.ceil(document.documentElement.getBoundingClientRect().height);
}

export function postEmbedHeight(force = false) {
    const height = getContentHeight();
    if (!force && height === lastHeight) {
        return;
    }
    lastHeight = height;
    postEmbedMessage({type: "tim-embed", event: "resize", height});
}

export function notifyEmbedAnswerSaved(taskId: string, points: number | null) {
    postEmbedMessage({
        type: "tim-embed",
        event: "answer-saved",
        taskId,
        points,
    });
}

/**
 * Starts reporting the content height to the host page.
 *
 * @param allowedOrigins Origins that may receive the messages (from the server config).
 */
export function initEmbedMode(allowedOrigins: string[]) {
    embedActive = true;
    embedTargetOrigin = resolveEmbedTargetOrigin(allowedOrigins);
    if (!embedTargetOrigin) {
        return;
    }
    window.addEventListener("load", () => postEmbedHeight(true));
    window.addEventListener("resize", () => postEmbedHeight());
    if ("ResizeObserver" in window) {
        // The answer browser, error output, images and MathJax all change the height after load.
        const observer = new ResizeObserver(() => postEmbedHeight());
        observer.observe(document.documentElement);
        observer.observe(document.body);
    }
    postEmbedHeight(true);
}
