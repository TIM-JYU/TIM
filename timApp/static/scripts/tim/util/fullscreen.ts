export interface INonStandardFullScreenProperties {
    webkitFullscreenElement?: Element;
    msFullscreenElement?: Element;
    msExitFullscreen?: () => void;
    webkitExitFullscreen?: () => void;
}

export interface INonStandardFullScreenElement {
    webkitRequestFullScreen?: () => void;
    msRequestFullscreen?: () => void;
}

export function getFullscreenElement(): Element | undefined {
    const doc = document as INonStandardFullScreenProperties & Document;
    return (
        doc.fullscreenElement ??
        doc.webkitFullscreenElement ??
        doc.msFullscreenElement
    );
}

// noinspection JSUnusedGlobalSymbols, used in template
/**
 * @returns {boolean} true if device supports fullscreen, otherwise false
 */
export function fullscreenSupported(e: HTMLElement) {
    const ele = e as INonStandardFullScreenElement & Element;
    return (
        ele.requestFullscreen != null ||
        ele.webkitRequestFullScreen != null ||
        ele.msRequestFullscreen != null
    );
}

/**
 * Makes an element fullscreen
 * @returns {boolean} true if element was toggled to fullscreen
 */
export function toggleFullScreen(e: HTMLElement): boolean {
    let wentFullscreen = true;
    if (!getFullscreenElement()) {
        const ele = e as INonStandardFullScreenElement & Element;
        if (ele.requestFullscreen) {
            ele.requestFullscreen();
        } else if (ele.webkitRequestFullScreen) {
            ele.webkitRequestFullScreen();
        } else if (ele.msRequestFullscreen) {
            ele.msRequestFullscreen();
        } else {
            wentFullscreen = false;
        }
    } else {
        exitFullScreen();
        wentFullscreen = false;
    }
    return wentFullscreen;
}

export function exitFullScreen() {
    const doc = document as INonStandardFullScreenProperties & Document;
    if (doc.exitFullscreen) {
        doc.exitFullscreen();
    } else if (doc.msExitFullscreen) {
        doc.msExitFullscreen();
    } else if (doc.webkitExitFullscreen) {
        doc.webkitExitFullscreen();
    }
}
