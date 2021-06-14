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
 */
export function goFullScreen(e: HTMLElement) {
    const doc = document as INonStandardFullScreenProperties & Document;
    if (!getFullscreenElement()) {
        let wentFullscreen = true;
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

        if (wentFullscreen) {
            ele.setAttribute(
                "style",
                "width: 100%; height: 100%; position: absolute; top: 0px;" +
                    "padding: 2em 5px 5px 5px; background: rgb(224, 224, 224); -webkit-box-sizing: border-box;" +
                    "-moz-box-sizing: border-box; box-sizing: border-box;"
            );
        }
    } else {
        if (doc.exitFullscreen) {
            doc.exitFullscreen();
        } else if (doc.msExitFullscreen) {
            doc.msExitFullscreen();
        } else if (doc.webkitExitFullscreen) {
            doc.webkitExitFullscreen();
        }
    }
}
