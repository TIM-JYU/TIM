export interface RGBA {
    r: number;
    g: number;
    b: number;
    a: number;
}

/**
 * Background color of the current page.
 */
export const DOCUMENT_BG = parseRGBAColor(
    getComputedStyle(document.body).backgroundColor
)!;

/**
 * Mix colors together.
 * @param color The first color.
 * @param bg Background color.
 */
export function applyOpacity(color: RGBA, bg: RGBA): RGBA {
    return {
        r: color.r * color.a + bg.r * (1 - color.a),
        g: color.g * color.a + bg.g * (1 - color.a),
        b: color.b * color.a + bg.b * (1 - color.a),
        a: color.a + bg.a * (1 - color.a),
    };
}

/**
 * Compute luminance of a color.
 * Luminance is the perceived brightness of a color.
 *
 * Based on https://www.w3.org/TR/AERT/#color-contrast
 * @param rgba Color to compute luminance for.
 * @returns Luminance of the color (0 = dark, 1 = bright).
 */
export function luma(rgba: RGBA): number {
    return 0.299 * rgba.r + 0.587 * rgba.g + 0.114 * rgba.b;
}

/**
 * Converts a hex color string to an RGBA object.
 * @param color Hex color string.
 * @returns RGBA object if parse is successful. Otherwise, undefined.
 */
export function parseHexColor(color: string): RGBA | undefined {
    const m = color.match(
        /^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})?$/i
    );
    if (!m) {
        return undefined;
    }
    const r = parseInt(m[1], 16) / 255;
    const g = parseInt(m[2], 16) / 255;
    const b = parseInt(m[3], 16) / 255;
    const a = m[4] ? parseInt(m[4], 16) / 255 : 1;
    return {r, g, b, a};
}

/**
 * Converts a rgb or rgba color string to an RGBA object.
 * @param color RGB or RGBA color string.
 * @returns RGBA object if parse is successful. Otherwise, undefined.
 */
export function parseRGBAColor(color: string): RGBA | undefined {
    // Match rgb or rgba
    const m = color.match(
        /^rgba?\((\d+),\s*(\d+),\s*(\d+)(?:,\s*(\d+(?:\.\d+)?))?\)$/
    );
    if (!m) {
        return undefined;
    }
    const r = parseInt(m[1], 10) / 255;
    const g = parseInt(m[2], 10) / 255;
    const b = parseInt(m[3], 10) / 255;
    const a = m[4] ? parseFloat(m[4]) : 1;
    return {r, g, b, a};
}
