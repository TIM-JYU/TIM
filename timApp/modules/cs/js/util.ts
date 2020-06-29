
export function countChars(s: string, c: string) {
    let n = 0;
    for (let i = 0; i < s.length; n += +(c === s[i++])) {
    }
    return n;
}

export function countLines(s: string): number {
    return countChars(s, '\n') + (s.length > 0 ? 1 : 0);
}

export function countWords(str: string): number {
    return str.trim().split(/\s+/).length;
}

export function getInt(s: string | number) {
    if (typeof s === "number") {
        return s;
    }
    const n = parseInt(s, 10);
    if (isNaN(n)) {
        return undefined;
    }
    return n;
}
