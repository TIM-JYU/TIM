export function countChars(s: string, c: string) {
    let n = 0;
    for (let i = 0; i < s.length; n += +(c === s[i++])) {}
    return n;
}

export function countLines(s: string): number {
    return countChars(s, "\n") + (s.length > 0 ? 1 : 0);
}

export function countWords(str: string): number {
    const s = str.trim().split(/\s+/);
    if (s.length === 1 && s[0].length === 0) {
        return 0;
    }
    // ignore punctuation characters that appear on their own, as the grammar/syntax in some languages (notably, French)
    // dictates whitespace before some punctuation, like question/exclamation marks etc.
    const ans = s.filter((e) => e.match(/\p{P}?\w/u));

    // return s.length;
    return ans.length;
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

export function sizeString(inBytes: number): string {
    if (inBytes < 1000) {
        return inBytes + " B";
    } else if (inBytes < 1000000) {
        return Math.round(inBytes / 1000) + " kB";
    } else if (inBytes < 1000000000) {
        return Math.round(inBytes / 1000000) + " MB";
    } else {
        return Math.round(inBytes / 1000000000) + " GB";
    }
}

export function timeString(): string {
    const date = new Date();
    return `${date.getHours().toString().padStart(2, "0")}:${date
        .getMinutes()
        .toString()
        .padStart(2, "0")}`;
}
