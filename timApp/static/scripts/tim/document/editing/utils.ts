/**
 * Wrap the given text to lines of max n characters splitting from space.
 */
export function wrapText(s: string, n: number) {
    if (n <= 0) {
        return {modified: false, s: s};
    }
    const lines = s.split("\n");
    let needJoin = false;
    for (let i = 0; i < lines.length; i++) {
        let line = lines[i];
        // lines[i] = "";
        let sep = "";
        if (line.length > n) {
            lines[i] = "";
            while (true) {
                let p = -1;
                if (line.length > n) {
                    p = line.lastIndexOf(" ", n);
                    if (p < 0) {
                        p = line.indexOf(" ");
                    } // long line
                }
                if (p < 0) {
                    lines[i] += sep + line;
                    break;
                }
                lines[i] += sep + line.substring(0, p);
                line = line.substring(p + 1);
                if (
                    i + 1 < lines.length &&
                    lines[i + 1].length > 0 &&
                    !" 0123456789-".includes(lines[i + 1][0])
                ) {
                    lines[i + 1] = line + " " + lines[i + 1];
                    needJoin = true;
                    break;
                }
                sep = "\n";
                needJoin = true;
            }
        }
    }
    if (needJoin) {
        return {modified: true, s: lines.join("\n")};
    }
    return {modified: false, s: s};
}
