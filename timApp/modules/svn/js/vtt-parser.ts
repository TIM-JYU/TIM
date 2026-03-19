export interface VttCue {
    startTime: string;
    endTime: string;
    startSeconds: number;
    endSeconds: number;
    text: string;
    highlightedText: string;
}

export function parseVttTimestamp(ts: string): number {
    const parts = ts.trim().split(":");
    if (parts.length === 3) {
        return (
            parseFloat(parts[0]) * 3600 +
            parseFloat(parts[1]) * 60 +
            parseFloat(parts[2])
        );
    } else if (parts.length === 2) {
        return parseFloat(parts[0]) * 60 + parseFloat(parts[1]);
    }
    return 0;
}

export function formatVttTimestamp(ts: string): string {
    const seconds = parseVttTimestamp(ts);
    const h = Math.floor(seconds / 3600);
    const m = Math.floor((seconds % 3600) / 60);
    const s = Math.floor(seconds % 60);
    if (h > 0) {
        return `${h}:${m.toString().padStart(2, "0")}:${s.toString().padStart(2, "0")}`;
    }
    return `${m}:${s.toString().padStart(2, "0")}`;
}

export function parseVtt(content: string): VttCue[] {
    const cues: VttCue[] = [];
    const blocks = content.replace(/\r\n/g, "\n").split(/\n\n+/);
    for (const block of blocks) {
        const lines = block.trim().split("\n");
        let tsLineIndex = -1;
        for (let i = 0; i < lines.length; i++) {
            if (lines[i].includes("-->")) {
                tsLineIndex = i;
                break;
            }
        }
        if (tsLineIndex < 0) {
            continue;
        }
        const tsParts = lines[tsLineIndex].split("-->");
        if (tsParts.length < 2) {
            continue;
        }
        const startTime = tsParts[0].trim();
        const endTime = tsParts[1].trim().split(/\s/)[0];
        const text = lines
            .slice(tsLineIndex + 1)
            .join(" ")
            .trim();
        if (!text) {
            continue;
        }
        cues.push({
            startTime: formatVttTimestamp(startTime),
            endTime: formatVttTimestamp(endTime),
            startSeconds: parseVttTimestamp(startTime),
            endSeconds: parseVttTimestamp(endTime),
            text,
            highlightedText: text,
        });
    }
    return cues;
}
