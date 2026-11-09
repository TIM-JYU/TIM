/**
 * Edit tracking for csplugin editors (markup option `track`).
 *
 * Records how text is written (typing bursts, pastes, copies, drops, large inserts,
 * undo, leaving the page, runs, resets) and sends the events in small batches to
 * the `/trackEvents` route, which appends them to a log file. A per-session summary
 * is also kept and sent with each answer (see `summary()`).
 *
 * Pasted/copied text itself is never stored, only its length and a short hash.
 * Line endings are normalized to "\n" first, as editors do when inserting text.
 *
 * Event format: arrays `[type, dt, ...]`, dt = seconds since the previous event in this
 * session (0.1 s precision, computed from rounded elapsed time so that t0 + sum(dt) is exact).
 *   ["init", 0, len, hash]      text when tracking started
 *   ["k", dt, ins, del]         typing burst (chars inserted / deleted)
 *   ["p", dt, len, hash]        paste
 *   ["c", dt, len, hash]        copy inside the editor
 *   ["x", dt, len, hash]        cut inside the editor
 *   ["d", dt, len]              drop
 *   ["big", dt, len]            large insert without paste/drop event
 *   ["u", dt, ins, del]         undo (chars inserted / deleted by it)
 *   ["r", dt, ins, del]         redo (chars inserted / deleted by it)
 *   ["revert", dt, old, new]    unsaved changes reverted to the saved answer (text lengths)
 *   ["away", dt] / ["back", dt] window/tab left / returned
 *   ["run", dt, saved]          run (saved = 1 if the answer is saved)
 *   ["reset", dt]               reset code
 *   ["mode", dt, name]          editor switched ("normal" | "ace")
 */

import {getCookie} from "tim/util/utils";

type TrackEventT = (string | number)[];

export type EditTrackSummary = {
    t: number; // session start, epoch seconds
    n: number; // number of events so far (used for merging on the server)
    firstEdit?: number; // seconds from page load to first edit
    typed: number;
    deleted: number;
    paste: number;
    pasteChars: number;
    ownPaste: number; // pastes of text copied/cut in this editor during the session
    drop: number;
    big: number;
    undo: number; // undo, redo and revert operations
    away: number;
    awaySec: number;
    maxPause: number; // longest gap between edits while on the page, seconds
};

const BURST_IDLE_MS = 2000;
const BIG_INSERT = 20;
const FLUSH_INTERVAL_MS = 30000; // also flushed immediately on away / pagehide / run
const MAX_QUEUE = 2000;
const TRACK_URL = "/trackEvents";

/** Converts "\r\n" and "\r" line endings to "\n". */
export function normalizeNewlines(s: string): string {
    return s.replace(/\r\n?/g, "\n");
}

/** 32-bit FNV-1a hash as 8 hex chars. */
export function shortHash(s: string): string {
    let h = 0x811c9dc5;
    for (let i = 0; i < s.length; i++) {
        h ^= s.charCodeAt(i);
        h = Math.imul(h, 0x01000193);
    }
    return (h >>> 0).toString(16).padStart(8, "0");
}

function nowTenths() {
    return Math.round(Date.now() / 100);
}

export class EditTracker {
    readonly sid = Math.random().toString(36).slice(2, 8);
    private readonly startTenths = nowTenths();
    private lastElapsed = 0; // tenths, elapsed at previous event
    private queue: TrackEventT[] = [];
    private eventCount = 0;
    private flushTimer?: number;
    private sending = false;
    private seq = 0; // batch number, for ordering lines in the log

    // Current typing burst
    private burst?: {start: number; last: number; ins: number; del: number};

    // Paste/drop just recorded; the matching insert from the editor is skipped.
    private expectedInsert = 0;

    private copiedHashes = new Set<string>();
    private editorKind?: string;

    private isAway = false;
    private awayStart = 0; // ms
    private awaySinceLastEdit = 0; // ms
    private lastEditMs?: number;

    private sum: EditTrackSummary;

    private readonly onVisibility = () => {
        if (document.visibilityState === "hidden") {
            this.away();
        } else {
            this.back();
        }
    };
    private readonly onBlur = () => this.away();
    private readonly onFocus = () => this.back();
    private readonly onPageHide = () => this.flush(true);

    constructor(private readonly task: string, initialText: string) {
        this.sum = {
            t: this.startTenths / 10,
            n: 0,
            typed: 0,
            deleted: 0,
            paste: 0,
            pasteChars: 0,
            ownPaste: 0,
            drop: 0,
            big: 0,
            undo: 0,
            away: 0,
            awaySec: 0,
            maxPause: 0,
        };
        this.push(this.startTenths, [
            "init",
            initialText.length,
            shortHash(initialText),
        ]);
        document.addEventListener("visibilitychange", this.onVisibility);
        window.addEventListener("blur", this.onBlur);
        window.addEventListener("focus", this.onFocus);
        window.addEventListener("pagehide", this.onPageHide);
        this.flushTimer = window.setInterval(
            () => this.flush(false),
            FLUSH_INTERVAL_MS
        );
    }

    destroy() {
        document.removeEventListener("visibilitychange", this.onVisibility);
        window.removeEventListener("blur", this.onBlur);
        window.removeEventListener("focus", this.onFocus);
        window.removeEventListener("pagehide", this.onPageHide);
        if (this.flushTimer !== undefined) {
            window.clearInterval(this.flushTimer);
        }
        this.flush(true);
    }

    /** Summary of this session, keyed by session id, to be sent with an answer. */
    summary(): Record<string, EditTrackSummary> {
        return {[this.sid]: {...this.sum, n: this.eventCount}};
    }

    // ---- recording -------------------------------------------------------

    /** Adds an event that happened at time `atTenths` (epoch tenths). */
    private push(atTenths: number, rest: (string | number)[]) {
        const elapsed = Math.max(atTenths - this.startTenths, this.lastElapsed);
        const dt = (elapsed - this.lastElapsed) / 10;
        this.lastElapsed = elapsed;
        const [type, ...args] = rest;
        this.queue.push([type, dt, ...args]);
        if (this.queue.length > MAX_QUEUE) {
            this.queue.splice(0, this.queue.length - MAX_QUEUE);
        }
        this.eventCount++;
    }

    /** Closes the current typing burst (if any) and records other events after it. */
    private closeBurst() {
        if (!this.burst) {
            return;
        }
        const b = this.burst;
        this.burst = undefined;
        this.push(b.start, ["k", b.ins, b.del]);
    }

    private event(...rest: (string | number)[]) {
        this.closeBurst();
        this.push(nowTenths(), rest);
    }

    /** Book-keeping for summary: first edit and pauses between edits. */
    private noteEdit() {
        const now = Date.now();
        if (this.sum.firstEdit === undefined) {
            this.sum.firstEdit =
                Math.round(now / 100 - this.startTenths) / 10;
        }
        if (this.lastEditMs !== undefined) {
            const pause =
                (now - this.lastEditMs - this.awaySinceLastEdit) / 1000;
            this.sum.maxPause = Math.max(
                this.sum.maxPause,
                Math.round(pause * 10) / 10
            );
        }
        this.lastEditMs = now;
        this.awaySinceLastEdit = 0;
    }

    /** Called by an editor when it becomes active. Records editor switches. */
    attached(kind: "normal" | "ace") {
        if (this.editorKind !== undefined && this.editorKind !== kind) {
            this.event("mode", kind);
        }
        this.editorKind = kind;
    }

    /**
     * Text changed by the user (typing, deleting, autocomplete...).
     * Inserts that belong to a just-recorded paste/drop are skipped.
     */
    changed(ins: number, del: number) {
        if (ins > 0 && this.expectedInsert > 0) {
            if (ins === this.expectedInsert) {
                this.expectedInsert = 0;
                ins = 0;
            } else {
                this.expectedInsert = 0;
            }
        }
        if (ins === 0 && del === 0) {
            return;
        }
        this.noteEdit();
        if (ins >= BIG_INSERT) {
            this.sum.big++;
            this.event("big", ins);
            ins = 0;
            if (del === 0) {
                return;
            }
        }
        this.sum.typed += ins;
        this.sum.deleted += del;
        const now = nowTenths();
        if (this.burst && (now - this.burst.last) * 100 > BURST_IDLE_MS) {
            this.closeBurst();
        }
        if (!this.burst) {
            this.burst = {start: now, last: now, ins: 0, del: 0};
        }
        this.burst.ins += ins;
        this.burst.del += del;
        this.burst.last = now;
    }

    paste(text: string, expectInsert: boolean) {
        text = normalizeNewlines(text);
        const h = shortHash(text);
        this.noteEdit();
        this.sum.paste++;
        this.sum.pasteChars += text.length;
        if (this.copiedHashes.has(h)) {
            this.sum.ownPaste++;
        }
        this.event("p", text.length, h);
        this.expectedInsert = expectInsert ? text.length : 0;
    }

    copy(text: string) {
        if (!text) {
            return;
        }
        text = normalizeNewlines(text);
        const h = shortHash(text);
        this.copiedHashes.add(h);
        this.event("c", text.length, h);
    }

    cut(text: string) {
        if (!text) {
            return;
        }
        text = normalizeNewlines(text);
        const h = shortHash(text);
        this.copiedHashes.add(h);
        this.event("x", text.length, h);
    }

    drop(len: number, expectInsert: boolean) {
        this.noteEdit();
        this.sum.drop++;
        this.event("d", len);
        this.expectedInsert = expectInsert ? len : 0;
    }

    /** Undo in the editor; its text changes are not counted as typing. */
    undo(ins: number, del: number) {
        this.noteEdit();
        this.sum.undo++;
        this.event("u", ins, del);
    }

    /** Redo in the editor; its text changes are not counted as typing. */
    redo(ins: number, del: number) {
        this.noteEdit();
        this.sum.undo++;
        this.event("r", ins, del);
    }

    /** Unsaved changes were reverted to the saved answer (e.g. the plugin's Undo link). */
    revert(oldLen: number, newLen: number) {
        this.noteEdit();
        this.sum.undo++;
        this.event("revert", oldLen, newLen);
    }

    run(saved: boolean) {
        this.event("run", saved ? 1 : 0);
        this.flush(false);
    }

    reset() {
        this.event("reset");
    }

    private away() {
        if (this.isAway) {
            return;
        }
        this.isAway = true;
        this.awayStart = Date.now();
        this.sum.away++;
        this.event("away");
        this.flush(false);
    }

    private back() {
        if (!this.isAway) {
            return;
        }
        this.isAway = false;
        const ms = Date.now() - this.awayStart;
        this.awaySinceLastEdit += ms;
        this.sum.awaySec = Math.round(this.sum.awaySec * 10 + ms / 100) / 10;
        this.event("back");
    }

    // ---- sending ---------------------------------------------------------

    /**
     * Sends queued events. A burst still in progress is kept unless it has been idle
     * long enough or the page is going away (`final`).
     */
    flush(final: boolean) {
        if (
            this.burst &&
            (final || (nowTenths() - this.burst.last) * 100 > BURST_IDLE_MS)
        ) {
            this.closeBurst();
        }
        if (this.queue.length === 0 || (this.sending && !final)) {
            return;
        }
        const ev = this.queue;
        this.queue = [];
        const body = JSON.stringify({
            task: this.task,
            sid: this.sid,
            seq: this.seq++,
            t0: this.startTenths / 10,
            ev: ev,
        });
        this.sending = true;
        fetch(TRACK_URL, {
            method: "POST",
            keepalive: true,
            credentials: "same-origin",
            headers: {
                "Content-Type": "application/json",
                "X-XSRF-TOKEN": getCookie("XSRF-TOKEN") ?? "",
            },
            body: body,
        })
            .then((r) => {
                if (!r.ok) {
                    this.requeue(ev);
                }
            })
            .catch(() => this.requeue(ev))
            .finally(() => {
                this.sending = false;
            });
    }

    private requeue(ev: TrackEventT[]) {
        this.queue = ev.concat(this.queue);
        if (this.queue.length > MAX_QUEUE) {
            this.queue.splice(0, this.queue.length - MAX_QUEUE);
        }
    }
}
