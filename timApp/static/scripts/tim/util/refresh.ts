import type {ApplicationRef, NgZone} from "@angular/core";

// NOTE: This module is imported by a lot of code, so it must not import other TIM modules.

let appRef: ApplicationRef | undefined;
let ngZone: NgZone | undefined;
let scheduled = false;
const listeners: Array<() => void> = [];
const extraTargets: Array<{appRef: ApplicationRef; zone: NgZone}> = [];

/**
 * Adds another Angular application that refreshUI updates. The modules that are loaded only on some pages
 * run as their own applications (see registerModuleServerComponents).
 */
export function addRefreshTarget(a: ApplicationRef, z: NgZone) {
    extraTargets.push({appRef: a, zone: z});
}

/**
 * Registers a function that is called on every refresh, before Angular updates the views.
 */
export function onRefreshUI(f: () => void) {
    listeners.push(f);
}

/**
 * Called by AppModule when it is bootstrapped. Not meant to be called from elsewhere.
 */
export function setApplicationRef(a: ApplicationRef, z: NgZone) {
    appRef = a;
    ngZone = z;
}

/**
 * Runs the function inside the Angular zone if the application has been started.
 */
export function runInAngularZone<T>(f: () => T): T {
    return ngZone ? ngZone.run(f) : f();
}

/**
 * Asks Angular to update the views.
 *
 * The document view (ViewCtrl and its helper classes) is plain TypeScript that runs outside the Angular zone,
 * so Angular does not notice when that code changes something that components show.
 * Call this after such a change. Several calls in a row cause only one update.
 */
export function refreshUI() {
    if (scheduled) {
        return;
    }
    scheduled = true;
    const run = () => {
        scheduled = false;
        for (const l of listeners) {
            l();
        }
        if (!appRef || !ngZone) {
            return;
        }
        ngZone.run(() => appRef!.tick());
        for (const t of extraTargets) {
            t.zone.run(() => t.appRef.tick());
        }
    };
    if (ngZone) {
        // The timer must not be patched by the zone, because the zone would then run change detection twice.
        ngZone.runOutsideAngular(() => window.setTimeout(run));
    } else {
        window.setTimeout(run);
    }
}

/**
 * A promise returned by {@link uiTimeout}. It can be cancelled with uiTimeout.cancel.
 */
export interface ITimeoutPromise<T> extends Promise<T> {
    timerId?: number;
}

/**
 * Waits for the given time, optionally runs a function, and then refreshes the views (see {@link refreshUI}).
 *
 * @param fnOrDelay The function to run after the delay, or the delay in milliseconds.
 * @param delay The delay in milliseconds if the first parameter is a function.
 * @returns A promise that resolves to the return value of the function.
 */
export function uiTimeout<T = void>(
    fnOrDelay?: (() => T) | number,
    delay?: number
): ITimeoutPromise<T> {
    const fn = typeof fnOrDelay === "function" ? fnOrDelay : undefined;
    const ms = typeof fnOrDelay === "number" ? fnOrDelay : delay;
    let timerId: number | undefined;
    const p: ITimeoutPromise<T> = new Promise<T>((resolve, reject) => {
        timerId = window.setTimeout(() => {
            try {
                resolve(fn ? fn() : (undefined as unknown as T));
            } catch (e) {
                reject(e);
            } finally {
                refreshUI();
            }
        }, ms);
    });
    p.timerId = timerId;
    return p;
}

/**
 * Cancels a timeout that has not fired yet. The promise of a cancelled timeout never settles.
 */
uiTimeout.cancel = (p: ITimeoutPromise<unknown> | null | undefined) => {
    if (p?.timerId != null) {
        window.clearTimeout(p.timerId);
    }
};
