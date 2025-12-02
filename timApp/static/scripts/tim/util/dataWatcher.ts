/**
 * @packageDocumentation
 * @module data-watcher
 *
 * Install a data watcher for a property
 * on a target object/element/component.
 * see {@link installDataWatcher}
 * Done by GitHub Copilot
 */
type Target = HTMLElement | {nativeElement?: HTMLElement} | object | null;

type NgWindow = {ng?: {getComponent?: (el: Element) => unknown}};

/**
 * Resolve target to a component-like object (Element -> component via window.ng.getComponent,
 * ElementRef-like with nativeElement, or raw object instance).
 */
function resolveComponentFromTarget(target: Target): object | null {
    if (!target) {
        return null;
    }

    // If target is an Element, try to get Angular component via window.ng.getComponent
    if (target instanceof Element) {
        const win = window as unknown as NgWindow;
        if (typeof win.ng?.getComponent === "function") {
            try {
                const comp = win.ng.getComponent(target);
                if (comp && typeof comp === "object") {
                    return comp;
                }
            } catch {
                // fallthrough to returning element itself as object
            }
        }
        return target as unknown as object;
    }

    // If target looks like ElementRef: use its nativeElement
    const maybeNative = (target as {nativeElement?: unknown})?.nativeElement;
    if (maybeNative instanceof Element) {
        const win = window as unknown as NgWindow;
        if (typeof win.ng?.getComponent === "function") {
            try {
                const comp = win.ng.getComponent(maybeNative);
                if (comp && typeof comp === "object") {
                    return comp;
                }
            } catch {
                // ignore
            }
        }
        return maybeNative as unknown as object;
    }

    // If already an object instance (component), return it
    if (typeof target === "object") {
        return target as object;
    }

    return null;
}

function getCallerInfo(skipFrames = 0): string {
    const err = new Error("getCallerInfo");
    if (!err.stack) {
        return "";
    }

    const ignorePatterns: RegExp[] = [
        /\.set \[/,
        /getCallerInfo/,
        /Generator/,
        /_next/,
        /polyfills?\.js/i,
        /vendor\.js/i,
    ];

    const lines = err.stack
        .split("\n")
        .map((l) => l.trim())
        .slice(skipFrames);

    let result = "";
    for (const line of lines) {
        if (ignorePatterns.some((rx) => rx.test(line))) {
            continue;
        }
        result += line + "\n";
    }
    return result;
}

/**
 * Install a watcher for a property (usually an array)
 * on a target object/element/component.
 * To use add include:
 *   import {installDataWatcher} from "tim/util/dataWatcher";
 * and then call the function to install data wathers as needed.
 * Put breakpoint to this function or check console logs to see
 * when and where the watched property is mutated or reassigned.
 *
 * The watcher proxies common array mutation methods and detects reassignment of the watched
 * property so callers can be notified or perform side effects (logging, triggering change
 * detection, etc.). Intended for debugging or lightweight reactive hooks in UI code.
 *
 * @param target - Owner of the property. May be:
 *   - an Angular component instance (e.g. `this`)
 *   - an `ElementRef`-like object (has `nativeElement`)
 *   - a raw DOM `Element` / `HTMLElement`
 * @param propPath - Dot\-separated path to the property on `target` (examples: `srows`, `selectedCells.srows`).
 *   Nested paths are supported; the final property should typically be an array to observe mutations.
 * @param label - Optional short identifier used for logging / debug messages. Defaults to `"data-watcher"`.
 *
 * @remarks
 * - This function returns `void`. It does not provide an automatic teardown callback; if removal
 *   of the watcher is required, perform cleanup in your component (for example in `ngOnDestroy`)
 *   or enhance the watcher to return an unsubscribe/teardown function.
 * - When passed an Angular component instance, the watcher may be able to call change detection
 *   helpers if they are available on the component.
 *
 * @example
 * // Watch srows array on the selectedCells object
 * installDataWatcher(this.selectedCells, "srows", "tim-table-selected-srows");
 *
 * @example
 * // Watch nested property from the component instance
 * installDataWatcher(this, "selectedCells.srows");
 *
 * @returns void
 */
// TypeScript
export function installDataWatcher(
    target: Target,
    propPath: string,
    label: string = "data-watcher"
): void {
    if (!target || !propPath) {
        return;
    }

    const root = resolveComponentFromTarget(target);
    if (!root) {
        return;
    }

    const parts = propPath.split(".");
    if (parts.length === 0) {
        return;
    }

    // Walk to parent object of the final property
    let parent: unknown = root;
    for (let i = 0; i < parts.length - 1; i++) {
        if (parent == null || typeof parent !== "object") {
            return;
        }
        parent = (parent as Record<string, unknown>)[parts[i]];
    }
    if (parent == null || typeof parent !== "object") {
        return;
    }

    const key = parts[parts.length - 1];
    const parentRec = parent as Record<string, unknown>;

    // Helper to wrap arrays with a proxy that logs mutations
    const makeArrayProxy = (arr: unknown[], lbl: string) =>
        new Proxy(arr, {
            get(targetArr, prop, receiver) {
                const orig = Reflect.get(targetArr, prop, receiver);
                // Wrap mutating methods so they log
                if (typeof prop === "string" && typeof orig === "function") {
                    const mutators = new Set([
                        "push",
                        "pop",
                        "shift",
                        "unshift",
                        "splice",
                        "sort",
                        "reverse",
                        "fill",
                        "copyWithin",
                    ]);
                    if (mutators.has(prop)) {
                        return function (...args: unknown[]) {
                            const res = (
                                orig as (...a: unknown[]) => unknown
                            ).apply(targetArr, args);
                            console.info(
                                `[${lbl}] array method \`${prop}\` called`,
                                args
                            );
                            return res;
                        };
                    }
                }
                return orig;
            },
            set(targetArr, prop, value, receiver) {
                const res = Reflect.set(targetArr, prop, value, receiver);
                // Log index assignments and length changes
                if (typeof prop === "string") {
                    console.info(`[${lbl}] array set [${prop}] =`, value);
                }
                return res;
            },
        });

    // Helper: find property descriptor up the prototype chain
    const findDescriptor = (
        obj: object,
        propName: string
    ): {owner: object; desc: PropertyDescriptor} | null => {
        let cur: object | null = obj;
        while (cur) {
            const d = Object.getOwnPropertyDescriptor(cur, propName);
            if (d) {
                return {owner: cur, desc: d};
            }
            cur = Object.getPrototypeOf(cur);
        }
        return null;
    };

    // If there's an existing descriptor (possibly on prototype), wrap it
    const existing = findDescriptor(parentRec, key);

    // We'll keep an internal proxied value to return from the new accessor
    let internal: unknown = parentRec[key];

    // If current value is an array, wrap it immediately
    if (Array.isArray(internal)) {
        internal = makeArrayProxy(internal as unknown[], label);
        try {
            // replace with proxied array on the instance to reflect immediate changes
            parentRec[key] = internal;
            console.info(
                `[${label}] proxied initial array for \`${propPath}\``
            );
        } catch {
            // ignore if assignment fails
        }
    }

    try {
        if (existing && (existing.desc.get || existing.desc.set)) {
            // There is an existing accessor (likely the TS getter/setter on prototype).
            const origGet = existing.desc.get
                ? existing.desc.get.bind(parentRec)
                : undefined;
            const origSet = existing.desc.set
                ? existing.desc.set.bind(parentRec)
                : undefined;

            // Initialize internal from original getter if present
            try {
                const valFromOrig = origGet ? origGet() : parentRec[key];
                if (Array.isArray(valFromOrig)) {
                    internal = makeArrayProxy(valFromOrig as unknown[], label);
                } else {
                    internal = valFromOrig;
                }
            } catch {
                // swallow
            }

            Object.defineProperty(parentRec, key, {
                configurable: true,
                enumerable: true,
                get() {
                    try {
                        if (origGet) {
                            const val = origGet();
                            if (Array.isArray(val)) {
                                // proxify current stored array and keep internal in sync
                                internal = makeArrayProxy(
                                    val as unknown[],
                                    label
                                );
                                return internal;
                            }
                            internal = val;
                            return internal;
                        }
                    } catch {
                        // fallthrough to returning closure internal
                    }
                    return internal;
                },
                set(v: unknown) {
                    const caller = getCallerInfo();
                    const wasArray = Array.isArray(internal);
                    try {
                        // Forward raw value to original setter if present
                        if (origSet) {
                            origSet(v);
                            // read back actual stored value via original getter (if any)
                            const stored = origGet ? origGet() : v;
                            if (Array.isArray(stored)) {
                                internal = makeArrayProxy(
                                    stored as unknown[],
                                    label
                                );
                                console.info(
                                    `[${label}] property \`${propPath}\` reassigned to array (proxied) [wrapped orig accessor]`,
                                    caller
                                );
                            } else {
                                internal = stored;
                                console.info(
                                    `[${label}] property \`${propPath}\` reassigned [forwarded to orig setter]`,
                                    v,
                                    caller
                                );
                            }
                        } else {
                            // No original setter: keep value in our internal closure
                            if (Array.isArray(v)) {
                                internal = makeArrayProxy(
                                    v as unknown[],
                                    label
                                );
                                console.info(
                                    `[${label}] property \`${propPath}\` reassigned to array (proxied)`,
                                    caller
                                );
                            } else {
                                internal = v;
                                console.info(
                                    `[${label}] property \`${propPath}\` reassigned`,
                                    v,
                                    caller
                                );
                            }
                        }
                    } catch (e) {
                        console.warn(
                            `[${label}] error while forwarding assignment for \`${propPath}\``,
                            e,
                            caller
                        );
                    }

                    if (wasArray && !Array.isArray(internal)) {
                        console.info(
                            `[${label}] property \`${propPath}\` changed from array to non-array`,
                            caller
                        );
                    }
                },
            });
        } else {
            // No existing accessor: install a new one as before.
            // Current value already captured in `internal` and proxied if array.
            Object.defineProperty(parentRec, key, {
                configurable: true,
                enumerable: true,
                get() {
                    return internal;
                },
                set(v: unknown) {
                    const caller = getCallerInfo();
                    const wasArray = Array.isArray(internal);
                    internal = v;
                    if (Array.isArray(v)) {
                        internal = makeArrayProxy(v as unknown[], label);
                        console.info(
                            `[${label}] property \`${propPath}\` reassigned to array (proxied)`,
                            caller
                        );
                    } else {
                        console.info(
                            `[${label}] property \`${propPath}\` reassigned`,
                            v,
                            caller
                        );
                    }
                    if (wasArray && !Array.isArray(v)) {
                        console.info(
                            `[${label}] property \`${propPath}\` changed from array to non-array`,
                            caller
                        );
                    }
                },
            });

            if (Array.isArray(parentRec[key])) {
                console.info(
                    `[${label}] proxied initial array for \`${propPath}\``
                );
            }
        }
    } catch (e) {
        // Some objects (e.g. from frameworks) may not allow redefining props
        console.warn(
            `[${label}] could not install watcher on \`${propPath}\``,
            e
        );
    }

    console.info(`[${label}] watcher installed for \`${propPath}\``);
}

/**
 * Class decorator that logs entry, exit, and errors of all methods in the class.
 * Useful for debugging and tracing method calls.
 * To use, simply add `@LogAllMethods` above your class definition
 * before @Component or other decorators.
 *
 * @param target - The class constructor to decorate.
 * @returns void
 */
// eslint-disable-next-line @typescript-eslint/ban-types
export function LogAllMethods(target: Function): void {
    const className = target.name;

    const wrapFn = (
        kind: "method" | "getter" | "setter",
        propName: string,
        orig: (...args: unknown[]) => unknown
    ) => {
        const label = `${className}.${propName} (${kind})`;
        return function (this: unknown, ...args: unknown[]) {
            // eslint-disable-next-line no-console
            console.log(`[LOG] Enter ${label}`, {args});
            try {
                const result = orig.apply(this as never, args);
                if (
                    result &&
                    typeof (result as {then?: unknown}).then === "function"
                ) {
                    return (result as Promise<unknown>)
                        .then((res) => {
                            // eslint-disable-next-line no-console
                            console.log(`[LOG] Exit  ${label} (async)`, {
                                result: res,
                            });
                            return res;
                        })
                        .catch((err) => {
                            // eslint-disable-next-line no-console
                            console.error(
                                `[LOG] Error in ${label} (async)`,
                                err
                            );
                            throw err;
                        });
                }
                // eslint-disable-next-line no-console
                console.log(`[LOG] Exit  ${label}`, {result});
                return result;
            } catch (err) {
                // eslint-disable-next-line no-console
                console.error(`[LOG] Error in ${label}`, err);
                throw err;
            }
        };
    };

    // 1\) Kääri prototyypin metodit + getterit/setterit (nykyinen logiikka)
    let proto: object | null = target.prototype;

    while (proto && proto !== Object.prototype) {
        for (const key of Object.getOwnPropertyNames(proto)) {
            if (key === "constructor") {
                continue;
            }

            const desc = Object.getOwnPropertyDescriptor(proto, key);
            if (!desc) {
                continue;
            }

            const newDesc: PropertyDescriptor = {...desc};

            if (typeof desc.value === "function") {
                newDesc.value = wrapFn("method", key, desc.value);
            }
            if (typeof desc.get === "function") {
                newDesc.get = wrapFn("getter", key, desc.get);
            }
            if (typeof desc.set === "function") {
                newDesc.set = wrapFn("setter", key, desc.set);
            }

            if (
                newDesc.value !== desc.value ||
                newDesc.get !== desc.get ||
                newDesc.set !== desc.set
            ) {
                Object.defineProperty(proto, key, newDesc);
            }
        }

        proto = Object.getPrototypeOf(proto);
    }

    // 2\) Kääri myös instanssin omat funktiokentät (esim. doCopy = () => {})
    const OriginalCtor = target as new (...args: unknown[]) => unknown;

    const wrapInstanceMethods = (instance: unknown): void => {
        if (!instance || typeof instance !== "object") {
            return;
        }
        for (const key of Object.getOwnPropertyNames(instance)) {
            if (key === "constructor") {
                continue;
            }
            const desc = Object.getOwnPropertyDescriptor(instance, key);
            if (!desc || typeof desc.value !== "function") {
                continue;
            }
            const origFn = desc.value as (...a: unknown[]) => unknown;
            const wrapped = wrapFn("method", key, origFn);
            Object.defineProperty(instance, key, {
                ...desc,
                value: wrapped,
            });
        }
    };

    const NewCtor: new (...args: unknown[]) => unknown = function (
        this: unknown,
        ...args: unknown[]
    ) {
        const instance = new OriginalCtor(...args);
        wrapInstanceMethods(instance);
        return instance;
    } as unknown as new (...args: unknown[]) => unknown;

    NewCtor.prototype = OriginalCtor.prototype;
    Object.setPrototypeOf(NewCtor, OriginalCtor);

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return NewCtor as any;
}
