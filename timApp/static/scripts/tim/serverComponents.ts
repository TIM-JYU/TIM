import type {
    ComponentRef,
    EnvironmentInjector,
    NgModuleRef,
    Type,
} from "@angular/core";
import {
    ApplicationRef,
    createComponent,
    NgZone,
    reflectComponentType,
} from "@angular/core";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {addRefreshTarget} from "tim/util/refresh";

/**
 * Server components: Angular components that are created on elements of HTML that comes from the server
 * (the page templates and the document paragraphs).
 *
 * A component is registered with {@link registerServerComponent}. {@link createServerComponents} then
 * finds the elements with the registered tag names and creates the components on them.
 * It is called for the whole page when the application starts and for every piece of HTML that is
 * added to the page later (see ParagraphCompiler).
 *
 * The inputs of the component are read from the attributes of the element:
 *
 *  * `my-input="text"` gives the string "text".
 *  * `[my-input]="value"` and `bind-my-input="value"` give the value parsed as JSON; single-quoted strings are
 *    also accepted. If the value is not a literal, it is given as a string. There are no expressions.
 *
 * The child nodes of the element are projected to the `<ng-content>` slots of the component.
 *
 * Elements inside an element that has the attribute `ng-non-bindable` are skipped. The server puts that
 * attribute to the content of document paragraphs, unless the paragraph has the attribute `allowangular`.
 *
 * Compared to custom elements (see customElements.ts), a server component can also be used
 * in the templates of other Angular components without being created twice.
 */

interface IRegistration {
    component: Type<unknown>;
    // The module that declares the component, if it is not AppModule.
    moduleType?: Type<unknown>;
}

const registry = new Map<string, IRegistration>();
const created = new Map<
    Element,
    {ref: ComponentRef<unknown>; appRef: ApplicationRef}
>();
const pending = new WeakSet<Element>();
const moduleRefs = new Map<Type<unknown>, Promise<NgModuleRef<unknown>>>();
let appRef: ApplicationRef | undefined;
let rootInjector: EnvironmentInjector | undefined;
let observer: MutationObserver | undefined;
let cleanupScheduled = false;

/**
 * Registers a component to be created on server-rendered elements.
 *
 * The component must be declared in AppModule or in a module that AppModule imports.
 * For other modules, use {@link registerModuleServerComponents}.
 *
 * @param selector The tag name of the element.
 * @param component The Angular component.
 */
export function registerServerComponent(
    selector: string,
    component: Type<unknown>
) {
    registry.set(selector.toLowerCase(), {component});
}

/**
 * Registers components of a module that is not part of AppModule, i.e. a module that is loaded only
 * on the pages that need it. The module is started as its own Angular application when the first of
 * its components is needed, so it must have an (empty) ngDoBootstrap method.
 *
 * @param moduleType The module that declares the components.
 * @param components The components by the tag names of their elements.
 */
export function registerModuleServerComponents(
    moduleType: Type<unknown>,
    components: Record<string, Type<unknown>>
) {
    for (const [selector, component] of Object.entries(components)) {
        registry.set(selector.toLowerCase(), {component, moduleType});
    }
    // The module may have been loaded after the page was processed.
    if (appRef) {
        const zone = appRef.injector.get(NgZone);
        zone.run(() => createServerComponents(document.body));
    }
}

function getModuleRef(moduleType: Type<unknown>) {
    let ref = moduleRefs.get(moduleType);
    if (!ref) {
        // Each module is its own Angular application with its own zone, like the main application.
        const bootstrap = () =>
            platformBrowserDynamic()
                .bootstrapModule(moduleType)
                .then((moduleRef) => {
                    addRefreshTarget(
                        moduleRef.injector.get(ApplicationRef),
                        moduleRef.injector.get(NgZone)
                    );
                    return moduleRef;
                });
        // The new zone must not be created inside the zone of the main application.
        // Otherwise its events would run inside that zone, which Angular does not allow.
        const mainZone = appRef?.injector.get(NgZone);
        ref = mainZone ? mainZone.runOutsideAngular(bootstrap) : bootstrap();
        moduleRefs.set(moduleType, ref);
    }
    return ref;
}

/**
 * Called by AppModule when it is bootstrapped. Not meant to be called from elsewhere.
 */
export function initServerComponents(
    a: ApplicationRef,
    injector: EnvironmentInjector
) {
    appRef = a;
    rootInjector = injector;
}

function kebabToCamel(s: string) {
    return s.replace(/-([a-z0-9])/g, (_, c: string) => c.toUpperCase());
}

function parseBoundValue(value: string): unknown {
    const s = value.trim();
    if (s === "") {
        return undefined;
    }
    try {
        return JSON.parse(s);
    } catch {
        // not JSON
    }
    if (s.length >= 2 && s.startsWith("'") && s.endsWith("'")) {
        return s.slice(1, -1);
    }
    try {
        // Lists and objects that use single quotes, e.g. ['view', 'edit'].
        return JSON.parse(s.replace(/'/g, '"'));
    } catch {
        return value;
    }
}

function getInputs(el: Element, component: Type<unknown>) {
    const mirror = reflectComponentType(component);
    const result: Array<[string, unknown]> = [];
    if (!mirror) {
        return result;
    }
    const byLowerName = new Map<string, string>();
    for (const i of mirror.inputs) {
        byLowerName.set(i.templateName.toLowerCase(), i.templateName);
    }
    for (const attr of Array.from(el.attributes)) {
        let name = attr.name;
        let bound = false;
        if (name.startsWith("[") && name.endsWith("]")) {
            name = name.slice(1, -1);
            bound = true;
        } else if (name.startsWith("bind-")) {
            name = name.slice("bind-".length);
            bound = true;
        }
        const input = byLowerName.get(kebabToCamel(name).toLowerCase());
        if (!input) {
            continue;
        }
        result.push([input, bound ? parseBoundValue(attr.value) : attr.value]);
    }
    return result;
}

function getProjectableNodes(el: Element, component: Type<unknown>) {
    const selectors = reflectComponentType(component)?.ngContentSelectors ?? [];
    if (selectors.length === 0) {
        return undefined;
    }
    const nodes = Array.from(el.childNodes);
    const slots: Node[][] = selectors.map(() => []);
    const wildcard = selectors.indexOf("*");
    for (const node of nodes) {
        let slot = -1;
        if (node instanceof Element) {
            slot = selectors.findIndex((s) => s !== "*" && node.matches(s));
        }
        if (slot < 0) {
            slot = wildcard;
        }
        if (slot >= 0) {
            slots[slot].push(node);
        }
    }
    return slots;
}

function scheduleCleanup() {
    if (cleanupScheduled) {
        return;
    }
    cleanupScheduled = true;
    window.setTimeout(() => {
        cleanupScheduled = false;
        for (const [el, c] of created) {
            if (!el.isConnected) {
                created.delete(el);
                c.appRef.detachView(c.ref.hostView);
                c.ref.destroy();
            }
        }
    }, 1000);
}

function ensureObserver() {
    if (observer) {
        return;
    }
    // Destroys the components whose elements have been removed from the page.
    observer = new MutationObserver((records) => {
        if (
            created.size > 0 &&
            records.some((r) => r.removedNodes.length > 0)
        ) {
            scheduleCleanup();
        }
    });
    observer.observe(document.body, {childList: true, subtree: true});
}

function createOnElement(
    el: Element,
    component: Type<unknown>,
    injector: EnvironmentInjector,
    app: ApplicationRef
) {
    try {
        const inputs = getInputs(el, component);
        const ref = createComponent(component, {
            environmentInjector: injector,
            hostElement: el,
            projectableNodes: getProjectableNodes(el, component),
        });
        for (const [name, value] of inputs) {
            ref.setInput(name, value);
        }
        created.set(el, {ref, appRef: app});
        app.attachView(ref.hostView);
        ref.changeDetectorRef.detectChanges();
    } catch (e) {
        console.error(
            `Failed to create the component for <${el.tagName.toLowerCase()}>`,
            e
        );
    }
}

/**
 * Creates the registered components on the matching elements inside the given element (the element itself included).
 * Elements that already have a component are skipped, so this can be called several times for the same HTML.
 *
 * Must only be called for HTML that has come from the server, never for elements that Angular has rendered.
 *
 * @param root The element to process. It does not have to be attached to the document.
 */
export function createServerComponents(root: Element) {
    if (!appRef || !rootInjector || registry.size === 0) {
        return;
    }
    const selector = Array.from(registry.keys()).join(",");
    const elements: Element[] = [];
    if (root.matches(selector)) {
        elements.push(root);
    }
    elements.push(...Array.from(root.querySelectorAll(selector)));
    // Elements inside components that existed before this call have been rendered by Angular
    // (or were already processed), so they must be skipped.
    const hostsBefore = new Set<Element>(
        Array.from(root.querySelectorAll("[ng-version]"))
    );
    const isInsideOldHost = (el: Element) => {
        for (
            let p = el.parentElement;
            p && p !== root.parentElement;
            p = p.parentElement
        ) {
            if (hostsBefore.has(p)) {
                return true;
            }
        }
        return false;
    };
    for (const el of elements) {
        if (
            created.has(el) ||
            pending.has(el) ||
            el.hasAttribute("ng-version") ||
            isInsideOldHost(el) ||
            // Paragraph content is marked with ng-non-bindable unless the paragraph
            // has explicitly been allowed to contain components.
            el.closest("[ng-non-bindable]")
        ) {
            continue;
        }
        // The element was inside another server component that did not project it.
        if (el !== root && !root.contains(el)) {
            continue;
        }
        const reg = registry.get(el.tagName.toLowerCase());
        if (!reg) {
            continue;
        }
        if (reg.moduleType) {
            pending.add(el);
            void getModuleRef(reg.moduleType).then((moduleRef) => {
                pending.delete(el);
                moduleRef.injector
                    .get(NgZone)
                    .run(() =>
                        createOnElement(
                            el,
                            reg.component,
                            moduleRef.injector,
                            moduleRef.injector.get(ApplicationRef)
                        )
                    );
            });
        } else {
            createOnElement(el, reg.component, rootInjector, appRef);
        }
    }
    ensureObserver();
}

/**
 * Returns the component instance that was created on the element, if any.
 */
export function getServerComponent<T>(el: Element): T | undefined {
    return created.get(el)?.ref.instance as T | undefined;
}

/**
 * Creates the given component on the element and attaches it to the application.
 * The child nodes of the element are projected to the component.
 *
 * @param component The Angular component.
 * @param el The host element.
 * @param inputs The values of the inputs of the component.
 * @returns The component instance.
 */
export function createComponentOnElement<T>(
    component: Type<T>,
    el: Element,
    inputs: Record<string, unknown> = {}
): T {
    if (!appRef || !rootInjector) {
        throw new Error("The application has not been started");
    }
    const ref = createComponent(component, {
        environmentInjector: rootInjector,
        hostElement: el,
        projectableNodes: getProjectableNodes(el, component),
    });
    for (const [name, value] of Object.entries(inputs)) {
        ref.setInput(name, value);
    }
    created.set(el, {ref, appRef});
    appRef.attachView(ref.hostView);
    ref.changeDetectorRef.detectChanges();
    ensureObserver();
    return ref.instance;
}
