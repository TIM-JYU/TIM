import type {Injector, Type} from "@angular/core";
import {createCustomElement} from "@angular/elements";

const registeredElements = new Map<string, Type<unknown>>();

/**
 * Registers an Angular component to be defined as a custom element (https://angular.io/guide/elements).
 *
 * The browser creates the component whenever the element appears in the DOM, also inside document paragraphs
 * whose content is otherwise left alone (see serverComponents.ts for the alternative).
 * Use this for components that appear in HTML coming from the server (e.g. in document paragraphs).
 *
 * Things to note:
 *
 *  * The inputs are given as plain HTML attributes (in dash-case), so their values are always strings.
 *    Binding syntax (`[input]="..."`, `bind-input="..."`) does not work.
 *  * A component must not be registered both as a custom element and as a server component.
 *  * The element must not be used in the template of another Angular component. Angular would create
 *    the component there and the browser would create it a second time on the same element.
 *
 * The elements are defined when AppModule is bootstrapped (see defineRegisteredElements).
 *
 * @param selector The tag name of the element. Must contain a dash.
 * @param component The Angular component.
 */
export function registerElement(selector: string, component: Type<unknown>) {
    registeredElements.set(selector, component);
}

/**
 * Defines all registered components as custom elements. Called by AppModule when it is bootstrapped.
 *
 * @param injector The injector that the components will use.
 */
export function defineRegisteredElements(injector: Injector) {
    for (const [selector, component] of registeredElements) {
        if (customElements.get(selector)) {
            continue;
        }
        customElements.define(
            selector,
            createCustomElement(component, {injector})
        );
    }
}

/**
 * Converts the value of a boolean input that may have been given as an HTML attribute.
 */
export function attrToBoolean(value: unknown): boolean {
    return value === true || value === "true";
}
