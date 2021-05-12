export type PleaseUseJqueryToRemoveHtmlElements = never;

/**
 * See {@link NotRemovableHTMLElement}.
 */
export interface NotRemovableElement extends Element {
    parentElement: NotRemovableHTMLElement | null;
    nextElementSibling: NotRemovableElement | null;
    remove: PleaseUseJqueryToRemoveHtmlElements;
}

/**
 * A modified HTMLElement interface to make sure we never call remove() directly.
 * We need to use jQuery for removing elements at least as long as AngularJS is used.
 * Otherwise the components won't get cleaned up properly.
 */
export interface NotRemovableHTMLElement extends HTMLElement {
    parentElement: NotRemovableHTMLElement | null;
    nextElementSibling: NotRemovableElement | null;
    remove: PleaseUseJqueryToRemoveHtmlElements;
}
