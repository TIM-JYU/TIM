import {getEditLine} from "tim/document/structure/paragraph";

/**
 * Represents the help paragraph that is only visible when the document is empty.
 */
export class HelpPar {
    isHelp = true as const;

    constructor(public el: HTMLElement) {}

    getEditLine() {
        return getEditLine(this.el);
    }
}
