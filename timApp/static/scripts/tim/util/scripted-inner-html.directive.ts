import type {OnChanges, SimpleChanges} from "@angular/core";
import {Directive, ElementRef, Input} from "@angular/core";

/**
 * A directive that allows the user to specify the HTML content of an element.
 * Unlike the normal innerHTML directive, this directive also automatically runs available <script> tags.
 *
 * NOTE: This directive does not sanitize the HTML! You need to ensure the HTML is safe before using this directive.
 */
@Directive({
    selector: "[scriptedInnerHTML]",
})
export class ScriptedInnerHTMLDirective implements OnChanges {
    @Input() scriptedInnerHTML!: string;

    constructor(private el: ElementRef<HTMLElement>) {}

    private refresh() {
        const e = this.el.nativeElement;
        e.innerHTML = this.scriptedInnerHTML;
        const oldScripts = [...e.querySelectorAll("script")];
        oldScripts.forEach((s) => {
            const script = document.createElement("script");
            const oldAttributes = [...s.attributes];
            oldAttributes.forEach((attr) => {
                script.setAttribute(attr.name, attr.value);
            });
            script.appendChild(document.createTextNode(s.innerHTML));
            s.parentNode?.replaceChild(script, s);
        });
    }

    ngOnChanges(changes: SimpleChanges): void {
        if (changes.scriptedInnerHTML) {
            this.refresh();
        }
    }
}
