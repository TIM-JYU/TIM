import type {OnChanges} from "@angular/core";
import {Directive, ElementRef, Input, SecurityContext} from "@angular/core";
import type {SafeResourceUrl} from "@angular/platform-browser";
import {DomSanitizer} from "@angular/platform-browser";

export interface IIframeAttributes {
    src: SafeResourceUrl;
    sandbox: string;
    allow: string | null;
}

/**
 * Sets the src, sandbox and allow attributes of an iframe from an object.
 *
 * Angular does not allow binding the sandbox and allow attributes of an iframe in a template,
 * because a binding could take effect after the content has been loaded.
 * This directive sets the attributes in a safe order: the restrictions first and the address last.
 */
@Directive({
    selector: "iframe[timIframeSettings]",
    standalone: false,
})
export class IframeSettingsDirective implements OnChanges {
    @Input() timIframeSettings?: IIframeAttributes;
    /**
     * Whether to set the sandbox attribute. If false, the iframe is not sandboxed.
     */
    @Input() timIframeSandbox = true;
    private currentSrc?: string;

    constructor(
        private el: ElementRef<HTMLIFrameElement>,
        private sanitizer: DomSanitizer
    ) {}

    ngOnChanges() {
        const frame = this.el.nativeElement;
        const settings = this.timIframeSettings;
        if (!settings) {
            return;
        }
        if (this.timIframeSandbox) {
            frame.setAttribute("sandbox", settings.sandbox);
        } else {
            frame.removeAttribute("sandbox");
        }
        if (settings.allow != null) {
            frame.setAttribute("allow", settings.allow);
        } else {
            frame.removeAttribute("allow");
        }
        const src =
            this.sanitizer.sanitize(
                SecurityContext.RESOURCE_URL,
                settings.src
            ) ?? "";
        if (src !== this.currentSrc) {
            this.currentSrc = src;
            frame.src = src;
        }
    }
}
