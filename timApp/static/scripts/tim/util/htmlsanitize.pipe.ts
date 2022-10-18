/**
 * A sanitizer based on html-sanitize.
 *
 * NOTE: Prefer using PurifyModule instead! It allows faster and better sanitization.
 *       Use this module only for cases where PurifyModule does not yield desired sanitization.
 */

import type {PipeTransform} from "@angular/core";
import {Pipe} from "@angular/core";
import * as sanitizeHtml from "sanitize-html";
import {DomSanitizer} from "@angular/platform-browser";

@Pipe({
    name: "htmlSanitize",
})
export class HTMLSanitizePipe implements PipeTransform {
    constructor(protected sanitizer: DomSanitizer) {}

    transform(value: unknown) {
        if (typeof value !== "string") {
            return value;
        }

        const attrs = {...sanitizeHtml.defaults.allowedAttributes};
        attrs.th = ["colspan"];
        return this.sanitizer.bypassSecurityTrustHtml(
            sanitizeHtml(value, {
                allowedAttributes: attrs,
            })
        );
    }
}
