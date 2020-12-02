import {Pipe, PipeTransform} from "@angular/core";
import {DomSanitizer} from "@angular/platform-browser";
import DOMPurify from "dompurify";

@Pipe({
    name: "purify",
})
export class PurifyPipe implements PipeTransform {
    constructor(protected sanitizer: DomSanitizer) {}

    public transform(value: unknown) {
        if (typeof value !== "string") {
            return value;
        }
        return this.sanitizer.bypassSecurityTrustHtml(
            DOMPurify.sanitize(value, {ADD_ATTR: ["target"]})
        );
    }
}
