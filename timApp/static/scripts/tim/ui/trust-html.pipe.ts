import {Pipe, PipeTransform} from "@angular/core";
import {DomSanitizer} from "@angular/platform-browser";

@Pipe({
    name: "trust_html",
})
export class TrustHtmlPipe implements PipeTransform {

    constructor(protected sanitizer: DomSanitizer) {
    }

    public transform(value: unknown) {
        if (typeof value !== "string") {
            return value;
        }
        return this.sanitizer.bypassSecurityTrustHtml(value);
    }
}
