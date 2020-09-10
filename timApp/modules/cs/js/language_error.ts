import {Component} from "@angular/core";
import {CsBase} from "./csPlugin";

@Component({
    selector: "cs-error",
    template: `
    <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
    <div *ngIf="!markupError" class="error">
        <p>Error(s) initializing csPlugin:</p>
        <pre *ngIf="error">{{error}}</pre>
        <pre *ngIf="own_error">{{own_error}}</pre>
    </div>`,
})
export class CsErrorComponent extends CsBase {
    error?: string;
    own_error?: string;

    ngOnInit() {
        super.ngOnInit();

        this.error = this.attrsall.error ?? "";
        this.own_error = this.attrsall.own_error ?? "";
    }
}
