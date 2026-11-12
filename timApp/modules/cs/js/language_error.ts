import {Component} from "@angular/core";
import {CsBase} from "./csPlugin";

@Component({
    selector: "cs-error",
    template: `
    @if (markupError) {
      <tim-markup-error [data]="markupError"></tim-markup-error>
    }
    @if (!markupError) {
      <div class="error">
        <p>Error(s) initializing csPlugin:</p>
        @if (error) {
          <pre>{{error}}</pre>
        }
        @if (own_error) {
          <pre>{{own_error}}</pre>
        }
      </div>
    }`,
    standalone: false,
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
