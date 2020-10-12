import {Component, Input, Optional} from "@angular/core";
import {NgModel} from "@angular/forms";
import {InputService} from "./input.service";
import {formErrorMessages} from "./formErrorMessages";

@Component({
    selector: "tim-error-message",
    template: `
        <tim-alert *ngIf="for && !for.pristine && for.invalid" severity="danger">
            {{ getMessage() }}
        </tim-alert>
    `,
})
export class ErrorMessageComponent {
    constructor(@Optional() private inputService?: InputService) {
        if (inputService) {
            (async () => {
                this.for = await inputService.defer.promise;
            })();
        }
    }

    getMessage() {
        if (!this.for || !this.for.errors) {
            return;
        }
        return formErrorMessages[Object.keys(this.for.errors)[0]];
    }

    @Input() for?: NgModel;
}
