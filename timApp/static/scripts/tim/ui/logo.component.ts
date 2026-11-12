import {Component, ChangeDetectionStrategy} from "@angular/core";

@Component({
    selector: "tim-logo",
    template: `
        <img alt="TIM - The Interactive Material"
             src="/static/images/tim-logo.svg"/>
    `,
    styleUrls: ["./logo.component.scss"],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class LogoComponent {}
