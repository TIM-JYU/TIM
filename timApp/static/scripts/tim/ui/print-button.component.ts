import {Component, Input} from "@angular/core";

@Component({
    selector: "tim-print-button",
    template: `
    <a [class.timButton]="isButton" (click)="doPrint()">
        <ng-content></ng-content>
    </a>
  `,
})
export class PrintButtonComponent {
    @Input() isButton = false;

    doPrint() {
        window.print();
    }
}
