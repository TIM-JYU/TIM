import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";

@Component({
    selector: "tim-quantum-gate-menu",
    template: `
    <p>
      quantum-gate-menu works!
    </p>
  `,
    styleUrls: ["./quantum-gate-menu.component.scss"],
})
export class QuantumGateMenuComponent implements OnInit {
    constructor() {}

    ngOnInit(): void {}
}
