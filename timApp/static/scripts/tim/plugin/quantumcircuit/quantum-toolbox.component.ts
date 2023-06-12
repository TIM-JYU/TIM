import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";

@Component({
    selector: "tim-quantum-toolbox",
    template: `
    <p>
      quantum-toolbox works!
    </p>
  `,
    styleUrls: ["./quantum-toolbox.component.scss"],
})
export class QuantumToolboxComponent implements OnInit {
    constructor() {}

    ngOnInit(): void {}
}
