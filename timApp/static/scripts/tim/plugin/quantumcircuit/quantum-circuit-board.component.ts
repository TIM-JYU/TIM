import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";

@Component({
    selector: "tim-quantum-circuit-board",
    template: `
    <p>
      quantum-circuit-board works!
    </p>
  `,
    styleUrls: ["./quantum-circuit-board.component.scss"],
})
export class QuantumCircuitBoardComponent implements OnInit {
    constructor() {}

    ngOnInit(): void {}
}
