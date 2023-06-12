import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";

@Component({
    selector: "tim-quantum-stats",
    template: `
    <p>
      quantum-stats works!
    </p>
  `,
    styleUrls: ["./quantum-stats.component.scss"],
})
export class QuantumStatsComponent implements OnInit {
    constructor() {}

    ngOnInit(): void {}
}
