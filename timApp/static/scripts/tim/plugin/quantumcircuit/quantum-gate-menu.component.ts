import type {OnInit} from "@angular/core";
import {Component, Input} from "@angular/core";
import {CircuitStyleOptions} from "tim/plugin/quantumcircuit/quantum-circuit.component";

interface Gate {
    name: string;
}

@Component({
    selector: "tim-quantum-gate-menu",
    template: `
        <!--suppress HtmlUnknownAttribute -->
        <div class="gate-container">
            <div class="svg-container" *ngFor="let gate of gates" draggable="true"
                 (dragstart)="handleDragStart($event, gate)">
                <svg [attr.width]="circuitStyleOptions.baseSize" [attr.height]="circuitStyleOptions.baseSize">
                    <rect [attr.x]="0" [attr.y]="0" [attr.width]="circuitStyleOptions.baseSize" [attr.height]="circuitStyleOptions.baseSize" [attr.fill]="colors.light"
                          [attr.stroke]="colors.dark"/>
                    <text x="50%" y="50%" [attr.fill]="colors.dark" [attr.stroke]="colors.dark"
                          dominant-baseline="middle"
                          text-anchor="middle">{{gate.name}}</text>
                </svg>
            </div>
        </div>

    `,
    styleUrls: ["./quantum-gate-menu.component.scss"],
})
export class QuantumGateMenuComponent implements OnInit {
    gates: Gate[] = [];

    @Input()
    circuitStyleOptions!: CircuitStyleOptions;

    constructor() {}

    get colors() {
        return this.circuitStyleOptions.colors;
    }

    ngOnInit(): void {
        this.gates = [
            {
                name: "H",
            },
            {
                name: "X",
            },
            {
                name: "Y",
            },
        ];
    }

    handleDragStart(event: DragEvent, gate: Gate) {
        event.dataTransfer?.setData("text/plain", gate.name);
    }
}
