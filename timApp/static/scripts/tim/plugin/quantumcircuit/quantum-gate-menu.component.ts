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
            <p class="gate-container-heading">Portit</p>

            <div class="gate-list">
                <div class="svg-container" *ngFor="let gate of gates" draggable="true"
                     (dragstart)="handleDragStart($event, gate)">
                    <svg [attr.width]="circuitStyleOptions.gateSize" [attr.height]="circuitStyleOptions.gateSize">
                        <rect [attr.x]="0" [attr.y]="0" [attr.width]="circuitStyleOptions.gateSize"
                              [attr.height]="circuitStyleOptions.gateSize" [attr.fill]="colors.light"
                              [attr.stroke]="colors.dark"
                              rx="2"/>
                        <text x="50%" y="50%" [attr.fill]="colors.dark" [attr.stroke]="colors.dark"
                              dominant-baseline="middle"
                              text-anchor="middle">{{gate.name}}</text>
                    </svg>
                </div>
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

    /**
     * Add gate's name to datatransfer so it can be used when drop happens
     * @param event dragging of a gate starts
     * @param gate gate being dragged
     */
    handleDragStart(event: DragEvent, gate: Gate) {
        event.dataTransfer?.setData("text/plain", gate.name);
    }
}
