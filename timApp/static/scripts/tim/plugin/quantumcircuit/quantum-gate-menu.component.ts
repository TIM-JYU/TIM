import type {OnInit} from "@angular/core";
import {Component, Input} from "@angular/core";
import {CircuitStyleOptions} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {Gate} from "tim/plugin/quantumcircuit/gate.service";
import {GateService} from "tim/plugin/quantumcircuit/gate.service";

@Component({
    selector: "tim-quantum-gate-menu",
    template: `
        <!--suppress HtmlUnknownAttribute -->

        <div class="gate-container">
            <p class="gate-container-heading">Portit</p>

            <div class="gate-list">
                <div class="svg-container" *ngFor="let gate of gates" draggable="true"
                     (dragstart)="handleDragStart($event, gate.name)">
                    <div [ngSwitch]="gate.name">
                        <svg *ngSwitchCase="'control'" [attr.width]="circuitStyleOptions.gateSize"
                             [attr.height]="circuitStyleOptions.gateSize">
                            <circle [attr.cx]="circuitStyleOptions.gateSize/2"
                                    [attr.cy]="circuitStyleOptions.gateSize/2"
                                    [attr.r]="circuitStyleOptions.gateSize/4"/>
                        </svg>

                        <svg *ngSwitchCase="'swap'" [attr.width]="circuitStyleOptions.gateSize" [attr.height]="circuitStyleOptions.gateSize"
                             class="swap-gate">
                            <text x="50%" y="25%" [attr.fill]="colors.dark" [attr.stroke]="colors.dark"
                                  dominant-baseline="middle" text-anchor="middle"
                                  [attr.font-size]="circuitStyleOptions.gateSize / 3">X
                            </text>
                            <line [attr.stroke]="colors.dark" stroke-width="2" x1="50%" x2="50%" y1="25%"
                                  y2="75%"></line>
                            <text x="50%" y="75%" [attr.fill]="colors.dark" [attr.stroke]="colors.dark"
                                  dominant-baseline="middle" text-anchor="middle"
                                  [attr.font-size]="circuitStyleOptions.gateSize / 3">X
                            </text>
                        </svg>
                        
                        <svg *ngSwitchDefault [attr.width]="circuitStyleOptions.gateSize"
                             [attr.height]="circuitStyleOptions.gateSize">
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
        </div>

    `,
    styleUrls: ["./quantum-gate-menu.component.scss"],
})
export class QuantumGateMenuComponent implements OnInit {
    gates: Gate[] = [];

    @Input()
    circuitStyleOptions!: CircuitStyleOptions;

    constructor(private gateService: GateService) {}

    get colors() {
        return this.circuitStyleOptions.colors;
    }

    ngOnInit(): void {
        this.gateService.getGates().subscribe((gates) => (this.gates = gates));
    }

    /**
     * Add gate's name to datatransfer, so it can be used when drop happens.
     * @param event dragging of a gate starts
     * @param gate gate being dragged
     */
    handleDragStart(event: DragEvent, gate: string) {
        event.dataTransfer?.setData("text/plain", gate);
    }
}
