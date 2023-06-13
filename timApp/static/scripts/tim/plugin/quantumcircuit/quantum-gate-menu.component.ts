import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";
import {COLORS} from "tim/plugin/quantumcircuit/quantum-circuit-board.component";

interface Gate {
    name: string;
}

@Component({
    selector: "tim-quantum-gate-menu",
    template: `
        <div class="gate-container">
            <div class="svg-container" *ngFor="let gate of gates" draggable="true"
                 (dragstart)="handleDragStart($event, gate)">
                <svg [attr.width]="50" [attr.height]="50">
                    <rect [attr.x]="0" [attr.y]="0" [attr.width]="50" [attr.height]="50" [attr.fill]="COLORS.light"
                          [attr.stroke]="COLORS.dark"/>
                    <text x="50%" y="50%" [attr.fill]="COLORS.dark" [attr.stroke]="COLORS.dark"
                          dominant-baseline="middle"
                          text-anchor="middle">{{gate.name}}</text>
                </svg>
            </div>

        </div>

    `,
    styleUrls: ["./quantum-gate-menu.component.scss"],
})
export class QuantumGateMenuComponent implements OnInit {
    protected readonly COLORS = COLORS;

    constructor() {}

    gates: Gate[] = [];

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
