/**
 * Quantum circuit board.
 */

import type {OnInit} from "@angular/core";
import {Component, EventEmitter, Input, Output} from "@angular/core";
import type {
    Gate,
    Qubit,
    QubitOutput,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";

/**
 * Colors used in drawing circuits.
 */
export const COLORS = {
    dark: "black",
    medium: "grey",
    light: "white",
};

@Component({
    selector: "tim-quantum-circuit-board",
    template: `
        <svg [attr.width]="svgW" [attr.height]="svgH">
            <g>
                <g *ngFor="let gate of board[0] let i=index">
                    <rect [attr.x]="gate.x" [attr.y]="0" [attr.width]="gate.w" [attr.height]="gate.h"
                          [attr.fill]="COLORS.light" [attr.stroke]="COLORS.dark"/>
                    <text [attr.x]="gate.textX" [attr.y]="gate.h / 2" fill="black">{{i}}</text>
                </g>
            </g>

            <g>
                <g *ngFor="let qubit of qubits" (click)="toggleQubit($event, qubit.id)">
                    <rect [attr.x]="qubit.x" [attr.y]="qubit.y" [attr.width]="qubit.w" [attr.height]="qubit.h"
                          [attr.fill]="COLORS.light" [attr.stroke]="COLORS.dark"/>

                    <text [attr.x]="qubit.x" [attr.y]="qubit.textY" [attr.fill]="COLORS.dark">{{qubit.text}}</text>
                    <text [attr.x]="qubit.textX" [attr.y]="qubit.textY" [attr.fill]="COLORS.dark"
                          [attr.stroke]="COLORS.dark">{{qubit.value}}</text>
                </g>

            </g>

            <g *ngFor="let gates of board">
                <g *ngFor="let gate of gates" (drop)="handleDrop($event, gate)" (dragover)="handleDragOver($event, gate)">
                    <rect [attr.x]="gate.x" [attr.y]="gate.y" [attr.width]="gate.w" [attr.height]="gate.h"
                          [attr.fill]="COLORS.light" [attr.stroke]="COLORS.dark"/>
                    <text [attr.x]="gate.textX" [attr.y]="gate.textY" [attr.fill]="COLORS.dark">{{gate.name}}</text>
                    <line *ngIf="gate.name === undefined" [attr.stroke]="COLORS.medium" [attr.x1]="gate.x"
                          [attr.y1]="gate.textY" [attr.x2]="gate.textX+gate.w" [attr.y2]="gate.textY"></line>
                </g>
            </g>

            <g>
                <g *ngFor="let qubit of qubitOutputs">
                    <rect [attr.x]="qubit.x" [attr.y]="qubit.y" [attr.width]="qubit.w" [attr.height]="qubit.h"
                          [attr.fill]="COLORS.light" [attr.stroke]="COLORS.dark"/>
                    <text [attr.x]="qubit.textX" [attr.y]="qubit.textY" fill="black">{{qubit.value}}</text>
                </g>
            </g>
        </svg>
    `,
    styleUrls: ["./quantum-circuit-board.component.scss"],
})
export class QuantumCircuitBoardComponent implements OnInit {
    @Input()
    board: Gate[][] = [];

    @Input()
    svgH!: number;

    @Input()
    svgW!: number;

    @Input()
    qubits!: Qubit[];

    @Input()
    nMoments!: number;

    @Input()
    qubitOutputs!: QubitOutput[];

    @Output()
    qubitChange = new EventEmitter<number>();

    @Output()
    gateDrop = new EventEmitter<Gate>();

    constructor() {}

    ngOnInit(): void {}

    toggleQubit(event: MouseEvent, id: number) {
        this.qubitChange.emit(id);
    }

    handleDrop(event: DragEvent, gate: Gate) {
        event.preventDefault();
        console.log(event);
        const gateName = event.dataTransfer?.getData("text/plain");
        if (gateName === undefined) {
            return;
        }
        console.log(gateName);
        gate.name = gateName;
        this.gateDrop.emit(gate);
    }

    handleDragOver(event: DragEvent, gate: Gate) {
        event.preventDefault();
    }

    protected readonly COLORS = COLORS;
}
