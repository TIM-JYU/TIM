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
            <!-- gates-->
            <g>
                <g *ngFor="let gates of board">
                    <g *ngFor="let gate of gates" (drop)="handleDrop($event, gate)"
                       (dragover)="handleDragOver($event, gate)" (dragleave)="handleDragLeave()">
                        <rect [class.drag-over-element]="isBeingDraggedOver(gate)" *ngIf="!gate.name" [attr.x]="gate.x"
                              [attr.y]="gate.y" [attr.width]="gate.w"
                              [attr.height]="gate.h"
                              [attr.fill]="COLORS.light" [attr.stroke]="COLORS.light"/>
                    </g>
                </g>

            </g>
            <!-- lines -->
            <g>
                <line *ngFor="let qubit of qubits" [attr.stroke]="COLORS.medium" [attr.x1]="0"
                      [attr.y1]="qubit.textY" [attr.x2]="svgW" [attr.y2]="qubit.textY"></line>
            </g>

            <!-- gates-->
            <g>
                <g *ngFor="let gates of board">
                    <g *ngFor="let gate of gates" (drop)="handleDrop($event, gate)"
                       (dragover)="handleDragOver($event, gate)" (dragleave)="handleDragLeave()">
                        <rect [class.drag-over-element]="isBeingDraggedOver(gate)" *ngIf="gate.name" [attr.x]="gate.x+5"
                              [attr.y]="gate.y+5" [attr.width]="gate.w-10"
                              [attr.height]="gate.h-10"
                              [attr.fill]="COLORS.light" [attr.stroke]="COLORS.medium"/>
                        <text *ngIf="gate.name" [attr.x]="gate.textX" [attr.y]="gate.textY"
                              [attr.fill]="COLORS.dark">{{gate.name}}</text>

                    </g>
                </g>

            </g>


            <!-- moments -->
            <g>
                <g *ngFor="let gate of board[0] let i=index">
                    <rect [attr.x]="gate.x" [attr.y]="0" [attr.width]="gate.w" [attr.height]="gate.h"
                          [attr.fill]="COLORS.light" [attr.stroke]="COLORS.dark"/>
                    <text [attr.x]="gate.textX" [attr.y]="gate.h / 2" fill="black">{{i}}</text>
                </g>
            </g>

            <!-- qubits -->
            <g>
                <g *ngFor="let qubit of qubits" (click)="toggleQubit($event, qubit.id)">
                    <rect [attr.x]="qubit.x" [attr.y]="qubit.y" [attr.width]="qubit.w" [attr.height]="qubit.h"
                          [attr.fill]="COLORS.light" [attr.stroke]="COLORS.dark"/>

                    <text [attr.x]="qubit.x" [attr.y]="qubit.textY" [attr.fill]="COLORS.dark">{{qubit.text}}</text>
                    <text [attr.x]="qubit.textX" [attr.y]="qubit.textY" [attr.fill]="COLORS.dark"
                          [attr.stroke]="COLORS.dark">{{qubit.value}}</text>
                </g>

            </g>

            <!-- outputs -->
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
    protected readonly COLORS = COLORS;

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

    dragOverElement?: Gate;

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
        this.dragOverElement = undefined;
        console.log(gateName);
        gate.name = gateName;
        this.gateDrop.emit(gate);
    }

    handleDragOver(event: DragEvent, gate: Gate) {
        this.dragOverElement = gate;
        event.preventDefault();
    }

    handleDragLeave() {
        this.dragOverElement = undefined;
    }

    isBeingDraggedOver(gate: Gate) {
        if (!this.dragOverElement) {
            return false;
        }
        return (
            this.dragOverElement.time === gate.time &&
            this.dragOverElement.target === gate.target
        );
    }
}
