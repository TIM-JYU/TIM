/**
 * Quantum circuit board.
 */

import type {OnInit, PipeTransform} from "@angular/core";
import {Component, Input, Pipe} from "@angular/core";
import type {
    Gate,
    Qubit,
    QubitOutput,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";

/**
 * Turns two dimensional array into one dimensional.
 */
@Pipe({name: "flatten"})
export class FlatteningPipe implements PipeTransform {
    transform(board: Gate[][]): Gate[] {
        return board.flat();
    }
}

@Component({
    selector: "tim-quantum-circuit-board",
    template: `
        
        <svg [attr.width]="svgW" [attr.height]="svgH">
            <g *ngFor="let qubit of qubits">
                <rect [attr.x]="qubit.x" [attr.y]="qubit.y" [attr.width]="qubit.w" [attr.height]="qubit.h" fill="white" stroke="black"/>
                <text [attr.x]="qubit.textX" [attr.y]="qubit.textY" fill="black">{{qubit.text}} {{qubit.value}}</text>
            </g>

            <g *ngFor="let gate of board|flatten">
                <rect [attr.x]="gate.x" [attr.y]="gate.y" [attr.width]="gate.w" [attr.height]="gate.h" fill="white" stroke="black"/>
                <text [attr.x]="gate.textX" [attr.y]="gate.textY" fill="black">{{gate.name}}</text>
                <line *ngIf="gate.name === undefined" stroke="black" [attr.x1]="gate.x" [attr.y1]="gate.textY" [attr.x2]="gate.textX+gate.w" [attr.y2]="gate.textY"></line>
            </g>
            
            <g *ngFor="let qubit of qubitOutputs">
                <rect [attr.x]="qubit.x" [attr.y]="qubit.y" [attr.width]="qubit.w" [attr.height]="qubit.h" fill="white" stroke="black"/>
                <text [attr.x]="qubit.textX" [attr.y]="qubit.textY" fill="black">{{qubit.value}}</text>
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
    qubitOutputs!: QubitOutput[];

    constructor() {}

    ngOnInit(): void {}
}
