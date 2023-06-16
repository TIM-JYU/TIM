/**
 * Quantum circuit board.
 */

import type {OnInit, PipeTransform} from "@angular/core";
import {
    Component,
    EventEmitter,
    Input,
    Output,
    Pipe,
    ViewChild,
    ElementRef,
} from "@angular/core";
import {CircuitStyleOptions} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {
    Gate,
    Qubit,
    QubitOutput,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";

@Pipe({name: "flatten"})
export class FlatteningPipe implements PipeTransform {
    transform(board: Gate[][]): Gate[] {
        return board.flat();
    }
}

@Component({
    selector: "tim-quantum-circuit-board",
    template: `
        <!--suppress HtmlUnknownAttribute -->

        <div class="circuit-container">
            <div class="qubits">
                <div class="left-block" [style.height.px]="circuitStyleOptions.timeAxisHeight"
                     [style.width.%]="100"></div>
                <div *ngFor="let qubit of qubits" [style.height.px]="getH()" class="qubit">
                    <div>{{qubit.name}}</div>
                    <button class="qubit-toggle-button"
                            (click)="toggleQubit($event, qubit.id)">{{getQubitText(qubit)}}</button>
                </div>
            </div>


            <div class="circuit-right-container">
                <div class="moments">
                    <div class="moment" *ngFor="let gate of board[0]; let i = index" [style.width.px]="getW()"
                         [style.height.px]="circuitStyleOptions.timeAxisHeight">
                        <div>{{i}}</div>
                    </div>
                </div>

                <svg #svgElement [attr.width]="getNeededWidth()" [attr.height]="getNeededHeight()">

                    <!-- lines -->
                    <line *ngFor="let qubit of qubits; let i=index" [attr.stroke]="colors.dark"
                          [attr.x1]="0" [attr.y1]="getH() * i + getH() / 2"
                          [attr.x2]="getNeededWidth()" [attr.y2]="getH() * i + getH() / 2"></line>

                    <!-- empty gate placeholders-->
                    <g *ngFor="let gate of board |flatten" (drop)="handleDrop($event, gate)"
                       (dragover)="handleDragOver($event, gate)" (dragleave)="handleDragLeave()">
                        <rect [class.drag-over-element]="isBeingDraggedOver(gate)" *ngIf="!gate.name"
                              [attr.x]="gate.x"
                              [attr.y]="gate.y" [attr.width]="gate.w"
                              [attr.height]="gate.h"
                              [attr.fill]="colors.light" fill-opacity="0.5"/>
                    </g>

                    <!-- gates -->
                    <g *ngFor="let gate of board |flatten" (drop)="handleDrop($event, gate)"
                       (mousedown)="handleDragStart($event, gate)"
                       (mousemove)="handleDrag($event)"
                       (mouseup)="handleDragEnd()"
                       (mouseleave)="handleDragEnd()"
                       (dragover)="handleDragOver($event, gate)" (dragleave)="handleDragLeave()">
                        <rect [class.drag-over-element]="isBeingDraggedOver(gate)" *ngIf="gate.name"
                              [attr.x]="gate.x + (gate.w - circuitStyleOptions.gateSize) / 2"
                              [attr.y]="gate.y + (gate.h - circuitStyleOptions.gateSize) / 2"
                              [attr.width]="circuitStyleOptions.gateSize"
                              [attr.height]="circuitStyleOptions.gateSize"
                              [attr.rx]="circuitStyleOptions.gateBorderRadius"
                              [attr.fill]="colors.light" [attr.stroke]="colors.medium"/>
                        <text *ngIf="gate.name"
                              class="gate-text"
                              [attr.x]="gate.textX"
                              [attr.y]="gate.textY"
                              dominant-baseline="middle"
                              text-anchor="middle"
                              [attr.fill]="colors.dark">{{gate.name}}</text>
                    </g>

                </svg>


            </div>

            <div class="output-container">
                <div class="right-block" [style.height.px]="circuitStyleOptions.timeAxisHeight"></div>
                <div class="output" *ngFor="let output of qubitOutputs" [style.height.px]="getH()"
                     [style.width.px]="getW()">
                    <button class="output-value">{{output.value}}</button>
                </div>
            </div>

        </div>

    `,
    styleUrls: ["./quantum-circuit-board.component.scss"],
})
export class QuantumCircuitBoardComponent implements OnInit {
    dragOverElement?: Gate;

    gateBeingDragged: {
        group: SVGGElement;
        gate: Gate;
        offset: [number, number];
    } | null = null;

    @ViewChild("svgElement")
    svgElement!: ElementRef<SVGSVGElement>;

    @Input()
    circuitStyleOptions!: CircuitStyleOptions;

    @Input()
    board: Gate[][] = [];

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

    get colors() {
        return this.circuitStyleOptions.colors;
    }

    getQubitText(qubit: Qubit) {
        if (this.circuitStyleOptions.useBraket) {
            return `|${qubit.value}>`;
        }
        return qubit.value.toString();
    }

    ngOnInit(): void {}

    toggleQubit(event: MouseEvent, id: number) {
        this.qubitChange.emit(id);
    }

    handleDrop(event: DragEvent, gate: Gate) {
        event.preventDefault();
        const gateName = event.dataTransfer?.getData("text/plain");
        if (gateName === undefined) {
            return;
        }

        this.dragOverElement = undefined;
        gate.name = gateName;
        this.gateDrop.emit(gate);
    }

    getW() {
        if (this.board.length > 0 && this.board[0].length > 0) {
            return this.board[0][0].w;
        }
        return 0;
    }

    getH() {
        if (this.board.length > 0 && this.board[0].length > 0) {
            return this.board[0][0].h;
        }
        return 0;
    }

    getNeededWidth() {
        let totalWidth = 0;

        // sum up column widths
        for (let col = 0; col < this.nMoments; col++) {
            let widestRowInColumn = 0;
            for (const item of this.board) {
                widestRowInColumn = Math.max(widestRowInColumn, item[col].w);
            }
            totalWidth += widestRowInColumn;
        }

        return totalWidth;
    }

    getNeededHeight() {
        let totalHeight = 0;

        // add row heights
        for (const row of this.board) {
            let tallestColInRow = 0;
            for (const col of row) {
                tallestColInRow = Math.max(tallestColInRow, col.h);
            }
            totalHeight += tallestColInRow;
        }

        return totalHeight;
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

    /**
     * https://www.petercollingridge.co.uk/tutorials/svg/interactive/dragging/
     */
    getMousePosition(x: number, y: number): [number, number] | null {
        const CTM = this.svgElement.nativeElement.getScreenCTM();
        if (!CTM) {
            return null;
        }
        return [(x - CTM.e) / CTM.a, (y - CTM.f) / CTM.d];
    }

    handleDragStart(event: MouseEvent, gate: Gate) {
        if (!event.currentTarget) {
            return;
        }
        const offset = this.getMousePosition(event.clientX, event.clientY);
        if (!offset) {
            return;
        }

        const group = event.currentTarget as SVGGElement;

        // Get all the transforms currently on this element
        const transforms = group.transform.baseVal;

        this.gateBeingDragged = {
            gate: gate,
            group: group,
            offset: offset,
        };

        // Ensure the first transform is a translate transform
        if (
            transforms.length === 0 ||
            transforms.getItem(0).type !== SVGTransform.SVG_TRANSFORM_TRANSLATE
        ) {
            // Create a transform that translates by (0, 0)
            const translate =
                this.svgElement.nativeElement.createSVGTransform();
            translate.setTranslate(0, 0);

            group.transform.baseVal // Add the translation to the front of the transforms list
                .insertItemBefore(translate, 0);
        }

        // Get initial translation amount
        const transform = transforms.getItem(0);
        this.gateBeingDragged.offset[0] -= transform.matrix.e;
        this.gateBeingDragged.offset[1] -= transform.matrix.f;
    }

    handleDrag(event: MouseEvent) {
        if (this.gateBeingDragged) {
            event.preventDefault();
            const xy = this.getMousePosition(event.clientX, event.clientY);
            if (!xy) {
                return;
            }
            const [x, y] = xy;
            const [offsetX, offsetY] = this.gateBeingDragged.offset;

            const transform =
                this.gateBeingDragged.group.transform.baseVal.getItem(0);
            transform.setTranslate(x - offsetX, y - offsetY);
        }
    }

    handleDragEnd() {
        this.gateBeingDragged = null;
    }
}
