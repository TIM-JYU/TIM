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

export interface GatePos {
    target: number;
    time: number;
}

export interface GateMove {
    from: GatePos;
    to: GatePos;
}

export interface GateDrop {
    time: number;
    target: number;
    name: string;
}

@Component({
    selector: "tim-quantum-circuit-board",
    template: `
        <!--suppress HtmlUnknownAttribute -->

        <div class="circuit-container">
            <div class="qubits">
                <div class="left-block" [style.height.px]="circuitStyleOptions.timeAxisHeight"
                     [style.width.%]="100"></div>
                <div *ngFor="let qubit of qubits; let i=index" [style.height.px]="circuitStyleOptions.baseSize"
                     class="qubit">
                    <div>{{qubit.name}}</div>
                    <button class="qubit-toggle-button"
                            (click)="toggleQubit($event, i)">{{getQubitText(qubit)}}</button>
                </div>
            </div>


            <div class="circuit-right-container">
                <div class="moments">
                    <div class="moment" *ngFor="let gate of board[0]; let i = index"
                         [style.width.px]="circuitStyleOptions.baseSize"
                         [style.height.px]="circuitStyleOptions.timeAxisHeight">
                        <div>{{i}}</div>
                    </div>
                </div>

                <svg #svgElement [attr.width]="getWidth()"
                     [attr.height]="getHeight()">
                    <!-- lines -->
                    <line *ngFor="let qubit of qubits; let i=index" [attr.stroke]="colors.dark"
                          [attr.x1]="0" [attr.y1]="circuitStyleOptions.baseSize * i + circuitStyleOptions.baseSize / 2"
                          [attr.x2]="getWidth()"
                          [attr.y2]="circuitStyleOptions.baseSize * i + circuitStyleOptions.baseSize / 2"></line>

                    <!-- empty gate placeholders-->
                    <g *ngFor="let gates of board; let i=index">
                        <g *ngFor="let gate of gates; let j=index"
                           (drop)="handleDrop($event, i, j)"
                           (dragover)="handleDragOver($event, i, j)"
                           (dragleave)="handleDragLeave()"
                           [attr.data-time]="j"
                           [attr.data-target]="i"
                           class="gate-drop">
                            <rect [class.drag-over-element]="isBeingDraggedOver(i,j)" *ngIf="!gate"
                                  [attr.x]="j * circuitStyleOptions.baseSize"
                                  [attr.y]="i * circuitStyleOptions.baseSize"
                                  [attr.width]="circuitStyleOptions.baseSize"
                                  [attr.height]="circuitStyleOptions.baseSize"
                                  [attr.fill]="colors.light" fill-opacity="0.5"
                            />
                        </g>
                    </g>


                    <!-- gates -->
                    <g *ngFor="let gates of board; let i=index">
                        <g *ngFor="let gate of gates; let j=index" (drop)="handleDrop($event, i, j)"
                           (mousedown)="handleDragStart($event, i, j)"
                           (mousemove)="handleDrag($event)"
                           (mouseup)="handleDragEnd($event)"
                           (mouseleave)="handleDragEnd($event)"
                           (dragover)="handleDragOver($event, i, j)"
                           (dragleave)="handleDragLeave()"
                           [attr.data-time]="j"
                           [attr.data-target]="i"
                           class="gate-drop">
                            <rect [class.drag-over-element]="isBeingDraggedOver(i,j)" *ngIf="gate"
                                  [attr.x]="j * circuitStyleOptions.baseSize + (circuitStyleOptions.baseSize - circuitStyleOptions.gateSize) / 2"
                                  [attr.y]="i * circuitStyleOptions.baseSize + (circuitStyleOptions.baseSize - circuitStyleOptions.gateSize) / 2"
                                  [attr.width]="circuitStyleOptions.gateSize"
                                  [attr.height]="circuitStyleOptions.gateSize"
                                  [attr.rx]="circuitStyleOptions.gateBorderRadius"
                                  [attr.fill]="colors.light" [attr.stroke]="colors.medium"/>
                            <text *ngIf="gate"
                                  class="gate-text"
                                  [attr.x]="(j * circuitStyleOptions.baseSize) + (circuitStyleOptions.baseSize / 2)"
                                  [attr.y]="(i * circuitStyleOptions.baseSize) + (circuitStyleOptions.baseSize / 2)"
                                  dominant-baseline="middle"
                                  text-anchor="middle"
                                  [attr.fill]="colors.dark">{{gate.name}}</text>
                        </g>
                    </g>
                </svg>
            </div>

            <div class="output-container">
                <div class="right-block" [style.height.px]="circuitStyleOptions.timeAxisHeight"></div>
                <div class="output" *ngFor="let output of qubitOutputs" [style.height.px]="circuitStyleOptions.baseSize"
                     [style.width.px]="circuitStyleOptions.baseSize">
                    <button class="output-value">{{output.value}}</button>
                </div>
            </div>

        </div>

    `,
    styleUrls: ["./quantum-circuit-board.component.scss"],
})
export class QuantumCircuitBoardComponent implements OnInit {
    dragOverElement?: GatePos;

    gateBeingDragged: {
        group: SVGGElement;
        gate: GatePos;
        offset: [number, number];
    } | null = null;

    @ViewChild("svgElement")
    svgElement!: ElementRef<SVGSVGElement>;

    @Input()
    circuitStyleOptions!: CircuitStyleOptions;

    @Input()
    board: (Gate | undefined)[][] = [];

    @Input()
    qubits!: Qubit[];

    @Input()
    qubitOutputs!: QubitOutput[];

    @Output()
    qubitChange = new EventEmitter<number>();

    @Output()
    gateDrop = new EventEmitter<GateDrop>();

    @Output()
    gateMove = new EventEmitter<GateMove>();

    constructor() {}

    get colors() {
        return this.circuitStyleOptions.colors;
    }

    getWidth() {
        if (this.board.length > 0) {
            return this.board[0].length * this.circuitStyleOptions.baseSize;
        }

        return 0;
    }

    getHeight() {
        if (this.board.length > 0) {
            return this.board.length * this.circuitStyleOptions.baseSize;
        }
        return 0;
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

    handleDrop(event: DragEvent, i: number, j: number) {
        event.preventDefault();
        const gateName = event.dataTransfer?.getData("text/plain");
        if (gateName === undefined) {
            return;
        }

        this.dragOverElement = undefined;
        this.gateDrop.emit({
            target: i,
            time: j,
            name: gateName,
        });
    }

    handleDragOver(event: DragEvent | MouseEvent, i: number, j: number) {
        this.dragOverElement = {
            target: i,
            time: j,
        };
        event.preventDefault();
    }

    handleDragLeave() {
        this.dragOverElement = undefined;
    }

    isBeingDraggedOver(i: number, j: number) {
        if (!this.dragOverElement) {
            return false;
        }
        return (
            this.dragOverElement.time === j && this.dragOverElement.target === i
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

    handleDragStart(event: MouseEvent, i: number, j: number) {
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
            gate: {target: i, time: j},
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

    handleDragEnd(event: MouseEvent) {
        if (this.gateBeingDragged) {
            const rect = this.gateBeingDragged.group.children.item(0);
            if (!rect || !(rect instanceof SVGRectElement)) {
                console.log("missing rect");
                this.gateBeingDragged = null;
                return;
            }

            const mouseX = event.clientX;
            const mouseY = event.clientY;

            const gates =
                this.svgElement.nativeElement.getElementsByClassName(
                    "gate-drop"
                );
            for (const gate of gates) {
                if (
                    gate instanceof SVGGElement &&
                    gate !== this.gateBeingDragged.group
                ) {
                    const r = gate.getBoundingClientRect();
                    // cursor is inside cell
                    if (
                        r.x <= mouseX &&
                        mouseX <= r.x + r.width &&
                        r.y <= mouseY &&
                        mouseY <= r.y + r.height
                    ) {
                        const target = gate.getAttribute("data-target");
                        const time = gate.getAttribute("data-time");
                        if (target === null || time === null) {
                            return;
                        }
                        const targetNum = parseInt(target, 10);
                        const timeNum = parseInt(time, 10);

                        this.gateMove.emit({
                            from: this.gateBeingDragged.gate,
                            to: {
                                target: targetNum,
                                time: timeNum,
                            },
                        });
                    }
                }
            }

            // find if some cell collides with that rect

            // put this gate to that cell
        }
        this.gateBeingDragged = null;
    }
}
