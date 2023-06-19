/**
 * Quantum circuit board.
 */

import type {OnInit} from "@angular/core";
import {
    Component,
    EventEmitter,
    Input,
    Output,
    ViewChild,
    ElementRef,
} from "@angular/core";
import {CircuitStyleOptions} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {
    Gate,
    Qubit,
    QubitOutput,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";

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
                     [attr.height]="getHeight()"
                     (mousemove)="handleDrag($event)"
                     (mouseup)="handleDragEnd($event)"
                     (mouseleave)="handleDragEnd($event)"
                     (touchmove)="handleDrag($event)"
                     (touchend)="handleDragEnd($event)"
                     (touchcancel)="handleDragEnd($event)"
                >
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
                                  [attr.fill]="colors.light" fill-opacity="0"
                            />
                        </g>
                    </g>

                    <!-- gates -->
                    <g *ngFor="let gates of board; let i=index">
                        <g *ngFor="let gate of gates; let j=index"
                           (drop)="handleDrop($event, i, j)"
                           (mousedown)="handleDragStart($event, i, j)"
                           (touchstart)="handleDragStart($event, i, j)"
                           (dragover)="handleDragOver($event, i, j)"
                           (dragleave)="handleDragLeave()"
                           [attr.data-time]="j"
                           [attr.data-target]="i"
                           class="gate-drop">
                            <rect *ngIf="gate && !isBeingDragged(i, j)"
                                  class="gate"
                                  [class.drag-over-element]="isBeingDraggedOver(i,j)"
                                  [attr.x]="j * circuitStyleOptions.baseSize + (circuitStyleOptions.baseSize - circuitStyleOptions.gateSize) / 2"
                                  [attr.y]="i * circuitStyleOptions.baseSize + (circuitStyleOptions.baseSize - circuitStyleOptions.gateSize) / 2"
                                  [attr.width]="circuitStyleOptions.gateSize"
                                  [attr.height]="circuitStyleOptions.gateSize"
                                  [attr.rx]="circuitStyleOptions.gateBorderRadius"
                                  [attr.fill]="colors.light" [attr.stroke]="colors.medium"/>
                            <text *ngIf="gate && !isBeingDragged(i, j)"
                                  class="gate-text"
                                  [attr.x]="(j * circuitStyleOptions.baseSize) + (circuitStyleOptions.baseSize / 2)"
                                  [attr.y]="(i * circuitStyleOptions.baseSize) + (circuitStyleOptions.baseSize / 2)"
                                  dominant-baseline="middle"
                                  text-anchor="middle"
                                  [attr.fill]="colors.dark">{{gate.name}}</text>
                        </g>


                    </g>

                    <!-- Gate being dragged placed after others so it's on top -->
                    <g *ngIf="gateBeingDragged"
                       (mousedown)="handleDragStart($event, gateBeingDragged.gate.target, gateBeingDragged.gate.time)"
                       (touchstart)="handleDragStart($event, gateBeingDragged.gate.target, gateBeingDragged.gate.time)"
                       [attr.data-time]="gateBeingDragged.gate.time"
                       [attr.data-target]="gateBeingDragged.gate.target"
                       class="gate-drop chosen">
                        <rect
                                class="gate"
                                [attr.x]="gateBeingDragged.gate.time * circuitStyleOptions.baseSize + (circuitStyleOptions.baseSize - circuitStyleOptions.gateSize) / 2"
                                [attr.y]="gateBeingDragged.gate.target * circuitStyleOptions.baseSize + (circuitStyleOptions.baseSize - circuitStyleOptions.gateSize) / 2"
                                [attr.width]="circuitStyleOptions.gateSize"
                                [attr.height]="circuitStyleOptions.gateSize"
                                [attr.rx]="circuitStyleOptions.gateBorderRadius"
                                [attr.fill]="colors.light" [attr.stroke]="colors.medium"/>
                        <text
                                class="gate-text"
                                [attr.x]="(gateBeingDragged.gate.time * circuitStyleOptions.baseSize) + (circuitStyleOptions.baseSize / 2)"
                                [attr.y]="(gateBeingDragged.gate.target * circuitStyleOptions.baseSize) + (circuitStyleOptions.baseSize / 2)"
                                dominant-baseline="middle"
                                text-anchor="middle"
                                [attr.fill]="colors.dark">{{board[gateBeingDragged.gate.target][gateBeingDragged.gate.time]?.name}}</text>
                    </g>
                </svg>
            </div>

            <div class="output-container">
                <div class="right-block" [style.height.px]="circuitStyleOptions.timeAxisHeight"></div>
                <div class="output" *ngFor="let output of qubitOutputs"
                     [style.height.px]="circuitStyleOptions.baseSize">
                    <img alt="measurement icon" src="/static/images/quantum-measurement.svg"
                         [style.height.px]="circuitStyleOptions.gateSize"
                         [style.width.px]="circuitStyleOptions.gateSize"/>
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

    @Output()
    gateRemove = new EventEmitter<GatePos>();

    constructor() {}

    get colors() {
        return this.circuitStyleOptions.colors;
    }

    isBeingDragged(i: number, j: number) {
        return (
            this.gateBeingDragged?.gate.target === i &&
            this.gateBeingDragged.gate.time === j
        );
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

    getActiveGroup(): SVGGElement | null {
        const res =
            this.svgElement.nativeElement.getElementsByClassName("chosen")[0];
        if (res instanceof SVGGElement) {
            return res;
        }
        return null;
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

    addStartTransform(group: SVGGElement) {
        // Get all the transforms currently on this element
        const transforms = group.transform.baseVal;

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
    }

    getCursorPosition(event: MouseEvent | TouchEvent): [number, number] {
        if (event instanceof MouseEvent) {
            return [event.clientX, event.clientY];
        }
        return [
            event.changedTouches[0].clientX,
            event.changedTouches[0].clientY,
        ];
    }

    handleDragStart(event: MouseEvent | TouchEvent, i: number, j: number) {
        event.preventDefault();
        if (!event.currentTarget) {
            return;
        }
        const [cursorX, cursorY] = this.getCursorPosition(event);
        const offset = this.getMousePosition(cursorX, cursorY);
        if (!offset) {
            return;
        }

        const group = event.currentTarget as SVGGElement;

        // Get all the transforms currently on this element
        const transforms = group.transform.baseVal;

        this.gateBeingDragged = {
            gate: {target: i, time: j},
            offset: offset,
        };

        this.addStartTransform(group);

        // Get initial translation amount
        const transform = transforms.getItem(0);
        this.gateBeingDragged.offset[0] -= transform.matrix.e;
        this.gateBeingDragged.offset[1] -= transform.matrix.f;
    }

    handleDrag(event: MouseEvent | TouchEvent) {
        event.preventDefault();

        const group = this.getActiveGroup();
        if (this.gateBeingDragged && group) {
            // dragged group is added to top of list so reinitialize transform
            this.addStartTransform(group);

            const [cursorX, cursorY] = this.getCursorPosition(event);
            // move the element to where cursor is
            const xy = this.getMousePosition(cursorX, cursorY);
            if (!xy) {
                return;
            }
            const [x, y] = xy;
            const [offsetX, offsetY] = this.gateBeingDragged.offset;

            const transform = group.transform.baseVal.getItem(0);
            transform.setTranslate(x - offsetX, y - offsetY);

            // highlight gate or empty cell that moved gate is on top of
            const colliding = this.getColliding(cursorX, cursorY);
            if (colliding) {
                this.dragOverElement = colliding;
            }
        }
    }

    getColliding(mouseX: number, mouseY: number): GatePos | undefined {
        if (!this.gateBeingDragged) {
            return undefined;
        }
        const group = this.getActiveGroup();
        if (!group) {
            return undefined;
        }
        const rect = group.children.item(0);
        if (!rect || !(rect instanceof SVGRectElement)) {
            return undefined;
        }

        const gates =
            this.svgElement.nativeElement.getElementsByClassName("gate-drop");
        for (const gate of gates) {
            if (gate instanceof SVGGElement && gate !== group) {
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
                        return undefined;
                    }
                    const targetNum = parseInt(target, 10);
                    const timeNum = parseInt(time, 10);

                    return {
                        target: targetNum,
                        time: timeNum,
                    };
                }
            }
        }
        return undefined;
    }

    handleDragEnd(event: MouseEvent | TouchEvent) {
        event.preventDefault();
        if (!this.gateBeingDragged) {
            this.gateBeingDragged = null;
            return;
        }
        const group = this.getActiveGroup();
        if (!group) {
            this.gateBeingDragged = null;
            return;
        }
        const rect = group.children.item(0);
        if (!rect || !(rect instanceof SVGRectElement)) {
            this.gateBeingDragged = null;
            return;
        }

        // if cursor is outside the circuit (svg element) then remove the gate being dragged
        const svgBounds = this.svgElement.nativeElement.getBoundingClientRect();
        const [cursorX, cursorY] = this.getCursorPosition(event);
        if (
            cursorX < svgBounds.x ||
            cursorX > svgBounds.right ||
            cursorY < svgBounds.y ||
            cursorY > svgBounds.bottom
        ) {
            this.gateRemove.emit(this.gateBeingDragged.gate);
            this.gateBeingDragged = null;
            this.dragOverElement = undefined;
            return;
        }

        const colliding = this.getColliding(cursorX, cursorY);

        if (colliding) {
            this.gateMove.emit({
                from: this.gateBeingDragged.gate,
                to: colliding,
            });
        }

        this.gateBeingDragged = null;
        this.dragOverElement = undefined;
    }
}
