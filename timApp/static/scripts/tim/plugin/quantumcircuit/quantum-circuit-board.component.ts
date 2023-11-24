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
import {CircuitOptions} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {QubitOutput} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import {
    Control,
    QuantumBoard,
    Swap,
} from "tim/plugin/quantumcircuit/quantum-board";
import type {Qubit} from "tim/plugin/quantumcircuit/qubit";

/**
 * Position on circuit board.
 */
export interface GatePos {
    time: number;
    target: number;
}

/**
 * Movement from gate position to another.
 */
export interface GateMove {
    from: GatePos;
    to: GatePos;
}

/**
 * Gate is dropped on some position on board. Dropped gate is identified by name.
 */
export interface GateDrop {
    time: number;
    target: number;
    name: string;
}

// gate that is currently being dragged
export interface GateBeingDragged {
    gate: GatePos;
    offset: [number, number];
    originalBounds: DOMRect;
}

@Component({
    selector: "tim-quantum-circuit-board",
    template: `
        <!--suppress HtmlUnknownAttribute -->

        <div class="circuit-container">
            <div class="qubits">
                <div class="left-block" [style.height.px]="circuitOptions.timeAxisHeight">
                    <div [ngSwitch]="middleAxisLabel">
                        <div *ngSwitchCase="'Step'" class="axis-text-time">
                            <ng-container i18n>Step</ng-container>
                        </div>
                        <div *ngSwitchDefault class="axis-text-time">
                            {{middleAxisLabel}}
                        </div>
                    </div>

                    <div [ngSwitch]="leftAxisLabel">
                        <div *ngSwitchCase="'Qubit'" class="axis-text-qubit">
                            <ng-container i18n>Qubit</ng-container>
                        </div>
                        <div *ngSwitchCase="'Bit'" class="axis-text-qubit">
                            <ng-container i18n>Bit</ng-container>
                        </div>
                        <div *ngSwitchDefault class="axis-text-qubit">
                            {{leftAxisLabel}}
                        </div>
                    </div>
                </div>
                <div *ngFor="let qubit of qubits; let i=index"
                     [style.height.px]="circuitOptions.baseSize"
                     [class.qubit-editable]="qubit.editable"
                     [class.qubit-uneditable]="!qubit.editable"
                     class="qubit">
                    <div class="qubit-name" (click)="toggleQubit(i)">{{qubit.name}}</div>
                    <button class="qubit-toggle-button"
                            (click)="toggleQubit(i)">{{qubit.text}}</button>
                </div>
            </div>

            <div class="circuit-right-container">
                <div class="moments">
                    <div class="moment" *ngFor="let w of circuitOptions.columnWidths; let i = index"
                         [style.width.px]="w"
                         [style.height.px]="circuitOptions.timeAxisHeight">
                        <div>{{i+1}}</div>
                    </div>
                </div>

                <svg #svgElement [attr.width]="circuitOptions.columnWidthsSum"
                     [attr.height]="board.nQubits * circuitOptions.baseSize"
                     (mousemove)="handleDrag($event)"
                     (mouseup)="handleDragEnd($event)"
                     (mouseleave)="handleDragEnd($event)"
                     (touchmove)="handleDrag($event)"
                     (touchend)="handleDragEnd($event)"
                     (touchcancel)="handleDragEnd($event)"
                >
                    <!-- horizontal lines -->
                    <line *ngFor="let qubit of qubits; let i=index" [attr.stroke]="circuitOptions.colors.dark"
                          [attr.x1]="0" [attr.y1]="circuitOptions.baseSize * i + circuitOptions.baseSize / 2"
                          [attr.x2]="circuitOptions.columnWidthsSum"
                          [attr.y2]="circuitOptions.baseSize * i + circuitOptions.baseSize / 2"></line>

                    <!-- wires -->
                    <g *ngFor="let gates of board.board; let i=index">
                        <g *ngFor="let gate of gates; let j=index">
                            <line *ngIf="gate|instanceof: Control as c"
                                  [attr.stroke]="circuitOptions.colors.dark"
                                  [attr.x1]="(circuitOptions.columnWidthSums ? circuitOptions.columnWidthSums[j] : 0) - (circuitOptions.columnWidths ? circuitOptions.columnWidths[j] : 0) / 2"
                                  [attr.y1]="i * circuitOptions.baseSize + circuitOptions.baseSize / 2"
                                  [attr.x2]="(circuitOptions.columnWidthSums ? circuitOptions.columnWidthSums[j] : 0) - (circuitOptions.columnWidths ? circuitOptions.columnWidths[j] : 0) / 2"
                                  [attr.y2]="c.target * circuitOptions.baseSize + circuitOptions.baseSize / 2 + circuitOptions.gateSize / 2"
                            ></line>

                            <g *ngIf="gate|instanceof: Swap as s">
                                <line *ngIf="s.target > i"
                                      [attr.stroke]="circuitOptions.colors.dark"
                                      [attr.x1]="(circuitOptions.columnWidthSums ? circuitOptions.columnWidthSums[j] : 0) - (circuitOptions.columnWidths ? circuitOptions.columnWidths[j] : 0) / 2"
                                      [attr.y1]="i * circuitOptions.baseSize + circuitOptions.baseSize / 2"
                                      [attr.x2]="(circuitOptions.columnWidthSums ? circuitOptions.columnWidthSums[j] : 0) - (circuitOptions.columnWidths ? circuitOptions.columnWidths[j] : 0) / 2"
                                      [attr.y2]="s.target * circuitOptions.baseSize + circuitOptions.baseSize / 2"
                                ></line>
                            </g>
                        </g>
                    </g>

                    <g *ngFor="let gates of board.board; let i = index">
                        <g *ngFor="let gate of gates; let j=index" tim-quantum-cell
                           [circuitOptions]="circuitOptions"
                           [cell]="gate"
                           [board]="board"
                           [isSelected]="selectedGate !== null && i === selectedGate.target && j ===
                            selectedGate.time"
                           [isBeingDraggedOver]="dragOverElement !== null && dragOverElement.time === j &&
                            dragOverElement.target === i"
                           [target]="i"
                           [time]="j"
                           (drop)="handleDrop($event, i, j)"
                           (dragenter)="handleDragEnter($event)"
                           (mousedown)="handleDragStart($event, i, j)"
                           (touchstart)="handleDragStart($event, i, j)"
                           (dragover)="handleDragOver($event, i, j)"
                           (dragleave)="handleDragLeave(i, j)"
                           [attr.data-time]="j"
                           [attr.data-target]="i"
                           class="gate-drop"
                           [class.uneditable]="gate !== undefined && gate?.editable === false"
                           [class.chosen]="gateBeingDragged !== null && gateBeingDragged.gate.target === i && gateBeingDragged.gate.time === j">
                        </g>

                    </g>
                </svg>
            </div>

            <div class="output-container">
                <div class="right-block" [style.height.px]="circuitOptions.timeAxisHeight">
                    <div [ngSwitch]="middleAxisLabel">
                        <div *ngSwitchCase="'Step'" class="out-axis-time">
                            <ng-container i18n>Step</ng-container>
                        </div>
                        <div *ngSwitchDefault class="out-axis-time">
                            {{middleAxisLabel}}
                        </div>
                    </div>
                    
                    <div [ngSwitch]="rightAxisLabel">
                        <div *ngSwitchCase="'Output'" class="out-axis-text">
                            <ng-container i18n>Output</ng-container>
                        </div>
                        <div *ngSwitchDefault class="out-axis-text">
                            {{rightAxisLabel}}
                        </div>
                    </div>
                    
                </div>
                <div class="output" *ngFor="let output of qubitOutputs"
                     [style.height.px]="circuitOptions.baseSize">
                    <img alt="measurement icon" src="/static/images/quantum-measurement.svg"
                         [style.height.px]="circuitOptions.gateSize"
                         [style.width.px]="circuitOptions.gateSize"/>

                    <svg *ngIf="showOutputBits" class="output-value"
                         [attr.height]="circuitOptions.gateSize"
                         [attr.width]="circuitOptions.gateSize">
                        <rect [attr.height]="circuitOptions.gateSize"
                              [attr.width]="circuitOptions.gateSize"
                              [attr.stroke]="circuitOptions.colors.dark"
                              x="0" y="0"
                              fill="white"
                        ></rect>
                        <rect [attr.height]="circuitOptions.gateSize"
                              [attr.width]="circuitOptions.gateSize * (output.probability/100)"
                              x="0" y="0"
                              fill="aqua"
                        ></rect>
                        <text dominant-baseline="middle"
                              text-anchor="middle"
                              x="50%"
                              y="50%"
                              [attr.stroke]="circuitOptions.colors.dark">{{output.value}}</text>
                        <title>{{output.probabilityText}}</title>
                    </svg>
                    <div *ngIf="output.name" class="output-name">
                        {{output.name}}
                    </div>
                </div>
            </div>

        </div>

    `,
    styleUrls: ["./quantum-circuit-board.component.scss"],
})
export class QuantumCircuitBoardComponent implements OnInit {
    protected readonly Control = Control;
    protected readonly Swap = Swap;

    // Cell in board that is being dragged over
    dragOverElement: GatePos | null = null;

    // gate that is currently being dragged
    gateBeingDragged: GateBeingDragged | null = null;

    @ViewChild("svgElement")
    svgElement!: ElementRef<SVGSVGElement>;

    @Input()
    circuitOptions!: CircuitOptions;

    @Input()
    board!: QuantumBoard;

    @Input()
    qubits!: Qubit[];

    @Input()
    selectedGate: GatePos | null = null;

    @Input()
    qubitOutputs!: QubitOutput[];

    @Input()
    showOutputBits: boolean = true;

    @Input()
    leftAxisLabel!: string;

    @Input()
    rightAxisLabel!: string;

    @Input()
    middleAxisLabel!: string;

    @Output()
    qubitChange = new EventEmitter<number>();

    @Output()
    gateDrop = new EventEmitter<GateDrop>();

    @Output()
    gateMove = new EventEmitter<GateMove>();

    @Output()
    gateRemove = new EventEmitter<GatePos>();

    @Output()
    gateSelect = new EventEmitter<GatePos>();

    constructor() {}

    ngOnInit(): void {}

    /**
     * The value of qubit changed.
     * @param id identifier for qubit
     */
    toggleQubit(id: number) {
        this.qubitChange.emit(id);
    }

    /**
     * Handle drop event on gate on board.
     * @param event drop on this element
     * @param target qubit of the cell that drop happened on
     * @param time time of the cell that drop happened on
     */
    handleDrop(event: DragEvent, target: number, time: number) {
        event.preventDefault();
        if (
            target === this.gateBeingDragged?.gate.target &&
            time === this.gateBeingDragged.gate.time
        ) {
            return;
        }
        const gateName = event.dataTransfer?.getData("text/plain");
        if (gateName === undefined) {
            return;
        }

        this.dragOverElement = null;
        this.gateDrop.emit({
            target: target,
            time: time,
            name: gateName,
        });
    }

    /**
     * Sets the element being dragged over.
     * @param event event fired when cell is being dragged on
     * @param target
     * @param time
     */
    handleDragOver(
        event: DragEvent | MouseEvent,
        target: number,
        time: number
    ) {
        if (
            target === this.gateBeingDragged?.gate.target &&
            time === this.gateBeingDragged.gate.time
        ) {
            return;
        }
        this.dragOverElement = {
            target: target,
            time: time,
        };
        event.preventDefault();
    }

    /**
     * Remove drag over status from element after drag leaves it.
     */
    handleDragLeave(target: number, time: number) {
        if (
            target === this.gateBeingDragged?.gate.target &&
            time === this.gateBeingDragged.gate.time
        ) {
            return;
        }
        this.dragOverElement = null;
    }

    /**
     * Get the group svg element of the gate that is being dragged dynamically.
     */
    getActiveGroup(): SVGGElement | null {
        const res =
            this.svgElement.nativeElement.getElementsByClassName("chosen")[0];
        if (res instanceof SVGGElement) {
            return res;
        }
        return null;
    }

    /**
     * Gets the position of the mouse in svg coordinates.
     * https://www.petercollingridge.co.uk/tutorials/svg/interactive/dragging/
     * @param x x-coordinate of mouse on page
     * @param y y-coordinate of mouse on page
     */
    getMousePosition(x: number, y: number): [number, number] | null {
        const CTM = this.svgElement.nativeElement.getScreenCTM();
        if (!CTM) {
            return null;
        }
        return [(x - CTM.e) / CTM.a, (y - CTM.f) / CTM.d];
    }

    /**
     * Adds initial transform to transform list if needed, so it isn't empty when it's value is needed.
     * @param group group being transformed
     */
    addInitialTransform(group: SVGGElement) {
        // Get all the transforms currently on this element
        const transforms = group.transform.baseVal;

        // Ensure the first transform is a translation transform
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

    /**
     * Gets cursor position on page from either touch device or mouse.
     * @param event touch or mouse event
     */
    getCursorPosition(event: MouseEvent | TouchEvent): [number, number] {
        if (event instanceof MouseEvent) {
            return [event.clientX, event.clientY];
        }
        return [
            event.changedTouches[0].clientX,
            event.changedTouches[0].clientY,
        ];
    }

    handleDragEnter(event: MouseEvent | TouchEvent) {
        event.preventDefault();
    }

    /**
     * Initialize dragging of gate.
     * @param event initial click or touch on a gate
     * @param target cell's qubit that drag started on
     * @param time cell's time that drag started on
     */
    handleDragStart(
        event: MouseEvent | TouchEvent,
        target: number,
        time: number
    ) {
        event.preventDefault();
        if (!event.currentTarget) {
            return;
        }
        if (this.board.get(target, time) === undefined) {
            this.gateSelect.emit({target: target, time: time});
            return;
        }
        if (this.board.get(target, time)?.editable === false) {
            this.gateSelect.emit({target: target, time: time});
            return;
        }
        if (
            target === this.gateBeingDragged?.gate.target &&
            time === this.gateBeingDragged.gate.time
        ) {
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
            gate: {target: target, time: time},
            offset: offset,
            originalBounds: group.getBoundingClientRect(),
        };

        this.addInitialTransform(group);

        // Get initial translation amount
        const transform = transforms.getItem(0);
        this.gateBeingDragged.offset[0] -= transform.matrix.e;
        this.gateBeingDragged.offset[1] -= transform.matrix.f;
    }

    /**
     * Update the position of the dragged element to where mouse is.
     * @param event movement on screen event
     */
    handleDrag(event: MouseEvent | TouchEvent) {
        const group = this.getActiveGroup();
        if (this.gateBeingDragged && group) {
            // dragged group is added to top of list so reinitialize transform
            this.addInitialTransform(group);

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

    /**
     * Find the cell that mouse is on.
     * @param mouseX the x-coordinate of mouse
     * @param mouseY the y-coordinate of mouse
     */
    getColliding(mouseX: number, mouseY: number): GatePos | undefined {
        if (!this.gateBeingDragged) {
            return undefined;
        }
        const group = this.getActiveGroup();
        if (!group) {
            return undefined;
        }

        // Iterate over all cell's and check if mouse position is inside that cell
        // Notice that this is quite slow but there aren't that many cells, so it should be fast enough.
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

    /**
     * Handle all drag ending actions.
     * Remove gate that was dragged if it went outside the board
     * Replace cell that was dropped on with the gate that was dragged
     * @param event drag ending event
     */
    handleDragEnd(event: MouseEvent | TouchEvent) {
        event.preventDefault();
        if (!this.gateBeingDragged) {
            this.gateBeingDragged = null;
            this.dragOverElement = null;
            return;
        }

        const group = this.getActiveGroup();
        if (!group) {
            this.gateBeingDragged = null;
            this.dragOverElement = null;
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
            this.dragOverElement = null;
            return;
        }

        // cursor is inside same element as when started dragging
        const groupBounds = this.gateBeingDragged.originalBounds;
        if (
            groupBounds.x <= cursorX &&
            cursorX <= groupBounds.right &&
            groupBounds.y <= cursorY &&
            cursorY <= groupBounds.bottom
        ) {
            this.gateSelect.emit(this.gateBeingDragged.gate);
            this.gateBeingDragged = null;
            this.dragOverElement = null;
            return;
        }

        // Replace cell if cursor is in any
        const colliding = this.getColliding(cursorX, cursorY);

        if (colliding) {
            this.gateMove.emit({
                from: this.gateBeingDragged.gate,
                to: colliding,
            });
        }

        this.gateBeingDragged = null;
        this.dragOverElement = null;
    }
}
