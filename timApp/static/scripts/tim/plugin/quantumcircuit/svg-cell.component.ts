import type {
    AfterViewInit,
    OnChanges,
    OnInit,
    SimpleChanges,
} from "@angular/core";
import {Component, Input} from "@angular/core";
import {
    Cell,
    Control,
    Gate,
    MultiQubitGate,
    Swap,
} from "tim/plugin/quantumcircuit/quantum-board";
import type {Color} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import {CircuitStyleOptions} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import {GateService} from "tim/plugin/quantumcircuit/gate.service";

@Component({
    selector: "[tim-svg-cell]",
    template: `
        <!--suppress HtmlUnknownAttribute -->
        <!-- Empty gate placeholder or backdrop for any other element -->
        <svg:rect
                  [class.drag-over-element]="isBeingDraggedOver"
                  [class.gate]="cell !== undefined"
                  [attr.x]="x"
                  [attr.y]="y"
                  [attr.width]="circuitStyleOptions.baseSize"
                  [attr.height]="backGroundHeight"
                  [attr.fill]="circuitStyleOptions.colors.light" fill-opacity="0"
        ></svg:rect>

        <!-- normal gate-->
        <svg:rect *ngIf="cell|instanceof: Gate as g"
                  [style.stroke]="isSelected ? color.selection : color.fill"
                  [style.stroke-width]="isSelected ? 2 : 1"
                  class="gate"
                  [class.drag-over-element]="isBeingDraggedOver"
                  [attr.x]="gx"
                  [attr.y]="gy"
                  [attr.width]="gw"
                  [attr.height]="gh"
                  [attr.rx]="circuitStyleOptions.gateBorderRadius"
                  [attr.fill]="color.fill"></svg:rect>

        <svg:text *ngIf="cell|instanceof: Gate as g"
                  class="gate-text"
                  [attr.x]="cx"
                  [attr.y]="cy"
                  dominant-baseline="middle"
                  text-anchor="middle"
                  [attr.stroke]="color.text">{{g.name}}</svg:text>

        <!-- control gate -->
        <svg:circle *ngIf="cell|instanceof: Control as c"
                    class="gate"
                    [class.drag-over-element]="isBeingDraggedOver"
                    [attr.cx]="cx"
                    [attr.cy]="cy"
                    [attr.r]="circuitStyleOptions.gateSize/4"
                    [attr.fill]="circuitStyleOptions.colors.dark"
                    [attr.stroke]="circuitStyleOptions.colors.dark"></svg:circle>

        <!-- Swap gate -->
        <svg:text *ngIf="cell|instanceof: Swap as s"
                  class="gate"
                  [class.drag-over-element]="isBeingDraggedOver"
                  [attr.x]="cx"
                  [attr.y]="cy"
                  [attr.stroke]="circuitStyleOptions.colors.dark"
                  [attr.font-size]="circuitStyleOptions.gateSize / 2"
                  dominant-baseline="central" text-anchor="middle">X
        </svg:text>

        <!-- MultiQubit gate -->
        <svg:rect *ngIf="cell|instanceof: MultiQubitGate as g"
                  class="gate"
                  [class.drag-over-element]="isBeingDraggedOver"
                  [attr.x]="gx"
                  [attr.y]="gy"
                  [attr.width]="gw"
                  [attr.height]="gh"
                  [attr.rx]="circuitStyleOptions.gateBorderRadius"
                  [attr.fill]="circuitStyleOptions.colors.light"
                  [attr.stroke]="circuitStyleOptions.colors.medium"></svg:rect>
        <svg:text *ngIf="cell|instanceof: MultiQubitGate as g"
                  class="gate-text"
                  [attr.x]="cx"
                  [attr.y]="cy"
                  dominant-baseline="middle"
                  text-anchor="middle"
                  [attr.stroke]="circuitStyleOptions.colors.dark">{{g.name}}</svg:text>


    `,
    styleUrls: ["./quantum-circuit-board.component.scss"],
})
export class SvgCellComponent implements OnInit, AfterViewInit, OnChanges {
    protected readonly Gate = Gate;
    protected readonly Control = Control;
    protected readonly Swap = Swap;
    protected readonly MultiQubitGate = MultiQubitGate;

    @Input()
    cell!: Cell;

    @Input()
    circuitStyleOptions!: CircuitStyleOptions;

    @Input()
    target!: number;

    @Input()
    time!: number;

    @Input()
    isBeingDraggedOver: boolean = false;

    @Input()
    isSelected!: boolean;

    cx!: number;
    cy!: number;
    gx!: number;
    gy!: number;

    x!: number;
    y!: number;

    gw!: number;
    gh!: number;

    color!: Color;

    backGroundHeight!: number;

    constructor(private gateService: GateService) {}

    updateSizes() {
        const baseSize = this.circuitStyleOptions.baseSize;
        const gateSize = this.circuitStyleOptions.gateSize;
        const x = this.time * baseSize;
        const y = this.target * baseSize;
        this.x = x;
        this.y = y;

        this.gx = x + (baseSize - gateSize) / 2;
        this.gy = y + (baseSize - gateSize) / 2;

        this.gw = gateSize;
        this.gh = gateSize;

        if (this.cell instanceof MultiQubitGate) {
            this.gh = baseSize * this.cell.size - (baseSize - gateSize);
            this.backGroundHeight = baseSize * this.cell.size;
        } else {
            this.backGroundHeight = baseSize;
        }

        this.cx = this.x + baseSize / 2;
        this.cy = this.y + baseSize / 2;
    }

    ngAfterViewInit(): void {
        this.updateSizes();
    }

    assignColor() {
        let color;
        if (this.cell instanceof Gate || this.cell instanceof MultiQubitGate) {
            const group = this.gateService.getGateGroup(this.cell.name);
            color = this.circuitStyleOptions.gateColors.get(group ?? "");
        } else if (this.cell instanceof Control) {
            const group = this.gateService.getGateGroup("control");
            color = this.circuitStyleOptions.gateColors.get(group ?? "");
        } else if (this.cell instanceof Swap) {
            const group = this.gateService.getGateGroup("swap");
            color = this.circuitStyleOptions.gateColors.get(group ?? "");
        }
        if (!color) {
            this.color = {
                fill: "white",
                text: "black",
                selection: "yellow",
            };
        } else {
            this.color = color;
        }
    }

    ngOnInit(): void {
        this.assignColor();
    }

    /**
     * Update size
     * @param changes possible change to sizes
     */
    ngOnChanges(changes: SimpleChanges): void {
        if (changes.cell || changes.circuitStyleOptions) {
            this.updateSizes();
        }
    }
}
