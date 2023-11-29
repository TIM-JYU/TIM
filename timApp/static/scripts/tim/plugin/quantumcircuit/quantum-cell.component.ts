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
    QuantumBoard,
    Swap,
} from "tim/plugin/quantumcircuit/quantum-board";
import {CircuitOptions} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {Color} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import {GateService} from "tim/plugin/quantumcircuit/gate.service";

@Component({
    selector: "[tim-quantum-cell]",
    template: `
        <!--suppress HtmlUnknownAttribute -->
        <!-- Empty gate placeholder or backdrop for any other element -->
        <svg:rect
                  [class.drag-over-element]="isBeingDraggedOver"
                  [class.gate]="cell?.editable"
                  [attr.x]="x"
                  [attr.y]="y"
                  [attr.width]="cellWidth"
                  [attr.height]="backGroundHeight"
                  [attr.fill]="circuitOptions.colors.light" fill-opacity="0"
        ></svg:rect>

        <!-- normal gate-->
        <svg:rect *ngIf="cell|instanceof: Gate as g"
                  [style.stroke]="isSelected ? color.selection : color.fill"
                  [style.stroke-width]="isSelected ? 2 : 1"
                  [class.gate]="g.editable"
                  [class.drag-over-element]="isBeingDraggedOver"
                  [attr.x]="gx"
                  [attr.y]="gy"
                  [attr.width]="gw"
                  [attr.height]="gh"
                  [attr.rx]="rx"
                  [attr.fill]="color.fill"></svg:rect>

        <svg:text *ngIf="cell|instanceof: Gate as g"
                  class="gate-text"
                  [attr.x]="cx"
                  [attr.y]="cy"
                  dominant-baseline="middle"
                  text-anchor="middle"
                  [attr.fill]="color.text"
                  [attr.stroke]="color.text">{{g.name}}</svg:text>

        <!-- control gate -->
        <svg:circle *ngIf="cell|instanceof: Control as c"
                    [class.gate]="c.editable"
                    [class.drag-over-element]="isBeingDraggedOver"
                    [attr.cx]="cx"
                    [attr.cy]="cy"
                    [attr.r]="circuitOptions.gateSize/4"
                    [attr.fill]="color.fill"
                    [attr.stroke]="circuitOptions.colors.dark"></svg:circle>

        <!-- Swap gate -->
        <svg:text *ngIf="cell|instanceof: Swap as s"
                  [class.gate]="s.editable"
                  [class.drag-over-element]="isBeingDraggedOver"
                  [attr.x]="cx"
                  [attr.y]="cy"
                  [attr.stroke]="circuitOptions.colors.dark"
                  [attr.font-size]="circuitOptions.gateSize / 2"
                  dominant-baseline="central" text-anchor="middle">X
        </svg:text>

        <!-- MultiQubit gate -->
        <svg:rect *ngIf="cell|instanceof: MultiQubitGate as g"
                  [class.gate]="g.editable"
                  [class.drag-over-element]="isBeingDraggedOver"
                  [attr.x]="gx"
                  [attr.y]="gy"
                  [attr.width]="gw"
                  [attr.height]="gh"
                  [attr.rx]="rx"
                  [attr.fill]="color.fill"
                  [attr.stroke]="circuitOptions.colors.medium"></svg:rect>
        <svg:text *ngIf="cell|instanceof: MultiQubitGate as g"
                  class="gate-text"
                  [attr.x]="cx"
                  [attr.y]="cy"
                  dominant-baseline="middle"
                  text-anchor="middle"
                  [attr.fill]="color.text"
                  [attr.stroke]="circuitOptions.colors.dark">{{g.name}}</svg:text>


    `,
    styleUrls: ["./quantum-circuit-board.component.scss"],
})
export class QuantumCellComponent implements OnInit, AfterViewInit, OnChanges {
    protected readonly Gate = Gate;
    protected readonly Control = Control;
    protected readonly Swap = Swap;
    protected readonly MultiQubitGate = MultiQubitGate;

    @Input()
    cell!: Cell;

    @Input()
    board!: QuantumBoard;

    @Input()
    circuitOptions!: CircuitOptions;

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

    cellWidth!: number;

    x!: number;
    y!: number;

    gw!: number;
    gh!: number;

    color!: Color;

    backGroundHeight!: number;

    rx!: number;

    constructor(private gateService: GateService) {}

    updateSizes() {
        const baseSize = this.circuitOptions.baseSize;
        const gateSize = this.circuitOptions.gateSize;
        let baseWidth = this.circuitOptions.baseWidth;
        let gateWidth = this.circuitOptions.gateWidth;
        let x = this.time * baseWidth;
        const y = this.target * baseSize;
        if (this.circuitOptions.columnWidths) {
            const w = this.circuitOptions.columnWidths[this.time];
            baseWidth = w;
            gateWidth = 0.8 * w;
            x = 0;
            for (let i = 0; i < this.time; i++) {
                x += this.circuitOptions.columnWidths[i];
            }
        }
        this.x = x;
        this.y = y;

        this.gx = x + (baseWidth - gateWidth) / 2;
        this.gy = y + (baseSize - gateSize) / 2;

        this.gw = gateWidth;
        this.gh = gateSize;

        if (this.cell instanceof MultiQubitGate) {
            this.gh = baseSize * this.cell.size - (baseSize - gateSize);
            this.backGroundHeight = baseSize * this.cell.size;
        } else {
            this.backGroundHeight = baseSize;
        }

        this.cx = this.x + baseWidth / 2;
        this.cy = this.y + baseSize / 2;

        this.cellWidth = baseWidth;

        if (this.cell?.editable === false) {
            this.rx = 0;
        } else {
            this.rx = this.circuitOptions.gateBorderRadius;
        }
    }

    ngAfterViewInit(): void {
        this.updateSizes();
    }

    assignColor() {
        let groupColor;
        let serviceGate;
        if (this.cell instanceof Gate || this.cell instanceof MultiQubitGate) {
            const group = this.gateService.getGateGroup(this.cell.name);
            serviceGate = this.gateService.getGate(this.cell.name);
            groupColor = this.circuitOptions.gateColors.get(group ?? "");
        } else if (this.cell instanceof Control) {
            const targetCell = this.board.get(this.cell.target, this.time);
            // use same color for control as its target
            if (
                targetCell instanceof Gate ||
                targetCell instanceof MultiQubitGate ||
                targetCell instanceof Swap
            ) {
                const name =
                    targetCell instanceof Swap ? "swap" : targetCell.name;
                const targetGroup = this.gateService.getGateGroup(name);
                groupColor = this.circuitOptions.gateColors.get(
                    targetGroup ?? ""
                );
                serviceGate = this.gateService.getGate(name);
            }
        } else if (this.cell instanceof Swap) {
            const group = this.gateService.getGateGroup("swap");
            groupColor = this.circuitOptions.gateColors.get(group ?? "");
        }
        this.color = {
            fill: serviceGate?.color ?? groupColor?.fill ?? "white",
            text: serviceGate?.textColor ?? groupColor?.text ?? "black",
            selection: groupColor?.selection ?? "yellow",
        };
    }

    ngOnInit(): void {
        this.assignColor();
    }

    /**
     * Update size
     * @param changes possible change to sizes
     */
    ngOnChanges(changes: SimpleChanges): void {
        this.updateSizes();
    }
}
