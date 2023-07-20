import type {OnChanges, OnDestroy, OnInit, SimpleChanges} from "@angular/core";
import {Component, Input, ViewChild, ElementRef} from "@angular/core";
import type {Color} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import {CircuitStyleOptions} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {ServiceGate} from "tim/plugin/quantumcircuit/gate.service";
import {GateService} from "tim/plugin/quantumcircuit/gate.service";
import type {Subscription} from "rxjs";

interface MenuGate {
    color: Color;
    name: string;
    description: string;
}

@Component({
    selector: "tim-quantum-gate-menu",
    template: `
        <!--suppress HtmlUnknownAttribute -->

        <div class="gate-container" *ngIf="gates.length > 0">
            <p class="gate-container-heading">Raahaa portti piiriin tästä</p>

            <div class="gate-list" #gateListElement
                 [ngStyle]="{'grid-template-columns': colsStyle}"
            >
                <div class="svg-container" *ngFor="let gate of gates" draggable="true"
                     [title]="gate.description"
                     (dragstart)="handleDragStart($event, gate.name)">
                    <div [ngSwitch]="gate.name">
                        <svg *ngSwitchCase="'control'" 
                             [attr.width]="circuitStyleOptions.gateSize"
                             [attr.height]="circuitStyleOptions.gateSize"
                        >
                            <circle [attr.fill]="gate.color.fill"
                                    [attr.cx]="circuitStyleOptions.gateSize/2"
                                    [attr.cy]="circuitStyleOptions.gateSize/2"
                                    [attr.r]="circuitStyleOptions.gateSize/4"/>
                        </svg>

                        <svg *ngSwitchCase="'swap'" [attr.width]="circuitStyleOptions.gateSize"
                             [attr.height]="circuitStyleOptions.gateSize"
                             class="swap-gate">
                            <text x="50%" y="25%"
                                  [attr.fill]="gate.color.fill"
                                  [attr.stroke]="circuitStyleOptions.colors.dark"
                                  dominant-baseline="middle" text-anchor="middle"
                                  [attr.font-size]="circuitStyleOptions.gateSize / 3">X
                            </text>
                            <line [attr.stroke]="circuitStyleOptions.colors.dark" stroke-width="2" x1="50%" x2="50%"
                                  y1="25%"
                                  y2="75%"></line>
                            <text x="50%" y="75%"
                                  [attr.fill]="gate.color.text"
                                  [attr.stroke]="circuitStyleOptions.colors.dark"
                                  dominant-baseline="middle" text-anchor="middle"
                                  [attr.font-size]="circuitStyleOptions.gateSize / 3">X
                            </text>
                        </svg>

                        <svg *ngSwitchDefault [attr.width]="circuitStyleOptions.gateSize"
                             [attr.height]="circuitStyleOptions.gateSize">
                            <rect [attr.x]="0" [attr.y]="0" [attr.width]="circuitStyleOptions.gateSize"
                                  [attr.height]="circuitStyleOptions.gateSize"
                                  [attr.fill]="gate.color.fill"
                                  [attr.stroke]="circuitStyleOptions.colors.dark"
                                  rx="2"/>
                            <text x="50%" y="50%"
                                  [attr.stroke]="gate.color.text"
                                  dominant-baseline="middle"
                                  text-anchor="middle">{{gate.name}}</text>
                        </svg>
                    </div>
                </div>
            </div>
        </div>

    `,
    styleUrls: ["./quantum-gate-menu.component.scss"],
})
export class QuantumGateMenuComponent implements OnInit, OnDestroy, OnChanges {
    gates: MenuGate[] = [];
    subscription!: Subscription;

    colsStyle!: string;

    @ViewChild("gateListElement")
    gateListElement!: ElementRef<HTMLDivElement>;

    @Input()
    circuitStyleOptions!: CircuitStyleOptions;

    constructor(private gateService: GateService) {}

    ngOnChanges(changes: SimpleChanges): void {
        if (changes.circuitStyleOptions && this.gateListElement) {
            this.updateColumns();
        }
    }

    /**
     * Update css-grid to have as many columns as fit in available space.
     */
    updateColumns() {
        const nCols = Math.floor(
            this.gateListElement.nativeElement.clientWidth /
                this.circuitStyleOptions.baseSize
        );
        this.colsStyle = `repeat(${nCols}, ${this.circuitStyleOptions.baseSize}px)`;
    }
    ngOnInit(): void {
        this.subscription = this.gateService
            .getMenuGates()
            .subscribe((gates) => {
                this.gates = gates.map((g) => ({
                    name: g.name,
                    color: this.getColor(g),
                    description: g.description,
                }));
                if (this.gateListElement) {
                    this.updateColumns();
                } else {
                    setTimeout(() => {
                        this.updateColumns();
                    }, 0);
                }
            });
    }

    getColor(gate: ServiceGate): Color {
        const color = this.circuitStyleOptions.gateColors.get(gate.group);
        if (color) {
            return color;
        } else {
            return {
                fill: "white",
                text: "black",
                selection: "00ff00",
            };
        }
    }

    /**
     * Add gate's name to datatransfer, so it can be used when drop happens.
     * @param event dragging of a gate starts
     * @param gate gate being dragged
     */
    handleDragStart(event: DragEvent, gate: string) {
        event.dataTransfer?.setData("text/plain", gate);
    }

    ngOnDestroy(): void {
        this.subscription.unsubscribe();
    }
}
