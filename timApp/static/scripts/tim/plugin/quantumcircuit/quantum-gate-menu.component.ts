import type {OnDestroy, OnInit} from "@angular/core";
import {Component, EventEmitter, Input, Output} from "@angular/core";
import type {Color} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import {CircuitOptions} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {ServiceGate} from "tim/plugin/quantumcircuit/gate.service";
import {GateService} from "tim/plugin/quantumcircuit/gate.service";
import type {Subscription} from "rxjs";

interface MenuGate {
    color: Color;
    name: string;
    info: string;
    width: number;
}

@Component({
    selector: "tim-quantum-gate-menu",
    template: `
        <!--suppress HtmlUnknownAttribute -->

        <div class="gate-container" *ngIf="gates.length > 0">
            <p class="gate-container-heading"><ng-container i18n>Drag gate to circuit</ng-container></p>

            <div class="gate-list">
                <div class="svg-container" *ngFor="let gate of gates" draggable="true"
                     [title]="gate.info"
                     [style.width.px]="gate.name === 'control' || gate.name === 'swap' ? circuitOptions.gateSize : gate.width"
                     [style.height.px]="circuitOptions.gateSize"
                     (click)="handleClick(gate.name)"
                     (dragstart)="handleDragStart($event, gate.name)">
                    <div [ngSwitch]="gate.name">
                        <svg *ngSwitchCase="'control'"
                             [style.height.px]="circuitOptions.gateSize"
                             [style.width.px]="circuitOptions.gateSize"
                        >
                            <circle [attr.fill]="gate.color.fill"
                                    [attr.cx]="circuitOptions.gateSize/2"
                                    [attr.cy]="circuitOptions.gateSize/2"
                                    [attr.r]="circuitOptions.gateSize/4"/>
                        </svg>

                        <svg *ngSwitchCase="'swap'"
                             [style.height.px]="circuitOptions.gateSize"
                             [style.width.px]="circuitOptions.gateSize"
                             class="swap-gate">
                            <text x="50%" y="25%"
                                  [attr.fill]="gate.color.fill"
                                  [attr.stroke]="circuitOptions.colors.dark"
                                  dominant-baseline="middle" text-anchor="middle"
                                  [attr.font-size]="circuitOptions.gateSize / 3">X
                            </text>
                            <line [attr.stroke]="circuitOptions.colors.dark" stroke-width="2" x1="50%" x2="50%"
                                  y1="25%"
                                  y2="75%"></line>
                            <text x="50%" y="75%"
                                  [attr.fill]="gate.color.text"
                                  [attr.stroke]="circuitOptions.colors.dark"
                                  dominant-baseline="middle" text-anchor="middle"
                                  [attr.font-size]="circuitOptions.gateSize / 3">X
                            </text>
                        </svg>

                        <svg *ngSwitchDefault
                             [style.width.px]="gate.width"
                             [attr.height]="circuitOptions.gateSize">
                            <rect [attr.x]="0" [attr.y]="0"
                                  [attr.height]="circuitOptions.gateSize"
                                  [style.width.px]="gate.width"
                                  [attr.fill]="gate.color.fill"
                                  [attr.stroke]="circuitOptions.colors.dark"
                                  rx="2"/>
                            <text x="50%" y="50%"
                                  [attr.stroke]="gate.color.text"
                                  [attr.fill]="gate.color.text"
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
export class QuantumGateMenuComponent implements OnInit, OnDestroy {
    gates: MenuGate[] = [];
    subscription!: Subscription;

    @Input()
    circuitOptions!: CircuitOptions;

    @Output()
    select = new EventEmitter<string>();

    constructor(private gateService: GateService) {}

    ngOnInit(): void {
        this.subscription = this.gateService
            .getMenuGates()
            .subscribe((gates) => {
                this.gates = gates.map((g) => ({
                    name: g.name,
                    color: this.getColor(g),
                    info: g.info,
                    width: Math.max(
                        this.gateService.getTextWidth(g.name),
                        this.circuitOptions.gateSize
                    ),
                }));
            });
    }

    /**
     * Select colors for gate. Custom gates have their own colors that are prioritizes
     * else group color is used.
     * @param gate gate to the colors for
     */
    getColor(gate: ServiceGate): Color {
        const color = this.circuitOptions.gateColors.get(gate.group);
        return {
            fill: gate.color ?? color?.fill ?? "white",
            text: gate.textColor ?? color?.text ?? "black",
            selection: color?.selection ?? "yellow",
        };
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

    handleClick(name: string) {
        this.select.emit(name);
    }
}
