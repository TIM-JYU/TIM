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

        <div class="gate-container" *ngIf="gates.length > 0" [class.gate-container-static-height]="isToolboxStaticHeight">
            <p class="gate-container-heading"><ng-container i18n>Drag gate to circuit</ng-container></p>

            <div class="gate-list">
                <div class="svg-container" *ngFor="let gate of gates" draggable="true"
                     [title]="gate.info"
                     [style.width.px]="gate.name === 'control' || gate.name == 'antiControl' || gate.name === 'swap' ? circuitOptions.gateSize : gate.width"
                     [style.height.px]="circuitOptions.gateSize"
                     (click)="handleClick($event, gate.name)"
                     (touchstart)="handleTouchStart($event, gate.name)"
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
                        
                        <svg *ngSwitchCase="'antiControl'"
                             [style.height.px]="circuitOptions.gateSize"
                             [style.width.px]="circuitOptions.gateSize"
                        >
                            <circle [attr.fill]="circuitOptions.colors.light"
                                    [attr.stroke]="gate.color.fill"
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
                                  [attr.width]="gate.width"
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

    @Input()
    isToolboxStaticHeight: boolean = true;

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
        // if touch has initiaded "drag", do nothing
        if (this.activeGate) {
            event.preventDefault();
            return;
        }
        event.dataTransfer?.setData("text/plain", gate);
    }

    ngOnDestroy(): void {
        this.subscription.unsubscribe();
    }

    handleClick(event: MouseEvent | TouchEvent, name: string) {
        event.preventDefault();
        this.select.emit(name);
    }

    // Simulate drag for touch devices

    private activeGate: string | null = null;
    private dragElement: HTMLElement | null = null;
    private dragOverElement: Element | null = null;
    // Offset to move the element slightly above the finger
    private yOffset = 40; // Adjust this value as needed

    handleTouchStart(event: TouchEvent | MouseEvent, gate: string) {
        this.yOffset = this.gateService.getMarkup().touchOffset;
        if (this.yOffset <= 0) {
            return;
        }
        event.preventDefault();
        this.activeGate = gate;

        // Clone the target element for visual feedback
        const target = event.target as HTMLElement;

        const isSVG = target instanceof SVGElement;

        if (isSVG) {
            // Clone the SVG element directly
            const svgElement = target.closest("svg");
            const svgClone = svgElement?.cloneNode(true) as SVGElement;

            // Wrap the SVG wrapper in a div for styling
            const divWrapper = document.createElement("div");
            divWrapper.appendChild(svgClone);

            // Set styles for the div wrapper
            const boundingBox = target.getBoundingClientRect();
            divWrapper.style.width = `${boundingBox.width}px`;
            divWrapper.style.height = `${boundingBox.height}px`;
            divWrapper.style.position = "absolute";
            divWrapper.style.pointerEvents = "none";
            divWrapper.style.opacity = "0.7";
            divWrapper.style.zIndex = "10000";
            divWrapper.style.left = "0px";
            divWrapper.style.top = "0px";

            this.dragElement = divWrapper;
        } else {
            this.dragElement = target.cloneNode(true) as HTMLElement;
        }

        document.body.appendChild(this.dragElement);

        const [cursorX, cursorY] = this.getCursorPosition(event);
        this.updateDragElementPosition(cursorX, cursorY);

        // Add move and end listeners
        const moveHandler = (moveEvent: TouchEvent | MouseEvent) => {
            moveEvent.preventDefault(); // Estää oletuskäyttäytymisen, kuten vierityksen
            const [moveX, moveY] = this.getCursorPosition(moveEvent);
            this.updateDragElementPosition(moveX, moveY);
            const dropTarget = document.elementFromPoint(
                moveX,
                moveY - this.yOffset
            );
            const gateHover = dropTarget?.closest(".gate-drop");
            if (this.dragOverElement !== gateHover) {
                this.dragOverElement?.classList.remove("touch-hover");
            }
            if (!gateHover) {
                return;
            }
            this.dragOverElement = gateHover;
            this.dragOverElement?.classList.add("touch-hover");
        };

        const endHandler = (endEvent: TouchEvent | MouseEvent) => {
            endEvent.preventDefault();
            this.handleDrop(endEvent);
            this.cleanupDrag();
            document.removeEventListener("mousemove", moveHandler);
            document.removeEventListener("mouseup", endHandler);
            document.removeEventListener("touchmove", moveHandler);
            document.removeEventListener("touchend", endHandler);
        };

        document.addEventListener("mousemove", moveHandler);
        document.addEventListener("mouseup", endHandler);
        document.addEventListener("touchmove", moveHandler);
        document.addEventListener("touchend", endHandler);
    }
    updateDragElementPosition(x: number, y: number) {
        if (this.dragElement) {
            // Account for page scroll offset
            const scrollX = window.scrollX;
            const scrollY = window.scrollY;

            // Update position
            this.dragElement.style.left = `${x + scrollX}px`;
            this.dragElement.style.top = `${y + scrollY - this.yOffset}px`;
            this.dragElement.style.transform = "translate(-50%, -50%)"; // Center the element on the cursor
        }
    }

    handleDrop(event: TouchEvent | MouseEvent) {
        const [dropX, dropY] = this.getCursorPosition(event);
        const dropTarget = document.elementFromPoint(
            dropX,
            dropY - this.yOffset
        );
        const gateDrop = dropTarget?.closest(".gate-drop");
        if (gateDrop) {
            const dropEvent = new DragEvent("drop", {
                bubbles: true,
                cancelable: true,
                dataTransfer: new DataTransfer(),
            });

            // Esimerkki datan lisäämisestä dataTransferiin
            dropEvent.dataTransfer?.setData(
                "text/plain",
                this.activeGate ?? ""
            );

            // Lähetä tapahtuma
            gateDrop.dispatchEvent(dropEvent);
        } else {
            console.log("Dropped outside valid target " + dropX + " " + dropY);
        }
    }

    cleanupDrag() {
        if (this.dragElement) {
            document.body.removeChild(this.dragElement);
            this.dragElement = null;
        }
        this.activeGate = null;
        if (this.dragOverElement) {
            // this.dragOverElement.classList.remove("touch-hover");
            this.dragOverElement = null;
        }
    }

    getCursorPosition(event: TouchEvent | MouseEvent): [number, number] {
        if (event instanceof MouseEvent) {
            return [event.clientX, event.clientY];
        }
        const touch = event.touches[0] || event.changedTouches[0];
        return [touch.clientX, touch.clientY];
    }
}
