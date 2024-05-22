/* eslint-disable no-underscore-dangle */
import {Component, ElementRef, Input, ViewChild} from "@angular/core";
import type {ISize} from "tim/util/utils";
import type {Size} from "angular2-draggable/lib/models/size";
import type {IPosition, Position} from "angular2-draggable/lib/models/position";
import {AngularDraggableDirective} from "angular2-draggable";
import {Subject} from "rxjs";
import type {IResizeEvent} from "angular2-draggable/lib/models/resize-event";

export interface IAngularResizableDirectivePublic {
    _currSize: Size;
    _currPos: Position;

    doResize(): void;
    resetSize(): void;
}

class ResizableDraggableWrapper {
    constructor(
        private ngResizable: IAngularResizableDirectivePublic,
        private ngDraggable: AngularDraggableDirective
    ) {}

    getSize() {
        return this.ngResizable._currSize;
    }

    getPos() {
        return this.ngDraggable.getCurrentOffset();
    }

    doResize() {
        this.ngResizable.doResize();
    }

    boundsCheck() {
        this.ngDraggable.boundsCheck();
    }

    resetSize() {
        this.ngResizable.resetSize();
    }
}

@Component({
    selector: "tim-dialog-frame",
    template: `
        <div class="modal no-pointer-events in" role="dialog"
             [ngStyle]="{'z-index': 1050 + index*10, display: 'block', position: anchor}" tabindex="-1" #bounds>
            <div [ngDraggable]
                 [ngResizable]="canResize && !areaMinimized"
                 [inBounds]="true"
                 [bounds]="bounds"
                 #resizable="ngResizable"
                 #draggable="ngDraggable"
                 #dragelem
                 [preventDefaultEvent]="true"
                 [handle]="draghandle"
                 [position]="position"
                 (stopped)="onPosChange($event)"
                 (rzStop)="onSizeChange($event)"
                 class="modal-dialog modal-{{size}}"
                 style="pointer-events: auto">
                <div class="draghandle"
                     [ngClass]="{attached: !canDrag()}">
                    <p #draghandle>
                        <ng-content select="[header]"></ng-content>
                    </p>
                    <i *ngIf="detachable" (click)="toggleDetach()" title="Attach"
                       class="glyphicon glyphicon-arrow-left"></i>
                    <i *ngIf="minimizable" title="Minimize dialog" (click)="toggleMinimize()"
                       class="glyphicon"
                       [class.glyphicon-minus]="!areaMinimized"
                       [class.glyphicon-unchecked]="areaMinimized"
                    ></i>
                    <tim-close-button *ngIf="showCloseIcon && closeFn" (click)="closeFn!()">
                    </tim-close-button>
                </div>
                <div class="draggable-content modal-content" [class.minimized]="areaMinimized">
                    <div #body id="modal-body" class="modal-body">
                        <ng-content select="[body]"></ng-content>
                    </div>
                    <div class="modal-footer">
                        <ng-content select="[footer]"></ng-content>
                    </div>
                </div>
            </div>
        </div>
    `,
    styleUrls: [
        "../../../../../node_modules/angular2-draggable/css/resizable.min.css",
        "./dialog-frame.component.scss",
    ],
})
export class DialogFrame {
    detachable = false;
    @Input() minimizable = true;
    @Input() canResize = true;
    @Input() showCloseIcon = true;

    // If true, indicates that there may be some asynchronous loading going on when opening the dialog.
    // This info is only used when initializing the position of the dialog.
    // TODO: replace with something better.
    @Input() mightBeAsync = true;

    @Input() anchor: "absolute" | "fixed" = "fixed";
    @Input() autoHeight = true;
    @Input() initialAutoHeight = false;
    closeFn?: () => void;
    index = 1;
    areaMinimized = false;
    private oldSize: ISize = {width: 600, height: 400};
    @ViewChild("resizable")
    private ngResizable!: IAngularResizableDirectivePublic;
    @ViewChild("draggable") private ngDraggable!: AngularDraggableDirective;
    @ViewChild("dragelem") dragelem!: ElementRef<HTMLElement>;
    @ViewChild("body") modalBody!: ElementRef<HTMLElement>;
    @Input() size: "sm" | "md" | "lg" | "xs" = "md"; // xs is custom TIM style
    resizable!: ResizableDraggableWrapper;
    position: IPosition = {x: 0, y: 0};
    sizeOrPosChanged = new Subject<void>();

    ngAfterViewInit() {
        this.resizable = new ResizableDraggableWrapper(
            this.ngResizable,
            this.ngDraggable
        );
    }

    canDrag() {
        return true;
    }

    setPos(p: IPosition) {
        this.position = p;
    }

    getPos() {
        return this.position;
    }

    toggleDetach() {}

    toggleMinimize() {
        const res = this.ngResizable;
        this.areaMinimized = !this.areaMinimized;
        const cs = res._currSize;
        const cp = res._currPos;
        const minimizedWidth = 100;
        if (this.areaMinimized) {
            this.oldSize = {width: cs.width, height: cs.height};
            cs.width = minimizedWidth;
            cs.height = 0;
        } else {
            cs.set(this.oldSize);
        }
        cp.x +=
            ((this.oldSize.width - minimizedWidth) / 2) *
            (this.areaMinimized ? 1 : -1);
        res.doResize();
    }

    onSizeChange(e: IResizeEvent) {
        this.sizeOrPosChanged.next();
    }

    onPosChange(e: IPosition) {
        this.sizeOrPosChanged.next();
    }
}
