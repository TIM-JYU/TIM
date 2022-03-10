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
    _initSize: Size;

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
        <ng-container [ngSwitch]="detachable">
            <div *ngSwitchCase="false" class="modal no-pointer-events in" role="dialog"
                 [style.z-index]="1050 + index * 10"
                 [style.display]="'block'"
                 [style.position]="anchor" tabindex="-1" #bounds>
                <ng-container [ngTemplateOutlet]="draggableModal"></ng-container>
            </div>
            <ng-container *ngSwitchCase="true" [ngTemplateOutlet]="draggableModal"></ng-container>
        </ng-container>
        
        <ng-template #draggableModal>
            <div [ngDraggable]="detached"
                 [ngResizable]="detached && canResize && !areaMinimized"
                 [inBounds]="true"
                 [bounds]="bounds?.nativeElement!"
                 #resizable="ngResizable"
                 #draggable="ngDraggable"
                 #dragelem
                 [preventDefaultEvent]="true"
                 [handle]="draghandle"
                 [zIndex]="detachedIndex"
                 [position]="position"
                 (stopped)="onPosChange($event)"
                 (rzStop)="onSizeChange($event)"
                 [class.attached]="!detached"
                 [style.position]="detachable && !detached ? '' : 'fixed'"
                 class="modal-dialog modal-{{size}}"
                 style="pointer-events: auto">
                <div class="draghandle" [class.attached]="!detached">
                    <p #draghandle>
                        <ng-content select="[header]"></ng-content>
                    </p>
                    <i *ngIf="detachable" (click)="toggleDetach()" title="Attach"
                       class="glyphicon glyphicon-arrow-left" 
                       [class.glyphicon-arrow-left]="detached"
                       [class.glyphicon-arrow-right]="!detached"></i>
                    <i *ngIf="minimizable" title="Minimize dialog" (click)="toggleMinimize()"
                       class="glyphicon"
                       [class.glyphicon-minus]="!areaMinimized"
                       [class.glyphicon-unchecked]="areaMinimized"
                    ></i>
                    <tim-close-button *ngIf="closeFn" (click)="closeFn!()">
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
        </ng-template>
    `,
    styleUrls: [
        "../../../../../node_modules/angular2-draggable/css/resizable.min.css",
        "./dialog-frame.component.scss",
    ],
})
export class DialogFrame {
    @Input() detachable = false;
    @Input() minimizable = true;
    @Input() canResize = true;

    // If true, indicates that there may be some asynchronous loading going on when opening the dialog.
    // This info is only used when initializing the position of the dialog.
    // TODO: replace with something better.
    @Input() mightBeAsync = true;

    @Input() anchor: "absolute" | "fixed" = "fixed";
    @Input() autoHeight = true;
    @Input() initialAutoHeight = false;
    closeFn?: () => void;
    index = 1;
    detachedIndex: string = "";
    areaMinimized = false;
    detached = true;
    private oldSize: ISize = {width: 600, height: 400};
    @ViewChild("resizable")
    private ngResizable!: IAngularResizableDirectivePublic;
    @ViewChild("draggable") private ngDraggable!: AngularDraggableDirective;
    @ViewChild("dragelem") dragelem!: ElementRef<HTMLElement>;
    @ViewChild("body") modalBody!: ElementRef<HTMLElement>;
    @ViewChild("bounds", {static: false}) bounds?: ElementRef<HTMLElement>;
    @Input() size: "sm" | "md" | "lg" | "xs" = "md"; // xs is custom TIM style
    resizable!: ResizableDraggableWrapper;
    position: IPosition = {x: 0, y: 0};
    sizeOrPosChanged = new Subject<void>();

    ngOnInit() {
        if (this.detachable) {
            this.detachedIndex = `${1050 + this.index * 10}`;
            this.detached = false;
        }
    }

    ngAfterViewInit() {
        this.resizable = new ResizableDraggableWrapper(
            this.ngResizable,
            this.ngDraggable
        );

        // Fix initial size to account for Chrome/Firefox using decimal values for width/height
        const initSize = this.ngResizable._initSize;
        const r = this.dragelem.nativeElement.getBoundingClientRect();
        initSize.width = Math.round(r.width);
        initSize.height = Math.round(r.height);
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

    toggleDetach() {
        this.detached = !this.detached;
        this.position = {x: 0, y: 0};
        this.detachedIndex = this.detached ? `${1050 + this.index * 10}` : "";
        if (!this.detached) {
            this.ngResizable.resetSize();
        }
    }

    toggleMinimize() {
        const res = this.ngResizable;
        this.areaMinimized = !this.areaMinimized;
        const cs = res._currSize;
        const cp = res._currPos;
        const minimizedWidth = 100;
        if (this.areaMinimized) {
            const rect = this.dragelem.nativeElement.getBoundingClientRect();
            this.oldSize = {
                width: Math.round(rect.width),
                height: Math.round(rect.height),
            };
            if (this.detached) {
                cs.width = minimizedWidth;
            }
            cs.height = 0;
        } else {
            cs.set(this.oldSize);
        }
        if (this.detached) {
            cp.x +=
                ((this.oldSize.width - minimizedWidth) / 2) *
                (this.areaMinimized ? 1 : -1);
        }
        res.doResize();
    }

    onSizeChange(e: IResizeEvent) {
        this.sizeOrPosChanged.next();
    }

    onPosChange(e: IPosition) {
        this.sizeOrPosChanged.next();
    }
}
