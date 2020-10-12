/* eslint-disable no-underscore-dangle */
import {Component, ElementRef, ViewChild} from "@angular/core";
import {ISize} from "tim/util/utils";
import {Size} from "angular2-draggable/lib/models/size";
import {IPosition, Position} from "angular2-draggable/lib/models/position";
import {AngularDraggableDirective} from "angular2-draggable";

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

    resetSize() {
        this.ngResizable.resetSize();
    }
}

@Component({
    selector: "tim-dialog-frame",
    template: `
        <div class="modal no-pointer-events in" role="dialog"
             [ngStyle]="{'z-index': 1050 + index*10, display: 'block'}" tabindex="-1" #bounds>
            <div [ngDraggable]
                 [ngResizable]="!areaMinimized"
                 [inBounds]="true"
                 [bounds]="bounds"
                 #resizable="ngResizable"
                 #draggable="ngDraggable"
                 #dragelem
                 [preventDefaultEvent]="true"
                 [handle]="draghandle"
                 [position]="position"
                 class="modal-dialog modal-md"
                 style="pointer-events: auto">
                <div #draghandle
                     class="draghandle drag"
                     [ngClass]="{attached: !canDrag()}">
                    <p class="drag">
                        <ng-content select="[header]"></ng-content>
                    </p>
                    <i *ngIf="detachable" (click)="toggleDetach()" title="Attach"
                       class="glyphicon glyphicon-arrow-left"></i>
                    <i *ngIf="click" title="Minimize dialog" (click)="toggleMinimize()"
                       class="glyphicon"
                       [class.glyphicon-minus]="!areaMinimized"
                       [class.glyphicon-unchecked]="areaMinimized"
                    ></i>
                    <tim-close-button *ngIf="closeFn" (click)="closeFn!()">
                    </tim-close-button>
                </div>
                <div class="draggable-content modal-content" [class.minimized]="areaMinimized">
                    <div id="modal-body" class="modal-body">
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
    click = true;
    closeFn?: () => void;
    index = 1;
    areaMinimized = false;
    private oldSize: ISize = {width: 600, height: 400};
    @ViewChild("resizable")
    private ngResizable!: IAngularResizableDirectivePublic;
    @ViewChild("draggable") private ngDraggable!: AngularDraggableDirective;
    @ViewChild("dragelem") dragelem!: ElementRef<HTMLElement>;
    resizable!: ResizableDraggableWrapper;
    position: IPosition = {x: 0, y: 0};

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
}
