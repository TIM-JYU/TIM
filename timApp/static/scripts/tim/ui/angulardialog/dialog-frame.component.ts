/* eslint-disable no-underscore-dangle */
import {
    Component,
    ElementRef,
    HostListener,
    Input,
    ViewChild,
} from "@angular/core";
import type {ISize} from "tim/util/utils";
import type {Size} from "angular2-draggable/lib/models/size";
import type {IPosition, Position} from "angular2-draggable/lib/models/position";
import {AngularDraggableDirective} from "angular2-draggable";
import type {IResizeEvent} from "angular2-draggable/lib/models/resize-event";
import {getViewPortSize, timeout, TimStorage} from "tim/util/utils";
import * as t from "io-ts";
import {IDialogOptions} from "tim/ui/angulardialog/angular-dialog-component.directive";

function clamp(val: number, min: number, max: number) {
    if (val < min) {
        return min;
    }
    if (val > max) {
        return max;
    }
    return val;
}

enum SizeNeedsRefresh {
    Yes,
    No,
}

function getCurrentSize(el: Element) {
    const computed = window.getComputedStyle(el);
    return {
        width: parseInt(computed.width, 10),
        height: parseInt(computed.height, 10),
    };
}

const TwoTuple = t.tuple([t.number, t.number]);

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
                 [class.detachable]="detachable"
                 [style.position]="(!detachable || detachable && !detached) ? '' : 'fixed'"
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
        "./dialog-frame.component.scss",
        "../../../../../node_modules/angular2-draggable/css/resizable.min.css",
    ],
})
export class DialogFrame {
    @Input() detachable = false;
    @Input() minimizable = true;
    @Input() canResize = true;
    @Input() dialogName: string = "";
    @Input() dialogOptions?: IDialogOptions;
    // If true, indicates that there may be some asynchronous loading going on when opening the dialog.
    // This info is only used when initializing the position of the dialog.
    // TODO: replace with something better.
    // mightBeAsync needs to be false by default, otherwise it resets positioning quite indiscriminately
    @Input() mightBeAsync = false;
    @Input() anchor: "absolute" | "fixed" = "fixed";
    @Input() align: "left" | "center" | "right" = "center";
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
    protected extraVerticalSize = 0;
    xOrigin?: number;
    resizable!: ResizableDraggableWrapper;
    position: IPosition = {x: 0, y: 0};
    private sizeStorage?: TimStorage<[number, number]>;
    private posStorage?: TimStorage<[number, number]>;
    private bodyObserver?: MutationObserver;

    ngOnInit() {
        if (this.detachable) {
            this.detachedIndex = `${1050 + this.index * 10}`;
            this.detached = false;
        }
    }

    ngOnDestroy() {
        this.bodyObserver?.disconnect();
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

        // Observe changes to frame content and resize accordingly
        if (this.autoHeight) {
            this.bodyObserver = new MutationObserver(async (record) => {
                if (this.isDetachedDialogMutation(record)) {
                    return;
                }
                await this.setHeightAutomatic();
            });
            this.bodyObserver.observe(this.modalBody.nativeElement, {
                childList: true,
                subtree: true,
            });
        }

        // Set dialog size according to dialogOptions
        let sizehint = null;
        if (this.dialogOptions?.resetSize) {
            this.resizable.resetSize();
        } else if (this.dialogName) {
            const savedSize = this.getSizeStorage().get();
            if (savedSize) {
                sizehint = {width: savedSize[0], height: savedSize[1]};
                this.resizable.getSize().set(sizehint);
            }
        }
        this.fixPosSizeInbounds(sizehint);
        this.resizable.doResize();

        // Set dialog position according to dialogOptions,
        if (this.dialogOptions?.pos) {
            this.setPos({
                x: this.dialogOptions.pos.x,
                y: this.dialogOptions.pos.y,
            });
        } else if (this.dialogName) {
            // or use the dialog's saved position,
            const savedPos = this.getPosStorage().get();
            if (
                savedPos?.[0] != null &&
                savedPos[1] != null &&
                !this.dialogOptions?.resetPos
            ) {
                this.setPos({x: savedPos[0], y: savedPos[1]});
            } else {
                // otherwise, align to dialog's default position.
                this.alignDialogToPos(initSize);
            }
        }
        // Frame content might be loaded asynchronously,
        // resize frame as content loads
        (async () => {
            if (this.mightBeAsync) {
                await timeout(500);
            } else {
                await timeout(0);
            }
            this.resizable.doResize();
            if (this.autoHeight || this.initialAutoHeight) {
                await this.setHeightAutomatic();
            }
        })();
    }

    getSavePrefix() {
        return this.dialogName;
    }
    getSizeStorage() {
        return (this.sizeStorage ??= new TimStorage(
            this.getSizeKey(),
            TwoTuple
        ));
    }

    getPosStorage() {
        return (this.posStorage ??= new TimStorage(this.getPosKey(), TwoTuple));
    }

    saveSize() {
        const {width, height} = this.resizable.getSize();

        // Height can sometimes be zero, at least when the dialog is minimized.
        if (height > 0) {
            this.getSizeStorage().set([width, height]);
        }
    }

    savePos(elementPos?: IPosition) {
        if (elementPos) {
            this.getPosStorage().set([elementPos.x, elementPos.y]);
        } else {
            try {
                const {x, y} = this.resizable.getPos();
                this.getPosStorage().set([x, y]);
            } catch {
                console.warn("resizable.getPos threw an error");
            }
        }
    }

    private getPosKey() {
        return `${this.getSavePrefix()}Pos`;
    }

    private getSizeKey() {
        return `${this.getSavePrefix()}Size`;
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
        this.saveSize();
    }

    onPosChange(e: IPosition) {
        this.savePos();
    }

    @HostListener("window:resize", ["$event"])
    onResize(event: Event) {
        if (this.bounds) {
            const boundary = this.bounds.nativeElement.getBoundingClientRect();
            const elem = this.dragelem.nativeElement.getBoundingClientRect();

            let translateX = elem.x;
            let translateY = elem.y;
            if (boundary.top > elem.top) {
                translateY = boundary.top;
            }
            if (boundary.bottom < elem.bottom) {
                translateY = boundary.bottom - elem.height;
            }
            if (boundary.right < elem.right) {
                translateX = boundary.right - elem.width;
            }
            if (boundary.left > elem.left) {
                translateX = boundary.left;
            }
            this.setPos({x: translateX, y: translateY});
        }
    }

    async setHeightAutomatic() {
        this.dragelem.nativeElement.style.height = "auto";
        await this.ensureFullyInViewport();
    }

    protected async ensureFullyInViewport() {
        await timeout();
        this.resizable.boundsCheck();
    }

    private isDetachedDialogMutation(record: MutationRecord[]) {
        return record.every((r) => {
            if (!(r.target instanceof HTMLElement)) {
                return false;
            }
            const cl = r.target.classList;
            return (
                cl.contains("modal-dialog") &&
                cl.contains("detachable") &&
                !cl.contains("attached")
            );
        });
    }

    fixPosSizeInbounds(sizeHint: ISize | null): SizeNeedsRefresh {
        const vp =
            this.anchor === "fixed"
                ? getViewPortSize()
                : {
                      width: document.documentElement.scrollWidth,
                      height: document.documentElement.scrollHeight,
                  };

        // First clamp the frame so it fits inside the viewport
        const {width, height} =
            sizeHint ?? getCurrentSize(this.dragelem.nativeElement);
        const newWidth = Math.min(width, vp.width);
        const newHeight = Math.min(
            height + this.extraVerticalSize,
            vp.height - 50
        );
        this.extraVerticalSize = 0;

        // Then clamp x/y so that the element is at least within the viewport
        let {x, y} = this.getPos();
        const xOrigin = vp.width / 2 - width / 2;
        this.xOrigin = xOrigin;
        x = clamp(x, 0 - xOrigin, vp.width - newWidth - xOrigin);
        y = clamp(y, 0, vp.height - newHeight);

        this.resizable.getSize().set({
            width: newWidth,
            height: newHeight,
        });
        this.setPos({x, y});
        return newWidth !== width || newHeight !== height
            ? SizeNeedsRefresh.Yes
            : SizeNeedsRefresh.No;
    }

    /**
     * Aligns the element/component to a predefined position on the page.
     * @param initSize initialized size of the component
     * @private
     */
    private alignDialogToPos(initSize: ISize) {
        let windowCenter = {x: initSize.width, y: initSize.height};
        if (this.bounds) {
            windowCenter = {
                x: Math.round(
                    this.bounds.nativeElement.getBoundingClientRect().width / 2
                ),
                y: Math.round(
                    this.bounds.nativeElement.getBoundingClientRect().height / 2
                ),
            };
        }
        const elemPosition = {x: 0, y: 0};
        if (!this.detached) {
            return;
        } // only align the out-most parent window/dialog

        // Set dialog position based on its align property
        switch (this.align) {
            case "left":
                elemPosition.x = 0;
                elemPosition.y = windowCenter.y - initSize.height / 2;
                break;
            case "center":
                elemPosition.x = windowCenter.x - initSize.width / 2;
                elemPosition.y = windowCenter.y - initSize.height / 2;
                break;
            case "right":
                elemPosition.x = this.bounds
                    ? this.bounds.nativeElement.getBoundingClientRect().right -
                      initSize.width
                    : 0;
                elemPosition.y = windowCenter.y - initSize.height / 2;
                break;
            default:
                elemPosition.x = windowCenter.x - initSize.width / 2;
                elemPosition.y = windowCenter.y - initSize.height / 2;
                break;
        }
        this.setPos(elemPosition);
        this.savePos(elemPosition);
    }
}
