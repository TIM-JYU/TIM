import {
    AfterViewInit,
    Directive,
    EventEmitter,
    HostListener,
    Input,
    Output,
    ViewChild,
} from "@angular/core";
import {DialogFrame} from "tim/ui/angulardialog/dialog-frame.component";
import {TimDefer} from "tim/util/timdefer";
import {getViewPortSize, ISize, timeout, TimStorage} from "tim/util/utils";
import * as t from "io-ts";
import {Subscription} from "rxjs";
import {Pos} from "tim/ui/pos";
import {IPosition} from "angular2-draggable/lib/models/position";

export interface IDialogOptions {
    resetSize?: boolean;
    pos?: Pos;
}

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

const TwoTuple = t.tuple([t.number, t.number]);

export enum Alignment {
    Center,
    Right,
}

@Directive()
export abstract class AngularDialogComponent<Params, Result>
    implements AfterViewInit
{
    @Input() data!: Params;
    @Output() closeWithResult = new EventEmitter<Result>();
    @ViewChild(DialogFrame) frame!: DialogFrame;
    @Input() dialogOptions?: IDialogOptions;

    private resultDefer = new TimDefer<Result>();
    protected abstract dialogName: string;
    protected extraVerticalSize = 0;
    protected closed = false;
    protected initialHorizontalAlign?: Alignment = Alignment.Center;
    protected initialPosition?: IPosition;
    private sub?: Subscription;
    private bodyObserver?: MutationObserver;
    private sizeStorage?: TimStorage<[number, number]>;
    private posStorage?: TimStorage<[number, number]>;
    private hasCustomPosition: boolean = false;

    @HostListener("keydown.esc", ["$event"])
    escPressed(e: KeyboardEvent) {
        this.dismiss();
        e.stopPropagation();
    }

    @HostListener("window:resize", ["$event"])
    handleResize() {
        if (this.fixPosSizeInbounds()) {
            this.frame.resizable.doResize();
        }
    }

    get result() {
        return this.resultDefer.promise;
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

    isClosed() {
        return this.closed;
    }

    getSavePrefix() {
        return this.dialogName;
    }

    ngOnDestroy() {
        this.sub?.unsubscribe();
        this.bodyObserver?.disconnect();
    }

    ngAfterViewInit() {
        if (!this.frame) {
            throw Error(
                "All AngularDialogComponents must have tim-angular-dialog component in their template."
            );
        }
        if (this.frame.autoHeight) {
            this.bodyObserver = new MutationObserver(async (record) => {
                if (this.isDetachedDialogMutation(record)) {
                    return;
                }
                await this.setHeightAutomatic();
            });
            this.bodyObserver.observe(this.frame.modalBody.nativeElement, {
                childList: true,
                subtree: true,
            });
        }
        this.frame.closeFn = () => this.dismiss();
        let sizehint = null;
        if (this.dialogOptions?.resetSize) {
            this.frame.resizable.resetSize();
        } else {
            const savedSize = this.getSizeStorage().get();
            if (savedSize) {
                sizehint = {width: savedSize[0], height: savedSize[1]};
                this.frame.resizable.getSize().set(sizehint);
            }
        }
        const savedPos = this.getPosStorage().get();
        if (savedPos) {
            this.hasCustomPosition = true;
            this.frame.pos = {x: savedPos[0], y: savedPos[1]};
        } else if (this.initialPosition) {
            this.frame.pos = this.initialPosition;
        }

        this.sub = this.frame.sizeOrPosChanged.subscribe(() => {
            this.savePosSize();
        });

        this.fixPosSizeInbounds(sizehint);
        this.frame.resizable.doResize();

        if (this.dialogOptions?.pos) {
            if (this.dialogOptions.pos) {
                this.frame.pos = {
                    x: this.dialogOptions.pos.x,
                    y: this.dialogOptions.pos.y,
                };
            }
        }

        (async () => {
            if (this.frame.mightBeAsync) {
                await timeout(500);
            } else {
                await timeout(0);
            }
            this.fixPosSizeInbounds();
            this.frame.resizable.doResize();
            if (this.frame.autoHeight || this.frame.initialAutoHeight) {
                await this.setHeightAutomatic();
            }
        })();
    }

    private get viewPort() {
        return this.frame.anchor === "fixed"
            ? getViewPortSize()
            : {
                  width: document.documentElement.scrollWidth,
                  height: document.documentElement.scrollHeight,
              };
    }

    fixPosSizeInbounds(sizeHint?: ISize | null): SizeNeedsRefresh {
        const vp = this.viewPort;

        // First clamp the frame so it fits inside the viewport
        const {width, height} = sizeHint ?? this.frame.currentSize;
        const newWidth = clamp(width, 0, vp.width);
        const newHeight = clamp(
            height + this.extraVerticalSize,
            0,
            vp.height - 50
        );
        this.extraVerticalSize = 0;

        // Then clamp x/y so that the element is at least within the viewport
        // Use last position that the user dragged to
        const lastDraggedPos = this.frame.lastDraggedPos;
        console.log("last dragged", lastDraggedPos);

        if (!this.hasCustomPosition) {
            if (this.initialHorizontalAlign == Alignment.Center) {
                lastDraggedPos.x = vp.width / 2 - width / 2;
            } else if (this.initialHorizontalAlign == Alignment.Right) {
                lastDraggedPos.x = vp.width - width;
            }
        }

        const newPos = {
            x: clamp(lastDraggedPos.x, 0, vp.width - newWidth),
            y: clamp(lastDraggedPos.y, 0, vp.height - newHeight),
        };

        this.frame.resizable.getSize().set({
            width: newWidth,
            height: newHeight,
        });
        this.frame.pos = newPos;
        return newWidth !== width || newHeight !== height
            ? SizeNeedsRefresh.Yes
            : SizeNeedsRefresh.No;
    }

    close(r: Result) {
        this.closed = true;
        this.closeWithResult.emit(r);
        this.resultDefer.resolve(r);
        this.savePosSize();
    }

    dismiss() {
        this.closed = true;
        this.resultDefer.reject("Dialog was closed from the X button");
        this.savePosSize();
    }

    private savePosSize() {
        const {width, height} = this.frame.resizable.getSize();

        // Height can sometimes be zero, at least when the dialog is minimized.
        if (height > 0) {
            this.getSizeStorage().set([width, height]);
        }

        try {
            const {x, y} = this.frame.resizable.getPos();
            this.getPosStorage().set([x, y]);
            this.hasCustomPosition = true;
        } catch {
            console.warn("resizable.getPos threw an error");
        }
    }

    private getPosKey() {
        return `${this.getSavePrefix()}Pos`;
    }

    private getSizeKey() {
        return `${this.getSavePrefix()}Size`;
    }

    protected async setHeightAutomatic() {
        this.frame.dragelem.nativeElement.style.height = "auto";
        await this.ensureFullyInViewport();
    }

    protected async ensureFullyInViewport() {
        await timeout();
        this.frame.resizable.boundsCheck();
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
}
