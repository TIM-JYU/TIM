import type {AfterViewInit} from "@angular/core";
import {
    Directive,
    EventEmitter,
    HostListener,
    Input,
    Output,
    ViewChild,
} from "@angular/core";
import {DialogFrame} from "tim/ui/angulardialog/dialog-frame.component";
import {TimDefer} from "tim/util/timdefer";
import type {ISize} from "tim/util/utils";
import {getViewPortSize, timeout, TimStorage} from "tim/util/utils";
import * as t from "io-ts";
import type {Subscription} from "rxjs";
import type {Pos} from "tim/ui/pos";

export interface IDialogOptions {
    resetSize?: boolean;
    resetPos?: boolean;
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

function getCurrentSize(el: Element) {
    const computed = window.getComputedStyle(el);
    return {
        width: parseInt(computed.width, 10),
        height: parseInt(computed.height, 10),
    };
}

const TwoTuple = t.tuple([t.number, t.number]);

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
    private sub?: Subscription;
    protected xOrigin?: number;
    private bodyObserver?: MutationObserver;
    private sizeStorage?: TimStorage<[number, number]>;
    private posStorage?: TimStorage<[number, number]>;

    @HostListener("keydown.esc", ["$event"])
    escPressed(e: KeyboardEvent) {
        this.dismiss();
        e.stopPropagation();
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
                await this.setHeightAutomatic();
            });
            this.bodyObserver.observe(this.frame.modalBody.nativeElement, {
                childList: true,
                subtree: true,
            });
        }
        this.frame.closeFn = () => this.dismiss();
        this.sub = this.frame.sizeOrPosChanged.subscribe(() => {
            this.savePosSize();
        });
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
        this.fixPosSizeInbounds(sizehint);
        this.frame.resizable.doResize();
        if (this.dialogOptions?.pos) {
            if (this.dialogOptions.pos) {
                this.frame.setPos({
                    x: this.dialogOptions.pos.x - this.xOrigin!,
                    y: this.dialogOptions.pos.y,
                });
            }
        } else if (!this.dialogOptions?.resetPos) {
            const savedPos = this.getPosStorage().get();
            if (savedPos) {
                this.frame.setPos({x: savedPos[0], y: savedPos[1]});
            }
        }
        (async () => {
            if (this.frame.mightBeAsync) {
                await timeout(500);
            } else {
                await timeout(0);
            }
            this.fixPosSizeInbounds(null);
            this.frame.resizable.doResize();
            if (this.frame.autoHeight || this.frame.initialAutoHeight) {
                await this.setHeightAutomatic();
            }
        })();
    }

    fixPosSizeInbounds(sizeHint: ISize | null): SizeNeedsRefresh {
        const vp =
            this.frame.anchor === "fixed"
                ? getViewPortSize()
                : {
                      width: document.documentElement.scrollWidth,
                      height: document.documentElement.scrollHeight,
                  };

        // First clamp the frame so it fits inside the viewport
        const {width, height} =
            sizeHint ?? getCurrentSize(this.frame.dragelem.nativeElement);
        const newWidth = Math.min(width, vp.width);
        const newHeight = Math.min(
            height + this.extraVerticalSize,
            vp.height - 50
        );
        this.extraVerticalSize = 0;

        // Then clamp x/y so that the element is at least within the viewport
        let {x, y} = this.frame.getPos();
        const xOrigin = vp.width / 2 - width / 2;
        this.xOrigin = xOrigin;
        x = clamp(x, 0 - xOrigin, vp.width - newWidth - xOrigin);
        y = clamp(y, 0, vp.height - newHeight);

        this.frame.resizable.getSize().set({
            width: newWidth,
            height: newHeight,
        });
        this.frame.setPos({x, y});
        return newWidth !== width || newHeight !== height
            ? SizeNeedsRefresh.Yes
            : SizeNeedsRefresh.No;
    }

    close(r: Result) {
        if (this.closed) {
            return;
        }
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
}
