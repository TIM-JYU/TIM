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
import {getStorage, getViewPortSize, setStorage} from "tim/util/utils";
import * as t from "io-ts";

export interface IDialogOptions {
    resetSize?: boolean;
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

@Directive()
export abstract class AngularDialogComponent<Params, Result>
    implements AfterViewInit {
    @Input() data!: Params;
    @Output() closeWithResult = new EventEmitter<Result>();
    @ViewChild(DialogFrame) frame?: DialogFrame;
    @Input() dialogOptions?: IDialogOptions;

    private resultDefer = new TimDefer<Result>();
    protected abstract dialogName: string;
    protected extraVerticalSize = 0;

    @HostListener("keydown.esc", ["$event"])
    escPressed(e: KeyboardEvent) {
        this.dismiss();
        e.stopPropagation();
    }

    get result() {
        return this.resultDefer.promise;
    }

    getSavePrefix() {
        return this.dialogName;
    }

    ngAfterViewInit() {
        if (!this.frame) {
            throw Error(
                "All AngularDialogComponents must have tim-angular-dialog component in their template."
            );
        }
        this.frame.closeFn = () => this.dismiss();
        const TwoTuple = t.tuple([t.number, t.number]);
        let snr = SizeNeedsRefresh.No;
        if (this.dialogOptions?.resetSize) {
            this.frame.resizable.resetSize();
        } else {
            const savedSize = getStorage(this.getSizeKey());
            if (TwoTuple.is(savedSize)) {
                snr = SizeNeedsRefresh.Yes;
                this.frame.resizable
                    .getSize()
                    .set({width: savedSize[0], height: savedSize[1]});
            }
        }
        const savedPos = getStorage(this.getPosKey());
        if (TwoTuple.is(savedPos)) {
            this.frame.setPos({x: savedPos[0], y: savedPos[1]});
        }
        const fixpossnr = this.fixPosSizeInbounds();
        if (
            snr === SizeNeedsRefresh.Yes ||
            fixpossnr === SizeNeedsRefresh.Yes
        ) {
            // We don't want to call this unconditionally because it may make the initial size of the dialog too small.
            // It happens at least it the dialog has some asynchronous initialization.
            this.frame.resizable.doResize();
        }
    }

    fixPosSizeInbounds(): SizeNeedsRefresh {
        const vp = getViewPortSize();

        // First clamp the frame so it fits inside the viewport
        const {width, height} = this.frame!.resizable.getSize();
        const newWidth = Math.min(width, vp.width);
        const newHeight = Math.min(height + this.extraVerticalSize, vp.height);

        // Then clamp x/y so that the element is at least within the viewport
        let {x, y} = this.frame!.resizable.getPos();
        x = clamp(x, 0, vp.width - newWidth);
        y = clamp(y, 0, vp.height - newHeight);

        this.frame!.resizable.getSize().set({
            width: newWidth,
            height: newHeight,
        });
        this.frame!.setPos({x, y});
        return newWidth !== width || newHeight !== height
            ? SizeNeedsRefresh.Yes
            : SizeNeedsRefresh.No;
    }

    close(r: Result) {
        this.closeWithResult.emit(r);
        this.resultDefer.resolve(r);
        this.savePosSize();
    }

    dismiss() {
        this.resultDefer.reject("Dialog was closed from the X button");
        this.savePosSize();
    }

    private savePosSize() {
        const {width, height} = this.frame!.resizable.getSize();
        setStorage(this.getSizeKey(), [width, height]);

        const {x, y} = this.frame!.resizable.getPos();
        setStorage(this.getPosKey(), [x, y]);
    }

    private getPosKey() {
        return `${this.getSavePrefix()}Pos`;
    }

    private getSizeKey() {
        return `${this.getSavePrefix()}Size`;
    }
}
