import {AfterViewInit, Directive, EventEmitter, HostListener, Input, Output, ViewChild} from "@angular/core";
import {DialogFrame} from "tim/ui/angulardialog/dialog-frame.component";
import {TimDefer} from "tim/util/timdefer";
import {getStorage, setStorage} from "tim/util/utils";
import * as t from "io-ts";

export interface IDialogParams {
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

@Directive()
export abstract class AngularDialogComponent<Params, Result> implements AfterViewInit {
    @Input() data!: Params;
    @Output() closeWithResult = new EventEmitter<Result>();
    @ViewChild(DialogFrame) frame?: DialogFrame;
    @Input() dialogParams?: IDialogParams;

    private resultDefer = new TimDefer<Result>();
    protected abstract dialogName: string;

    // eslint-disable-next-line @typescript-eslint/tslint/config
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
            throw Error("All AngularDialogComponents must have tim-angular-dialog component in their template.");
        }
        this.frame.closeFn = () => this.dismiss();
        const TwoTuple = t.tuple([t.number, t.number]);
        if (this.dialogParams?.resetSize) {
            this.frame.resizable.resetSize();
        } else {
            const savedSize = getStorage(this.getSizeKey());
            if (TwoTuple.is(savedSize)) {
                this.frame.resizable.getSize().set({width: savedSize[0], height: savedSize[1]});
            }
        }
        const savedPos = getStorage(this.getPosKey());
        if (TwoTuple.is(savedPos)) {
            this.frame.setPos({x: savedPos[0], y: clamp(savedPos[1], 0, window.innerHeight)});
        }

        // TODO: Better way to clamp width and height of the viewport
        let {width, height} = this.frame.resizable.getSize();
        width = Math.min(width, window.innerWidth);
        height = Math.min(height, window.innerHeight);
        this.frame.resizable.getSize().set({ width, height});

        this.frame.resizable.doResize();
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
