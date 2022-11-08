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
import type {Pos} from "tim/ui/pos";

export interface IDialogOptions {
    resetSize?: boolean;
    resetPos?: boolean;
    pos?: Pos;
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
    protected closed = false;

    @HostListener("keydown.esc", ["$event"])
    escPressed(e: KeyboardEvent) {
        this.dismiss();
        e.stopPropagation();
    }

    get result() {
        return this.resultDefer.promise;
    }

    isClosed() {
        return this.closed;
    }

    ngOnDestroy() {}

    ngAfterViewInit() {
        if (!this.frame) {
            throw Error(
                "All AngularDialogComponents must have tim-angular-dialog component in their template."
            );
        }
        this.frame.closeFn = () => this.dismiss();
    }

    close(r: Result) {
        this.closed = true;
        this.closeWithResult.emit(r);
        this.resultDefer.resolve(r);
        this.frame.saveSize();
        this.frame.savePos();
    }

    dismiss() {
        this.closed = true;
        this.resultDefer.reject("Dialog was closed from the X button");
        this.frame.saveSize();
        this.frame.savePos();
    }
}
