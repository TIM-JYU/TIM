import {IController, IPromise, IScope} from "angular";
import {$rootScope} from "tim/util/ngimport";
import {Binding, Require} from "tim/util/utils";
import {DraggableController, Pos, VisibilityFix} from "tim/ui/draggable";
import {KEY_ESC} from "tim/util/keycodes";
import {IModalInstance} from "tim/ui/dialog";

type ModalScope =
    IScope
    & { $$topModalIndex?: number, $$childHead?: ModalScope, $$prevSibling: ModalScope, $$nextSibling: ModalScope };

/**
 * @deprecated In new dialogs, use AngularDialogComponent class. See BookmarkDialogComponent for an example.
 */
export abstract class DialogController<T, Ret> implements IController {
    public readonly ret!: Ret; // only used for typing
    public readonly resolve!: Binding<T, "<">;
    protected closed = false;
    protected readonly draggable!: Require<DraggableController>;
    private readonly modalInstance!: Binding<IModalInstance<DialogController<T, Ret>>, "<">;

    protected abstract getTitle(): string;

    constructor(protected element: JQLite, protected scope: IScope) {
    }

    // The signature void | undefined ensures that the $onInit in the derived class is not async.
    // They must not be async because otherwise Edge browser (at least certain versions of it) gives
    // "out of stack space" error when opening a dialog. It somehow generates an infinite chain of promises, even
    // though the return value of $onInit is not used by AngularJS.
    $onInit(): void | undefined {
        this.modalInstance.dialogInstance.resolve(this);
        this.draggable.setModal(this.modalInstance);
        this.draggable.setCloseFn(() => this.dismiss());
        this.draggable.setCaptionCb(() => this.getTitle());
        this.draggable.setDragClickFn(() => bringToFront(this.scope));
        this.draggable.setInitialLayout(this.getInitialVisibility());
        bringToFront(this.scope);
        document.addEventListener("keydown", this.handleEscPress);
    }

    handleEscPress = (e: KeyboardEvent) => {
        if (e.keyCode === KEY_ESC && this.isTopMostDialog()) {
            this.dismiss();
            e.stopPropagation();
        }
    };

    public getDraggable() {
        return this.draggable;
    }

    protected getInitialVisibility(): VisibilityFix {
        return VisibilityFix.Full;
    }

    public closePromise(): IPromise<unknown> {
        return this.modalInstance.closed;
    }

    close(returnValue: Ret) {
        this.closed = true;
        this.modalInstance.close(returnValue);
        document.removeEventListener("keydown", this.handleEscPress);
    }

    protected dismiss() {
        if (this.confirmDismiss()) {
            this.closed = true;
            this.modalInstance.dismiss();
            document.removeEventListener("keydown", this.handleEscPress);
        }
    }

    private isTopMostDialog() {
        const {modal, maxIndex} = getModalAndMaxIndex(this.scope);
        return modal.$$topModalIndex === maxIndex;
    }

    protected confirmDismiss() {
        return true;
    }

    public async moveTo(p: Pos) {
        await this.draggable.moveTo(p);
        this.draggable.ensureFullyInViewport();
    }
}

function getModalAndMaxIndex(scope: IScope) {
    let mymodal = scope as ModalScope;
    while (mymodal.$$topModalIndex === undefined) {
        mymodal = mymodal.$parent as ModalScope;
    }
    let modal = ($rootScope as ModalScope).$$childHead;
    while (modal?.$$prevSibling != null) {
        modal = modal.$$prevSibling;
    }
    let maxIndex = -1;
    while (modal != null) {
        if (modal.$$topModalIndex !== undefined) {
            maxIndex = Math.max(maxIndex, modal.$$topModalIndex);
        }
        modal = modal.$$nextSibling;
    }
    return {modal: mymodal, maxIndex};
}

function bringToFront(modalScope: IScope) {
    const {modal, maxIndex} = getModalAndMaxIndex(modalScope);
    modal.$$topModalIndex = maxIndex + 1;
}
