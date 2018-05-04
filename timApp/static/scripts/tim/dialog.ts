import {IController, IPromise, IRootElementService, IScope} from "angular";
import "angular-ui-bootstrap";
import {IModalInstanceService} from "angular-ui-bootstrap";
import {timApp} from "./app";
import * as dg from "./directives/draggable";
import {DraggableController} from "./directives/draggable";
import {$rootScope, $templateCache, $uibModal, $window} from "./ngimport";
import {markAsUsed} from "./utils";

markAsUsed(dg);

export abstract class DialogController<T, Ret, ComponentName extends string> implements IController {
    public readonly component: ComponentName;
    public readonly ret: Ret;
    public readonly resolve: T;
    protected closed = false;
    protected readonly draggable: DraggableController;
    private readonly modalInstance: IModalInstanceService;

    protected abstract getTitle(): string;

    constructor(protected element: IRootElementService, protected scope: any) {
        this.handleEscPress = this.handleEscPress.bind(this);
    }

    $onInit() {
        this.draggable.setModal(this.modalInstance);
        this.draggable.setCloseFn(() => this.dismiss());
        this.draggable.setCaption(this.getTitle());
        this.draggable.setDragClickFn(() => bringToFront(this.scope));
        this.draggable.setInitialLayout();
        bringToFront(this.scope);
        document.addEventListener("keydown", this.handleEscPress);
    }

    handleEscPress(e: KeyboardEvent) {
        if (e.keyCode === 27 && this.isTopMostDialog()) {
            this.dismiss();
        }
    }

    public getDraggable() {
        return this.draggable;
    }

    protected close(returnValue: Ret) {
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
}

export type Dialog<T extends DialogController<T["resolve"], T["ret"], T["component"]>> = DialogController<T["resolve"], T["ret"], T["component"]>;

class MessageDialogController extends DialogController<{message: string}, {}, "timMessageDialog"> {
    private static $inject = ["$element", "$scope"];

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    public getTitle() {
        return "Message";
    }

    public ok() {
        this.close({});
    }

    public getMessage() {
        return this.resolve.message;
    }
}

export function registerDialogComponent<T extends Dialog<T>>(name: T["component"],
                                                             controller: new (...args: any[]) => T,
                                                             tmpl: {template: string, templateUrl?: never}
                                                                 | {templateUrl: string, template?: never},
                                                             controllerAs: string = "$ctrl") {
    timApp.component(name, {
        bindings: {
            modalInstance: "<",
            resolve: "<",
        },
        controller,
        controllerAs,
        require: {
            draggable: "^timDraggableFixed",
        },
        ...tmpl,
    });
}

class TimDialogCtrl implements IController {
    private static $inject = ["$scope"];
    private draggable: DraggableController | undefined;

    constructor(private scope: IScope) {
    }

    $onInit() {
    }
}

function getModalAndMaxIndex(scope: any) {
    let mymodal = scope;
    while (mymodal.$$topModalIndex === undefined) {
        mymodal = mymodal.$parent;
    }
    let modal = ($rootScope as any).$$childHead;
    while (modal.$$prevSibling != null) {
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

function bringToFront(modalScope: any) {
    const {modal, maxIndex} = getModalAndMaxIndex(modalScope);
    modal.$$topModalIndex = maxIndex + 1;
}

timApp.component("timDialog", {
    template: `
<div style="display: none" ng-mousedown="$ctrl.bringToFront()" class="modal-header">
    <h4 class="modal-title" id="modal-title" ng-transclude="header">Modal</h4>
</div>
<div ng-mousedown="$ctrl.bringToFront()" class="modal-body" id="modal-body" ng-transclude="body">
</div>
<div ng-mousedown="$ctrl.bringToFront()" class="modal-footer" ng-transclude="footer">
</div>
    `,
    controller: TimDialogCtrl,
    require: {
        draggable: "?^timDraggableFixed",
    },
    transclude: {
        body: "dialogBody",
        footer: "dialogFooter",
        header: "dialogHeader",
    },
});

registerDialogComponent("timMessageDialog",
    MessageDialogController,
    {
        template: `
<tim-dialog>
    <dialog-header>
        Message
    </dialog-header>
    <dialog-body ng-bind-html="$ctrl.getMessage()">

    </dialog-body>
    <dialog-footer>
        <button class="timButton" type="button" ng-click="$ctrl.ok()">OK</button>
    </dialog-footer>
</tim-dialog>
        `,
    });

export async function showMessageDialog(message: string) {
    return showDialog<MessageDialogController>("timMessageDialog", {message: () => message});
}

export interface IModalInstance<Result> {
    result: IPromise<Result>;

    close(result: Result): void;
}

export function showDialog<T extends Dialog<T>>(component: T["component"],
                                                resolve: {[P in keyof T["resolve"]]: () => T["resolve"][P]},
                                                opts: {
                                                    saveKey?: string,
                                                    classes?: string[],
                                                    size?: "sm" | "md" | "lg",
                                                    absolute?: boolean,
                                                    forceMaximized?: boolean,
                                                } = {}): IModalInstance<T["ret"]> {
    $templateCache.put("uib/template/modal/window.html", `
<div tim-draggable-fixed
     click="true"
     resize="true"
     save="${opts.saveKey || component}"
     absolute="${opts.absolute || false}"
     force-maximized="${opts.forceMaximized || false}"
     style="pointer-events: auto;"
     class="modal-dialog {{size ? 'modal-' + size : ''}}">
    <div class="draggable-content modal-content" uib-modal-transclude>

    </div>
</div>`);
    const instance: IModalInstanceService = $uibModal.open({
        animation: !$window.IS_TESTING,
        backdrop: false,
        component: component,
        keyboard: false,
        openedClass: "unused-class", // prevents scrolling from being disabled
        resolve: resolve,
        windowClass: (opts.classes || ["no-pointer-events"]).join(" "), // no-pointer-events enables clicking things outside dialog
        size: opts.size || "md",
    });
    return instance;
}
