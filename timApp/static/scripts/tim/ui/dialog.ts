import {IController, IPromise, IScope, ITranscludeFunction} from "angular";
import "angular-ui-bootstrap";
import {IModalInstanceService} from "angular-ui-bootstrap";
import {timApp} from "../app";
import {KEY_ESC} from "../util/keycodes";
import {$rootScope, $templateCache, $uibModal} from "../util/ngimport";
import {Binding, markAsUsed, Require, TimDefer} from "../util/utils";
import * as dg from "./draggable";
import {DraggableController, Pos, VisibilityFix} from "./draggable";

markAsUsed(dg);

type ModalScope = IScope & {$$topModalIndex?: number, $$childHead?: ModalScope, $$prevSibling: ModalScope, $$nextSibling: ModalScope};

export abstract class DialogController<T, Ret> implements IController {
    public readonly ret!: Ret; // only used for typing
    public readonly resolve!: Binding<T, "<">;
    protected closed = false;
    protected readonly draggable!: Require<DraggableController>;
    private readonly modalInstance!: Binding<IModalInstance<DialogController<T, Ret>>, "<">;

    protected abstract getTitle(): string;

    constructor(protected element: JQLite, protected scope: IScope) {
    }

    $onInit() {
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

    public async moveTo(p: Pos) {
        await this.draggable.moveTo(p);
        this.draggable.ensureFullyInViewport();
    }
}

class MessageDialogController extends DialogController<{message: string}, {}> {
    static $inject = ["$element", "$scope"] as const;
    static readonly component = "timMessageDialog";

    constructor(protected element: JQLite, protected scope: IScope) {
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

interface IServiceMap {
    $element: JQLite;
    $scope: IScope;
}

type ServiceName = keyof IServiceMap;
type MapServiceNames<Names extends readonly [...ServiceName[]]> = {
    [K in keyof Names]: Names[K] extends keyof IServiceMap ? IServiceMap[Names[K]] : never;
};

export function registerDialogComponent<T extends DialogController<unknown, unknown>, ServiceNames extends readonly [...ServiceName[]]>(
    controller: {component: string, $inject: ServiceNames} & (new (...args: MapServiceNames<ServiceNames>) => T),
    tmpl: {template: string, templateUrl?: never}
        | {templateUrl: string, template?: never},
    controllerAs: string = "$ctrl") {
    timApp.component(controller.component, {
        bindings: {
            modalInstance: "<",
            resolve: "<",
        },
        controller: controller as unknown as new(...args: unknown[]) => IController,
        controllerAs,
        require: {
            draggable: "^timDraggableFixed",
        },
        ...tmpl,
    });
}

class TimDialogCtrl implements IController {
    static $inject = ["$scope", "$transclude"];
    private draggable: DraggableController | undefined;
    private hasFooter: boolean;

    constructor(private scope: IScope, private transclude: ITranscludeFunction) {
        this.hasFooter = transclude.isSlotFilled("footer");
    }

    $onInit() {
    }
}

function getModalAndMaxIndex(scope: IScope) {
    let mymodal = scope as ModalScope;
    while (mymodal.$$topModalIndex === undefined) {
        mymodal = mymodal.$parent as ModalScope;
    }
    let modal = ($rootScope as ModalScope).$$childHead;
    while (modal != null && modal.$$prevSibling != null) {
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

timApp.component("timDialog", {
    template: `
<div style="display: none" ng-mousedown="$ctrl.bringToFront()" class="modal-header">
    <h4 class="modal-title" id="modal-title" ng-transclude="header">Modal</h4>
</div>
<div ng-mousedown="$ctrl.bringToFront()" class="modal-body" id="modal-body" ng-transclude="body">
</div>
<div ng-if="$ctrl.hasFooter" ng-mousedown="$ctrl.bringToFront()" class="modal-footer" ng-transclude="footer">
</div>
    `,
    controller: TimDialogCtrl,
    require: {
        draggable: "?^timDraggableFixed",
    },
    transclude: {
        body: "dialogBody",
        footer: "?dialogFooter",
        header: "?dialogHeader",
    },
});

registerDialogComponent(MessageDialogController,
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
    return showDialog(MessageDialogController, {message: () => message});
}

export interface IModalInstance<T extends DialogController<unknown, unknown>> extends IModalInstanceService {
    result: Promise<T["ret"]>;
    dialogInstance: TimDefer<T>;

    close(result: T["ret"]): void;
}

export function showDialog<T extends DialogController<unknown, unknown>, ServiceNames extends readonly [...ServiceName[]]>(
    component: {component: string, $inject: ServiceNames} & (new (...args: MapServiceNames<ServiceNames>) => T),
    resolve: { [K in keyof T["resolve"]]: () => T["resolve"][K] },
    opts: {
        saveKey?: string,
        classes?: string[],
        showMinimizeButton?: boolean,
        size?: "sm" | "md" | "lg" | "xs", // xs is custom TIM style
        absolute?: boolean,
        forceMaximized?: boolean,
        backdrop?: boolean,
    } = {}): IModalInstance<T> {
    $templateCache.put("uib/template/modal/window.html", `
<div tim-draggable-fixed
     click="${opts.showMinimizeButton !== undefined ? opts.showMinimizeButton : true}"
     resize="true"
     save="${opts.saveKey || component.component}"
     absolute="${opts.absolute || false}"
     force-maximized="${opts.forceMaximized || false}"
     style="pointer-events: auto;"
     class="modal-dialog {{size ? 'modal-' + size : ''}}">
    <div class="draggable-content modal-content" uib-modal-transclude>

    </div>
</div>`);
    const instance: IModalInstanceService = $uibModal.open({
        animation: false,
        backdrop: opts.backdrop || false,
        component: component.component,
        keyboard: false,
        openedClass: "unused-class", // prevents scrolling from being disabled
        resolve: resolve,
        windowClass: (opts.classes || ["no-pointer-events"]).join(" "), // no-pointer-events enables clicking things outside dialog
        size: opts.size || "md",
    });
    const custom = instance as IModalInstance<T>;
    custom.dialogInstance = new TimDefer<T>();
    return custom;
}
