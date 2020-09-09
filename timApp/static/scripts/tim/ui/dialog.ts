import {IController, IScope, IModule} from "angular";
import "angular-ui-bootstrap";
import {TimDefer} from "tim/util/timdefer";
import {DialogController} from "tim/ui/dialogController";
import {timApp} from "../app";
import {$templateCache, $uibModal} from "../util/ngimport";
import {markAsUsed} from "../util/utils";
import * as dg from "./draggable";

type IModalInstanceService = angular.ui.bootstrap.IModalInstanceService;

markAsUsed(dg);

class MessageDialogController extends DialogController<{message: string}, unknown> {
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
    registerDialogComponentForModule(timApp, controller, tmpl, controllerAs);
}

export function registerDialogComponentForModule<T extends DialogController<unknown, unknown>, ServiceNames extends readonly [...ServiceName[]]>(
    module: IModule,
    controller: {component: string, $inject: ServiceNames} & (new (...args: MapServiceNames<ServiceNames>) => T),
    tmpl: {template: string, templateUrl?: never}
        | {templateUrl: string, template?: never},
    controllerAs: string = "$ctrl") {
    module.component(controller.component, {
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

export function showMessageDialog(message: string) {
    return showDialog(MessageDialogController, {message: () => message}).result;
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
     click="${(opts.showMinimizeButton !== undefined ? opts.showMinimizeButton : true).toString()}"
     resize="true"
     save="${opts.saveKey ?? component.component}"
     absolute="${(opts.absolute ?? false).toString()}"
     force-maximized="${(opts.forceMaximized ?? false).toString()}"
     style="pointer-events: auto;"
     class="modal-dialog {{size ? 'modal-' + size : ''}}">
    <div class="draggable-content modal-content" uib-modal-transclude>

    </div>
</div>`);
    const instance: IModalInstanceService = $uibModal.open({
        animation: false,
        backdrop: opts.backdrop ?? false,
        component: component.component,
        keyboard: false,
        openedClass: "unused-class", // prevents scrolling from being disabled
        resolve: resolve,
        windowClass: (opts.classes ?? ["no-pointer-events"]).join(" "), // no-pointer-events enables clicking things outside dialog
        size: opts.size ?? "md",
    });
    const custom = instance as IModalInstance<T>;
    custom.dialogInstance = new TimDefer<T>();
    return custom;
}
