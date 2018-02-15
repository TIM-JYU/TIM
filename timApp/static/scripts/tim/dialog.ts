import angular from "angular";
import {IController} from "angular";
import "angular-ui-bootstrap";
import {IModalInstanceService} from "angular-ui-bootstrap";
import {timApp} from "./app";
import {$templateCache, $uibModal, $window} from "./ngimport";

export abstract class DialogController<T, Ret, ComponentName extends string> implements IController {
    public readonly component: ComponentName;
    public readonly ret: Ret;
    public readonly resolve: T;
    protected closed = false;
    private readonly modalInstance: angular.ui.bootstrap.IModalInstanceService;

    abstract getTitle(): string;

    protected close(returnValue: Ret) {
        this.closed = true;
        this.modalInstance.close(returnValue);
    }

    protected dismiss() {
        this.closed = true;
        this.modalInstance.dismiss();
    }
}

export type Dialog<T extends DialogController<T["resolve"], T["ret"], T["component"]>> = DialogController<T["resolve"], T["ret"], T["component"]>;

class MessageDialogController extends DialogController<{message: string}, {}, "timMessageDialog"> {
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
        ...tmpl,
    });
}

registerDialogComponent("timMessageDialog",
    MessageDialogController,
    {
        template: `<div class="modal-header">
    <h4 class="modal-title" id="modal-title">Message</h4>
</div>
<div class="modal-body" id="modal-body" ng-bind-html="$ctrl.getMessage()">
</div>
<div class="modal-footer">
    <button class="timButton" type="button" ng-click="$ctrl.ok()">OK
    </button>
</div>
`,
    });

export async function showMessageDialog(message: string) {
    return showDialog<MessageDialogController>("timMessageDialog", {message: () => message});
}

export async function showDialog<T extends Dialog<T>>(component: T["component"], resolve: {[P in keyof T["resolve"]]: () => T["resolve"][P]}): Promise<T["ret"]> {
    $templateCache.put("uib/template/modal/window.html", `
<div tim-draggable-fixed="" style="pointer-events: auto;" class="modal-dialog {{size ? 'modal-' + size : ''}}">
    <div class="modal-content" uib-modal-transclude>

    </div>
</div>`);
    const instance: IModalInstanceService = $uibModal.open({
        animation: !$window.IS_TESTING,
        backdrop: false,
        component: component,
        openedClass: "unused-class", // prevents scrolling from being disabled
        resolve: resolve,
        windowClass: "no-pointer-events", // enables clicking things outside dialog
    });
    return await instance.result;
}
