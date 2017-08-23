import angular from "angular";
import {IController} from "angular";
import "angular-ui-bootstrap";
import {$templateCache, $uibModal} from "./ngimport";
import {IModalInstanceService} from "angular-ui-bootstrap";
import {timApp} from "./app";

export class DialogController<T, Ret = {}> implements IController {
    protected modalInstance: angular.ui.bootstrap.IModalInstanceService;
    protected resolve: T;

    protected close(returnValue: Ret) {
        this.modalInstance.close(returnValue);
    }

    protected dismiss() {
        this.modalInstance.dismiss();
    }
}

class MessageDialogController extends DialogController<{message: string}> {
    public ok() {
        this.close({});
    }

    public getMessage() {
        return this.resolve.message;
    }
}

export function registerDialogComponent<T>(name: string,
                                           controller: new (...args: any[]) => DialogController<T>,
                                           {template, templateUrl}: {template?: string, templateUrl?: string},
                                           controllerAs: string = "$ctrl") {
    timApp.component(name, {
        bindings: {
            modalInstance: "<",
            resolve: "<",
        },
        controller,
        controllerAs,
        template,
        templateUrl,
    });
}

registerDialogComponent("timMessageDialog",
    MessageDialogController,
    {
        template: `<div class="modal-header">
    <h4 class="modal-title" id="modal-title">Message</h4>
</div>
<div class="modal-body" id="modal-body">
    {{$ctrl.getMessage()}}
</div>
<div class="modal-footer">
    <button class="timButton" type="button" ng-click="$ctrl.ok()">OK
    </button>
</div>
`,
    });

export async function showMessageDialog(message: string) {
    return showDialog("timMessageDialog", {message: () => message});
}

export async function showDialog<T extends DialogController<Params, Ret>, Params, Ret>(component: string, resolve: {[P in keyof Params]: () => Params[P]}): Promise<Ret> {
    $templateCache.put("uib/template/modal/window.html", `
<div tim-draggable-fixed="" style="pointer-events: auto;" class="modal-dialog {{size ? 'modal-' + size : ''}}">
    <div class="modal-content" uib-modal-transclude>

    </div>
</div>`);
    const instance: IModalInstanceService = $uibModal.open({
        backdrop: false,
        component: component,
        openedClass: "unused-class", // prevents scrolling from being disabled
        resolve: resolve as any,
        windowClass: "no-pointer-events", // enables clicking things outside dialog
    });
    return await instance.result;
}
