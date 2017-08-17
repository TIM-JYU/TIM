import angular from "angular";
import {IController} from "angular";
import "angular-ui-bootstrap";
import {$uibModal} from "./ngimport";
import {timApp} from "./app";

class DialogController implements IController {
    private static $inject = ["$uibModalInstance", "message"];
    private instance: angular.ui.bootstrap.IModalInstanceService;
    private message: string;

    constructor(uibInstance: angular.ui.bootstrap.IModalInstanceService, message: string) {
        this.instance = uibInstance;
        this.message = message;
    }

    $onInit() {

    }

    public ok() {
        this.instance.close();
    }
}

timApp.component("timDialog", {
    controller: DialogController,
    template: `<div class="modal-header">
    <h4 class="modal-title" id="modal-title">Message</h4>
</div>
<div class="modal-body" id="modal-body">
    $ctrl.message
</div>
<div class="modal-footer">
    <button class="timButton" type="button" ng-click="$ctrl.ok()">OK
    </button>
</div>
`,
});

export function showDialog(message: string) {
    $uibModal.open({
        ariaDescribedBy: "modal-body",
        ariaLabelledBy: "modal-title",
        component: "timDialog",
        resolve: {
            message: () => message,
        },
    });
}
