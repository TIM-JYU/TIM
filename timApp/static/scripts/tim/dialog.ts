import * as angular from "angular";
import "angular-ui-bootstrap";
import {$uibModal} from "./ngimport";

export function showDialog(message: string) {
    class DialogController {
        private instance: angular.ui.bootstrap.IModalServiceInstance;

        constructor(uibInstance: angular.ui.bootstrap.IModalServiceInstance) {
            this.instance = uibInstance;
        }

        public ok() {
            this.instance.close();
        }
    }

    DialogController.$inject = ["$uibModalInstance"];

    $uibModal.open({
        ariaDescribedBy: "modal-body",
        ariaLabelledBy: "modal-title",
        controller: DialogController,
        controllerAs: "$ctrl",
        template: `<div class="modal-header">
    <h4 class="modal-title" id="modal-title">Message</h4>
</div>
<div class="modal-body" id="modal-body">
    ${message}
</div>
<div class="modal-footer">
    <button class="timButton" type="button" ng-click="$ctrl.ok()">OK
    </button>
</div>
`,
    });
}
