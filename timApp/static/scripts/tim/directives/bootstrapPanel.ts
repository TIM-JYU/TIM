import {IController} from "angular";
import {IRootElementService} from "angular";
import {timApp} from "tim/app";

class BootstrapPanelController implements IController {
    private static $inject = ["$element"];
    private element: IRootElementService;
    private closeFn: () => void;

    constructor(element: IRootElementService) {
        this.element = element;
    }

    $onInit() {

    }

    public close() {
        this.closeFn();
    }
}

timApp.component("bootstrapPanel", {
    bindings: {
        closeFn: "&",
        showClose: "=?",
        title: "@?",
    },
    controller: BootstrapPanelController,
    templateUrl: "/static/templates/bootstrapPanel.html",
    transclude: true,
});
