import {IController} from "angular";
import {IRootElementService} from "angular";
import {timApp} from "tim/app";
import {Binding, Require} from "../utils";

class BootstrapPanelController implements IController {
    private static $inject = ["$element"];
    private element: IRootElementService;
    private closeFn!: Require<() => void>;
    private show: Binding<boolean | undefined, "<">;
    private showClose: Binding<boolean | undefined, "<">;
    private title: Binding<string | undefined, "<">;

    constructor(element: IRootElementService) {
        this.element = element;
    }

    $onInit() {

    }

    public close() {
        this.show = false;
        this.closeFn();
    }
}

timApp.component("bootstrapPanel", {
    bindings: {
        closeFn: "&",
        show: "=?",
        showClose: "=?",
        title: "@?",
    },
    controller: BootstrapPanelController,
    templateUrl: "/static/templates/bootstrapPanel.html",
    transclude: true,
});
