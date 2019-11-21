import {IController} from "angular";
import {timApp} from "tim/app";
import {Binding} from "../util/utils";

class BootstrapPanelController implements IController {
    static $inject = ["$element"];
    private element: JQLite;
    private closeFn?: Binding<() => void, "&">;
    private show: Binding<boolean | undefined, "<">;
    private showClose: Binding<boolean | undefined, "<">;
    private title: Binding<string | undefined, "<">;

    constructor(element: JQLite) {
        this.element = element;
    }

    $onInit() {

    }

    public close() {
        this.show = false;
        if (this.closeFn) {
            this.closeFn();
        }
    }
}

timApp.component("bootstrapPanel", {
    bindings: {
        closeFn: "&?",
        show: "=?",
        showClose: "=?",
        title: "@?",
    },
    controller: BootstrapPanelController,
    templateUrl: "/static/templates/bootstrapPanel.html",
    transclude: true,
});
