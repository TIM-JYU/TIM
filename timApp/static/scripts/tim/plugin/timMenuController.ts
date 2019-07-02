/**
 *
 */
import {IController, IRootElementService} from "angular";
import {timApp as timMenuApp} from "../app";
import {pluginBindings, PluginMeta} from "./util";

class TimMenuController implements IController {
    static $inject = ["$element"];
    private pluginMeta: PluginMeta;

    constructor(private element: IRootElementService) {
        this.pluginMeta = new PluginMeta(element);
    }

    public $onInit() {
    }
}

timMenuApp.component("timMenuRunner", {
    bindings: pluginBindings,
    controller: TimMenuController,
    template: `
<div>
TimMenu plugin here
</div>
`,
});
