import {IController, IRootElementService, IScope} from "angular";
import {getParId} from "tim/document/parhelpers";
import {timApp} from "../app";
import {onClick} from "../document/eventhandlers";
import {ViewCtrl} from "../document/viewctrl";
import {ParCompiler} from "../editor/parCompiler";
import {openEditorSimple} from "../editor/pareditor";
import {DestroyScope} from "../ui/destroyScope";
import {isArrowKey, KEY_DOWN, KEY_ENTER, KEY_ESC, KEY_F2, KEY_LEFT, KEY_RIGHT, KEY_TAB, KEY_UP} from "../util/keycodes";
import {$http, $timeout} from "../util/ngimport";
import {Binding} from "../util/utils";
import {hideToolbar, isToolbarEnabled, openTableEditorToolbar} from "./timTableEditorToolbar";
import {PluginMeta} from "./util";


export class TableFormController implements IController {
    private static $inject = ["$scope", "$element"];
    private error: string = "";
    private taskUrl: string = "";

    public viewctrl?: ViewCtrl;
    private task: boolean = false;
    private isRunning: boolean = false;
    private pluginMeta: PluginMeta;

    constructor(private scope: IScope, private element: IRootElementService) {
        this.pluginMeta = new PluginMeta(element);
    }

    getTaskUrl(): string {
        if (this.taskUrl) {
            return this.taskUrl;
        }
        const url = this.pluginMeta.getAnswerUrl();
        this.taskUrl = url;
        return url;
    }

    protected getParentAttr(name: string) {
        return this.element.parent().attr(name);
    }

    protected getTaskId() {
        return this.getParentAttr("id");
    }

    protected getPlugin() {
        return this.getParentAttr("data-plugin");
    }

    protected getRootElement() {
        return this.element[0];
    }

    $onInit() {
    }

}

timApp.component("tableForm", {
    controller: TableFormController,
    bindings: {
        //data: "<",
    },
    require: {
        viewctrl: "?^timView",
    },
    template: `
<div class="no-popup-menu">
    <input type="text"/>
    <tim-table></tim-table>
</div>
`,
});
