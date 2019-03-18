/**
 * Defines the client-side implementation of a drag plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "../../../static/scripts/tim/util/ngimport";
import {to} from "tim/util/utils";
import "../../../cs/js/cs-parsons/jquery.ui.touch-punch.min.js";

const dragApp = angular.module("dragApp", ["ngSanitize"]);
export const moduleDefs = [dragApp];

const DragMarkup = t.intersection([
    t.partial({
        inputstem: t.string,
        word: t.string,
        followid: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const DragAll = t.intersection([
    t.partial({
        word: t.string,
    }),
    t.type({markup: DragMarkup}),
]);

class DragController extends PluginBase<t.TypeOf<typeof DragMarkup>, t.TypeOf<typeof DragAll>, typeof DragAll> implements ITimComponent {
    private error?: string;
    private word?: string;
    private vctrl!: ViewCtrl;

    getDefaultMarkup() {
        return {};
    }

    getGroups(): string[] {
        return [""];
    }

    /**
     * Returns either the followid-attribute or the name given to the plugin
     */
    getName(): (string | undefined) {
        if (this.attrs.followid) {
            return this.attrs.followid;
        }
        const taskId = this.pluginMeta.getTaskId();
        if (taskId) {
            return taskId.split(".")[1];
        }
    }

    /**
     *
     * @param group
     */
    belongsToGroup(group: string): boolean {
        return false;
    }

    $onInit() {
        super.$onInit();
        this.word = this.attrs.word;
        this.addToCtrl();
    }

    /**
     * Adds this plugin to ViewCtrl so other plugins can get information about the plugin though it.
     */
    addToCtrl() {
        const taskid = this.pluginMeta.getTaskId() || ""; // TODO: fix this dirty stuff
        const name = taskid.split(".");
        this.vctrl.addTimComponent(this);
    }

    initCode() {
        this.error = undefined;
    }

    /**
     * Returns the word as a string
     * @returns {string} The word..
     */
    getContent(): string {
        return this.word || "Nothing here";
    }

    save(): string {
        this.doSave(false);
        return "";
    }

    async doSave(nosave: false) {
        const params = {
            input: {
                nosave: false,
                word: this.word,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }

        const url = this.pluginMeta.getAnswerUrl();
        const r = await to($http.put<{ web: { result: string, error?: string } }>(url, params));

        if (r.ok) {
            const data = r.result.data;
            this.error = data.web.error;
            // this.result = data.web.result;
        } else {
            this.error = "Infinite loop or some other error?";
        }
    }

    onDragged(e: JQuery.Event) {
        const d = e.originalEvent as DragEvent;
        d.dataTransfer!.setData("Text", "moi");
        //d.dataTransfer!.effectAllowed = "move";
        //console.log(d.dataTransfer!.getData("Name"));
    }

    onDragStart(e: JQuery.Event) {
        const d = e.originalEvent as DragEvent;
        d.dataTransfer!.setData("Text", "moi");
        //d.dataTransfer!.setDragImage();
        //d.dataTransfer!.effectAllowed = "move";
        //console.log(d.dataTransfer!.getData("Name"));
    }

    protected getAttributeType() {
        return DragAll;
    }

}

dragApp.component("dragRunner", {
    bindings: {
        json: "@",
    },
    controller: DragController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        <div class="dragword"
             draggable="true"
             ng-on-dragstart="$ctrl.onDragStart($event)"
             <!--ng-on-drag="$ctrl.onDragged($event)"-->
             <!--ng-on-touchmove="$ctrl.onDragged($event)"-->
             >
        {{$ctrl.word}}
        </div>
        </span></label>
    </div>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`
});
