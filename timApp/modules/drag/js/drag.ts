/**
 * Defines the client-side implementation of a drag plugin.
 */
import angular, {INgModelOptions} from "angular";
import {IRootElementService, IScope} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, getDefaults, Info, nullable, PluginBase, PluginMeta, withDefault} from "tim/plugin/util";
import {$http} from "../../../static/scripts/tim/util/ngimport";
import {to} from "tim/util/utils";
import ad from "angularjs-dragula";

const dragApp = angular.module("dragApp", ["ngSanitize", ad(angular)]);
export const moduleDefs = [dragApp];

const DragMarkup = t.intersection([
    t.partial({
        inputstem: t.string,
        words: t.array(t.string),
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
        words: t.array(t.string),
    }),
    t.type({
        info: Info,
        markup: DragMarkup,
        preview: t.boolean,
    }),
]);

class DragController extends PluginBase<t.TypeOf<typeof DragMarkup>, t.TypeOf<typeof DragAll>, typeof DragAll> implements ITimComponent {
    protected static $inject = ["$scope", "$element", "dragulaService"];
    private error?: string;
    private words?: string [];
    private vctrl!: ViewCtrl;


    constructor(
        protected scope: IScope,
        protected element: IRootElementService,
        protected dragulaService: any) {
        super(scope, element);
    }

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
        this.words = this.attrs.words || [];
        const scope = this.vctrl.scope;
        // console.log(scope);
        // console.log(this.scope.$parent);
        // console.log(this.scope.$parent.$parent);
        // console.log(this.scope.$parent.$parent.$parent);

        // this calls method from viewctrl, but for some reason dragula does seemingly the same but it doesn't call
        // console.log(this.vctrl.scope.$eval("$ctrl.getName()"));

        this.dragulaService.options(scope, this.getName(), {
            direction: "horizontal",
            copy: true,                        // elements are moved by default, not copied
        });

        scope.$on(`drag`, (e, el: JQuery) => {
            console.log("raahataan sanaa " + el);
        });
        scope.$on(`${this.getName()}.drag`, (e, el: JQuery) => {
            console.log("raahataan sanaa " + el);
        });
        scope.$on(`drop`, (e, el: JQuery) => {
            console.log("dropzone " + el);
        });
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
     * Returns contained words separated with a comma
     * @returns {string} The words..
     */
    getContent(): string {
        return this.words!.toString() || "No draggable words";
    }

     /**
     * Returns contained words as a string array
     * @returns {string} The word..
     */
    getContentArray(): string[] {
        return this.words!;
    }

    setPluginWords(words: string []) {
        // shuffle algorithm from csparsons.ts
        const result = words.slice();
        const n = words.length;
        for (let i = n - 1; i >= 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            const tmp = result[i];
            result[i] = result[j];
            result[j] = tmp;
        }

        this.words = result;
        console.log("setPluginWords", this.words, this.getName());
    }

    async save() {
        this.doSave(false);
        return "";
    }

    async doSave(nosave: false) {
        const params = {
            input: {
                nosave: false,
                words: this.words,
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
    <div class="form-inline">
     <div class="dropword" dragula-scope="$ctrl.vctrl.scope" dragula="$ctrl.getName()" dragula-model='$ctrl.words'>
        <span class="dragword"  ng-bind-html='item'  ng-repeat='item in $ctrl.words track by $index'>
        </span>
     </div>
    </div>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
