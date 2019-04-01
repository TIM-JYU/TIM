/**
 * Defines the client-side implementation of a drag plugin.
 */
import angular, {INgModelOptions} from "angular";
import {IRootElementService, IScope} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    getDefaults,
    Info,
    nullable,
    PluginBase,
    pluginBindings,
    PluginMeta,
    withDefault
} from "tim/plugin/util";
import {$http} from "../../../static/scripts/tim/util/ngimport";
import {markAsUsed, to} from "tim/util/utils";
//import ad from "angularjs-dragula";
import {polyfill} from "mobile-drag-drop";
import drag from "angular-drag-and-drop-lists";

markAsUsed(drag);

// optional import of scroll behaviour
import {scrollBehaviourDragImageTranslateOverride} from "mobile-drag-drop/scroll-behaviour";

const dragApp = angular.module("dragApp", ["ngSanitize", "dndLists"]); // ad(angular)
export const moduleDefs = [dragApp];

const DragMarkup = t.intersection([
    t.partial({
        inputstem: t.string,
        words: t.array(t.string),
        copy: t.boolean,
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
    protected static $inject = ["$scope", "$element"];
    private error?: string;
    private words?: string [];
    private copy?: boolean;
    private effectAllowed?: string;
    private vctrl!: ViewCtrl;
    private wordObjs?: {id: number, word: string}[];

    constructor(
        protected scope: IScope,
        protected element: IRootElementService,
        ) {
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
        this.copy = this.attrs.copy || false;
        if (this.copy) {
            this.effectAllowed = "copy";
            this.wordObjs = this.words.map((x, i) => ({id: i, word: x, effectAllowed: "copy"}));
        } else {
            this.effectAllowed = "move";
            this.wordObjs = this.words.map((x, i) => ({id: i, word: x, effectAllowed: "move"}));
        }

        // const kaikki = this.vctrl.getTimComponentsByRegex(".*");
        // for (let item of kaikki) {
        //    if (item instanceof DragController) {
        //        if (item.dragulaService) {
        //            this.dragulaService = item.dragulaService;
        //        }
        //    }
        // }
       // ////this.dragulaService.setOptions(this.vctrl.scope, 'dropzone');
        // if(this.dragulaService) {
        // this.dragulaService.options(this.vctrl.scope, 'dropzone', {
        //    direction: "horizontal",
        //    copy: (el: Element, source: DragController) => { return true; }
        // });
        // }

        polyfill({
            // use this to make use of the scroll behaviour
            dragImageTranslateOverride: scrollBehaviourDragImageTranslateOverride
        });

        // console.log(this.dragulaService);
        // const drake = this.dragulaService.find(this.vctrl.scope,'dropzone');
        //// if scope is vctrl.scope then there is only one dragulaservice and all use the same service
        //// therefore the options are the same for all the plugins

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
        if (this.wordObjs) {
            return this.wordObjs.map((el) => {
                return el.word;
            }).toString();
        } else {
            return "";
        }
    }

    /**
     * Returns contained words as a string array
     * @returns {string} The word..
     */
    getContentArray(): string[] {
        if (this.wordObjs) {
            return this.wordObjs.map((el) => {
                return el.word;
            });
        }
        return [];
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

        this.wordObjs = result.map((x, i) => ({id: i, word: x}));
        console.log("setPluginWords", this.words, this.getName());
    }

    setCopyOrMove(): string {
        if (this.copy === true) {
            return "copy";
        } else {
            return "move";
        }
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

    onDragStart(e: JQuery.Event) {
        const d = e.originalEvent as DragEvent;
        //e.
        const n = this.getName()!;
        d.dataTransfer!.setData("text", "kissa");
        d.dataTransfer!.effectAllowed = "move";
    }

    onDragOver(e: JQuery.Event) {
        const d = e.originalEvent as DragEvent;
        d.preventDefault();
    }

    onDrop(e: JQuery.Event) {
        const d = e.originalEvent as DragEvent;
        d.preventDefault();

        // const component = this.vctrl.getTimComponentByName(json[1]);
        // if (component === this) {
        //    return;
        // }
        // d.dataTransfer!.dropEffect = "move";
        // component!.setPluginWords!([]);
        // console.log(d.dataTransfer!.getData("sana"));
        // if (d.dataTransfer && this.wordObjs)
        {//

        //    this.words.push(d.dataTransfer.getData("text"));
        //    this.setPluginWords(this.words);
        }

    }

    protected getAttributeType() {
        return DragAll;
    }

}

dragApp.component("dragRunner", {
    bindings: pluginBindings,
    controller: DragController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div>
    <div class="form-inline">
    <div class="draggingarea">
     <ul class="dropword" dnd-list="$ctrl.wordObjs" dnd-horizontal-list="true" dnd-effect-allowed="{{$ctrl.effectAllowed}}">
        <li ng-repeat='item in $ctrl.wordObjs' class="dragword"
                    dnd-draggable="item"
                    dnd-moved="$ctrl.wordObjs.splice($index, 1)"
                    dnd-effect-allowed="{{item.effectAllowed}}">
        {{item.word}}
        </li>
     </ul>
    </div>
    </div>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>

`,
});
