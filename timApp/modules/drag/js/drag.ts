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
import {polyfill} from "mobile-drag-drop";
import drag from "angular-drag-and-drop-lists";

markAsUsed(drag);

// optional import of scroll behaviour
import {scrollBehaviourDragImageTranslateOverride} from "mobile-drag-drop/scroll-behaviour";

const dragApp = angular.module("dragApp", ["ngSanitize", "dndLists"]);
export const moduleDefs = [dragApp];

const DragMarkup = t.intersection([
    t.partial({
        inputstem: t.string,
        words: t.array(t.string),
        copy: t.keyof({target: null, source: null}),
        type: t.string,
        max: t.number,
        trash: t.boolean,
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
    private type?: string;
    private max?: number;
    private copy?: string;
    private trash?: boolean;
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
        this.max = this.attrs.max || Number.MAX_VALUE;
        this.copy = this.attrs.copy || "";
        this.type = this.attrs.type || "";
        this.trash = this.attrs.trash || false;
        this.wordObjs = [];
        this.createWordobjs(this.attrs.words || []);

        polyfill({
            // use this to make use of the scroll behaviour
            dragImageTranslateOverride: scrollBehaviourDragImageTranslateOverride
        });

        this.addToCtrl();
    }

    createWordobjs(words: string[]){
        if (!words) return;
        this.wordObjs = [];
        if (this.copy === "source") {
            this.effectAllowed = "copy";
            this.wordObjs = words.map((x, i) => ({
                id: i,
                word: x,
                effectAllowed: "copy",
                type: this.type
            }));
            this.max = this.wordObjs.length;
        } else if (this.copy === "target") {
            this.effectAllowed = "copy";
            this.wordObjs = words.map((x, i) => ({
                id: i,
                word: x,
                effectAllowed: "copy",
                type: this.type
            }));
        } else {
            if (this.effectAllowed === "copy") return;  // TODO: make better if-elses to remove this bubblegum fix
            this.effectAllowed = "move";
            this.wordObjs = words.map((x, i) => ({
                id: i,
                word: x,
                effectAllowed: "move",
                type: this.type
            }));
        }
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
        let s: string;
        s = "";
        if (this.wordObjs) {
            this.wordObjs.map((el) => {
                s = s + "," + el.word;
            });
        }
        return s;
        console.log(s);
    }

    /**
     * Returns contained words as a string array
     * @returns {string} The word..
     */
    getContentArray(): string[] {
        let words: string[];
        words = [];
        if (this.wordObjs) {
            this.wordObjs.map((el) => {
                words.push(el.word);
            });
        }
        return words;
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
        this.createWordobjs(result);
    }


    async save() {
        this.doSave(false);
        return "";
    }

    async doSave(nosave: false) {
        const params = {
            input: {
                nosave: false,
                words: this.getContentArray(),
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
    bindings: pluginBindings,
    controller: DragController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div>
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <div class="form-inline">
    <div class="draggingarea">
     <ul ng-if="::$ctrl.trash" class="dropword" dnd-list="[]" dnd-effect-allowed="all"><li> TRASHCAN </li></ul>
     <ul ng-if="::!$ctrl.trash" class="dropword" dnd-list="$ctrl.wordObjs" dnd-allowed-types="[$ctrl.type]"
        dnd-horizontal-list="true"
        dnd-disable-if="$ctrl.wordObjs.length >= $ctrl.max"
        dnd-effect-allowed="{{$ctrl.effectAllowed}}">
        <li ng-repeat='item in $ctrl.wordObjs' class="dragword"
                    dnd-draggable="item"
                    dnd-type = "item.type"
                    dnd-moved="$ctrl.wordObjs.splice($index, 1)"
                    dnd-canceled="($ctrl.copy !== 'target' )|| $ctrl.wordObjs.splice($index, 1)"
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
