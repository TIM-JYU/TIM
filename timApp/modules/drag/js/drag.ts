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
        max: t.refinement(t.number, (x) => x > 0, "a positive number value"),
        trash: t.boolean,
        shuffle: t.boolean,
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
    static $inject = ["$scope", "$element"];
    private error?: string;
    private forceSave = false;
    private type?: string;
    private max?: number;
    private copy?: string;
    private trash?: boolean;
    private shuffle?: boolean;
    private effectAllowed?: string;
    private vctrl!: ViewCtrl;
    private wordObjs?: Array<{id: number, word: string}>;

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
        this.type = this.attrs.type || " ";
        this.trash = this.attrs.trash || false;
        this.shuffle = this.attrs.shuffle || false;
        this.wordObjs = [];
        if (this.shuffle && this.attrs.words) {
            const words = this.shuffleWords(this.attrs.words);
            this.createWordobjs(words);
        } else {
            this.createWordobjs(this.attrs.words || []);
        }

        polyfill({
            // use this to make use of the scroll behaviour
            dragImageTranslateOverride: scrollBehaviourDragImageTranslateOverride,
        });

        window.addEventListener("touchmove", () => {
            // TODO: Find a proper fix or another shim since this is a tmp fix
        }, {passive: false});

        this.addToCtrl();
    }

    /**
     * Creates word-object-models that have attributes essential to the functionality of drag and drop actions.
     * In addition, initializes program logic.
     * @param words Array of strings to be transformed into draggable models.
     */
    createWordobjs(words: string[]){
        if (!words){
            return;
            //TODO: error message or default word objects?
        }
        this.wordObjs = [];
        this.effectAllowed = "move";
        if (this.copy === "source" || this.copy === "target") {
            this.effectAllowed = "copy";
        }
       this.wordObjs = words.map((x, i) => ({
           effectAllowed: this.effectAllowed,
           id: i,
           type: this.type,
           word: x,
       }));
        if (this.copy === "source") {
            this.max = this.wordObjs.length;
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


    /**
     * Returns contained words as a string separated with a comma
     * @returns {string} The words.
     */
    getContent(): string | undefined {
        let s: string;
        s = "";
        if (this.wordObjs) {
            this.wordObjs.map((el) => {
                s = s + "," + el.word;
            });
        }
        if (s.length < 1) return undefined;
        return s;
    }

    /**
     * Returns contained words as a string array
     * @returns {string} The words
     */
    getContentArray(): string[] | undefined {
        let words: string[];
        words = [];
        if (this.wordObjs) {
            this.wordObjs.map((el) => {
                words.push(el.word);
            });
        }
        if (words.length === 0) return undefined;
        return words;
    }
    /**
     * Force the plugin to save its information
     *
     * @param force Whether to force a save.
     */
    setForceAnswerSave(force: boolean) {
        this.forceSave = force;
    }

    /**
     * Sets words in plugin.
     * @param words The words to be set as draggable words in plugin.
     * @param shuffle Whether to shuffle words.
     */
    setPluginWords(words: string []) {
        if (this.shuffle) {
            this.createWordobjs(this.shuffleWords(words));
        } else {
            this.createWordobjs(words);
        }
    }

    /**
     * Shuffles string array
     * @param words Array of strings to be shuffled.
     */
    shuffleWords(words: string []): string []{
        // shuffle algorithm from csparsons.ts
        const result = words.slice();
        const n = words.length;
        for (let i = n - 1; i >= 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            const tmp = result[i];
            result[i] = result[j];
            result[j] = tmp;
        }
        return result;
    }

    async save() {
        const failure = await this.doSave(false);
        return failure;
    }

    async doSave(nosave: false) {
        const params = {
            input: {
                nosave: false,
                words: this.getContentArray(),
            },
            options: {
                forceSave: this.forceSave,
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
            if (data.web.error) {
                return data.web.error;
            }
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
        dnd-disable-if="{{$ctrl.wordObjs.length >= $ctrl.max}}"
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
