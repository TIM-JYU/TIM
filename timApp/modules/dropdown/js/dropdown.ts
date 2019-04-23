/**
 * Defines the client-side implementation of a dropdown plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, PluginBase, pluginBindings, withDefault} from "tim/plugin/util";
import {to} from "tim/util/utils";
import {$http} from "tim/util/ngimport";

const dropdownApp = angular.module("dropdownApp", ["ngSanitize"]);
export const moduleDefs = [dropdownApp];

const DropdownMarkup = t.intersection([
    t.partial({
        words: t.array(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial,
        instruction: withDefault(t.boolean, false),
        radio: withDefault(t.boolean, false),
        shuffle: withDefault(t.boolean, false),
    }),
]);
const DropdownAll = t.intersection([
    t.partial({
        userword: t.string,
    }),
    t.type({
        info: Info,
        markup: DropdownMarkup,
        preview: t.boolean,
    }),
]);

class DropdownController extends PluginBase<t.TypeOf<typeof DropdownMarkup>, t.TypeOf<typeof DropdownAll>, typeof DropdownAll> implements ITimComponent {
    private error?: string;
    private wordList?: string[];
    private selectedWord?: string;
    private vctrl!: ViewCtrl;
    private forceSave = false;
    private radio?: boolean;
    private shuffle?: boolean;


    getDefaultMarkup() {
        return {};
    }

    $onInit() {
        super.$onInit();
        this.shuffle = this.attrs.shuffle;
        if (this.shuffle && this.attrs.words) {
            this.wordList = this.shuffleWords(this.attrs.words);
        } else {
            this.wordList = this.attrs.words || [];
        }
        this.addToCtrl();
        this.radio = this.attrs.radio;
    }

    /**
     * Adds this plugin to ViewCtrl so other plugins can get information about the plugin though it.
     */
    addToCtrl() {
        this.vctrl.addTimComponent(this);
    }

    /**
     * Returns the selected choice from the dropdown-list.
     * @returns {string} The selected choice.
     */
    getContent() {
        return this.selectedWord;
    }

    /**
     * TODO: whole sentence, selected option, plugin type?,
     */
    async save() {
        const failure = await this.doSave(this.attrs.instruction);
        return failure;
    }

    async doSave(nosave: boolean) {
        const params = {
            input: {
                nosave: false,
                selectedWord: this.selectedWord,
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
            // this.result = data.web.result;
        } else {
            this.error = "Infinite loop or some other error?";
        }
    }

    /**
     * Force the plugin to save its information
     *
     * @param force Whether to force a save
     */
    setForceAnswerSave(force: boolean) {
        this.forceSave = force;
    }

    /**
     * Sets the words visible in the plugin and randomizes their order
     *
     * @param words List of words to be shown in the plugin
     */
    setPluginWords(words: string[]) {
        if (this.shuffle) {
            this.wordList = this.shuffleWords(words);
        } else {
            this.wordList = words;
        }

        this.selectedWord = undefined;
    }

    /**
     * Shuffles string array
     * @param words Array of strings to be shuffled.
     */
    shuffleWords(words: string []): string [] {
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

    protected getAttributeType() {
        return DropdownAll;
    }
}

dropdownApp.component("dropdownRunner", {
    bindings: pluginBindings,
    controller: DropdownController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div>
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label><span>
        <li class="dropradio" ng-if="::$ctrl.radio" ng-repeat="item in $ctrl.wordList">
        <label><input type="radio" name="selection" value="{{item}}" ng-model="$ctrl.selectedWord">
        {{item}}
        </label>
        </li>
        <select ng-if="::!$ctrl.radio" ng-model="$ctrl.selectedWord" ng-options="item for item in $ctrl.wordList">
        </select>
        </span></label>
    </div>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
