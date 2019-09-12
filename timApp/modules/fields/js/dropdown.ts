/**
 * Defines the client-side implementation of a dropdown plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, nullable, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings, shuffleStrings} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";

const dropdownApp = angular.module("dropdownApp", ["ngSanitize"]);
export const moduleDefs = [dropdownApp];

const DropdownMarkup = t.intersection([
    t.partial({
        words: t.array(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        // All withDefaults should come here, NOT in t.partial.
        answers: withDefault(t.boolean, false),
        autosave: withDefault(t.boolean, false),
        instruction: withDefault(t.boolean, false),
        radio: withDefault(t.boolean, false),
        shuffle: withDefault(t.boolean, false),
    }),
]);
const DropdownAll = t.intersection([
    t.partial({
    }),
    t.type({
        info: Info,
        markup: DropdownMarkup,
        preview: t.boolean,
        state: nullable(t.type({c: t.string})),
    }),
]);

class DropdownController extends PluginBase<t.TypeOf<typeof DropdownMarkup>, t.TypeOf<typeof DropdownAll>, typeof DropdownAll> implements ITimComponent {
    private error?: string;
    // noinspection JSMismatchedCollectionQueryUpdate
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
        this.selectedWord = (this.attrsall.state && this.attrsall.state.c) || undefined;
        this.shuffle = this.attrs.shuffle;
        if (this.shuffle && this.attrs.words) {
            this.wordList = shuffleStrings(this.attrs.words);
        } else {
            this.wordList = this.attrs.words || [];
        }
        if (!this.attrsall.preview) {
            this.addToCtrl();
        }
        this.radio = this.attrs.radio;
        if (this.attrs.answers) {
            // TODO: Implement showing and hiding the answer browser if it is deemed useful.
        }
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
     * Saves the selection that in the plugin.
     */
    async save() {
        return await this.doSave(this.attrs.instruction);
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
        } else {
            this.error = r.result.data.error;
        }
        return {saved: r.ok, message: this.error};
    }

    selfSave() {
        if (this.attrs.autosave || this.attrs.autosave === undefined) {
            this.save();
        }
    }

    /**
     * Force the plugin to save its information.
     *
     * @param force Whether to force the plugin to always save itself when the answer route is called.
     */
    setForceAnswerSave(force: boolean) {
        this.forceSave = force;
    }

    /**
     * Sets the words visible in the plugin and randomizes their order if desired.
     *
     * @param words List of words to be shown in the plugin.
     */
    setPluginWords(words: string[]) {
        if (this.shuffle) {
            this.wordList = shuffleStrings(words);
        } else {
            this.wordList = words;
        }

        this.selectedWord = undefined;
    }

    protected getAttributeType() {
        return DropdownAll;
    }

    isUnSaved() {
        return false; // TODO
    }

    supportsSetAnswer(): boolean {
        return true;
    }

    setAnswer(content: { [index: string]: any }): { ok: boolean, message: (string | undefined) } {
        let message;
        let ok = true;
        if (Object.keys(content).length == 0) {
            this.resetField();
        } else {
            try {
                this.selectedWord = content.c;
            } catch (TypeError) {
                this.selectedWord = "";
                ok = false;
                message = "Couldn't find related content (\"c\")";
            }
        }
        // this.initialValue = this.selectedWord;
        return {ok: ok, message: message};

    }

    resetField(): undefined {
        this.selectedWord = "";
        return undefined;
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
    <p class="stem" ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label><span>
        <li class="dropradio" ng-if="::$ctrl.radio" ng-repeat="item in $ctrl.wordList">
        <label><input type="radio"
                      name="selection"
                      value="{{item}}"
                      ng-model="$ctrl.selectedWord"
                      ng-change="::$ctrl.selfSave()">
        {{item}}
        </label>
        </li>
        <select ng-if="::!$ctrl.radio"
                ng-model="$ctrl.selectedWord"
                ng-options="item for item in $ctrl.wordList"
                ng-change="::$ctrl.selfSave()">
        </select>
        </span></label>
    </div>
    <div class="error" ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
