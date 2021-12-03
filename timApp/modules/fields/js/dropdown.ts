/**
 * Defines the client-side implementation of a dropdown plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {
    ChangeType,
    FormModeOption,
    ISetAnswerResult,
    ITimComponent,
    ViewCtrl,
} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {
    getFormBehavior,
    PluginBase,
    pluginBindings,
    shuffleStrings,
} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {defaultErrorMessage, defaultTimeout, to} from "tim/util/utils";

const dropdownApp = angular.module("dropdownApp", ["ngSanitize"]);
export const moduleDefs = [dropdownApp];

const DropdownMarkup = t.intersection([
    t.partial({
        tag: t.string,
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
    t.partial({}),
    t.type({
        info: Info,
        markup: DropdownMarkup,
        preview: t.boolean,
        state: nullable(t.type({c: t.string})),
    }),
]);

class DropdownController
    extends PluginBase<
        t.TypeOf<typeof DropdownMarkup>,
        t.TypeOf<typeof DropdownAll>,
        typeof DropdownAll
    >
    implements ITimComponent
{
    private error?: string;
    // noinspection JSMismatchedCollectionQueryUpdate
    private wordList?: string[];
    private selectedWord?: string;
    private initialWord?: string;
    private vctrl!: ViewCtrl;
    private forceSave = false;
    private radio?: boolean;
    private shuffle?: boolean;
    private changes = false;
    private connectionErrorMessage?: string;

    getDefaultMarkup() {
        return {};
    }

    $onInit() {
        super.$onInit();
        this.selectedWord = this.attrsall.state?.c ?? undefined;
        this.shuffle = this.attrs.shuffle;
        if (this.shuffle && this.attrs.words) {
            this.wordList = shuffleStrings(this.attrs.words);
        } else {
            this.wordList = this.attrs.words ?? [];
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
        this.vctrl.addTimComponent(this, this.attrs.tag);
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
        this.connectionErrorMessage = undefined;
        // TODO: Check whether to skip undefined inputs or save empty strings
        if (this.selectedWord == undefined) {
            this.selectedWord = "";
        }
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
        const r = await to(
            $http.put<{web: {result: string; error?: string}}>(url, params, {
                timeout: defaultTimeout,
            })
        );

        if (r.ok) {
            this.changes = false;
            this.updateListeners(ChangeType.Saved);
            const data = r.result.data;
            this.error = data.web.error;
        } else {
            this.error = r.result.data?.error;
            this.connectionErrorMessage =
                this.error ??
                this.attrs.connectionErrorMessage ??
                defaultErrorMessage;
        }
        this.initialWord = this.selectedWord;
        return {saved: r.ok, message: this.error};
    }

    updateSelection() {
        if (!this.changes) {
            this.changes = true;
            this.updateListeners(ChangeType.Modified);
        }
        if (this.attrs.autosave || this.attrs.autosave === undefined) {
            this.save();
        }
    }

    updateListeners(state: ChangeType) {
        if (!this.vctrl) {
            return;
        }
        const taskId = this.pluginMeta.getTaskId();
        if (!taskId) {
            return;
        }
        this.vctrl.informChangeListeners(taskId, state, this.attrs.tag);
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
        this.initialWord = this.selectedWord;
    }

    getAttributeType() {
        return DropdownAll;
    }

    isUnSaved() {
        return this.changes;
    }

    formBehavior(): FormModeOption {
        return getFormBehavior(this.attrs.form, FormModeOption.IsForm);
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    setAnswer(content: Record<string, any>): ISetAnswerResult {
        this.error = undefined;
        let message;
        let ok = true;
        if (Object.keys(content).length == 0) {
            this.resetField();
        } else {
            try {
                this.selectedWord = content.c;
            } catch (e) {
                this.selectedWord = "";
                ok = false;
                message = `Couldn't find related content ("c") from ${JSON.stringify(
                    content
                )}`;
                this.error = message;
            }
        }
        this.changes = false;
        this.updateListeners(ChangeType.Saved);
        this.initialWord = this.selectedWord;
        return {ok: ok, message: message};
    }

    resetField(): undefined {
        this.selectedWord = "";
        this.initialWord = this.selectedWord;
        this.changes = false;
        this.error = undefined;
        this.updateListeners(ChangeType.Saved);
        return undefined;
    }

    resetChanges(): void {
        this.selectedWord = this.initialWord;
        this.changes = false;
        this.updateListeners(ChangeType.Saved);
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
    <tim-markup-error ng-if="::$ctrl.markupError" [data]="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p class="stem" ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label><span>
        <li class="dropradio" ng-if="::$ctrl.radio" ng-repeat="item in $ctrl.wordList">
        <label><input type="radio"
                      name="selection"
                      value="{{item}}"
                      ng-model="$ctrl.selectedWord"
                      ng-change="::$ctrl.updateSelection()">
        {{item}}
        </label>
        </li>
        <select ng-if="::!$ctrl.radio"
                ng-model="$ctrl.selectedWord"
                ng-options="item for item in $ctrl.wordList"
                ng-change="::$ctrl.updateSelection()"
                ng-class="{warnFrame: $ctrl.isUnSaved()}">
        </select>
        </span></label>
    </div>
    <div ng-if="$ctrl.connectionErrorMessage" class="error" style="font-size: 12px" ng-bind-html="$ctrl.connectionErrorMessage"></div>
    <div class="error" ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
