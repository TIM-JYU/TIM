/**
 * Defines the client-side implementation of a dropdown plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {to} from "../../../static/scripts/tim/util/utils";
import {$http} from "../../../static/scripts/tim/util/ngimport";

const dropdownApp = angular.module("dropdownApp", ["ngSanitize"]);
export const moduleDefs = [dropdownApp];

const DropdownMarkup = t.intersection([
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
const DropdownAll = t.intersection([
    t.partial({
        userword: t.string,
    }),
    t.type({markup: DropdownMarkup}),
]);

class DropdownController extends PluginBase<t.TypeOf<typeof DropdownMarkup>, t.TypeOf<typeof DropdownAll>, typeof DropdownAll> implements ITimComponent {
    private error?: string;
    private wordlist?: string[];
    private selectedWord?: string;
    private vctrl!: ViewCtrl;

    getDefaultMarkup() {
        return {};
    }

    $onInit() {
        super.$onInit();
        this.wordlist = this.attrs.words || [];
        this.addToCtrl();
    }

    /**
     * Adds this plugin to ViewCtrl so other plugins can get information about the plugin though it.
     */
    addToCtrl() {
        this.vctrl.addTimComponent(this);
    }

    initCode() {
        this.error = undefined;
        this.selectedWord = undefined;
    }

    /**
     * Returns the selected choice from the dropdown-list.
     * @returns {string} The selected choice..
     */
    getContent(): string {
        return this.selectedWord || "Nothing selected";
    }

    /**
     * TODO: whole sentence, selected option, plugin type?,
     */
    save(): string {
      this.doSave(false);
      return "";
    }

    async doSave(nosave: false) {
        const params = {
            input: {
                nosave: false,
                selectedWord: this.selectedWord,
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

    /**
     *
     */
    getGroups(): string[] {
        return [""];
    }

    /**
     *
     */
    getName(): (string | undefined) {
        if (this.attrs.followid) { return this.attrs.followid; }
        const taskId = this.pluginMeta.getTaskId();
        if (taskId) { return taskId.split(".")[1]; }
    }

    /**
     *
     * @param group
     */
    belongsToGroup(group: string): boolean {
        return false;
    }

    /**
     *
     */
    protected getAttributeType() {
        return DropdownAll;
    }
}

dropdownApp.component("dropdownRunner", {
    bindings: {
        json: "@",
    },
    controller: DropdownController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        <select ng-model="$ctrl.selectedWord" ng-options="item for item in $ctrl.wordlist" ng-change="$ctrl.save()">
        </select>
        </span></label>
    </div>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
