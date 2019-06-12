/**
 * Defines the client-side implementation of a plugin that calls other plugins' save methods.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";

const multisaveApp = angular.module("multisaveApp", ["ngSanitize"]);
export const moduleDefs = [multisaveApp];

const multisaveMarkup = t.intersection([
    t.partial({
        areas: t.array(t.string),
        fields: t.array(t.string),
        followid: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
    }),
]);
const multisaveAll = t.intersection([
    t.partial({
    }),
    t.type({
        info: Info,
        markup: multisaveMarkup,
        preview: t.boolean,
    }),
]);

export class MultisaveController extends PluginBase<t.TypeOf<typeof multisaveMarkup>, t.TypeOf<typeof multisaveAll>, typeof multisaveAll> {
    private isSaved = false;
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"
    private vctrl!: ViewCtrl;
    private savedFields: number = 0;

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "Save";
    }

    $onInit() {
        super.$onInit();
    }

    /**
     * Calls the save method of all ITimComponent plugins that match the given attributes
     * - Save all plugins defined in "fields" attribute that match the given regexp
     * - Save all plugins that are in the areas defined by "areas" attribute
     * - If fields/areas are not given then save only plugins in the same area with the multisave plugin
     * - If fields/areas are not given and multisave is not within any areas then just call save for every ITimComponent
     *   plugin in the same document
     */
    async save() {
        let componentsToSave: ITimComponent[] = [];
        // TODO: componentsToSave as a map?
        if (this.attrs.fields) {
            for (const i of this.attrs.fields) {
                const timComponents = this.vctrl.getTimComponentsByRegex("^" + i + "$");
                for (const v of timComponents) {
                    if (!componentsToSave.includes(v)) { componentsToSave.push(v); }
                }
            }
        }

        if (this.attrs.areas) {
            for (const i of this.attrs.areas) {
                const timComponents = this.vctrl.getTimComponentsByGroup(i);
                for (const v of timComponents) {
                    if (!componentsToSave.includes(v)) { componentsToSave.push(v); }
                }
            }
        }

        let ownArea: string | undefined;
        const parents = this.element.parents(".area");
        // parents returns only one element because nested areas are in separate divs
        if (parents[0]) {
            ownArea = parents[0].classList[parents[0].classList.length - 1].replace("area_", "");
        }

        // no given followids or areas but the plugin is inside an area
        if (!this.attrs.fields && !this.attrs.areas && ownArea) {
            componentsToSave = this.vctrl.getTimComponentsByGroup(ownArea);
        }

        // no given followids / areas and no own area found
        if (!this.attrs.fields && !this.attrs.areas && !ownArea) {
            componentsToSave = this.vctrl.getTimComponentsByRegex(".*");
        }

        const promises = [];
        for (const v of componentsToSave) {
            const result = v.save();
            promises.push(result);
        }

        this.isSaved = false;
        this.savedFields = 0;
        for (const p of promises) {
            const result = await p;
            if (result.saved) {
                this.savedFields++;
            }
        }
        if (this.savedFields !== 0 ) {
            this.isSaved = true;
        }

        return this.attrs.followid || this.pluginMeta.getTaskId() || "";
    }

    protected getAttributeType() {
        return multisaveAll;
    }
}

multisaveApp.component("multisaveRunner", {
    bindings: pluginBindings,
    controller: MultisaveController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="no-popup-menu">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-click="$ctrl.save()">
        {{::$ctrl.buttonText()}}
    </button>
    <p class="savedtext" ng-if="$ctrl.isSaved">Saved {{$ctrl.savedFields}} fields!</p>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
