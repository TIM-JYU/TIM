/**
 * Defines the client-side implementation of an example plugin (a multisavendrome checker).
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, PluginBase, pluginBindings, withDefault} from "tim/plugin/util";
import {escapeRegExp, valueDefu} from "tim/util/utils";

const multisaveApp = angular.module("multisaveApp", ["ngSanitize"]);
export const moduleDefs = [multisaveApp];

const multisaveMarkup = t.intersection([
    t.partial({
        followid: t.string,
        fields: t.array(t.string),
        areas: t.array(t.string)
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
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

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "Save";
    }

    $onInit() {
        super.$onInit();
        this.modelOpts = {debounce: this.autoupdate};
    }


    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    get resetText() {
        return valueDefu(this.attrs.resetText, "Reset");
    }

    protected getAttributeType() {
        return multisaveAll;
    }


    save(): string {
        let componentsToSave: ITimComponent[] = [];
        //TODO: componentsToSave as a map?
        if(this.attrs.fields){
            for (const i of this.attrs.fields){
                let timComponents = this.vctrl.getTimComponentsByRegex("^" + i + "$");
                for (const v of timComponents)
                {
                    if (!componentsToSave.includes(v)) componentsToSave.push(v)
                }
            }
        }

        if(this.attrs.areas)
        {
            for(const i of this.attrs.areas){
                let timComponents = this.vctrl.getTimComponentsByGroup(i);
                for(const v of timComponents)
                {
                    if (!componentsToSave.includes(v)) componentsToSave.push(v)
                }
            }
        }

        let ownArea: string | undefined;
        const parents = this.element.parents('.area');
        //parents returns only one element because nested areas are in separate divs
        if(parents[0]){
            ownArea = parents[0].classList[parents[0].classList.length - 1].replace("area_","");
        }

        // no given followids or areas but the plugin is inside an area
        if(!this.attrs.fields && !this.attrs.areas && ownArea){
            componentsToSave = this.vctrl.getTimComponentsByGroup(ownArea);
        }

        // no given followids / areas and no own area found
        if(!this.attrs.fields && !this.attrs.areas && !ownArea){
            componentsToSave = this.vctrl.getTimComponentsByRegex(".*");
        }

        for(const v of componentsToSave)
        {
            v.save();
        }
        this.isSaved = true;

        return this.attrs.followid || this.pluginMeta.getTaskId() || "";
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
    <pre class="savedtext" ng-if="$ctrl.isSaved"> Saved! </pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
