/**
 * Defines the client-side implementation of an example plugin (a tableFormndrome checker).
 */
import angular, {INgModelOptions, IRootElementService, IScope} from "angular";
import * as t from "io-ts";
import {
    GenericPluginMarkup,
    GenericPluginTopLevelFields,
    nullable,
    PluginBase,
    pluginBindings, PluginMeta,
    withDefault,
} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";
import {timApp} from "../app";
import {None} from "../../jspm_packages/npm/fp-ts@1.11.1/lib/Option";

const tableFormApp = angular.module("tableFormApp", ["ngSanitize"]);
export const moduleDefs = [tableFormApp];

const TableFormMarkup = t.intersection([
    t.partial({
        initword: t.string
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const TableFormAll = t.intersection([
    t.partial({
        userword: t.string,
        data: t.Dictionary,
    }),
    GenericPluginTopLevelFields,
    t.type({markup: TableFormMarkup}),
]);


class TableFormController extends PluginBase<t.TypeOf<typeof TableFormMarkup>, t.TypeOf<typeof TableFormAll>, typeof TableFormAll> {
    private result?: string;
    private error?: string;
    private isRunning = false;
    private userword = "";
    private runTestGreen = false;
    private data: any = {};
    private userdata!: {};
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "TODO";
    }



    $onInit() {
        super.$onInit();
        this.userword = this.attrsall.userword || this.attrs.initword || "";
        this.data = this.attrsall.data;
    }

    /**
     * Temporary default TimTable data for demonstartion purposes
     */
    setData(){
        return {
  "task": true,
  "table": {
    "rows": [
      {
        "backgroundColor": "gray"
      },
      {
        "row": [
          {
            "color": "white",
            "backgroundColor": "black",
            "cell": "Erilaisia sisältöjä",
            "colspan": 5
          }
        ]
      },
      {
        "row": [
          {
            "height": "250px",
            "width": "300px",
            "cell": "<figure>\n<img src=\"/images/157964/C360_2010-12-22_12-49-34-SNOW.gif\" alt=\"Kaunis talvimaisema\" /><figcaption>Kaunis talvimaisema</figcaption>\n</figure>"
          },
          {
            "cell": "kaava <span class=\"math display\">\\[\\int f(x^6) dx\\]</span>",
            "colspan": 2,
            "horizontal-align": "right"
          }
        ]
      },
      {
        "row": [
          "<figure>\n<img src=\"/images/157965/IMAG1226.jpg\" alt=\"Sumuinen aamu\" /><figcaption>Sumuinen aamu</figcaption>\n</figure>",
          {
            "textAlign": "middle",
            "cell": "<span class=\"timButton\">Painike<span>"
          }
        ]
      },
      {
        "row": [
          {
            "cell": "<table>\n<thead>\n<tr class=\"header\">\n<th>Kissat vai koirat</th>\n<th style=\"text-align: left;\">Kissat</th>\n<th style=\"text-align: right;\">Koirat</th>\n</tr>\n</thead>\n<tbody>\n<tr class=\"odd\">\n<td>1. äänestys</td>\n<td style=\"text-align: left;\">14</td>\n<td style=\"text-align: right;\">5</td>\n</tr>\n<tr class=\"even\">\n<td>2. äänestys</td>\n<td style=\"text-align: left;\">6</td>\n<td style=\"text-align: right;\">6</td>\n</tr>\n</tbody>\n</table>",
            "colspan": null
          }
        ]
      }
    ]
  }
}
    }

    initCode() {
        this.userword = this.attrs.initword || "";
        this.error = undefined;
        this.result = undefined;
    }

    saveText() {
        this.doSaveText(false);
    }

    async doSaveText(nosave: boolean) {
        this.error = "... saving ...";
        this.isRunning = true;
        this.result = undefined;
        const params = {
            input: {
                nosave: false,
                userword: this.userword,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to($http.put<{web: {result: string, error?: string}}>(url, params));
        this.isRunning = false;
        if (r.ok) {
            const data = r.result.data;
            this.error = data.web.error;
            this.result = data.web.result;
        } else {
            this.error = "Infinite loop or some other error?";
        }
    }

    protected getAttributeType() {
        return TableFormAll;
    }
}

timApp.component("tableformRunner", {
    bindings: pluginBindings,

    controller: TableFormController,
    template: `
<div class="csRunDiv no-popup-menu">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" ng-bind-html="::$ctrl.stem"></p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        <input type="text"
               class="form-control"
               ng-model="$ctrl.userword"
               ng-model-options="::$ctrl.modelOpts"
               ng-trim="false"
               ng-readonly="::$ctrl.readonly"
               size="{{::$ctrl.cols}}"></span></label>
        <tim-table data="$ctrl.data" userdata="$ctrl.userdata" task-url="{{$ctrl.pluginMeta.getAnswerUrl()}}"></tim-table>
        <!-- TODO: taskid="{{ $ctrl.pluginm }}", vie pluginmeta & taskid-->
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-disabled="$ctrl.isRunning || !$ctrl.userword || $ctrl.readonly"
            ng-click="$ctrl.saveText()">
        {{::$ctrl.buttonText()}}
    </button>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
