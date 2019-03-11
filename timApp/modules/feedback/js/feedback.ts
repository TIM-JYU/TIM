/**
 * Defines the client-side implementation of a feedback-plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {valueDefu} from "tim/util/utils";
import {find} from "../../../static/scripts/jspm_packages/npm/fp-ts@1.11.1/lib/Foldable";

const feedbackApp = angular.module("feedbackApp", ["ngSanitize"]);
export const moduleDefs = [feedbackApp];

const FeedbackMarkup = t.intersection([
    t.partial({
        inputstem: t.string,
        followid: t.string,
        field: t.string,
        feedbackLevel: t.Integer,
        userword: t.string
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const FeedbackAll = t.intersection([
    t.partial({
        userword: t.string,
    }),
    t.type({markup: FeedbackMarkup}),
]);

class FeedbackController extends PluginBase<t.TypeOf<typeof FeedbackMarkup>, t.TypeOf<typeof FeedbackAll>, typeof FeedbackAll> implements ITimComponent {
    private result?: string;
    private error?: string;
    private vctrl!: ViewCtrl;
    private userword = "";

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return "OK";
    }

    $onInit() {
        super.$onInit();
        this.addToCtrl();
    }

    /**
     * Adds this plugin to ViewCtrl so other plugins can get information about the plugin though it.
     */
    addToCtrl() {
        const taskid = this.pluginMeta.getTaskId() || ""; // TODO: fix this dirty stuff
        const name = taskid.split(".");
        this.vctrl.addTimComponent(this, this.attrs.followid || name[1] || "");
    }

    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    async doSave(nosave: boolean) {
        this.error = "... saving ...";

        this.result = undefined;
        const params = {
            input: {
                nosave: false,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to($http.put<{web: {result: string, error?: string}}>(url, params));
        if (r.ok) {
            const data = r.result.data;
            this.error = data.web.error;
            this.result = data.web.result;
        } else {
            this.error = "Infinite loop or some other error?";
        }
    }

    /**
     * Returns the content inside this plugin
     * @returns {string} The content inside this plugin
     */
    getContent(): string {
      return "";
    }

    /**
     * Does nothing at the moment
     */
    save(): string {
        this.doSave(false);
        return "";
    }

    /**
     * Gets the user's answer from the plugin this feedback-plugin is assigned to.
     * Doesn't work for paragraphs with multiple plugins at the moment.
     */
    getAnswer() {
        const timComponent = this.vctrl.getTimComponent(this.attrs.field || "");
        if(timComponent) {
            const par = timComponent.getPar();
            const content = par.children(".parContent");
            const nodes = content[0].childNodes;
            // console.log(nodes);
            let answer = "";
            for (let n in nodes) {
                let node = nodes[n];
                if(node.nodeName == "#text") {
                    let text = node.textContent || "";
                    text.trim();
                    answer = answer + text;
                }
                if(node.nodeName == "TIM-PLUGIN-LOADER") {
                    // ei toimi usean lohkon sisäisen inline pluginin kanssa
                    answer = answer + timComponent.getContent();
                }
            }
            this.userword = answer;
        }
    }

    protected getAttributeType() {
        return FeedbackAll;
    }
}

feedbackApp.component("feedbackRunner", {
    bindings: {
        json: "@",
    },
    controller: FeedbackController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="csRunDiv no-popup-menu">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        {{$ctrl.userword}}</span></label>
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-click="$ctrl.getAnswer()">
        {{::$ctrl.buttonText()}}
    </button>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});

/*

<div class="csRunDiv no-popup-menu">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        {{$ctrl.userword}}</span></label>
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-click="$ctrl.getAnswer()">
        {{::$ctrl.buttonText()}}
    </button>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
 */