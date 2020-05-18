/* eslint-disable @typescript-eslint/no-explicit-any,@typescript-eslint/tslint/config */
/**
 * Defines the client-side implementation of the JSAV animations plugin.
 * Originally programmed by Mikko Merikivi with help from the GeoGebra plugin by Vesa Lappalainen
 */
import angular from "angular";
import * as t from "io-ts";
import {ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$http, $sce, $timeout} from "tim/util/ngimport";
import {defaultTimeout, to} from "tim/util/utils";

const jsavApp = angular.module("jsavApp", ["ngSanitize"]);
export const moduleDefs = [jsavApp];

/**
 * This is the "state" of the plugin, used through this.attrs.
 * For example, the user can specify attributes here when writing markup for the plugin
 */
const JsavMarkup = t.intersection([
    t.partial({
        beforeOpen: t.string,
        open: t.boolean,
        width: t.number,
        height: t.number,
        message: t.string,
        buttonBottom: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        lang: withDefault(t.string, "fi"),
        // all withDefaults should come here; NOT in t.partial
    }),
]);

/**
 * This is all the JSON content saved
 */
const JsavAll = t.intersection([
    t.partial({
        usercode: t.string,
    }),
    t.type({
        info: Info,
        markup: JsavMarkup,
        preview: t.boolean,
    }),
]);

/**
 * Methods and properties in JSAV library's Exercise class that we want to be able to use remotely
 */
interface JSAVExercise extends Object {
    showModelanswer(): void;
}

/**
 * Methods and properties inside the sandboxed iframe that we want to be able to use remotely
 */
interface JSFrameWindow extends Window {
    getData(): string;
    exercise: JSAVExercise;
}

/**
 * An interface for sandboxed iframes
 */
interface CustomFrame<T extends Window> extends HTMLIFrameElement {
    contentWindow: T;
}

/**
 * The high-level operational logic behind this plugin
 */
class JsavController extends PluginBase<t.TypeOf<typeof JsavMarkup>, t.TypeOf<typeof JsavAll>, typeof JsavAll> {
    get english() {
        return this.attrs.lang === "en";
    }

    /**
     * What the save button's text should be
     */
    buttonText() {
        const txt = super.buttonText();
        if (txt) {
            return txt;
        }
        return this.english ? "Save" : "Tallenna";
    }

    /**
     * What the model answer button's text should be
     */
    modelAnswerButtonText() {
        return this.english ? "Model Answer" : "Mallivastaus";
    }

    public viewCtrl!: ViewCtrl;
    private error: string = "";
    private isRunning: boolean = false;
    private jsavOutput: string = "";
    private isOpen: boolean = true;
    private htmlUrl: string = "";
    private button: string = "";
    private modelAnswerButton: string = "";
    private console: string = "";
    private message: string = "";
    private userCode: string = "";

    private taskUrl: string = "";

    $onInit() {
        super.$onInit();
        this.button = this.buttonText();
        this.modelAnswerButton = this.modelAnswerButtonText();
        if (this.attrs.open) {
            this.isOpen = true;
        }
        const aa = this.attrsall;
        this.userCode = aa.usercode ??  "";
        this.message = this.attrs.message ?? "";
    }

    getTaskUrl(): string {
        if (this.taskUrl) {
            return this.taskUrl;
        }
        const url = this.pluginMeta.getAnswerUrl();
        this.taskUrl = url;
        return url;
    }

    /**
     * Save the state in TIM's database
     * @param data The user's current score in a JSON exercise
     * @param answerChecked Whether the user has looked at the model answer
     */
    async runSend(data: any, answerChecked: boolean) {
        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            return;
        }
        this.error = "";
        this.isRunning = true;
        const url = this.getTaskUrl();
        data.type = "jsav";
        if (answerChecked) {
            data.model = "y";
        }
        if (!data.usercode) {
            data.usercode = "y";
            this.userCode = "y";
        }

        const params = {
            input: data,
        };

        this.console = "";

        const r = await to($http<{
            web: {error?: string, console?: string},
        }>({method: "PUT", url: url, data: params, timeout: defaultTimeout},
        ));
        this.isRunning = false;

        if (!r.ok) {
            this.error = r.result.data.error;
            return;
        }
        if (!r.result.data.web) {
            this.error = "No web reply from csPlugin!";
            return;
        }
        if (r.result.data.web.error) {
            this.error = r.result.data.web.error;
            return;
        }
        if (r.result.data.web.console) {
            this.console = r.result.data.web.console;
            return;
        }
    }

    /**
     * This asks the getData function to get the state and then saves the state in TIM's database
     * @param answerChecked Whether the user has looked at the model answer
     */
    getData(answerChecked: boolean) {
        const frameElem = this.element.find(".jsFrameContainer")[0];
        const f = frameElem.firstChild as CustomFrame<JSFrameWindow>;
        if (!f.contentWindow.getData) {
            return;
        }
        const s: any = f.contentWindow.getData();
        if (s.message) {
            this.message = s.message;
        }
        this.runSend(s, answerChecked);
    }

    /**
     * This shows the model answer
     */
    modelAnswer() {
        this.console = "";
        const frameElem = this.element.find(".jsFrameContainer")[0];
        const f = frameElem.firstChild as CustomFrame<JSFrameWindow>;

        if (f.contentWindow.exercise) {
            this.getData(true);
            f.contentWindow.exercise.showModelanswer();
        } else {
            this.console = this.english ?
                "No model answer available." :
                "Mallivastausta ei ole saatavilla.";
        }
    }

    getDefaultMarkup() {
        return {};
    }

    /**
     * This outputs the code for the JSAV plugin itself in a sandbox
     */
    outputAsHtml(): string {
        $timeout(0);
        const tid = this.pluginMeta.getTaskId()!;
        const taskId = tid.docTask();
        const ab = this.viewCtrl.getAnswerBrowser(taskId);
        let anr = 0;
        if (ab) {
            anr = ab.findSelectedAnswerIndex();
        }
        const selectedUser = this.viewCtrl.selectedUser;
        const userId = selectedUser.id;
        const w = this.attrs.width ?? 800;
        const h = this.attrs.height ?? 600;
        this.jsavOutput = "<iframe id=\"jsav-iframe1\"\n" +
            "        style=\"width:calc(" + w + "px + 2px);height:calc(" + h + "px + 2px);border: none;\"\n" +
            "        sandbox=\"allow-scripts allow-same-origin\"\n" +
            "        class=\"jsavFrame\"\n" +
            'src="' + this.getHtmlUrl() + "/" + userId + "/" + anr + '"' +
            "</iframe>";

        return $sce.trustAsHtml(this.jsavOutput);
    }

    getHtmlUrl(): string {
        if (this.htmlUrl) {
            return this.htmlUrl;
        }
        const url = "/iframehtml" + this.pluginMeta.getAnswerUrl().replace("/answer", "");

        this.htmlUrl = url;
        return url;
    }

    getAttributeType() {
        return JsavAll;
    }
}

const common = {
    bindings: pluginBindings,
    controller: JsavController,
};

/**
 * This is the HTML code that is placed when the JSAV plugin is used in a TIM document.
 */
jsavApp.component("csJsavRunner", {
    ...common,
    require: {
        viewCtrl: "^timView",
    },
    template: `
<tim-markup-error ng-if="::$ctrl.markupError" [data]="::$ctrl.markupError"></tim-markup-error>
<div class="csRunDiv no-popup-menu">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" class="stem" ng-bind-html="::$ctrl.stem"></p>
    <p ng-if="!$ctrl.isOpen" class="stem" ng-bind-html="::$ctrl.attrs.beforeOpen"></p>

    <!-- The JSAV plugin itself in a sandbox -->
    <div ng-cloak id="output" class="jsFrameContainer jsavOutput" ng-bind-html="::$ctrl.outputAsHtml()">
    </div>

    <p class="csRunMenu">
        <!-- The save button -->
        <button ng-if="$ctrl.isOpen"
                ng-disabled="$ctrl.isRunning"
                title="(Ctrl-S)"
                class="timButton btn-sm"
                ng-click="$ctrl.getData(false)"
                ng-bind-html="::$ctrl.button"></button>
        <!-- The model answer button -->
        <button ng-if="$ctrl.isOpen && $ctrl.userCode"
                ng-disabled="$ctrl.isRunning"
                title="(Ctrl-M)"
                class="timButton btn-sm"
                ng-click="$ctrl.modelAnswer()"
                ng-bind-html="::$ctrl.modelAnswerButton"></button>
        <!-- User-specified messages -->
        <span class="jsav message"
              ng-if="$ctrl.message"
              ng-bind-html="$ctrl.message"></span>
        <!-- Plugin's messages -->
        <span class="jsav message"
              ng-if="$ctrl.console"
              ng-bind-html="$ctrl.console"></span>
    </p>
</div>
`,

});
