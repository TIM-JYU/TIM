import angular from "angular";
import * as t from "io-ts";
import {IAnswer} from "tim/answer/IAnswer";
import {ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$http, $sce, $timeout} from "tim/util/ngimport";
import {to} from "tim/util/utils";

const jsframeApp = angular.module("jsframeApp", ["ngSanitize"]);
export const moduleDefs = [jsframeApp];

// this.attrs
const JsframeMarkup = t.intersection([
    t.partial({
        beforeOpen: t.string,
        buttonBottom: t.boolean,
        correctresponse: t.boolean,
        generalfeedback: t.boolean,
        showButton: t.string,
        srchtml: t.string,
        message: t.string,
        width: t.number,
        height: t.number,
        tool: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        open: withDefault(t.boolean, true),
        borders: withDefault(t.boolean, true),
        lang: withDefault(t.string, "fi"),
        // autoplay: withDefault(t.boolean, true),
        // file: t.string,
        // open: withDefault(t.boolean, false),
    }),
]);
const JsframeAll = t.intersection([
    t.partial({
        usercode: t.string,
        srchtml: t.string,
        norun: t.boolean,
    }),
    t.type({
        info: Info,
        markup: JsframeMarkup,
        preview: t.boolean,
    }),
]);

type JsframeResult = string | {
    answernotes: any,
    api_time: number,
    error: false,
    formatcorrectresponse: string,
    generalfeedback: string,
    questiontext: string,
    request_time: number,
    score: number,
    summariseresponse: any,
} | {
    error: true,
    message: string,
};

interface JSFrameWindow extends Window {
    getData(): string;
    setData(state: any): void;
}

interface CustomFrame<T extends Window> extends HTMLIFrameElement {
    contentWindow: T;
}

class JsframeController extends PluginBase<t.TypeOf<typeof JsframeMarkup>,
    t.TypeOf<typeof JsframeAll>,
    typeof JsframeAll> {

    get english() {
        return this.attrs.lang === "en";
    }

    buttonText() {
        const txt = super.buttonText();
        if (txt) {
            return txt;
        }
        return this.english ? "Save" : "Tallenna";
    }

    showButton() {
        const txt = this.attrs.showButton;
        if (txt) {
            return txt;
        }
        return this.english ? "Show task" : "Näytä tehtävä";
    }

    public viewctrl?: ViewCtrl;
    private span: string = "";
    private error: string = "";
    private console: string = "";
    private message: string = "";
    private userCode: string = "";
    private jsframeoutput: string = "";
    private jsframeinputfeedback: string = "";
    private jsframepeek: boolean = false;
    private isRunning: boolean = false;
    private isOpen: boolean = false;
    private button: string = "";

    private timer: NodeJS.Timer | undefined;

    private taskUrl: string = "";
    private htmlUrl: string = "";

    $onInit() {
        super.$onInit();
        this.button = this.buttonText();
        const aa = this.attrsall;
        this.userCode = aa.usercode ||  "";

        this.message = this.attrs.message || "";

        if (this.attrs.open) {
            this.isOpen = true;
        }
        // this.addListener();
    }

    runShowTask() {
        this.isOpen = true;
    }

    changeAnswer(a: IAnswer) {
        const frameElem = this.element.find(".jsFrameContainer")[0];
        const f = frameElem.firstChild as CustomFrame<JSFrameWindow>;
        if ( !f.contentWindow.setData ) {
            return;
        }
        f.contentWindow.setData(JSON.parse(a.content));
    }

    outputAsHtml() {
        // if ( !this.attrs.srchtml ) return "";
        if (this.attrsall.preview) {
            // return "";
        } // TODO: replace when preview delay and preview from markup ready
        $timeout(0);
        const tid = this.pluginMeta.getTaskId()!.split(".") || ["", ""];
        const taskId = tid[0] + "." + tid[1];
        let ab = null;

        let userId = 1;
        if (this.viewctrl) {
            ab = this.viewctrl.getAnswerBrowser(taskId);
            const selectedUser = this.viewctrl.selectedUser;
            userId = selectedUser.id;
        }
        if (ab) {
            ab.registerAnswerListener((a) => this.changeAnswer(a));
        }
        let anr = 0;
        if (ab) {
            anr = ab.findSelectedAnswerIndex();
            if (anr < 0) {
                anr = 0;
            }
        }
        const html: string = this.attrs.srchtml || "";
        const datasrc = btoa(html);
        const w = this.attrs.width || 800;
        const h = this.attrs.height || 450;

        // let url = this.getHtmlUrl() + "/" + userId + "/" + anr;
        // url = url.replace("//", "/");
        this.jsframeoutput = "<iframe id='jsxFrame-stack-jsxgraph-1-div1'\n" +
            "        class='showJsframe jsframeFrame' \n" +
            "        style='margin-left: auto;\n" +
            "               margin-right: auto;\n" +
            "               display: block;" +
            "               width:calc(" + w + "px + 2px);height:calc(" + h + "px + 2px);border: none;'\n" +
            "        sandbox='allow-scripts allow-same-origin'\n" +
            "        src='data:text/html;base64," + datasrc + "'>\n" +
            // "src=\"https://www.jsframe.org/material/iframe/id/23587/width/1600/height/715/border/888888/rc/false/ai/false/sdz/false/smb/false/stb/false/stbh/true/ld/false/sri/false\"" +
            // 'src="'+ '/cs/reqs' + '"' +
            // 'src="' + url + '"' +
            "</iframe>";
        return $sce.trustAsHtml(this.jsframeoutput);
    }

    getHtmlUrl(): string {
        if (this.htmlUrl) {
            return this.htmlUrl;
        }
        const url = "/iframehtml" + this.pluginMeta.getAnswerUrl().replace("/answer", "");

        this.htmlUrl = url;
        return url;
    }

    getTaskUrl(): string {
        if (this.taskUrl) {
            return this.taskUrl;
        }
        const url = this.pluginMeta.getAnswerUrl();
        this.taskUrl = url;
        return url;
    }

    async runSend(data: any) {
        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            return;
        }
        this.jsframepeek = false;
        this.error = "";
        this.isRunning = true;
        const url = this.getTaskUrl();
        const jsframeData = "";
        data.type = "jsframe";
        const params = {
            input: data,
        };

        this.console = "";

        const r = await to($http<{
            web: {jsframeResult: JsframeResult, error?: string, console?: string},
        }>({method: "PUT", url: url, data: params, timeout: 20000},
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
        const jsframeResult = r.result.data.web.jsframeResult;
    }

    getData() {
        const frameElem = this.element.find(".jsFrameContainer")[0];
        const f = frameElem.firstChild as CustomFrame<JSFrameWindow>;
        if (!f.contentWindow.getData) {
            return;
        }
        const s: any = f.contentWindow.getData();
        if (s.message) {
            this.message = s.message;
        }
        this.runSend(s);
    }

    tempSetData() {
        const newData = {  labels: [1, 2, 3, 4, 5, 6],
                           data:   [4, 5, 6, 12, 3, 10] };
        this.send({msg: "setData", data: newData });
    }

    tempGetData() {
        const frameElem = this.element.find(".jsFrameContainer")[0];
        const f = frameElem.firstChild as CustomFrame<JSFrameWindow>;
        this.send({msg: "getData" });
    }

    send(obj: any) {
        this.addListener();
        this.channel.port1.postMessage(obj);
    }

    private channel: any = 0;

    addListener() {
        if ( this.channel ) { return; }
        this.channel = new MessageChannel();
        this.channel.port1.onmessage =  (event: any) => {
            console.log(event);
        };
        const frameElem = this.element.find(".jsFrameContainer")[0];
        const f = frameElem.firstChild as CustomFrame<JSFrameWindow>;
        f.contentWindow.postMessage({msg: "init" }, "*", [this.channel.port2]);
    }

    getDefaultMarkup() {
        return {};
    }

    protected getAttributeType() {
        return JsframeAll;
    }

    private stopTimer(): boolean {
        if (!this.timer) {
            return false;
        }
        clearTimeout(this.timer);
        this.timer = undefined;
        return true;
    }
}

const common = {
    bindings: pluginBindings,
    controller: JsframeController,
    require: {
        viewctrl: "?^timView",
    },
};

/*

*/

jsframeApp.component("jsframeRunner", {
    ...common,
    template: `
<div ng-cloak ng-class="::{'csRunDiv': $ctrl.attrs.borders}"  class="math que jsframe no-popup-menu" >
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" class="stem" ng-bind-html="::$ctrl.stem"></p>
    <p ng-if="!$ctrl.isOpen" class="stem" ng-bind-html="::$ctrl.attrs.beforeOpen"></p>

    <div ng-cloak ng-if="$ctrl.isOpen" id="output" class="jsFrameContainer jsframeOutput" ng-bind-html="::$ctrl.outputAsHtml()">
    <!--<div ng-cloak id="output" ng-if="::!$ctrl.timWay" class="jsframeOutput" ng-bind-html="$ctrl.output">-->
    </div>
    <!-- <div class="peekdiv" id="peek" ng-bind-html="$ctrl.jsframepeek"></div> -->
    <p class="csRunMenu">
        <button ng-if="!$ctrl.isOpen"  ng-click="$ctrl.runShowTask()"  ng-bind-html="$ctrl.showButton()"></button>
        <button ng-if="$ctrl.isOpen && !$ctrl.attrs.norun" ng-disabled="$ctrl.isRunning" title="(Ctrl-S)" ng-click="$ctrl.getData()"
                ng-bind-html="::$ctrl.button"></button>
        <span class="jsframe message"
              ng-if="$ctrl.message"
              ng-bind-html="$ctrl.message"></span>
        <span class="jsframe message"
              ng-if="$ctrl.console"
              ng-bind-html="$ctrl.console"></span>
    </p>
    <span class="csRunError"
          ng-if="$ctrl.error"
          ng-style="$ctrl.tinyErrorStyle" ng-bind-html="$ctrl.error"></span>

    <p class="plgfooter" ng-if="::$ctrl.footer" ng-bind-html="::$ctrl.footer"></p>
    <button ng-click="$ctrl.tempSetData()">Set data</button>
    <button ng-click="$ctrl.tempGetData()">Get data</button>
</div>
`,
});
