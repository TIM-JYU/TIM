import angular from "angular";
import * as t from "io-ts";
import {IAnswer} from "tim/answer/IAnswer";
import {ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$http, $sce, $timeout} from "tim/util/ngimport";
import {to} from "tim/util/utils";

const geogebraApp = angular.module("geogebraApp", ["ngSanitize"]);
export const moduleDefs = [geogebraApp];

// this.attrs
const GeogebraMarkup = t.intersection([
    t.partial({
        beforeOpen: t.string,
        buttonBottom: t.boolean,
        correctresponse: t.boolean,
        generalfeedback: t.boolean,
        showButton: t.string,
        // srchtml: t.string,
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
const GeogebraAll = t.intersection([
    t.partial({
        usercode: t.string,
        // srchtml: t.string,
        norun: t.boolean,
    }),
    t.type({
        info: Info,
        markup: GeogebraMarkup,
        preview: t.boolean,
    }),
]);

type GeogebraResult = string | {
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

interface IGeogebraData {
    answer: {[name: string]: string};
    prefix: string;
    seed?: number;
    verifyvar: string;
}

interface JSFrameWindow extends Window {
    getData(): string;
    setData(state: any): void;
}

interface CustomFrame<T extends Window> extends HTMLIFrameElement {
    contentWindow: T;
}

class GeogebraController extends PluginBase<t.TypeOf<typeof GeogebraMarkup>,
    t.TypeOf<typeof GeogebraAll>,
    typeof GeogebraAll> {

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
    private geogebraoutput: string = "";
    private geogebrainputfeedback: string = "";
    private geogebrapeek: boolean = false;
    private geogebrafeedback: string = "";
    private geogebraformatcorrectresponse: string = "";
    private geogebrascore: string = "";
    private geogebrasummariseresponse: string = "";
    private geogebraanswernotes: string = "";
    private geogebratime: string = "";
    private isRunning: boolean = false;
    private inputrows: number = 1;
    private isOpen: boolean = false;
    private lastInputFieldId: string = "";
    private lastInputFieldValue: string = "";
    private lastInputFieldElement: HTMLInputElement | undefined;
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
            return "";
        } // TODO: replace when preview delay and preview from markup ready
        $timeout(0);
        const tid = this.pluginMeta.getTaskId()!;
        const taskId = tid.docTask();
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
        // const html:string = this.attrs.srchtml;
        // const datasrc = btoa(html);
        let w = this.attrs.width || 800;
        let h = this.attrs.height || 450;

        if (this.attrs.tool) {
            w = Math.max(w, 1200);
            h = Math.max(w, 1100);
        }

        let url = this.getHtmlUrl() + "/" + userId + "/" + anr;
        url = url.replace("//", "/");
        this.geogebraoutput = "<iframe id=\"jsxFrame-stack-jsxgraph-1-div1\"\n" +
            "        class=\"showGeoGebra geogebraFrame\" \n" +
            "        style=\"width:calc(" + w + "px + 2px);height:calc(" + h + "px + 2px);border: none;\"\n" +
            "        sandbox=\"allow-scripts allow-same-origin\"\n" +
            // "        src=\"data:text/html;base64," + datasrc + "\">\n" +
            // "src=\"https://www.geogebra.org/material/iframe/id/23587/width/1600/height/715/border/888888/rc/false/ai/false/sdz/false/smb/false/stb/false/stbh/true/ld/false/sri/false\"" +
            // 'src="'+ '/cs/reqs' + '"' +
            'src="' + url + '"' +
            "</iframe>";
        const s = $sce.trustAsHtml(this.geogebraoutput);
        return s;
    }

    geogebrainputfeedbackAsHtml() {
        const s = $sce.trustAsHtml(this.geogebrainputfeedback);
        return s;
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
        this.geogebrapeek = false;
        this.error = "";
        this.isRunning = true;
        const url = this.getTaskUrl();
        const geogebraData = "";
        data.type = "geogebra";
        const params = {
            input: data,
        };

        this.console = "";

        const r = await to($http<{
            web: {geogebraResult: GeogebraResult, error?: string, console?: string},
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
        const geogebraResult = r.result.data.web.geogebraResult;
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

    getDefaultMarkup() {
        return {};
    }

    protected getAttributeType() {
        return GeogebraAll;
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
    controller: GeogebraController,
    require: {
        viewctrl: "?^timView",
    },
};

/*

*/

geogebraApp.component("geogebraRunner", {
    ...common,
    template: `
<div ng-cloak ng-class="::{'csRunDiv': $ctrl.attrs.borders}"  class="math que geogebra no-popup-menu" >
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" class="stem" ng-bind-html="::$ctrl.stem"></p>
    <p ng-if="!$ctrl.isOpen" class="stem" ng-bind-html="::$ctrl.attrs.beforeOpen"></p>

    <div ng-cloak ng-if="$ctrl.isOpen" id="output" class="jsFrameContainer geogebraOutput" ng-bind-html="::$ctrl.outputAsHtml()">
    <!--<div ng-cloak id="output" ng-if="::!$ctrl.timWay" class="geogebraOutput" ng-bind-html="$ctrl.output">-->
    </div>
    <!-- <div class="peekdiv" id="peek" ng-bind-html="$ctrl.geogebrapeek"></div> -->
    <p class="csRunMenu">
        <button ng-if="!$ctrl.isOpen"  ng-click="$ctrl.runShowTask()"  ng-bind-html="$ctrl.showButton()"></button>
        <button ng-if="$ctrl.isOpen && !$ctrl.attrs.norun" ng-disabled="$ctrl.isRunning" title="(Ctrl-S)" ng-click="$ctrl.getData()"
                ng-bind-html="::$ctrl.button"></button>
        <span class="geogebra message"
              ng-if="$ctrl.message"
              ng-bind-html="$ctrl.message"></span>
        <span class="geogebra message"
              ng-if="$ctrl.console"
              ng-bind-html="$ctrl.console"></span>
    </p>
    <span class="csRunError"
          ng-if="$ctrl.error"
          ng-style="$ctrl.tinyErrorStyle" ng-bind-html="$ctrl.error"></span>

    <p class="plgfooter" ng-if="::$ctrl.footer" ng-bind-html="::$ctrl.footer"></p>
</div>
`,
});
