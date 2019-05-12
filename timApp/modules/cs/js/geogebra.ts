import angular from "angular";
import * as t from "io-ts";
import {ParCompiler} from "tim/editor/parCompiler";
import {GenericPluginMarkup, Info, PluginBase, pluginBindings, withDefault} from "tim/plugin/util";
import {$http, $sce} from "tim/util/ngimport";
import {to} from "tim/util/utils";

const geogebraApp = angular.module("geogebraApp", ["ngSanitize"]);
export const moduleDefs = [geogebraApp];
const STACK_VARIABLE_PREFIX = "geogebraapi_";

// this.attrs
const GeogebraMarkup = t.intersection([
    t.partial({
        beforeOpen: t.string,
        buttonBottom: t.boolean,
        by: t.string,
        correctresponse: t.boolean,
        generalfeedback: t.boolean,
        open: t.boolean,
        timWay: t.boolean,
        srchtml: t.string,
        width: t.number,
        height: t.number
    }),
    GenericPluginMarkup,
    t.type({
        autopeek: withDefault(t.boolean, true),
        lang: withDefault(t.string, "fi"),
        // autoplay: withDefault(t.boolean, true),
        // file: t.string,
        // open: withDefault(t.boolean, false),
    }),
]);
const GeogebraAll = t.intersection([
    t.partial({
        by: t.string,
        timWay: t.boolean,
        usercode: t.string,
        srchtml: t.string,
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
        return this.english ? "Send" : "Lähetä";
    }

    private span: string = "";
    private error: string = "";
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
    private timWay: boolean = false; // if answer is given to TIM TextArea-field
    private isOpen: boolean = true;
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
        this.userCode = aa.usercode || this.attrs.by || "";
        this.timWay = aa.timWay || this.attrs.timWay || false;

        if (this.attrs.open) {
        }
    }


    outputAsHtml() {
        if ( !this.attrs.srchtml ) return "";
        let anr = 0
        const html:string = this.attrs.srchtml;
        const datasrc = btoa(html);
        const w = this.attrs.width || 800;
        const h = this.attrs.height || 600;
        this.geogebraoutput = "<iframe id=\"jsxFrame-stack-jsxgraph-1-div1\"\n" +
            "        style=\"width:calc("+ w + "px + 2px);height:calc(" + h+ "px + 2px);border: none;\"\n" +
            "        sandbox=\"allow-scripts allow-same-origin\"\n" +
            "        class=\"geogebraFrame\"\n" +
            // "        src=\"data:text/html;base64," + datasrc + "\">\n" +
            // "src=\"https://www.geogebra.org/material/iframe/id/23587/width/1600/height/715/border/888888/rc/false/ai/false/sdz/false/smb/false/stb/false/stbh/true/ld/false/sri/false\"" +
            // 'src="'+ '/cs/reqs' + '"' +
            'src="'+ this.getHtmlUrl() + '/' + anr + '"' +
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
        const url = '/iframehtml'+this.pluginMeta.getAnswerUrl().replace('/answer','');

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


    async runSend(data: string) {
        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            return;
        }
        this.geogebrapeek = false;
        this.error = "";
        this.isRunning = true;
        const url = this.getTaskUrl();
        const geogebraData = "";
        const params = {
            input: {
                jsData: data,
                type: "geogebra",
            },
        };

        const r = await to($http<{
            web: {geogebraResult: GeogebraResult, error?: string},
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
        const geogebraResult = r.result.data.web.geogebraResult;
    }



    getData() {
        const frameElem = this.element.find(".jsFrameContainer")[0] as HTMLElement;
        const f = frameElem.firstChild as CustomFrame<JSFrameWindow>;
        let s = f.contentWindow.getData();
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
};

/*


*/

geogebraApp.component("geogebraRunner", {
    ...common,
    template: `
<div ng-cloak class="csRunDiv math que geogebra no-popup-menu" >
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" class="stem" ng-bind-html="::$ctrl.stem"></p>
    <p ng-if="!$ctrl.isOpen" class="stem" ng-bind-html="::$ctrl.attrs.beforeOpen"></p>

    <div class="no-popup-menu geogebraOutput" ng-if="::$ctrl.timWay" >
        <div class="csRunCode"><textarea class="csRunArea csInputArea"
                                 name="geogebraapi_ans1" id="geogebraapi_ans1"
                                 rows={{$ctrl.inputrows}}
                                 ng-model="$ctrl.userCode"
                                 ng-trim="false"
                                 ng-change="$ctrl.autoPeek()"
                                 placeholder="{{$ctrl.inputplaceholder}}"></textarea></div>
    </div>
    <div ng-cloak id="output" class="jsFrameContainer geogebraOutput" ng-bind-html="::$ctrl.outputAsHtml()">
    <!--<div ng-cloak id="output" ng-if="::!$ctrl.timWay" class="geogebraOutput" ng-bind-html="$ctrl.output">-->
    </div>
    <!-- <div class="peekdiv" id="peek" ng-bind-html="$ctrl.geogebrapeek"></div> -->
    <p class="csRunMenu">
        <button ng-if="$ctrl.isOpen" ng-disabled="$ctrl.isRunning" title="(Ctrl-S)" ng-click="$ctrl.getData()"
                ng-bind-html="::$ctrl.button"></button>
    </p>
    <span class="csRunError"
          ng-if="$ctrl.error"
          ng-style="$ctrl.tinyErrorStyle" ng-bind-html="$ctrl.error"></span>

    <p class="plgfooter" ng-if="::$ctrl.footer" ng-bind-html="::$ctrl.footer"></p>
    <p>GeoGebra</p>
</div>
`,
});
