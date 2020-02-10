/* eslint-disable @typescript-eslint/tslint/config,@typescript-eslint/no-explicit-any */
import angular from "angular";
import * as t from "io-ts";
import {IAnswer} from "tim/answer/IAnswer";
import {ITimComponent, IUserChanged, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {IUser} from "tim/user/IUser";
import {$http, $sce, $timeout} from "tim/util/ngimport";
import {to} from "tim/util/utils";

const jsframeApp = angular.module("jsframeApp", ["ngSanitize"]);
export const moduleDefs = [jsframeApp];

// this.attrs
const JsframeMarkup = t.intersection([
    t.partial({
        beforeOpen: t.string,
        showButton: t.string,
        srchtml: t.string,
        iframeopts: t.string,
        message: t.string,
        width: t.number,
        height: t.number,
        tool: t.boolean,
        useurl: t.boolean,
        c: t.any,
        data: t.any,
        fielddata: t.any,
        fields: t.any,
        initListener: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        open: withDefault(t.boolean, true),
        borders: withDefault(t.boolean, false),
        norun: withDefault(t.boolean, true),
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
    }),
    t.type({
        info: Info,
        markup: JsframeMarkup,
        preview: t.boolean,
    }),
]);

interface JSFrameWindow extends Window {
    getData(): string;
    setData(state: any): void;
}

interface CustomFrame<T extends Window> extends HTMLIFrameElement {
    contentWindow: T;
}

class JsframeController extends PluginBase<t.TypeOf<typeof JsframeMarkup> ,
    t.TypeOf<typeof JsframeAll>,
    typeof JsframeAll> implements ITimComponent, IUserChanged {

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

    public viewctrl!: ViewCtrl;
    private span: string = "";
    private error: string = "";
    private console: string = "";
    private message: string = "";
    private userCode: string = "";
    private jsframeoutput: string = "";
    private jsframepeek: boolean = false;
    private isRunning: boolean = false;
    private isOpen: boolean = false;
    private button: string = "";

    private timer: NodeJS.Timer | undefined;

    private taskUrl: string = "";
    private htmlUrl: string = "";
    private initData: string = "";
    private userName: string = "";

    private saveResponse: {saved: boolean, message: (string | undefined)} = {saved: false, message: undefined};

    $onInit() {
        super.$onInit();
        this.button = this.buttonText();
        const aa: any = this.attrsall;
        this.userName = aa.user_id;
        this.userCode = aa.usercode ||  "";

        this.message = this.attrs.message ?? "";

        let jsobject = "window.";
        if (this.attrs.srchtml && this.attrs.srchtml.includes("TIMJS")) { jsobject = "TIMJS."; }

        if (this.attrs.open) {
            this.isOpen = true;
        }
        let data = this.attrs.data;
        if (this.attrs.c) { data = this.attrs.c; }
        if (data) { this.initData = "    " + jsobject + "initData = " + JSON.stringify(data) + ";\n"; }
        if (aa.markup.fielddata) { this.initData += "    " + jsobject + "fieldData = " + JSON.stringify(aa.markup.fielddata) + ";\n"; }
        // if ( data ) { this.setData(data); }
        this.viewctrl.addTimComponent(this);
        const tid = this.getTaskId();
        if (!this.attrs.forceBrowser && tid) {
            this.viewctrl.addUserChangeListener(tid.docTask(), this);
        }
        const wnd = this;
        if (this.attrs.initListener && !this.attrsall.preview) {
            setTimeout(function() { wnd.addListener(); }, 500);
        }
    }

    async userChanged(user: IUser) {
        // TODO: Experimental
        if (user.name == this.userName) { return; }
        if (!this.attrsall.markup.fields) { return; }
        this.userName = user.name;

        try {
            const tid = this.getTaskId();
            if (!tid) {
                return;
            }
            const res = await to($http.get<any>(`/jsframeUserChange/${tid.docTask()}/${user.id}`));
            this.initData = "";
            let data: any = this.attrs.data;
            if (this.attrs.c) { data = this.attrs.c; }
            if (res.result.data.fielddata) {
                if (!data) { data = {}; }
                data.fielddata = res.result.data.fielddata;
            }
            if (data) { this.setData(data); }
        } catch (e) {
            this.error = "Error fetching new data for user" + "\n" + e;
        }
    }

    runShowTask() {
        this.isOpen = true;
    }

    changeAnswer(a: IAnswer) {
        const frameElem = this.element.find(".jsFrameContainer")[0];
        const f = frameElem.firstChild as CustomFrame<JSFrameWindow>;
        if (!f.contentWindow.setData) {
            return;
        }
        f.contentWindow.setData(JSON.parse(a.content));
    }

    private communicationJS = `
    <script>
    // let port2;

    window.addEventListener('message', function(e) {
         if ( e.data.msg === "init" ) {
            window.port2 = e.ports[0];
            window.port2.onmessage = onMessage;
            window.port2.postMessage({msg:"Inited"});
         }
    });

    function onMessage(event) {
         // console.log(event.data);
         // console.log(event.origin);

         if ( event.data.msg === "setData" ) {
            setData(event.data.data);
            window.port2.postMessage({msg: "Got data!"});
         }
         if ( event.data.msg === "getData" ) {
            window.port2.postMessage({msg:"data", data: getData()});
         }
         if ( event.data.msg === "getDataSave" ) {
            window.port2.postMessage({msg:"datasave", data: getData()});
         }
    }

// INITDATA
    </script>
    `;

    outputAsHtml() {
        // if ( !this.attrs.srchtml ) return "";
        if (this.attrsall.preview) {
            // return "";
        } // TODO: replace when preview delay and preview from markup ready
        $timeout(0);
        const w = this.attrs.width ?? 800; // for some reason if w/h = 2, does not give hints on Chart.js
        const h = this.attrs.height ?? (208 * w / 400);  //  experimental result for Chart.js
        let src = "";
        if (this.attrs.useurl) {
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
            let url = this.getHtmlUrl() + "/" + userId + "/" + anr;
            url = url.replace("//", "/");
            src = 'src="' + url + '"';
        } else {
            let html: string = this.attrs.srchtml ?? "";
            html = html.replace("</body>", this.communicationJS + "\n</body>");
            html = html.replace("// INITDATA", this.initData);
            const datasrc = btoa(html);
            src = "src='data:text/html;base64," + datasrc;
        }

        const iframeopts = this.attrs.iframeopts ?? "sandbox='allow-scripts allow-same-origin'";

        // let url = this.getHtmlUrl() + "/" + userId + "/" + anr;
        // url = url.replace("//", "/");
        this.jsframeoutput = "<iframe id='jsIFrame'\n" +
            "        class='showJsframe jsframeFrame' \n" +
            "        style='margin-left: auto;\n" +
            "               margin-right: auto;\n" +
            "               display: block;" +
            "               width: " + w + "px; height: " + h + "px;border: none;'\n" +
            "        " + iframeopts + "\n" +
            src +
            "'>\n" +
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
            this.saveResponse.saved = false;
            return this.saveResponse;
        }
        this.jsframepeek = false;
        this.error = "";
        this.isRunning = true;
        const url = this.getTaskUrl();
        data.type = "jsframe";
        const params = {
            input: data,
        };

        this.console = "";

        const r = await to($http<{
            web: {error?: string, console?: string},
        }>({method: "PUT", url: url, data: params, timeout: 20000},
        ));
        this.isRunning = false;

        if (!r.ok) {
            this.error = r.result.data.error;
            this.saveResponse.saved = false;
            return this.saveResponse;
        }
        if (!r.result.data.web) {
            this.error = "No web reply from csPlugin!";
            this.saveResponse.saved = false;
            return this.saveResponse;
        }
        if (r.result.data.web.error) {
            this.error = r.result.data.web.error;
            this.saveResponse.saved = false;
            return this.saveResponse;
        }
        if (r.result.data.web.console) {
            this.console = r.result.data.web.console;
            this.saveResponse.saved = true;
            return this.saveResponse;
        }
        this.saveResponse.saved = true;
        return this.saveResponse;
    }

    getData(msg: string = "getData") {
        if (this.attrs.useurl) {
            const frameElem = this.element.find(".jsFrameContainer")[0];
            const f = frameElem.firstChild as CustomFrame<JSFrameWindow>;
            if (!f.contentWindow.getData) {
                return;
            }
            return this.getDataReady(f.contentWindow.getData());
        }
        this.send({msg: msg });
    }

    getDataReady(data: any, dosave: boolean = false) {
        if (data.message) {
            this.message = data.message;
        }
        if (dosave) { this.runSend({c: data}); }
        return data;
    }

    setData(data: any, save: boolean = false) {
        $timeout(0);

        if (save) { this.runSend({c: data}); }
        if (this.attrs.useurl) {
            const frameElem = this.element.find(".jsFrameContainer")[0];
            const f = frameElem.firstChild as CustomFrame<JSFrameWindow>;
            if (!f.contentWindow.setData) {
                return;
            }
            f.contentWindow.setData(data);
        } else {
            this.send({msg: "setData", data: data });
        }
    }

    getContent() {
        return this.getData();
    }

    async save() {
        const data = this.getData("getDataSave");
        return this.runSend({c: data});
    }

    isUnSaved() {
        return false;  // TODO: compare datas
    }

    tempSetData() {
        const newData = {  labels: [1, 2, 3, 4, 5, 6],
                           data:   [4, 5, 6, 12, 3, 10] };
        this.setData(newData);
    }

    tempGetData() {
        this.getData();
    }

    send(obj: any) {
        this.addListener();
        this.channel.port1.postMessage(obj);
    }

    private channel: any = 0;

    addListener() {
        if (this.channel) { return; }
        this.channel = new MessageChannel();
        this.channel.port1.onmessage =  (event: any) => {
            // console.log(event);
            const msg = event.data.msg;
            if (msg === "data") {
                this.getDataReady(event.data.data);
            }
            if (msg === "datasave") {
                this.getDataReady(event.data.data, true);
            }
        };
        const frameElem = this.element.find(".jsFrameContainer")[0];
        const f = frameElem.firstChild as CustomFrame<JSFrameWindow>;
        f.contentWindow.postMessage({msg: "init" }, "*", [this.channel.port2]);
    }

    getDefaultMarkup() {
        return {};
    }

    getAttributeType() {
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
        <button ng-if="$ctrl.isOpen && !$ctrl.attrs.norun" ng-disabled="$ctrl.isRunning" title="(Ctrl-S)" ng-click="$ctrl.getData('getDataSave')"
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
    <!--<button ng-click="$ctrl.tempSetData()">Set data</button>
    <button ng-click="$ctrl.tempGetData()">Get data</button>-->
</div>
`,
});
