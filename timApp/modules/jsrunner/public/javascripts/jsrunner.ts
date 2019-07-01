/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {ViewCtrl} from "tim/document/viewctrl";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import "../../stylesheets/jsrunner.css";
import {AnswerReturnBrowser, ErrorList, IError, JsrunnerAll, JsrunnerMarkup} from "./jsrunnertypes";

const jsrunnerApp = angular.module("jsrunnerApp", ["ngSanitize"]);
export const moduleDefs = [jsrunnerApp];

class JsrunnerController extends PluginBase<t.TypeOf<typeof JsrunnerMarkup>, t.TypeOf<typeof JsrunnerAll>, typeof JsrunnerAll> {
    private error?: IError;
    private isRunning = false;
    private output: string = "";
    private fieldlist: string = "";
    private vctrl!: ViewCtrl;
    private scriptErrors?: ErrorList;
    private isopen: boolean = true;
    private visible: number = -1;

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "Run script";
    }

    $onInit() {
        super.$onInit();
        if (this.attrs.fieldhelper && this.isVisible()) {
            this.isopen = this.attrs.open || false;
            const pluginlist = this.vctrl.getTimComponentsByRegex(".*");
            let tasks = "";
            if (this.attrs.docid) {
                for (const plug of pluginlist) {
                    const taskId = plug.getTaskId();
                    if (taskId) {
                        tasks += " - " + taskId.toString() + "\n";
                    }
                }
            } else {
                for (const plug of pluginlist) {
                    const name = plug.getName();
                    if (name) {
                        tasks += " - " + name.toString() + "\n";
                    }
                }
            }
            this.fieldlist = tasks;
        }
    }

    checkFields() {
        this.doCheckFields(false);
    }

    async doCheckFields(nosave: boolean) {
        this.isRunning = true;
        const params = {
            input: {
                nosave: false,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to($http.put<AnswerReturnBrowser>(url, params));
        this.isRunning = false;
        if (r.ok) {
            const data = r.result.data;
            if (data.web.fatalError) {
                this.error = data.web.fatalError;
            } else {
                this.error = undefined;
                this.scriptErrors = data.web.errors;
                this.output = data.web.output;
            }
        } else {
            this.error = {msg: r.result.data.error || "Unknown error occurred"};
        }
    }

    protected getAttributeType() {
        return JsrunnerAll;
    }

    protected isFieldHelper() {
        return this.attrs.fieldhelper;
    }

    protected hasAllAttributes() {
        return (this.attrs.fields || this.attrs.groups || this.attrs.program);
    }

    isVisible() {
        if ( this.visible >= 0 ) { return this.visible == 1; }
        this.visible = 0;
        if ( this.attrs.showInView ) { this.visible = 1; return true; }
        const pn = window.location.pathname;
        if ( pn.match("teacher|answers") ) { this.visible = 1; }
        return this.visible == 1;
    }

}

jsrunnerApp.component("jsrunnerError", {
    bindings: {
        e: "<",
    },
    controller: class {
        showTrace = false;

        toggleStackTrace() {
            this.showTrace = !this.showTrace;
        }
    },
    template: `
<tim-alert severity="danger">
  <span>{{ $ctrl.e.user }}:</span>
  <div ng-repeat="err in $ctrl.e.errors">
    <span>{{ err.msg }}</span>
    <button ng-if="err.stackTrace" class="timButton btn-sm" ng-click="$ctrl.toggleStackTrace()">Stack trace</button>
    <pre ng-if="err.stackTrace && $ctrl.showTrace">{{ err.stackTrace }}</pre>
  </div>
</tim-alert>
    `,
});

jsrunnerApp.component("jsRunner", {
    bindings: pluginBindings,
    controller: JsrunnerController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="jsRunnerDiv" ng-if="::$ctrl.isVisible()">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" ng-bind-html="::$ctrl.stem"></p>
    <button ng-if="::$ctrl.hasAllAttributes()" class="timButton"
            ng-disabled="$ctrl.isRunning || $ctrl.readonly"
            ng-click="$ctrl.checkFields()">
        {{::$ctrl.buttonText()}}
    </button>
    <p ng-if="$ctrl.error">Fatal error occurred, script results not saved.</p>
    <pre ng-if="$ctrl.error">{{$ctrl.error.msg}}</pre>
    <pre ng-if="$ctrl.error">{{$ctrl.error.stackTrace}}</pre>
    <jsrunner-error ng-repeat="err in $ctrl.scriptErrors" e="err"></jsrunner-error>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <pre ng-if="$ctrl.output">{{$ctrl.output}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
    <div ng-if="::$ctrl.isFieldHelper()">
    <p ng-show="!$ctrl.isopen" ng-click="$ctrl.isopen=true" >+ Show field list</p>
    <p ng-show="$ctrl.isopen" ng-click="$ctrl.isopen=false">- Hide field list</p>
    <pre ng-show="$ctrl.isopen">{{$ctrl.fieldlist}}</pre>
    </div>
</div>
`,
});
