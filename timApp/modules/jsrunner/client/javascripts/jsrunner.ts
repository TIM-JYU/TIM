/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {IJsRunner, RegexOption, ViewCtrl} from "tim/document/viewctrl";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {copyToClipboard, to} from "tim/util/utils";
import {
    AnswerReturnBrowser,
    ErrorList,
    IError,
    IncludeUsersOption,
    JsrunnerAll,
    JsrunnerMarkup,
} from "../../shared/jsrunnertypes";
import "style-loader!../stylesheets/jsrunner.css";

const jsrunnerApp = angular.module("jsrunnerApp", ["ngSanitize"]);
export const moduleDefs = [jsrunnerApp];

class JsrunnerController
    extends PluginBase<
        t.TypeOf<typeof JsrunnerMarkup>,
        t.TypeOf<typeof JsrunnerAll>,
        typeof JsrunnerAll
    >
    implements IJsRunner {
    private error?: IError;
    private isRunning = false;
    private output: string = "";
    private fieldlist: string = "";
    private vctrl!: ViewCtrl;
    private scriptErrors?: ErrorList;
    private isopen: boolean = true;
    private visible: number = -1;
    private userOpts = Object.keys(IncludeUsersOption.keys);
    private userOpt: t.TypeOf<typeof IncludeUsersOption> = "current";

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() ?? "Run script";
    }

    toggleFieldHelper() {
        this.isopen = !this.isopen;
        if (this.isopen) {
            this.showFieldHelper();
        }
    }

    showIncludeUsersOption() {
        return this.attrs.selectIncludeUsers;
    }

    showFieldHelper() {
        const pluginlist = this.vctrl.getTimComponentsByRegex(
            ".*",
            RegexOption.DontPrependCurrentDocId
        );
        let tasks = "";
        if (this.attrs.docid) {
            for (const plug of pluginlist) {
                const taskId = plug.getTaskId();
                if (taskId) {
                    tasks += " - " + taskId.docTask() + "\n";
                }
            }
        } else {
            for (const plug of pluginlist) {
                const name = plug.getName();
                if (name) {
                    tasks += " - " + name + "\n";
                }
            }
        }
        this.fieldlist = tasks;
    }

    $onInit() {
        super.$onInit();
        this.userOpt = this.attrs.includeUsers;
        if (this.attrs.fieldhelper && this.isVisible()) {
            this.isopen = this.attrs.open ?? false;
            if (this.isopen) {
                this.showFieldHelper();
            }
        }
        const tid = this.getTaskId();
        if (tid) {
            this.vctrl.addJsRunner(this, tid.docTask());
        }
    }

    checkFields() {
        this.doCheckFields(false);
    }

    addError(msg: string) {
        if (!this.error) {
            this.error = {msg: ""};
        }
        this.error.msg += msg;
    }

    async doCheckFields(nosave: boolean, userNames?: string[]) {
        if (this.getTaskId() == undefined) {
            this.error = {msg: "TaskId is missing."};
            return;
        }
        if (this.attrsall.markup.confirmText) {
            if (!window.confirm(this.attrsall.markup.confirmText)) {
                return;
            }
        }

        this.isRunning = true;
        this.error = undefined;
        const paramComps: Record<string, string | undefined> = {};
        if (this.attrsall.markup.paramFields) {
            for (const i of this.attrsall.markup.paramFields) {
                const timComponents = this.vctrl.getTimComponentsByRegex(
                    i,
                    RegexOption.PrependCurrentDocId
                );
                for (const v of timComponents) {
                    const cname = v.getName();
                    const value = v.getContent();
                    if (cname) {
                        paramComps[cname] = value;
                    }
                }
            }
        }

        const params = {
            input: {
                userNames: userNames,
                includeUsers: this.userOpt,
                nosave: nosave,
                paramComps: paramComps,
            },
        };

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
                if (this.attrsall.markup.updateFields) {
                    this.vctrl.updateFields(this.attrsall.markup.updateFields);
                    if (this.attrs.autoUpdateTables) {
                        this.vctrl.updateAllTables(
                            this.attrsall.markup.updateFields
                        );
                    }
                }

                if (this.attrs.nextRunner) {
                    this.vctrl.runJsRunner(this.attrs.nextRunner, []);
                }

                // temp code:
                const tempd = data.web;
                if (!tempd.outdata) {
                    return;
                }
                const exportdata = tempd.outdata.exportdata;
                if (!exportdata) {
                    return;
                }
                for (const edata of exportdata) {
                    const pname = edata.plugin;
                    if (!pname) {
                        continue;
                    }
                    const plugin = this.vctrl.getTimComponentByName(pname);
                    if (!plugin) {
                        this.addError(
                            `Plugin ${pname} not found. Check plugin names.`
                        );
                        continue;
                    }
                    const save = edata.save == true;
                    if (plugin.setData) {
                        plugin.setData(edata.data, save);
                    } else {
                        this.addError(
                            `Plugin ${pname} does not have setData method.`
                        );
                    }
                }
            }
        } else {
            this.error = {msg: r.result.data.error || "Unknown error occurred"};
        }
    }

    public copyText() {
        copyToClipboard(this.output);
    }

    getAttributeType() {
        return JsrunnerAll;
    }

    protected isFieldHelper() {
        return this.attrs.fieldhelper;
    }

    /**
     * If runner does not have any of the 'fields', 'groups' or 'program'-attributes, it is not considered runnable
     */
    protected hasAllAttributes() {
        return this.attrsall.runnable;
    }

    isVisible() {
        if (this.visible >= 0) {
            return this.visible == 1;
        }
        this.visible = 0;
        if (this.attrs.showInView) {
            this.visible = 1;
            return true;
        }
        const pn = window.location.pathname;
        if (pn.match("teacher|answers")) {
            this.visible = 1;
        }
        return this.visible == 1;
    }

    runScriptWithUsers(userNames: string[]) {
        this.doCheckFields(false, userNames);
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
<div ng-if="::$ctrl.isVisible()" style="display: inline-block">
    <tim-markup-error ng-if="::$ctrl.markupError" [data]="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" ng-bind-html="::$ctrl.stem"></p>
    <div class="form form-inline" ng-if="::$ctrl.showIncludeUsersOption()">
    Users to include:
    <select ng-options="o for o in $ctrl.userOpts"
            ng-model="$ctrl.userOpt"
            class="form-control">
    </select>
    </div>
    <button ng-if="::$ctrl.hasAllAttributes()" class="timButton"
            ng-disabled="$ctrl.isRunning || $ctrl.readonly"
            ng-click="$ctrl.checkFields()">
        {{::$ctrl.buttonText()}}
    </button>
    <tim-loading ng-if="$ctrl.isRunning"></tim-loading>
    <p class="error" ng-if="$ctrl.error">Error occurred, script results may not be saved.</p>
    <pre ng-if="$ctrl.error">{{$ctrl.error.msg}}</pre>
    <pre ng-if="$ctrl.error">{{$ctrl.error.stackTrace}}</pre>
    <jsrunner-error ng-repeat="err in $ctrl.scriptErrors" e="err"></jsrunner-error>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <div ng-if="$ctrl.output">
    <p class="pull-right">
        <a class="smalltext" ng-click="$ctrl.copyText()" title="Copy to clipboard" 
           style="position: absolute; right: 0;">copy</a>
    </p>
    <pre >{{$ctrl.output}}</pre>
    </div>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
    <div ng-if="::$ctrl.isFieldHelper()">
    <p ng-show="!$ctrl.isopen" ng-click="$ctrl.toggleFieldHelper()" >+ Show field list</p>
    <p ng-show="$ctrl.isopen" ng-click="$ctrl.toggleFieldHelper()">- Hide field list</p>
    <pre ng-show="$ctrl.isopen">{{$ctrl.fieldlist}}</pre>
    </div>
</div>
`,
});
