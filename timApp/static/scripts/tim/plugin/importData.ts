/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {ngStorage} from "ngstorage";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$http, $localStorage} from "tim/util/ngimport";
import {to} from "tim/util/utils";
import {timApp} from "../app";
import {GenericPluginMarkup, Info, nullable, withDefault} from "./attributes";

const importDataApp = angular.module("importDataApp", ["ngSanitize"]);
export const moduleDefs = [importDataApp];

// this.attrs
const ImportDataMarkup = t.intersection([
    t.partial({
        beforeOpen: t.string,
        buttonBottom: t.boolean,
        message: t.string,
        width: t.number,
        height: t.number,
        tool: t.boolean,
        fields: t.array(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        open: withDefault(t.boolean, false),
        borders: withDefault(t.boolean, true),
        lang: withDefault(t.string, "fi"),
        upload: withDefault(t.boolean, false),
        useurl: withDefault(t.boolean, false),
        useseparator: withDefault(t.boolean, false),
        usefields: withDefault(t.boolean, false),
        uploadstem: withDefault(t.string, "File to upload:"),
        loadButtonText: withDefault(t.string, "Load from URL"),
        urlstem: withDefault(t.string, "URL: "),
        separatorstem: withDefault(t.string, "Separator: "),
        separator: withDefault(t.string, ";"),
        url: withDefault(t.string, ""),
        beforeOpen: withDefault(t.string, "+ Open import"),
        placeholder: withDefault(t.string, "Put here content to import"),
        showInView: withDefault(t.boolean, false),
        useurltoken: withDefault(t.boolean, false),
    }),
]);
const ImportDataAll = t.intersection([
    t.partial({
    }),
    t.type({
        info: Info,
        markup: ImportDataMarkup,
        preview: t.boolean,
        state: nullable(t.partial({
            url: t.string,
            separator: t.string,
            fields: t.array(t.string),
        })),
    }),
]);

type ImportDataResult = string | {
    error: true,
    message: string,
    result: string,
};

interface IImportDataData {
    answer: {[name: string]: string};
}

class ImportDataController extends PluginBase<t.TypeOf<typeof ImportDataMarkup>, t.TypeOf<typeof ImportDataAll>, typeof ImportDataAll> {
    private isRunning = false;
    private importText: string = "";
    private error: {message?: string, stacktrace?: string} = {};
    private result: string = "";
    private isOpen: boolean = false;
    private url: string = "";
    private separator: string = ";";
    private visible: number = -1;
    private fields: string = "";
    private urlToken: string = "";
    private fetchingData = false;
    private storage!: ngStorage.StorageService & {importToken: null | string; importUrl: null | string};

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "Import";
    }

    $onInit() {
        super.$onInit();
        this.isOpen = this.attrs.open;
        const aa = this.attrsall;
        const state = aa.state;
        this.storage = $localStorage.$default({
            importUrl: null,
            importToken: null,
        });
        this.separator = (state && state.separator) || this.attrs.separator;
        this.url = (state && state.url) || this.storage.importUrl || this.attrs.url;
        this.urlToken = this.storage.importToken || "";
        this.fields = ((state && state.fields) || this.attrs.fields || []).join("\n");
    }

    getAttributeType() {
        return ImportDataAll;
    }

    isVisible() {
        if (this.visible >= 0) { return this.visible == 1; }
        this.visible = 0;
        if (this.attrs.showInView) { this.visible = 1; return true; }
        const pn = window.location.pathname;
        if (pn.match("teacher|answers")) { this.visible = 1; }
        return this.visible == 1;
    }

    // noinspection JSUnusedGlobalSymbols
    async pickFromWWW() {
        this.error = {};
        this.result = "";

        this.fetchingData = true;
        const r = await to($http.get<{
            data: string,
            status_code: number,
        }>("/getproxy", {
                params: {
                    url: this.url,
                    auth_token: this.urlToken,
                },
            },
        ));
        this.fetchingData = false;
        if (!r.ok) {
            this.error.message = r.result.data.error;
            return;
        }
        if (r.result.data.status_code >= 400) {
            this.error.message = r.result.data.data;
            return;
        }
        this.storage.importToken = this.urlToken;
        this.storage.importUrl = this.url;
        this.importText = r.result.data.data;
    }

    onFileSelect(file: File) {
        if (!file) { return; }
        const reader = new FileReader();
        // noinspection JSUnusedLocalSymbols
        reader.onload = ((_) => {
            this.scope.$evalAsync(() => {
                this.importText = reader.result as string;
            });
        });
        reader.readAsText(file);

    }

    // noinspection JSUnusedGlobalSymbols
    async doImport() {
        this.isRunning = true;
        const text = this.importText;
        const url = this.pluginMeta.getAnswerUrl();
        // url.replace("answer", "importData");
        const params = {
            input: {
                data: text,
                separator: this.separator,
                url: this.url,
                fields: this.fields ? this.fields.split("\n") : undefined,
            },
        };

        this.error = {};
        this.result = "";

        const r = await to($http<{
            web?: { result?: string, error?: string }
            error?: string,
        }>({method: "PUT", url: url, data: params, timeout: 20000},
        ));

        this.isRunning = false;
        if (!r.ok) {
            const e = r.result.data.error;
            if (e) {
                if (e.startsWith("{")) {
                    try {
                        const jse = JSON.parse(e) as object;
                        if (t.type({error: t.string}).is(jse)) {
                            this.error.message = jse.error;
                        }
                        if (this.error) { return; }
                    } catch { }
                }
                this.error.message = e;
                return;
            }
            this.error.message = (r.result.data).toString();
            return;
        }
        if (!r.result.data.web) {
            this.error.message = "No web reply from ImportData!";
            return;
        }
        if (r.result.data.error) {
            this.error.message = r.result.data.error;
            return;
        }
        if (r.result.data.web.error) {
            this.error.message = r.result.data.web.error;
            return;
        }
        if (r.result.data.web.result) {
            this.result = r.result.data.web.result;
            return;
        }
    }

}

timApp.component("importdataError", {
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
    <span ng-bind-html="$ctrl.e.message"></span>
    <button ng-if="$ctrl.e.stackTrace" class="timButton btn-sm" ng-click="$ctrl.toggleStackTrace()">Stack trace</button>
    <pre ng-if="$ctrl.e.stackTrace && $ctrl.showTrace">{{ $ctrl.e.stackTrace }}</pre>
</tim-alert>
    `,
});

timApp.component("importdataRunner", {
    bindings: pluginBindings,
    controller: ImportDataController,
    require: {
        vctrl: "^timView",
    },
    template: `
<tim-markup-error ng-if="::$ctrl.markupError" [data]="::$ctrl.markupError"></tim-markup-error>
<div ng-cloak ng-class="{'csRunDiv': ($ctrl.attrs.borders && $ctrl.isOpen)}" class="importDataDiv no-popup-menu"
     ng-if="::$ctrl.isVisible()">
    <p ng-if="!$ctrl.isOpen" class="stem" ng-bind-html="::$ctrl.attrs.beforeOpen" ng-click="$ctrl.isOpen = true"></p>
    <div ng-if="$ctrl.isOpen">
        <p class="closeButton" ng-click="$ctrl.isOpen = false"/>
        <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
        <div class="importDataInner">
            <p ng-if="::$ctrl.stem" class="stem" ng-bind-html="::$ctrl.stem"></p>
            <div ng-if="::$ctrl.attrs.useurl" class="form">
                <div class="form-group form-group-sm">
                    <label for="url" class="small">{{::$ctrl.attrs.urlstem}}</label>
                    <input id="url" class="form-control" ng-model="$ctrl.url" size="50">
                </div>
                <div ng-if="$ctrl.attrs.useurltoken" class="form-group form-group-sm">
                    <label for="urltoken" class="small">Auth token:</label>
                    <input id="urltoken" class="form-control" ng-model="$ctrl.urlToken">
                </div>
                <button ng-disabled="$ctrl.fetchingData || !$ctrl.url"
                        class="timButton btn-sm"
                        ng-click="$ctrl.pickFromWWW()">{{::$ctrl.attrs.loadButtonText}}
                </button>
                <tim-loading ng-if="$ctrl.fetchingData"></tim-loading>
            </div>
            <div ng-if="::$ctrl.attrs.upload" class="form-inline small">
                <div class="form-group small"> {{::$ctrl.attrs.uploadstem}}
                    <input type="file" ngf-select="$ctrl.onFileSelect($file)">
                </div>
                <div class="error" ng-show="$ctrl.fileError" ng-bind="$ctrl.fileError"></div>
                <div ng-if="$ctrl.uploadresult"><span ng-bind-html="$ctrl.uploadresult"></span></div>
            </div>
            <p class="form-inline small" ng-if="$ctrl.attrs.useseparator">{{$ctrl.attrs.separatorstem}}<input
                    ng-model="$ctrl.separator" size="5"/></p>
            <p>Fields:</p>
            <p><textarea class="form-control" ng-if="$ctrl.attrs.usefields" ng-model="$ctrl.fields" placeholder="fields"
                         rows="7" cols="30"></textarea></p>
            <p></p>
            <p><textarea class="form-control" ng-model="$ctrl.importText"
                         ng-attr-placeholder="{{$ctrl.attrs.placeholder}}" rows="10" cols="50"></textarea></p>
            <button class="timButton" ng-disabled="$ctrl.isRunning" ng-click="$ctrl.doImport()">
                {{::$ctrl.buttonText()}}
            </button>
            <tim-loading ng-if="$ctrl.isRunning"></tim-loading>
            <div ng-if="$ctrl.error.message">
                <p class="closeButton" ng-click="$ctrl.error = ''"/>
                <importdata-error ng-if="$ctrl.error" e="$ctrl.error"></importdata-error>
                <importdata-error ng-repeat="err in $ctrl.scriptErrors" e="err"></importdata-error>
            </div>
            <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
            <pre ng-if="$ctrl.output">{{$ctrl.output}}</pre>
            <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
            <p></p>
        </div>

    </div>
</div>
`,
});
