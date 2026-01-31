/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import type * as t from "io-ts";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, Input, NgModule} from "@angular/core";
import type {IJsRunner} from "tim/document/viewctrl";
import {RegexOption} from "tim/document/viewctrl";
import {copyToClipboard} from "tim/util/utils";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {showConfirm} from "tim/ui/showConfirmDialog";
import type {
    AnswerReturnBrowser,
    ErrorList,
    ExportData,
    IError,
    JsrunnerMarkup,
} from "../../shared/jsrunnertypes";
import {
    ErrorEntry,
    IncludeUsersOption,
    JsrunnerAll,
} from "../../shared/jsrunnertypes";

@Component({
    selector: "js-runner",
    template: `
<div *ngIf="isVisible()" style="display: inline-block" class="jsrunner">
    <tim-markup-error *ngIf="markupError" [data]="markupError!"></tim-markup-error>
    <h4 *ngIf="header" [innerHtml]="header"></h4>
    <p *ngIf="stem" [innerHtml]="stem"></p>
    <div class="form form-inline" *ngIf="showIncludeUsersOption()">
    Users to include:
    <select [(ngModel)]="userOpt" class="form-control">
        <option *ngFor="let o of userOpts" [value]="o">{{o}}</option>
    </select>
    </div>
    <button *ngIf="hasAllAttributes()" class="timButton"
            [disabled]="isRunning || readonly"
            (click)="runScript()">
        {{buttonText()}}
    </button>&nbsp;
    <tim-loading *ngIf="isRunning"></tim-loading>
    <p class="error" *ngIf="error">Error occurred, script results may not be saved.</p>
    <pre *ngIf="error">{{error?.msg}}</pre>
    <pre *ngIf="error">{{error?.stackTrace}}</pre>
    <jsrunner-error *ngFor="let err of scriptErrors" [e]="err"></jsrunner-error>
    <div *ngIf="md" class="md" [innerHTML]="md | purify"></div>    
    <div *ngIf="html" class="html" [innerHTML]="html | purify"></div>    
    <div class="jsrunner-output" *ngIf="output">
    <p class="pull-right">
        <a class="smalltext" (click)="copyText()" title="Copy to clipboard" 
           style="position: absolute; right: 0;">copy</a>
    </p>
    <pre >{{output}}</pre>
    </div>
    <p *ngIf="footer" [innerHtml]="footer | purify" class="plgfooter"></p>
    <div *ngIf="isFieldHelper()">
    <p *ngIf="!isopen" (click)="toggleFieldHelper()" >+ Show field list</p>
    <p *ngIf="isopen" (click)="toggleFieldHelper()">- Hide field list</p>
    <pre *ngIf="isopen">{{fieldlist}}</pre>
    </div>
</div>
`,
})
export class JsRunnerPluginComponent
    extends AngularPluginBase<
        t.TypeOf<typeof JsrunnerMarkup>,
        t.TypeOf<typeof JsrunnerAll>,
        typeof JsrunnerAll
    >
    implements IJsRunner
{
    error?: IError;
    isRunning = false;
    output: string = "";
    md: string = "";
    html: string = "";
    fieldlist: string = "";

    scriptErrors?: ErrorList;
    isopen: boolean = true;
    private visible: number = -1;
    userOpts = Object.keys(IncludeUsersOption.keys);
    userOpt: t.TypeOf<typeof IncludeUsersOption> = "current";

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
        return this.markup.selectIncludeUsers;
    }

    showFieldHelper() {
        const pluginlist = this.vctrl.getTimComponentsByRegex(
            ".*",
            RegexOption.DontPrependCurrentDocId
        );
        let tasks = "";
        if (this.markup.docid) {
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

    ngOnInit() {
        super.ngOnInit();

        this.userOpt = this.markup.includeUsers;
        if (this.markup.fieldhelper && this.isVisible()) {
            this.isopen = this.markup.open ?? false;
            if (this.isopen) {
                this.showFieldHelper();
            }
        }
        const tid = this.getTaskId();
        if (tid) {
            void this.vctrl.addJsRunner(this, tid.docTask(), tid.name);
        }
    }

    willAutoRefreshTables() {
        return (
            this.markup.updateFields !== undefined &&
            this.markup.autoUpdateTables
        );
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
            const res = await showConfirm(
                $localize`Confirm`,
                this.attrsall.markup.confirmText
            );
            if (!res) {
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

        const r = await this.postAnswer<AnswerReturnBrowser>(params);
        this.isRunning = false;
        if (r.ok) {
            const data = r.result;
            if (data.web.fatalError) {
                this.error = data.web.fatalError;
            } else {
                this.error = undefined;
                this.scriptErrors = data.web.errors;
                this.output = data.web.output;
                this.md = data.web.outdata?.md ?? "";
                this.html = data.web.outdata?.html ?? "";
                if (this.attrsall.markup.updateFields) {
                    await this.vctrl.updateFields(
                        this.attrsall.markup.updateFields
                    );
                    if (this.markup.autoUpdateTables) {
                        this.vctrl.updateAllTables(
                            this.attrsall.markup.updateFields,
                            this.getTaskId()
                        );
                    }
                }

                if (this.markup.nextRunner) {
                    await this.vctrl.runJsRunner(this.markup.nextRunner, []);
                }

                // temp code:
                const tempd = data.web;
                if (!tempd.outdata) {
                    return;
                }
                this.processExportData(tempd.outdata.exportdata);
                this.vctrl.processAreaVisibility(
                    tempd.outdata.areaVisibility,
                    tempd.outdata.saveAreaVisibility ?? true
                );

                if (tempd.outdata.refresh) {
                    if (tempd.outdata.refreshRunJSRunners) {
                        const urlParams = new URLSearchParams(
                            window.location.search
                        );
                        urlParams.set(
                            "run_jsrunners",
                            tempd.outdata.refreshRunJSRunners.join(",")
                        );
                        const currentUrl = window.location.href;
                        const url = new URL(window.location.href);
                        url.search = urlParams.toString();
                        const newUrl = url.toString();
                        if (newUrl != currentUrl) {
                            window.location.href = newUrl;
                        } else {
                            location.reload();
                        }
                    } else {
                        location.reload();
                    }
                }
                if (tempd.outdata.jumplink) {
                    window.location.href = tempd.outdata.jumplink;
                }
            }
        } else {
            this.error = {
                msg: r.result.error.error ?? "Unknown error occurred",
            };
        }
    }

    private processExportData(exportData?: ExportData) {
        if (!exportData) {
            return;
        }
        for (const edata of exportData) {
            const pname = edata.plugin;
            if (!pname) {
                continue;
            }
            const plugin = this.vctrl.getTimComponentByName(pname);
            if (!plugin) {
                this.addError(`Plugin ${pname} not found. Check plugin names.`);
                continue;
            }
            const save = edata.save == true;
            if (plugin.setData) {
                plugin.setData(edata.data, save);
            } else {
                this.addError(`Plugin ${pname} does not have setData method.`);
            }
        }
    }

    public copyText() {
        copyToClipboard(this.output);
    }

    getAttributeType() {
        return JsrunnerAll;
    }

    isFieldHelper() {
        return this.markup.fieldhelper;
    }

    /**
     * If runner does not have any of the 'fields', 'groups' or 'program'-attributes, it is not considered runnable
     */
    hasAllAttributes() {
        return this.attrsall.runnable;
    }

    isVisible() {
        if (this.visible >= 0) {
            return this.visible == 1;
        }
        this.visible = 0;
        if (this.markup.fieldhelper && this.isPreview()) {
            this.visible = 1;
            return true;
        }
        if (this.markup.showInView) {
            this.visible = 1;
            return true;
        }
        const pn = window.location.pathname;
        if (pn.match("teacher|answers")) {
            this.visible = 1;
        }
        return this.visible == 1;
    }

    async runScript() {
        await this.doCheckFields(false);
    }

    async runScriptWithUsers(userNames: string[]) {
        await this.doCheckFields(false, userNames);
    }
}

@Component({
    selector: "jsrunner-error",
    template: `
<tim-alert severity="danger">
  <span>{{ e.user }}:</span>
  <div *ngFor="let err of e.errors">
    <span>{{ err.msg }}</span>
    <button *ngIf="err.stackTrace" class="timButton btn-sm" (click)="toggleStackTrace()">Stack trace</button>
    <pre *ngIf="err.stackTrace && showTrace">{{ err.stackTrace }}</pre>
  </div>
</tim-alert>
    `,
})
export class JsRunnerErrorComponent {
    @Input() e!: ErrorEntry;
    showTrace = false;

    toggleStackTrace() {
        this.showTrace = !this.showTrace;
    }
}

@NgModule({
    declarations: [JsRunnerPluginComponent, JsRunnerErrorComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        TimUtilityModule,
        FormsModule,
        PurifyModule,
        TooltipModule.forRoot(),
    ],
})
export class JsRunnerModule implements DoBootstrap {
    ngDoBootstrap(_appRef: ApplicationRef) {}
}

registerPlugin("js-runner", JsRunnerModule, JsRunnerPluginComponent);
