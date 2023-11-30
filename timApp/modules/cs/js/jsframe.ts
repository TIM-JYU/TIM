import * as t from "io-ts";
import type {IAnswer} from "tim/answer/IAnswer";
import type {
    ITimComponent,
    IUserChanged,
    ViewCtrl,
} from "tim/document/viewctrl";
import {ChangeType} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, withDefault} from "tim/plugin/attributes";
import type {IUser} from "tim/user/IUser";
import {$http} from "tim/util/ngimport";
import {
    defaultErrorMessage,
    defaultTimeout,
    parseIframeopts,
    to,
} from "tim/util/utils";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {
    ChangeDetectorRef,
    Component,
    ElementRef,
    NgModule,
    NgZone,
    ViewChild,
} from "@angular/core";
import type {SafeResourceUrl} from "@angular/platform-browser";
import {DomSanitizer} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {TimDefer} from "tim/util/timdefer";
import {PurifyModule} from "tim/util/purify.module";
import type {
    ICtrlWithMenuFunctionEntry,
    IMenuFunctionEntry,
} from "tim/document/viewutils";
import {
    exitFullScreen,
    fullscreenSupported,
    toggleFullScreen,
} from "tim/util/fullscreen";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import type {AnswerBrowserComponent} from "tim/answer/answer-browser.component";
import {CommonModule} from "@angular/common";
import {communicationJS} from "./iframeutils";

const JsframeMarkup = t.intersection([
    t.partial({
        type: t.string,
        beforeOpen: t.string,
        saveButton: t.string,
        showButton: t.string,
        srchtml: t.string,
        iframeopts: t.string,
        message: t.string,
        width: t.number,
        height: t.number,
        tool: t.boolean,
        useurl: t.boolean,
        c: t.unknown,
        data: t.unknown,
        fielddata: t.unknown,
        fields: t.unknown,
        initListener: t.boolean,
        resetText: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        open: withDefault(t.boolean, true),
        borders: withDefault(t.boolean, false),
        norun: withDefault(t.boolean, true),
        lang: withDefault(t.string, "fi"),
        task: withDefault(t.boolean, true), // TODO: check if other jsframes are tasks or not
        apifullscreen: withDefault(t.boolean, false),
    }),
]);
const JsframeAll = t.intersection([
    t.partial({
        usercode: t.string,
        user_id: t.string,
        srchtml: t.string,
    }),
    t.type({
        info: Info,
        markup: JsframeMarkup,
        preview: t.boolean,
    }),
]);

interface JSFrameData {
    c: unknown;
    message?: string;
}

interface JSFrameWindow extends Window {
    getData?(): JSFrameData;
    setData?(state: JSFrameData): void;
}

interface CustomFrame<T extends Window> extends HTMLIFrameElement {
    contentWindow: T;
}

const CProp = t.type({
    c: t.unknown,
});

type MessageGet = "getData" | "getDataSave";

type MessageToFrame =
    | {
          msg: "setData";
          data: JSFrameData;
      }
    | {msg: "init"}
    | {msg: "getData"}
    | {msg: "getDataSave"}
    | {msg: "close"}
    | {msg: "toggleEditorOptions"}
    | {msg: "start"}
    | {
          msg: "jsRunnerData";
          output: string;
          taskId: string;
      };

type MessageFromFrame =
    | {
          msg: "data";
          data: JSFrameData;
      }
    | {
          msg: "datasave";
          data: JSFrameData;
      }
    | {
          msg: "update";
          data: JSFrameData;
      }
    | {
          msg: "frameInited" | "frameClosed";
          fullscreen: boolean;
      }
    | {msg: "Inited"}
    | {msg: "runJsRunner"; taskId: string};

/**
 * Returns the given data (un)wrapped so that there is exactly one layer of "c".
 *
 * This is a temporary hack because the database contains some incorrect data.
 *
 * Examples:
 *
 * {c: {c: 1, d: 2}} -> {c: 1, d: 2}
 * {c: 1} -> {c: 1}
 * 1 -> {c: 1}
 * {c: {c: 1}, d: 2} -> {c: 1}  (NOTE: discards the outer d)
 *
 * @param data The data to transform.
 */
function unwrapAllC(data: unknown): {c: unknown} {
    while (CProp.is(data) && CProp.is(data.c)) {
        data = data.c;
    }
    return CProp.is(data) ? data : {c: data};
}

export interface Iframesettings {
    allow: string | null;
    sandbox: string;
    src: SafeResourceUrl;
    width: number | null;
    height: number | null;
}

@Component({
    selector: "jsframe-runner",
    template: `
        <div [ngClass]="{'warnFrame': isUnSaved(), 'csRunDiv': borders}" [class.cs-has-header]="header" class="math que jsframe no-popup-menu">
            <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <h4 *ngIf="header" [innerHtml]="header | purify"></h4>
            <p *ngIf="stem" class="stem" [innerHtml]="stem | purify"></p>
            <p *ngIf="!isOpen" class="stem" [innerHtml]="beforeOpen"></p>
            <div *ngIf="isOpen && iframesettings" id="output" class="jsFrameContainer jsframeOutput">
                <iframe #frame
                        class='showJsframe jsframeFrame'
                        style='margin-left: auto;
                               margin-right: auto;
                               display: block; border: none;'
                        [style.width.px]="iframesettings.width"
                        [style.height.px]="iframesettings.height"
                        [src]="iframesettings.src"
                        [sandbox]="iframesettings.sandbox"
                        [attr.allow]="iframesettings.allow"
                        (load)="iframeloaded($event)"
                        [ngClass]="{fullScreen: fullScreen}"
                >
                </iframe>
            </div>
            <p class="csRunMenu">
                <button class="timButton btn-sm" *ngIf="!isOpen" (click)="runShowTask()" [innerHtml]="showButton() | purify"></button>
                <button class="timButton btn-sm" *ngIf="isOpen && !norun" [disabled]="isRunning" title="(Ctrl-S)"
                        (click)="getData('getDataSave')"
                        [innerHtml]="button | purify"></button>
                <button class="timButton btn-sm" *ngIf="saveButton"
                        (click)="getData('getDataSave')"
                        [disabled]="disableUnchanged && !isUnSaved()" [innerHtml]="saveButton | purify"></button>
                <ng-container>
                &nbsp;
                <a href="" *ngIf="undoButton && isUnSaved()" [title]="undoTitle" (click)="tryResetChanges($event)">{{undoButton}}</a>
                </ng-container>
                <ng-container *ngIf="resetText">
                &nbsp;
                <a href="#"
                   (click)="resetDrawing(); $event.preventDefault()">{{resetText}}</a>
                </ng-container>
                &nbsp;
                <span class="jsframe message"
                      *ngIf="message"
                      [innerHtml]="message | purify"></span>
                <span class="jsframe message"
                      *ngIf="console"
                      [innerHtml]="console | purify"></span>
            </p>
                    <div *ngIf="connectionErrorMessage" class="error" style="font-size: 12px" [innerHtml]="connectionErrorMessage"></div>
            <span class="csRunError"
                  *ngIf="error"
                  [innerHtml]="error"></span>

            <p class="plgfooter" *ngIf="footer" [innerHtml]="footer | purify"></p>
        </div>
    `,
})
export class JsframeComponent
    extends AngularPluginBase<
        t.TypeOf<typeof JsframeMarkup>,
        t.TypeOf<typeof JsframeAll>,
        typeof JsframeAll
    >
    implements ITimComponent, IUserChanged, ICtrlWithMenuFunctionEntry
{
    iframesettings?: Iframesettings;
    private ab?: AnswerBrowserComponent;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        public cdr: ChangeDetectorRef,
        private zone: NgZone
    ) {
        super(el, http, domSanitizer);
    }

    get english() {
        return this.markup.lang === "en";
    }

    get beforeOpen() {
        return this.markup.beforeOpen;
    }

    get borders() {
        return this.markup.borders;
    }

    get norun() {
        return this.markup.norun;
    }

    get resetText() {
        return this.markup.resetText;
    }

    get saveButton() {
        return this.markup.saveButton;
    }

    toggleDrawioEditors() {
        this.send({msg: "toggleEditorOptions"});
    }

    getMenuEntry(): IMenuFunctionEntry {
        return {
            func: () => this.toggleDrawioEditors(),
            desc: "Toggle diagram editor",
            show: !this.isTask(),
        };
    }

    buttonText() {
        const txt = super.buttonText();
        if (txt) {
            return txt;
        }
        return this.english ? "Save" : "Tallenna";
    }

    showButton() {
        const txt = this.markup.showButton;
        if (txt) {
            return txt;
        }
        return this.english ? "Show task" : "Näytä tehtävä";
    }

    public viewctrl!: ViewCtrl;
    error: string = "";
    console: string = "";
    message: string = "";
    isRunning: boolean = false;
    isOpen: boolean = false;
    button: string = "";
    edited: boolean = false;
    connectionErrorMessage?: string;
    private prevdata?: JSFrameData;
    private currentData?: JSFrameData;
    private initFrameData?: JSFrameData;
    fullScreen: boolean = false;

    private initData: string = "";
    private userName?: string;

    private channel?: MessageChannel;
    @ViewChild("frame") private frame?: ElementRef<HTMLIFrameElement>;
    private iframeload!: TimDefer<void>;

    b64EncodeUnicode(str: string, codeUTF: boolean) {
        // first we use encodeURIComponent to get percent-encoded UTF-8,
        // then we convert the percent encodings into raw bytes which
        // can be fed into btoa.
        if (!codeUTF) {
            return btoa(str);
        }
        const s = encodeURIComponent(str).replace(
            /%([0-9A-F]{2})/g,
            function toSolidBytes(match, p1) {
                return String.fromCharCode(parseInt("0x" + p1, 16));
            }
        );
        return btoa(s);
    }

    ngOnInit() {
        super.ngOnInit();
        this.viewctrl = vctrlInstance!;
        this.button = this.buttonText();
        const aa = this.attrsall;
        this.userName = aa.user_id;

        this.message = this.markup.message ?? "";

        let jsobject = "window.";
        if (this.markup.srchtml?.includes("TIMJS")) {
            jsobject = "TIMJS.";
        }

        if (this.markup.open) {
            this.isOpen = true;
        }
        const data = this.getDataFromMarkup();
        this.prevdata = data;
        this.currentData = data;
        if (data) {
            this.initData =
                "    " +
                jsobject +
                "initData = " +
                JSON.stringify(data) +
                ";\n";
        }
        if (aa.markup.fielddata) {
            this.initData +=
                "    " +
                jsobject +
                "fieldData = " +
                JSON.stringify(aa.markup.fielddata) +
                ";\n";
        }
        // if ( data ) { this.setData(data); }

        this.updateIframeSettings();

        if (!this.isTask()) {
            this.requiresTaskId = false;
        }
        if (this.isPreview()) {
            return;
        }

        this.viewctrl.addTimComponent(this);
        const tid = this.getTaskId();
        if (tid) {
            if (!this.markup.forceBrowser) {
                this.viewctrl.addUserChangeListener(tid.docTask(), this);
            }
            const taskId = tid.docTask();
            (async () => {
                const ab = await this.viewctrl.getAnswerBrowserAsync(taskId);
                this.ab = ab;
                if (ab) {
                    ab.setAnswerLoader((a) => this.changeAnswer(a));
                }
            })();
        }
        if (this.isDrawio()) {
            (async () => {
                await this.viewctrl.documentUpdate;
                this.viewctrl.addParMenuEntry(this, this.getPar()!);
            })();
        }
        this.initFrameData = unwrapAllC(this.markup.data);
    }

    ngOnDestroy() {
        if (!this.isPreview()) {
            this.viewctrl.removeTimComponent(this);
        }
    }

    isDrawio(): boolean {
        return this.markup.type?.toLowerCase() == "drawio";
    }

    private getDataFromMarkup() {
        return unwrapAllC(this.markup.c ?? this.markup.data);
    }

    async userChanged(user: IUser) {
        if (user.name == this.userName) {
            return;
        }
        if (!this.attrsall.markup.fields) {
            return;
        }
        this.userName = user.name;

        const tid = this.getTaskId();
        if (!tid) {
            return;
        }
        const res = await to(
            $http.get<unknown>(
                `/jsframe/userChange/${tid.docTask().toString()}/${user.id}`
            )
        );
        this.initData = "";
        let data: {c: unknown; fielddata?: unknown} = this.getDataFromMarkup();
        if (res.result.data) {
            // there os no more fielddata-attribute
            if (!data) {
                data = {c: undefined};
            }
            data.fielddata = res.result.data;
        }
        if (data) {
            this.setData(data);
        }
    }

    runShowTask() {
        this.isOpen = true;
    }

    changeAnswer(a: IAnswer) {
        const parse = JSON.parse(a.content) as unknown;
        const unwrap = unwrapAllC(parse);
        this.prevdata = unwrap;
        this.setData(unwrap);
    }

    private getFrame() {
        return this.frame!.nativeElement as CustomFrame<JSFrameWindow>;
    }

    updateIframeSettings() {
        if (this.attrsall.preview) {
            // return "";
        } // TODO: replace when preview delay and preview from markup ready
        const w = this.markup.width ?? 800; // for some reason if w/h = 2, does not give hints on Chart.js
        const h = this.markup.height ?? (208 * w) / 400; //  experimental result for Chart.js
        let src;
        if (this.markup.useurl) {
            const selectedUser = this.viewctrl.selectedUser;
            src = this.pluginMeta.getIframeHtmlUrl(
                selectedUser,
                this.ab?.selectedAnswer
            );
        } else {
            let html = this.markup.srchtml ?? "";
            html = html.replace("</body>", communicationJS + "\n</body>");
            if (!this.isDrawio()) {
                html = html.replace("// INITDATA", this.initData);
            }
            // const datasrc = btoa(html);
            const type = this.attrsall.markup.type;
            const datasrc = this.b64EncodeUnicode(
                html,
                type == "drawio" || type == "jsframe"
            );
            src = "data:text/html;base64," + datasrc;
        }

        const iframeopts =
            this.markup.iframeopts ??
            "sandbox='allow-scripts allow-same-origin allow-popups'";

        const opts = parseIframeopts(iframeopts, src);
        const sandbox = opts.sandbox;
        const allow = opts.allow;
        const source = this.domSanitizer.bypassSecurityTrustResourceUrl(src);
        this.iframesettings = {
            allow,
            sandbox,
            src: source,
            width: w,
            height: h,
        };
        this.iframeload = new TimDefer<void>();
    }

    isTask(): boolean {
        return this.attrsall.markup.task;
    }

    private showPreviewError() {
        this.error = "Cannot run plugin while previewing.";
        this.c();
    }

    async runSend(data: unknown) {
        this.connectionErrorMessage = undefined;
        const saveResponse = {
            saved: false,
            message: undefined,
        };
        if (this.pluginMeta.isPreview()) {
            this.showPreviewError();
            return saveResponse;
        }
        this.error = "";
        this.isRunning = true;
        let url = "";
        let params;
        if (this.isTask()) {
            if (!this.getTaskId()) {
                this.error = "Task-mode on but TaskId is missing!";
                this.c();
                return saveResponse;
            }
            url = this.pluginMeta.getAnswerUrl();
            params = {
                input: {
                    ...unwrapAllC(data), // Unwrap just in case there is a double "c".
                    type: "jsframe",
                },
            };
        } else {
            url = "/jsframe/drawIOData";
            params = {
                data: unwrapAllC(data).c,
                par_id: this.getPar()!.originalPar.id,
                doc_id: this.viewctrl.docId,
            };
        }

        this.console = "";

        const r = await to(
            $http<{
                web: {error?: string; console?: string};
            }>({method: "PUT", url: url, data: params, timeout: defaultTimeout})
        );
        this.isRunning = false;

        if (!r.ok) {
            this.error = r.result.data?.error;
            this.connectionErrorMessage =
                this.error ??
                this.attrsall.markup.connectionErrorMessage ??
                defaultErrorMessage;
            this.c();
            return saveResponse;
        }
        if (this.isTask()) {
            if (!r.result.data.web) {
                this.error = "No web reply from csPlugin!";
                this.c();
                return saveResponse;
            }
            if (r.result.data.web.error) {
                this.error = r.result.data.web.error;
                this.c();
                return saveResponse;
            }
        }
        this.edited = false;
        this.updateListeners(
            this.isUnSaved() ? ChangeType.Modified : ChangeType.Saved
        );
        this.prevdata = unwrapAllC(data);
        if (this.isTask() && r.result.data.web.console) {
            this.console = r.result.data.web.console;
        }
        saveResponse.saved = true;
        this.c();
        return saveResponse;
    }

    getData(msg: MessageGet = "getData") {
        if (this.markup.useurl) {
            const f = this.getFrame();
            if (!f.contentWindow.getData) {
                return;
            }
            return this.getDataReady(f.contentWindow.getData());
        }
        this.send({msg: msg});
    }

    /**
     * Resets drawio task to default state given in markup
     */
    resetDrawing() {
        if (!this.initFrameData) {
            return;
        }
        this.setData(this.initFrameData, false, true);
        // TODO: compare datas, handle "empty drawing" case. For now play it safe and assume init drawing differs from current state
        this.edited = true;
        this.currentData = this.initFrameData;
        this.send({msg: "close"});
        this.console = "";
        this.updateListeners(
            this.isUnSaved() ? ChangeType.Modified : ChangeType.Saved
        );
        this.c();
    }

    resetChanges() {
        if (!this.prevdata) {
            return;
        }
        this.setData(this.prevdata, false, true);
        this.currentData = this.prevdata;
        this.send({msg: "close"});
        this.edited = false;
        this.console = "";
        this.updateListeners(
            this.isUnSaved() ? ChangeType.Modified : ChangeType.Saved
        );
        this.c();
    }

    getDataReady<T extends JSFrameData>(data: T, dosave = false) {
        if (data.message) {
            this.message = data.message;
        }
        if (dosave) {
            this.runSend(data);
        }
        return data;
    }

    /**
     * Force change detection.
     */
    c() {
        this.cdr.detectChanges();
    }

    setData<T extends JSFrameData>(data: T, save = false, keepHeight = false) {
        if (this.iframesettings && !keepHeight) {
            // this.iframesettings.height = 900;
        }
        if (save) {
            this.runSend(data);
        }
        if (this.markup.useurl) {
            const f = this.getFrame();
            if (!f.contentWindow.setData) {
                return;
            }
            f.contentWindow.setData(data);
        } else {
            this.send({msg: "setData", data: data});
        }
    }

    getContent() {
        return JSON.stringify(this.getData());
    }

    async save() {
        return this.runSend(this.currentData);
    }

    isUnSaved() {
        return this.edited; // TODO: compare datas
    }

    async send(obj: MessageToFrame) {
        const c = await this.addListener();
        c.port1.postMessage(obj);
    }

    async addListener() {
        if (this.channel) {
            return this.channel;
        }

        // Before posting the init message, we need to be sure the iframe has been loaded.
        await this.iframeload.promise;

        this.channel = new MessageChannel();
        this.channel.port1.onmessage = (event: MessageEvent) => {
            const d = event.data as MessageFromFrame;
            if (d.msg === "data") {
                this.getDataReady(d.data);
            }
            if (d.msg === "update") {
                this.console = "";
                this.edited = true;
                this.currentData = unwrapAllC(this.getDataReady(d.data));
                this.updateListeners(
                    this.isUnSaved() ? ChangeType.Modified : ChangeType.Saved
                );
                this.c();
            }
            if (d.msg === "datasave") {
                this.getDataReady(d.data, true);
            }
            if (d.msg === "frameInited" && d.fullscreen) {
                if (
                    this.markup.apifullscreen &&
                    fullscreenSupported(this.getFrame())
                ) {
                    toggleFullScreen(this.getFrame());
                } else {
                    this.fullScreen = true;
                    document.body.classList.add("no-overflow"); // hide document scrollbar
                    document.body.classList.add("fullscreen-frame"); // add info about current state for other styles
                    this.c();
                }
            }
            if (d.msg === "frameClosed" && d.fullscreen) {
                if (
                    this.markup.apifullscreen &&
                    fullscreenSupported(this.getFrame())
                ) {
                    exitFullScreen();
                } else {
                    this.fullScreen = false;
                    document.body.classList.remove("no-overflow");
                    document.body.classList.remove("fullscreen-frame");
                    this.c();
                }
            }
            if (d.msg === "Inited" && this.isDrawio()) {
                const data = this.getDataFromMarkup();
                this.setData(data);
            }
            if (d.msg === "runJsRunner") {
                this.zone.run(async () => {
                    const runner = this.viewctrl.getJsRunner(d.taskId);
                    if (runner) {
                        await runner.runScript();
                        void this.send({
                            msg: "jsRunnerData",
                            output: runner.output,
                            taskId: d.taskId,
                        });
                    }
                });
            }
        };
        const f = this.getFrame();
        f.contentWindow.postMessage({msg: "init"}, "*", [this.channel.port2]);
        return this.channel;
    }

    getDefaultMarkup() {
        return {};
    }

    getAttributeType() {
        return JsframeAll;
    }

    iframeloaded(e: Event) {
        const fr = e.target as HTMLIFrameElement;
        // onIframeLoad gets called twice on chrome, on the first time src is empty
        if (fr.src == "") {
            return;
        }

        this.iframeload.resolve();
        if (this.markup.initListener) {
            this.addListener();
        }
    }
}

@NgModule({
    declarations: [JsframeComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        PurifyModule,
    ],
})
export class JsframeModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("jsframe-runner", JsframeModule, JsframeComponent);
