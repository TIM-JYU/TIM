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
import {
    defaultErrorMessage,
    defaultTimeout,
    parseIframeopts,
    toPromise,
} from "tim/util/utils";
import type {AngularError, Result} from "tim/util/utils";
import type {JsonValue} from "tim/util/jsonvalue";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {
    ChangeDetectorRef,
    Component,
    ElementRef,
    NgModule,
    NgZone,
    ViewChild,
    ChangeDetectionStrategy,
} from "@angular/core";
import type {SafeResourceUrl} from "@angular/platform-browser";
import {DomSanitizer} from "@angular/platform-browser";
import {
    HttpClient,
    HttpHeaders,
    provideHttpClient,
    withInterceptorsFromDi,
    withXhr,
} from "@angular/common/http";
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
        width: t.union([t.number, t.string]),
        height: t.union([t.number, t.string]),
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

interface JSFrameBase {
    message?: string;
    fielddata?: unknown;
}

interface CFrame extends JSFrameBase {
    c: unknown;
}

interface UserCodeFrame extends JSFrameBase {
    usercode: unknown;
    [key: string]: unknown;
}

type JSFrameData = CFrame | UserCodeFrame;

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
function unwrapAllC(data: unknown): JSFrameData {
    if (
        typeof data === "object" &&
        data !== null &&
        !Array.isArray(data) &&
        "usercode" in data
    ) {
        return data as JSFrameData;
    }
    while (CProp.is(data) && CProp.is(data.c)) {
        data = data.c;
    }
    return CProp.is(data) ? data : {c: data};
}

export interface Iframesettings {
    allow: string | null;
    sandbox: string;
    src: SafeResourceUrl;
    width: number | string | null;
    height: number | string | null;
}

@Component({
    selector: "jsframe-runner",
    template: `
        <div [ngClass]="{'warnFrame': isUnSaved(), 'csRunDiv': borders}" [class.cs-has-header]="header" class="math que jsframe no-popup-menu">
          @if (markupError) {
            <tim-markup-error [data]="markupError"></tim-markup-error>
          }
          @if (header) {
            <h4 [innerHtml]="header | purify"></h4>
          }
          @if (stem) {
            <p class="stem" [innerHtml]="stem | purify"></p>
          }
          @if (!isOpen) {
            <p class="stem" [innerHtml]="beforeOpen"></p>
          }
          @if (isOpen && iframesettings) {
            <div id="output" class="jsFrameContainer jsframeOutput">
              <iframe #frame
                class='showJsframe jsframeFrame'
                        style='margin-left: auto;
                               margin-right: auto;
                               display: block; border: none;'
                [style.width]="this.getDesiredSize(iframesettings.width)"
                [style.height]="this.getDesiredSize(iframesettings.height)"
                [timIframeSettings]="iframesettings"
                (load)="iframeloaded($event)"
                [ngClass]="{fullScreen: fullScreen}"
                >
              </iframe>
            </div>
          }
          <p class="csRunMenu">
            @if (!isOpen) {
              <button class="timButton btn-sm" (click)="runShowTask()" [innerHtml]="showButton() | purify"></button>
            }
            @if (isOpen && !norun) {
              <button class="timButton btn-sm" [disabled]="isRunning" title="(Ctrl-S)"
                (click)="getData('getDataSave')"
              [innerHtml]="button | purify"></button>
            }
            @if (saveButton) {
              <button class="timButton btn-sm"
                (click)="getData('getDataSave')"
              [disabled]="disableUnchanged && !isUnSaved()" [innerHtml]="saveButton | purify"></button>
            }
            <ng-container>
              &nbsp;
              @if (undoButton && isUnSaved()) {
                <a href="" [title]="undoTitle" (click)="tryResetChanges($event)">{{undoButton}}</a>
              }
            </ng-container>
            @if (resetText) {
              &nbsp;
              <a href="#"
              (click)="resetDrawing(); $event.preventDefault()">{{resetText}}</a>
            }
            <ng-container> &nbsp; </ng-container>
            @if (message) {
              <span class="jsframe message"
              [innerHtml]="message | purify"></span>
            }
            @if (console) {
              <span class="jsframe message"
              [innerHtml]="console | purify"></span>
            }
          </p>
          @if (connectionErrorMessage) {
            <div class="error" style="font-size: 12px" [innerHtml]="connectionErrorMessage"></div>
          }
          @if (error) {
            <span class="csRunError"
            [innerHtml]="error"></span>
          }
        
          @if (footer) {
            <p class="plgfooter" [innerHtml]="footer | purify"></p>
          }
        </div>
        `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
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

    showButton() {
        return this.markup.showButton ?? $localize`Show task`;
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
        this.button = super.buttonText() ?? $localize`Save`;
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
        const res = await this.httpGet<unknown>(
            `/jsframe/userChange/${tid.docTask().toString()}/${user.id}`
        );
        this.initData = "";
        let data: JSFrameData = this.getDataFromMarkup();
        if (res.ok && res.result) {
            // there is no more fielddata-attribute
            if (!data) {
                data = {c: undefined};
            }
            data.fielddata = res.result;
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

    getDesiredSize(value: number | string | null) {
        if (value != null) {
            if (typeof value == "number") {
                return `${value}px`;
            } else {
                const valid_percent = value.match(new RegExp("^[0-9]+%"));
                if (valid_percent) {
                    return value;
                }
            }
        }
        return "";
    }

    updateIframeSettings() {
        if (this.attrsall.preview) {
            // return "";
        } // TODO: replace when preview delay and preview from markup ready
        const w = this.markup.width ?? 800; // for some reason if w/h = 2, does not give hints on Chart.js
        const h =
            // this.markup.height ?? (208 *  w) / 400; //  experimental result for Chart.js
            // FIXME: we should probably just set an independent default height.
            this.markup.height ??
            (208 * (typeof w == "number" ? w : 800)) / 400;
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
        type SaveResponse = {web: {error?: string; console?: string}};
        let request: () => Promise<Result<SaveResponse, AngularError>>;
        if (this.isTask()) {
            if (!this.getTaskId()) {
                this.error = "Task-mode on but TaskId is missing!";
                this.c();
                return saveResponse;
            }
            const params = {
                input: {
                    ...unwrapAllC(data), // Unwrap just in case there is a double "c".
                    type: "jsframe",
                },
            };
            request = () =>
                this.postAnswer<SaveResponse>(
                    params as {input: JsonValue},
                    new HttpHeaders({timeout: `${defaultTimeout}`})
                );
        } else {
            const params = {
                data: unwrapAllC(data).c,
                par_id: this.getPar()!.originalPar.id,
                doc_id: this.viewctrl.docId,
            };
            request = () =>
                toPromise(
                    this.http.put<SaveResponse>("/jsframe/drawIOData", params, {
                        headers: new HttpHeaders({
                            timeout: `${defaultTimeout}`,
                        }),
                    })
                );
        }

        this.console = "";

        const r = await request();
        this.isRunning = false;

        if (!r.ok) {
            this.error = r.result.error?.error;
            this.connectionErrorMessage =
                this.error ??
                this.attrsall.markup.connectionErrorMessage ??
                defaultErrorMessage;
            this.c();
            return saveResponse;
        }
        if (this.isTask()) {
            if (!r.result.web) {
                this.error = "No web reply from csPlugin!";
                this.c();
                return saveResponse;
            }
            if (r.result.web.error) {
                this.error = r.result.web.error;
                this.c();
                return saveResponse;
            }
        }
        this.edited = false;
        this.updateListeners(
            this.isUnSaved() ? ChangeType.Modified : ChangeType.Saved
        );
        this.prevdata = unwrapAllC(data);
        if (this.isTask() && r.result.web.console) {
            this.console = r.result.web.console;
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
    imports: [CommonModule, FormsModule, TimUtilityModule, PurifyModule],
    providers: [provideHttpClient(withXhr(), withInterceptorsFromDi())],
})
export class JsframeModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("jsframe-runner", JsframeModule, JsframeComponent);
