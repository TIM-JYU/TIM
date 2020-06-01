﻿import * as t from "io-ts";
import {IAnswer} from "tim/answer/IAnswer";
import {ChangeType, ITimComponent, IUserChanged, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, withDefault} from "tim/plugin/attributes";
import {IUser} from "tim/user/IUser";
import {$http} from "tim/util/ngimport";
import {defaultErrorMessage, defaultTimeout, parseIframeopts, to} from "tim/util/utils";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {
    AfterViewInit,
    ApplicationRef,
    ChangeDetectorRef,
    Component,
    DoBootstrap,
    ElementRef,
    NgModule,
    OnDestroy,
    StaticProvider,
    ViewChild,
} from "@angular/core";
import {BrowserModule, DomSanitizer, SafeResourceUrl} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {AnswerBrowserController} from "tim/answer/answerbrowser3";
import {getParId} from "tim/document/parhelpers";
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
    }),
    GenericPluginMarkup,
    t.type({
        open: withDefault(t.boolean, true),
        borders: withDefault(t.boolean, false),
        norun: withDefault(t.boolean, true),
        lang: withDefault(t.string, "fi"),
        task: withDefault(t.boolean, true), // TODO: check if other jsframes are tasks or not
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

const FieldDataObj = t.type({
    fielddata: t.unknown,
});

const CProp = t.type({
    c: t.unknown,
});

type MessageType = "init" | MessageGet | "setData";
type MessageGet = "getData" | "getDataSave";

type MessageToFrame =
    | {
    msg: "setData";
    data: JSFrameData;
} | { msg: "init" }
    | { msg: "getData" }
    | { msg: "getDataSave" }
    | { msg: "close" };

type MessageFromFrame =
    | {
    msg: "data";
    data: JSFrameData;
} | {
    msg: "datasave";
    data: JSFrameData;
} | {
    msg: "update";
    data: JSFrameData;
};


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
function unwrapAllC<A>(data: unknown): { c: unknown } {
    while (CProp.is(data) && CProp.is(data.c)) {
        data = data.c;
    }
    return CProp.is(data) ? data : {c: data};
}

@Component({
    selector: "jsframe-runner",
    template: `
        <div [ngClass]="{'warnFrame': isUnSaved(), 'csRunDiv': borders}" class="math que jsframe no-popup-menu">
            <h4 *ngIf="header" [innerHtml]="header"></h4>
            <p *ngIf="stem" class="stem" [innerHtml]="stem"></p>
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
                >
                </iframe>
            </div>
            <p class="csRunMenu">
                <button class="timButton btn-sm" *ngIf="!isOpen" (click)="runShowTask()" [innerHtml]="showButton()"></button>
                <button class="timButton btn-sm" *ngIf="isOpen && !norun" [disabled]="isRunning" title="(Ctrl-S)"
                        (click)="getData('getDataSave')"
                        [innerHtml]="button"></button>
                <button class="timButton btn-sm" *ngIf="saveButton"
                        (click)="getData('getDataSave')"
                        [disabled]="disableUnchanged && !isUnSaved()" [innerHtml]="saveButton"></button>
                &nbsp;
                <a href="" *ngIf="undoButton && isUnSaved()" [title]="undoTitle" (click)="tryResetChanges($event)">{{undoButton}}</a>

                &nbsp;
                <span class="jsframe message"
                      *ngIf="message"
                      [innerHtml]="message"></span>
                <span class="jsframe message"
                      *ngIf="console"
                      [innerHtml]="console"></span>
            </p>
                    <div *ngIf="connectionErrorMessage" class="error" style="font-size: 12px" [innerHtml]="connectionErrorMessage"></div>
            <span class="csRunError"
                  *ngIf="error"
                  [innerHtml]="error"></span>

            <p class="plgfooter" *ngIf="footer" [innerHtml]="footer"></p>
        </div>
    `,
})
export class JsframeComponent extends AngularPluginBase<t.TypeOf<typeof JsframeMarkup>,
    t.TypeOf<typeof JsframeAll>,
    typeof JsframeAll> implements ITimComponent, IUserChanged, AfterViewInit, OnDestroy {
    iframesettings?: { sandbox: string, src: SafeResourceUrl, width: number, height: number };
    private ab?: AnswerBrowserController;

    constructor(el: ElementRef<HTMLElement>, http: HttpClient, domSanitizer: DomSanitizer, public cdr: ChangeDetectorRef) {
        super(el, http, domSanitizer);
    }

    get disableUnchanged() {
        return this.markup.disableUnchanged;
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

    get saveButton() {
        return this.markup.saveButton;
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
    private span: string = "";
    error: string = "";
    console: string = "";
    message: string = "";
    private userCode: string = "";
    private jsframepeek: boolean = false;
    isRunning: boolean = false;
    isOpen: boolean = false;
    button: string = "";
    edited: boolean = false;
    connectionErrorMessage?: string;
    private prevdata?: JSFrameData;

    private timer: NodeJS.Timer | undefined;

    private taskUrl: string = "";
    private htmlUrl: string = "";
    private initData: string = "";
    private userName?: string;


    private saveResponse: { saved: boolean, message: (string | undefined) } = {saved: false, message: undefined};
    private channel?: MessageChannel;
    @ViewChild("frame") private frame?: ElementRef<HTMLIFrameElement>;

    b64EncodeUnicode(str: string, codeUTF: boolean) {
        // first we use encodeURIComponent to get percent-encoded UTF-8,
        // then we convert the percent encodings into raw bytes which
        // can be fed into btoa.
        if (!codeUTF) { return btoa(str); }
        const s = encodeURIComponent(str).replace(/%([0-9A-F]{2})/g,
            function toSolidBytes(match, p1) {
                return String.fromCharCode(parseInt("0x" + p1, 16));
        });
        return btoa(s);
    }

    ngOnInit() {
        super.ngOnInit();
        this.viewctrl = vctrlInstance!;
        this.button = this.buttonText();
        const aa = this.attrsall;
        this.userName = aa.user_id;
        this.userCode = aa.usercode ?? "";

        this.message = this.markup.message ?? "";

        let jsobject = "window.";
        if (this.markup.srchtml && this.markup.srchtml.includes("TIMJS")) {
            jsobject = "TIMJS.";
        }

        if (this.markup.open) {
            this.isOpen = true;
        }
        const data = this.getDataFromMarkup();
        this.prevdata = data;
        if (data) {
            this.initData = "    " + jsobject + "initData = " + JSON.stringify(data) + ";\n";
        }
        if (aa.markup.fielddata) {
            this.initData += "    " + jsobject + "fieldData = " + JSON.stringify(aa.markup.fielddata) + ";\n";
        }
        // if ( data ) { this.setData(data); }
        this.viewctrl.addTimComponent(this);
        const tid = this.getTaskId();
        this.updateIframeSettings();
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
    }

    private getDataFromMarkup() {
        return unwrapAllC(this.markup.c ?? this.markup.data);
    }

    ngAfterViewInit() {
        if (this.markup.initListener && !this.attrsall.preview) {
            this.getFrame().addEventListener("load", () => {
                this.addListener();
            });
        }
    }

    ngOnDestroy() {
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
        const res = await to($http.get<unknown>(`/jsframe/userChange/${tid.docTask()}/${user.id}`));
        this.initData = "";
        let data: { c: unknown, fielddata?: unknown } = this.getDataFromMarkup();
        if (res.result.data) { // there os no more fielddata-attribute
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
        this.setData(unwrapAllC(parse));
    }

    private getFrame() {
        return this.frame!.nativeElement as CustomFrame<JSFrameWindow>;
    }

    updateIframeSettings() {
        if (this.attrsall.preview) {
            // return "";
        } // TODO: replace when preview delay and preview from markup ready
        const w = this.markup.width ?? 800; // for some reason if w/h = 2, does not give hints on Chart.js
        const h = this.markup.height ?? (208 * w / 400);  //  experimental result for Chart.js
        let src;
        if (this.markup.useurl) {
            const selectedUser = this.viewctrl.selectedUser;
            const userId = selectedUser.id;
            let anr = 0;
            if (this.ab) {
                anr = this.ab.findSelectedAnswerIndex();
                if (anr < 0) {
                    anr = 0;
                }
            }
            let url = this.getHtmlUrl() + "/" + userId + "/" + anr;
            url = url.replace("//", "/");
            src = url;
        } else {
            let html = this.markup.srchtml ?? "";
            html = html.replace("</body>", communicationJS + "\n</body>");
            html = html.replace("// INITDATA", this.initData);
            // const datasrc = btoa(html);
            const type = this.attrsall.markup.type;
            const datasrc = this.b64EncodeUnicode(html, type == "drawio");
            src = "data:text/html;base64," + datasrc;
        }

        const iframeopts = this.markup.iframeopts ?? "sandbox='allow-scripts allow-same-origin'";

        const sandbox = parseIframeopts(iframeopts).sandbox;
        const source = this.domSanitizer.bypassSecurityTrustResourceUrl(src);
        this.iframesettings = {sandbox, src: source, width: w, height: h};
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

    async runSend(data: unknown) {
        this.connectionErrorMessage = undefined;
        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            this.saveResponse.saved = false;
            this.c();
            return this.saveResponse;
        }
        this.jsframepeek = false;
        this.error = "";
        this.isRunning = true;
        let url = "";
        let params;
        if (this.attrsall.markup.task) {
            if (!this.getTaskId()) {
                this.error = "Task-mode on but TaskId is missing!";
                this.saveResponse.saved = false;
                this.c();
                return this.saveResponse;
            }
            url = this.getTaskUrl();
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
                par_id: getParId(this.getPar()),
                doc_id: this.viewctrl.docId,
            };
        }

        this.console = "";

        const r = await to($http<{
            web: { error?: string, console?: string },
        }>({method: "PUT", url: url, data: params, timeout: defaultTimeout},
        ));
        this.isRunning = false;

        if (!r.ok) {
            this.error = r.result.data?.error;
            this.connectionErrorMessage = this.error ?? this.attrsall.markup.connectionErrorMessage ?? defaultErrorMessage;
            this.saveResponse.saved = false;
            this.c();
            return this.saveResponse;
        }
        if (this.attrsall.markup.task) {
            if (!r.result.data.web) {
                this.error = "No web reply from csPlugin!";
                this.saveResponse.saved = false;
                this.c();
                return this.saveResponse;
            }
            if (r.result.data.web.error) {
                this.error = r.result.data.web.error;
                this.saveResponse.saved = false;
                this.c();
                return this.saveResponse;
            }
        }
        this.edited = false;
        this.updateListeners();
        this.prevdata = unwrapAllC(data);
        if (this.attrsall.markup.task && r.result.data.web.console) {
            this.console = r.result.data.web.console;
            this.saveResponse.saved = true;
            this.c();
            return this.saveResponse;
        }
        this.saveResponse.saved = true;
        this.c();
        return this.saveResponse;
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

    tryResetChanges(e?: Event): void {
        if (e) {
            e.preventDefault();
        }
        if (this.undoConfirmation && !window.confirm(this.undoConfirmation)) {
            return;
        }
        this.resetChanges();
    }

    resetChanges() {
        if (!this.prevdata) {
            return;
        }
        this.setData(this.prevdata, false, true);
        this.send({msg: "close"});
        this.edited = false;
        this.updateListeners();
        this.c();
    }

    updateListeners() {
        if (!this.viewctrl) {
            return;
        }
        const taskId = this.pluginMeta.getTaskId();
        if (!taskId) {
            return;
        }
        this.viewctrl.informChangeListeners(taskId, (this.edited ? ChangeType.Modified : ChangeType.Saved),
            (this.attrsall.markup.tag ? this.attrsall.markup.tag : undefined));
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
            this.iframesettings.height = 900;
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
        const data = this.getData("getDataSave");
        return this.runSend(data);
    }

    isUnSaved() {
        return this.edited;  // TODO: compare datas
    }

    send(obj: MessageToFrame) {
        const c = this.addListener();
        c.port1.postMessage(obj);
    }

    addListener() {
        if (this.channel) {
            return this.channel;
        }
        this.channel = new MessageChannel();
        this.channel.port1.onmessage = (event: MessageEvent) => {
            // console.log(event);
            const d = event.data as MessageFromFrame;
            const msg = d.msg;
            if (msg === "data") {
                this.getDataReady(d.data);
            }
            if (msg === "update") {
                this.console = "";
                this.edited = true;
                this.updateListeners();
                this.c();
            }
            if (msg === "datasave") {
                this.getDataReady(d.data, true);
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

    private stopTimer(): boolean {
        if (!this.timer) {
            return false;
        }
        clearTimeout(this.timer);
        this.timer = undefined;
        return true;
    }
}

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        JsframeComponent,
    ],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
    ],
})
export class JsframeModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
    }
}

const bootstrapFn = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(JsframeModule);
};

const angularJsModule = createDowngradedModule(bootstrapFn);
doDowngrade(angularJsModule, "jsframeRunner", JsframeComponent);
export const moduleDefs = [angularJsModule];
