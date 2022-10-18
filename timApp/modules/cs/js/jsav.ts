/* eslint-disable @typescript-eslint/no-explicit-any,@typescript-eslint/no-unsafe-member-access */
/**
 * Defines the client-side implementation of the JSAV animations plugin.
 * Originally programmed by Mikko Merikivi with help from the GeoGebra plugin by Vesa Lappalainen
 */
import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, ElementRef, NgModule, ViewChild} from "@angular/core";
import type {ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, withDefault} from "tim/plugin/attributes";
import {timeout} from "tim/util/utils";
import {HttpClientModule} from "@angular/common/http";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {PurifyModule} from "tim/util/purify.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {BrowserModule} from "@angular/platform-browser";
import type {Iframesettings} from "./jsframe";

/**
 * This is the "state" of the plugin, used through this.attrs.
 * For example, the user can specify attributes here when writing markup for the plugin
 */
const JsavMarkup = t.intersection([
    t.partial({
        beforeOpen: t.string,
        open: t.boolean,
        width: t.number,
        height: t.number,
        message: t.string,
        buttonBottom: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        lang: withDefault(t.string, "fi"),
        // all withDefaults should come here; NOT in t.partial
    }),
]);

/**
 * This is all the JSON content saved
 */
const JsavAll = t.intersection([
    t.partial({
        usercode: t.string,
    }),
    t.type({
        info: Info,
        markup: JsavMarkup,
        preview: t.boolean,
    }),
]);

/**
 * Methods and properties in JSAV library's Exercise class that we want to be able to use remotely
 */
interface JSAVExercise {
    showModelanswer(): void;
}

/**
 * Methods and properties inside the sandboxed iframe that we want to be able to use remotely
 */
interface JSFrameWindow extends Window {
    getData?(): {message?: string};
    exercise: JSAVExercise;
}

/**
 * An interface for sandboxed iframes
 */
interface CustomFrame<T extends Window> extends HTMLIFrameElement {
    contentWindow: T;
}

/**
 * The high-level operational logic behind this plugin
 */
@Component({
    selector: "tim-cs-jsav-runner",
    template: `
<tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
<div class="csRunDiv no-popup-menu">
    <h4 *ngIf="header" [innerHtml]="header | purify"></h4>
    <p *ngIf="stem" class="stem" [innerHtml]="stem | purify"></p>
    <p *ngIf="!isOpen" class="stem" [innerHtml]="beforeOpen | purify"></p>

    <!-- The JSAV plugin itself in a sandbox -->
    <div *ngIf="isOpen && iframesettings" id="output" class="jsFrameContainer jsavOutput">
        <iframe #frame
                style="border: none;"
                id="sxFrame-jsav-div1"
                class="jsavFrame"
                [style.width.px]="iframesettings.width"
                [style.height.px]="iframesettings.height"
                [sandbox]="iframesettings.sandbox"
                [attr.allow]="iframesettings.allow"
                [src]="iframesettings.src">
        </iframe>
    </div>

    <p class="csRunMenu">
        <!-- The save button -->
        <button *ngIf="isOpen"
                [disabled]="isRunning"
                title="(Ctrl-S)"
                class="timButton btn-sm"
                (click)="getData(false)"
                [innerHtml]="button | purify"></button>
        &nbsp;
        <!-- The model answer button -->
        <button *ngIf="isOpen && userCode"
                [disabled]="isRunning"
                title="(Ctrl-M)"
                class="timButton btn-sm"
                (click)="modelAnswer()"
                [innerHtml]="modelAnswerButton | purify"></button>
        &nbsp;
        <!-- User-specified messages -->
        <span class="jsav message"
              *ngIf="message"
              [innerHtml]="message | purify"></span>
        &nbsp;
        <!-- Plugin's messages -->
        <span class="jsav message"
              *ngIf="console"
              [innerHtml]="console | purify"></span>
    </p>
</div>
`,
})
export class JsavPluginComponent extends AngularPluginBase<
    t.TypeOf<typeof JsavMarkup>,
    t.TypeOf<typeof JsavAll>,
    typeof JsavAll
> {
    get english() {
        return this.markup.lang === "en";
    }

    /**
     * What the save button's text should be
     */
    buttonText() {
        const txt = super.buttonText();
        if (txt) {
            return txt;
        }
        return this.english ? "Save" : "Tallenna";
    }

    /**
     * What the model answer button's text should be
     */
    modelAnswerButtonText() {
        return this.english ? "Model Answer" : "Mallivastaus";
    }

    public viewCtrl!: ViewCtrl;
    private error: string = "";
    isRunning: boolean = false;
    isOpen: boolean = true;
    button: string = "";
    modelAnswerButton: string = "";
    console: string = "";
    message: string = "";
    userCode: string = "";
    beforeOpen: string = "";
    iframesettings?: Iframesettings;
    @ViewChild("frame") private iframe?: ElementRef<CustomFrame<JSFrameWindow>>;

    ngOnInit() {
        super.ngOnInit();
        this.viewCtrl = vctrlInstance!;
        this.button = this.buttonText();
        this.modelAnswerButton = this.modelAnswerButtonText();
        this.isOpen = this.markup.open ?? true;
        const aa = this.attrsall;
        this.userCode = aa.usercode ?? "";
        this.message = this.markup.message ?? "";
        this.beforeOpen = this.markup.beforeOpen ?? "";
        this.loadIFrame();
    }

    private loadIFrame() {
        const tid = this.pluginMeta.getTaskId()!;
        const taskId = tid.docTask();
        const ab = this.viewCtrl.getAnswerBrowser(taskId.toString());
        const selectedUser = this.viewCtrl.selectedUser;
        this.iframesettings = {
            allow: null,
            sandbox: "allow-scripts allow-same-origin allow-popups",
            width: (this.markup.width ?? 800) + 2,
            height: (this.markup.height ?? 600) + 2,
            src: this.domSanitizer.bypassSecurityTrustResourceUrl(
                this.pluginMeta.getIframeHtmlUrl(
                    selectedUser,
                    ab?.selectedAnswer
                )
            ),
        };
    }

    /**
     * Save the state in TIM's database
     * @param data The user's current score in a JSON exercise
     * @param answerChecked Whether the user has looked at the model answer
     */
    async runSend(data: any, answerChecked: boolean) {
        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            return;
        }
        this.error = "";
        this.isRunning = true;
        data.type = "jsav";
        if (answerChecked) {
            data.model = "y";
        }
        if (!data.usercode) {
            data.usercode = "y";
            this.userCode = "y";
        }

        const params = {
            input: data,
        };

        this.console = "";

        const r = await this.postAnswer<{
            web: {error?: string; console?: string};
        }>(params);
        this.isRunning = false;

        if (!r.ok) {
            this.error = r.result.error.error;
            return;
        }
        if (!r.result.web) {
            this.error = "No web reply from csPlugin!";
            return;
        }
        if (r.result.web.error) {
            this.error = r.result.web.error;
            return;
        }

        if (r.result.web.console) {
            this.console = r.result.web.console;
            return;
        }
    }

    /**
     * This asks the getData function to get the state and then saves the state in TIM's database
     * @param answerChecked Whether the user has looked at the model answer
     */
    async getData(answerChecked: boolean) {
        if (!this.iframe) {
            return;
        }
        const f = this.iframe.nativeElement;
        if (!f.contentWindow.getData) {
            return;
        }
        const s = f.contentWindow.getData();
        if (s.message) {
            this.message = s.message;
        }
        await this.runSend(s, answerChecked);
    }

    /**
     * This shows the model answer
     */
    async modelAnswer() {
        this.console = "";
        const f = this.iframe?.nativeElement;
        if (f?.contentWindow.exercise) {
            await this.getData(true);
            f.contentWindow.exercise.showModelanswer();
        } else {
            this.console = this.english
                ? "No model answer available."
                : "Mallivastausta ei ole saatavilla.";
        }
    }

    getDefaultMarkup() {
        return {};
    }

    /**
     * This outputs the code for the JSAV plugin itself in a sandbox
     */
    async outputAsHtml() {
        await timeout();
        const tid = this.pluginMeta.getTaskId()!;
        const taskId = tid.docTask();
        const ab = this.viewCtrl.getAnswerBrowser(taskId.toString());
        const selectedUser = this.viewCtrl.selectedUser;
        return this.domSanitizer.bypassSecurityTrustResourceUrl(
            this.pluginMeta.getIframeHtmlUrl(selectedUser, ab?.selectedAnswer)
        );
    }

    getAttributeType() {
        return JsavAll;
    }
}

@NgModule({
    declarations: [JsavPluginComponent],
    imports: [BrowserModule, HttpClientModule, TimUtilityModule, PurifyModule],
})
export class JsavModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("cs-jsav-runner", JsavModule, JsavPluginComponent);
