import * as t from "io-ts";
import {
    ApplicationRef,
    Component,
    DoBootstrap,
    ElementRef,
    NgModule,
    ViewChild,
} from "@angular/core";
import {IAnswer} from "tim/answer/IAnswer";
import {ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, withDefault} from "tim/plugin/attributes";
import {AnswerBrowserController} from "tim/answer/answerbrowser3";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {Iframesettings} from "./jsframe";

const GeogebraMarkup = t.intersection([
    t.partial({
        beforeOpen: t.string,
        showButton: t.string,
        message: t.string,
        width: t.number,
        height: t.number,
        tool: t.boolean,
        norun: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        open: withDefault(t.boolean, true),
        borders: withDefault(t.boolean, true),
        lang: withDefault(t.string, "fi"),
    }),
]);
const GeogebraAll = t.intersection([
    t.partial({
        usercode: t.string,
        norun: t.boolean,
    }),
    t.type({
        info: Info,
        markup: GeogebraMarkup,
        preview: t.boolean,
    }),
]);

interface GeogebraWindow extends Window {
    getData?(): {message?: string};

    setData?(state: unknown): void;
}

interface CustomFrame<T extends Window> extends HTMLIFrameElement {
    contentWindow: T;
}

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "tim-geogebra",
    template: `
        <div [class.csRunDiv]="markup.borders" class="math que geogebra no-popup-menu">
            <h4 *ngIf="header" [innerHtml]="header"></h4>
            <p *ngIf="stem" class="stem" [innerHtml]="stem"></p>
            <p *ngIf="!isOpen" class="stem" [innerHtml]="markup.beforeOpen"></p>

            <iframe
                    *ngIf="isOpen && iframesettings"
                    #frame
                    class="showGeoGebra"
                    style="border: none"
                    [src]="iframesettings.src"
                    [sandbox]="iframesettings.sandbox"
                    [style.width.px]="iframesettings.width"
                    [style.height.px]="iframesettings.height"
                    [attr.allow]="iframesettings.allow"
            >
            </iframe>
            <p class="csRunMenu">
                <button *ngIf="!isOpen"
                        class="timButton btn-sm"
                        (click)="runShowTask()"
                        [innerHtml]="showButton()"></button>
                <button *ngIf="isOpen && !markup.norun"
                        [disabled]="isRunning"
                        title="(Ctrl-S)"
                        (click)="getData()"
                        class="timButton btn-sm"
                        [innerHtml]="button"></button>
                <span class="geogebra message"
                      *ngIf="message"
                      [innerHtml]="message"></span>
                <span class="geogebra message"
                      *ngIf="console"
                      [innerHtml]="console"></span>
            </p>
            <span class="csRunError"
                  *ngIf="error"
                  [innerHtml]="error"></span>
            <p class="plgfooter" *ngIf="footer" [innerHtml]="footer"></p>
        </div>
    `,
})
export class GeogebraComponent extends AngularPluginBase<
    t.TypeOf<typeof GeogebraMarkup>,
    t.TypeOf<typeof GeogebraAll>,
    typeof GeogebraAll
> {
    private ab?: AnswerBrowserController;

    get english() {
        return this.markup.lang === "en";
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

    private viewctrl!: ViewCtrl;
    error?: string;
    console?: string;
    message?: string;
    iframesettings?: Iframesettings;
    isRunning = false;
    isOpen = false;
    button: string = "";
    @ViewChild("frame") private iframe?: ElementRef<
        CustomFrame<GeogebraWindow>
    >;

    ngOnInit() {
        super.ngOnInit();
        this.viewctrl = vctrlInstance!;
        this.button = this.buttonText();
        this.message = this.markup.message;
        this.isOpen = this.markup.open;
        const tid = this.pluginMeta.getTaskId()!;
        const taskId = tid.docTask();
        this.loadIframe();
        (async () => {
            const ab = await this.viewctrl.getAnswerBrowserAsync(taskId);
            this.ab = ab;
            if (ab) {
                ab.setAnswerLoader((a) => this.changeAnswer(a));
            }
        })();
    }

    runShowTask() {
        this.isOpen = true;
    }

    changeAnswer(a: IAnswer) {
        this.iframe?.nativeElement.contentWindow.setData?.(
            JSON.parse(a.content)
        );
    }

    loadIframe() {
        if (this.attrsall.preview) {
            return;
        } // TODO: replace when preview delay and preview from markup ready
        const selectedUser = this.viewctrl.selectedUser;
        let w = this.markup.width ?? 800;
        let h = this.markup.height ?? 450;

        if (this.markup.tool) {
            w = Math.max(w, 1200);
            h = Math.max(w, 1100);
        }

        const url = this.pluginMeta.getIframeHtmlUrl(
            selectedUser,
            this.ab?.selectedAnswer
        );
        this.iframesettings = {
            allow: null,
            sandbox: "allow-scripts",
            width: w + 2,
            height: h + 2,
            src: this.domSanitizer.bypassSecurityTrustResourceUrl(url),
        };
    }

    async runSend(data: Record<string, unknown>) {
        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            return;
        }
        this.error = undefined;
        this.console = undefined;
        this.isRunning = true;
        const params = {
            input: {...data, type: "geogebra"},
        };

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

    async getData() {
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
        await this.runSend(s);
    }

    getDefaultMarkup() {
        return {};
    }

    getAttributeType() {
        return GeogebraAll;
    }
}

@NgModule({
    declarations: [GeogebraComponent],
    imports: [BrowserModule, HttpClientModule, FormsModule, TimUtilityModule],
})
export class GeogebraModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                GeogebraModule
            )
        ),
        "timGeogebra",
        GeogebraComponent
    ),
];
