import moment from "moment";
import type {
    AfterViewInit,
    DoBootstrap,
    NgModuleRef,
    OnDestroy,
    OnInit,
    Type,
} from "@angular/core";
import {
    ApplicationRef,
    Component,
    ContentChild,
    createNgModule,
    ElementRef,
    Input,
    NgModule,
    NgZone,
    ViewChild,
    ViewContainerRef,
} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {ParCompiler} from "tim/editor/parCompiler";
import {PurifyModule} from "tim/util/purify.module";
import {getURLParameter, getViewName, timeout, toPromise} from "tim/util/utils";
import type {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {TimDefer} from "tim/util/timdefer";
import type {ReadonlyMoment} from "tim/util/readonlymoment";
import {timLogTime} from "tim/util/timTiming";
import {Users} from "tim/user/userService";
import type {AnswerBrowserComponent} from "tim/answer/answer-browser.component";
import {AnswerBrowserModule} from "tim/answer/answer-browser.component";
import type {IGenericPluginMarkup} from "tim/plugin/attributes";
import {TaskId, TaskIdWithDefaultDocId} from "tim/plugin/taskid";
import type {PluginJson} from "tim/plugin/angular-plugin-base.directive";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import type {IRegisteredPlugin} from "tim/plugin/pluginRegistry";
import {getPlugin} from "tim/plugin/pluginRegistry";
import {BrowserModule} from "@angular/platform-browser";

const LAZY_MARKER = "lazy";
const LAZY_MARKER_LENGTH = LAZY_MARKER.length;
const LAZY_COMMENT_MARKER = `<!--${LAZY_MARKER}`;

function isElement(n: Node): n is Element {
    return n.nodeType === Node.ELEMENT_NODE;
}

@Component({
    selector: "tim-plugin-loader",
    template: `
        <tim-alert *ngIf="error" severity="danger">
            {{ error }}
        </tim-alert>
        <answerbrowser *ngIf="type != 'none' && parsedTaskId && showBrowser && !preview && !unlockable && !lockedByPrerequisite"
                       [taskId]="parsedTaskId"
                       [answerId]="answerId">
        </answerbrowser>
        <div *ngIf="timed">
            <div *ngIf="running">
                <ng-container i18n>Time left:</ng-container>
                <tim-countdown [endTime]="endTime" (onFinish)="expireTask()"></tim-countdown>
            </div>
            <div i18n *ngIf="expired">
                Your access to this task has expired
            </div>
            <h4 *ngIf="accessHeader && taskHidden">{{accessHeader}}</h4>
            <div *ngIf="unlockable">
                <ng-container i18n>Unlock task. You will have {{accessDuration}} seconds to answer to this task.</ng-container>
                <button i18n i18n-title class="btn btn-primary" (click)="unlockTimedTask()" title="Unlock task">Unlock task</button>
            </div>
            <div *ngIf="expired && accessEndText">{{accessEndText}}</div>
        </div>
        <div *ngIf="lockedByPrerequisite">
            <div>
                {{getPrerequisiteLockedText()}}
            </div>
            <button class="btn btn-primary" (click)="unlockHiddenTask()"
                    title="{{getPrerequisiteUnlockText()}}">{{getPrerequisiteUnlockText()}}</button>
            <div *ngIf="lockedError">{{lockedError}}</div>
        </div>
        <div *ngIf="wrapper=='div'" [style.visibility]="taskHidden ? 'hidden' : 'visible'" #wrapper [attr.id]="id" [attr.plugin-type]="pluginType">
            <ng-container *ngTemplateOutlet="tempOutlet"></ng-container>
            <ng-container #pluginPlacement></ng-container>
            <div *ngIf="nonPluginHtml" [innerHTML]="nonPluginHtml | purify"></div>
        </div>
        <span *ngIf="wrapper=='span'" [style.visibility]="taskHidden ? 'hidden' : 'visible'" #wrapper [attr.id]="id" [attr.plugin-type]="pluginType">
            <ng-container *ngTemplateOutlet="tempOutlet"></ng-container>
            <ng-container #pluginPlacement></ng-container>
            <div *ngIf="nonPluginHtml" [innerHTML]="nonPluginHtml | purify"></div>
        </span>
        <ng-template #tempOutlet>
            <ng-content #contenthtml *ngIf="defaultload"></ng-content>
        </ng-template>
        <div *ngIf="feedback">
            <tim-alert [severity]="'warning'" [closeable]="true" (closing)="showFeedback('')">
                <div #feedBack [innerHTML]="feedback | purify"></div>
            </tim-alert>
        </div>

    `,
    styles: [],
})
export class PluginLoaderComponent implements AfterViewInit, OnDestroy, OnInit {
    @ViewChild("pluginPlacement", {read: ViewContainerRef, static: false})
    pluginPlacement!: ViewContainerRef;
    @ViewChild("feedback") feedBackElement?: ElementRef<HTMLDivElement>;
    private compiled = false;
    private viewctrl?: ViewCtrl;
    @Input() public taskId!: string;
    public nonPluginHtml?: string;
    public parsedTaskId?: TaskId;
    @Input() public type!: "full" | "lazyonly" | "none";
    @Input() public answerId?: number;
    @Input() public template?: string;
    @Input() public wrapper!: "div" | "span";
    @Input() public id!: string;
    @Input() public pluginType!: string;
    @Input() public preview?: boolean;
    @ViewChild("wrapper") pluginWrapper?: ElementRef<HTMLElement>;
    @ContentChild("contenthtml") contenthtml?: ElementRef<HTMLElement>;
    public defaultload = true;
    loadPluginAfterInit = false;
    public showBrowser: boolean = false;
    public hideBrowser: boolean = false;
    private forceBrowser: boolean = false;
    public showPlaceholder = true;
    public feedback?: string = "";
    public abLoad = new TimDefer<AnswerBrowserComponent | null>();
    @Input() public accessDuration?: number;
    @Input() public accessEnd?: string;
    @Input() public accessEndText?: string;
    @Input() public accessHeader?: string;
    @Input() public lockableByPrerequisite?: boolean;
    @Input() public lockedByPrerequisite?: boolean;
    @Input() public lockedText?: string;
    @Input() public lockedButtonText?: string;
    @Input() public lockedError?: string;
    error = "";

    timed = false;
    expired = false;
    unlockable = false;
    running = false;
    endTime?: ReadonlyMoment;
    taskHidden = false;

    private currentModRef?: NgModuleRef<unknown>;
    private currentModuleType?: Type<unknown>;

    constructor(
        private elementRef: ElementRef<HTMLElement>,
        private http: HttpClient,
        private zone: NgZone,
        private appRef: ApplicationRef,
        public vcr: ViewContainerRef
    ) {
        timLogTime("timPluginLoader constructor", "answ", 1);
    }

    get loaderElement(): HTMLElement {
        return this.elementRef.nativeElement;
    }

    private async refreshMath() {
        await ParCompiler.processAllMathDelayed($(this.loaderElement));
    }

    async ngOnInit() {
        this.viewctrl = vctrlInstance;
        if (this.viewctrl && !this.preview && this.taskId) {
            this.viewctrl.registerPluginLoader(this);
        }
        if (!this.taskId) {
            this.viewctrl?.registerPluginLoaderWithoutTaskId(this);
        }
        const r = TaskId.tryParse(this.taskId);
        if (r.ok) {
            this.parsedTaskId = r.result;
            if (
                (getURLParameter("task") === this.parsedTaskId.name &&
                    !this.preview) ||
                getViewName() == "review"
            ) {
                this.loadPluginAfterInit = true;
            }
        }
        if (this.lockedByPrerequisite) {
            this.hidePlugin();
        }
        if (this.type == "none") {
            // Light plugin loader doesn't have AB => mark as resolved
            this.abLoad.resolve(null);
        }

        await timeout();
        const m = this.pluginMarkup();
        if (
            m?.hideBrowser ||
            this.viewctrl?.docSettings.hideBrowser ||
            this.isUseCurrentUser() ||
            this.isInFormMode()
        ) {
            this.hideBrowser = true;
            this.showPlaceholder = false;
        }
        if (m?.forceBrowser) {
            this.forceBrowser = true;
        }

        this.showPlaceholder = !this.isInFormMode() && !this.hideBrowser;

        if (this.accessDuration && !this.viewctrl?.item.rights.teacher) {
            this.timed = true;
            if (!this.accessEnd) {
                this.unlockable = true;
                this.hidePlugin();
            } else {
                this.endTime = moment(this.accessEnd);
                if (this.endTime.isBefore(moment.now())) {
                    this.expireTask();
                } else {
                    this.startTask();
                }
            }
        }
        if (this.lockableByPrerequisite) {
            if (!m?.previousTask) {
                return;
            }
            if (this.lockedByPrerequisite) {
                this.viewctrl?.addLockListener(this);
                this.lockedText = m.previousTask.hideText;
                this.lockedButtonText = m.previousTask.unlockText;
            }
            this.toggleLockedAreas();
        }
    }

    ngAfterViewInit() {
        if (!this.pluginWrapper) {
            return;
        }

        this.elementRef.nativeElement.addEventListener("mouseenter", () =>
            this.loadPlugin()
        );
        this.elementRef.nativeElement.addEventListener("touchstart", () =>
            this.loadPlugin()
        );
        const pluginhtml = this.pluginWrapper.nativeElement.innerHTML;
        if (!pluginhtml.startsWith(LAZY_COMMENT_MARKER)) {
            const component = this.pluginWrapper.nativeElement
                .childNodes[0] as HTMLElement;
            this.determineAndSetComponent(component);
        }
        if (this.loadPluginAfterInit) {
            this.loadPlugin();
        }
    }

    ngOnDestroy() {
        this.removeActivationHandlers();
        this.currentModRef?.destroy();
    }

    private removeActivationHandlers() {
        this.elementRef.nativeElement.removeEventListener("mouseenter", () =>
            this.loadPlugin()
        );
        this.elementRef.nativeElement.removeEventListener("touchstart", () =>
            this.loadPlugin()
        );
    }

    showFeedback(txt?: string) {
        this.feedback = txt;
        if (this.feedBackElement) {
            ParCompiler.processAllMathDelayed(
                $(this.feedBackElement.nativeElement)
            );
        }
    }

    public pluginObject(): ITimComponent | undefined {
        if (!this.viewctrl || !this.taskId) {
            return undefined;
        }
        return this.viewctrl.getTimComponentByName(this.taskId);
    }

    public pluginMarkup(): IGenericPluginMarkup | undefined {
        const c = this.pluginObject();
        if (!c) {
            return undefined;
        }
        return c.markup;
    }

    public isUseCurrentUser() {
        const m = this.pluginMarkup();
        return m?.useCurrentUser ?? false;
    }

    public isGlobal(): boolean {
        return this.taskId.includes("GLO_");
        // return this.pluginMarkup().globalField;
    }

    loadAnswerBrowser() {
        if (this.viewctrl) {
            const ab = this.viewctrl.getAnswerBrowser(this.taskId);
            if (ab) {
                this.compiled = true;
                this.abLoad.resolve(ab);
            }
        }
        if (
            this.viewctrl &&
            (!this.viewctrl.noBrowser || this.forceBrowser) &&
            this.parsedTaskId &&
            this.type !== "lazyonly" &&
            Users.isLoggedIn()
        ) {
            this.showBrowser = true;
        } else {
            this.abLoad.resolve(null); // this plugin instance doesn't have answer browser
        }
    }

    async loadPlugin() {
        if (this.compiled) {
            return;
        }
        // PluginLoader is wrapped in AngularJS, so outside calls (e.g. via viewctrl) may not be properly tracked
        // Therefore, ensure the call is run inside the Angular zone to ensure change detection is triggered
        await this.zone.run(async () => {
            this.loadAnswerBrowser();
            const h = this.getNonLazyHtml();
            if (h && this.viewctrl) {
                this.defaultload = false;
                await this.loadFromHtml(h);
            }
            this.removeActivationHandlers();
        });
    }

    async loadFromHtml(html: string) {
        const elementToCompile = $(html)[0];
        this.defaultload = false;
        await this.determineAndSetComponent(elementToCompile);
        await this.refreshMath();
    }

    async determineAndSetComponent(component: HTMLElement) {
        if (component.nodeType === Node.COMMENT_NODE) {
            // empty loader, do nothing
            return;
        }
        if (component.tagName == undefined) {
            this.error = `Unknown component ${this.taskId}`;
            return;
        }
        const runnername = component.tagName.toLowerCase();
        if (runnername == "div") {
            // For now assume every non-component plugin html (e.g plugins in error state) is wrapped in div, so we
            // can catch plugins that are not properly handled yet
            this.setInnerHtml(component);
            return;
        }
        this.defaultload = false;
        this.nonPluginHtml = undefined;
        const json = component.getAttribute("json");
        const registeredPlugin = getPlugin(runnername);
        if (!registeredPlugin) {
            this.error = $localize`Unknown component: ${runnername}`;
            return;
        }
        if (!json) {
            this.error = $localize`Component ${runnername} is missing attribute json`;
            return;
        }
        await this.setComponent(registeredPlugin, json);
    }

    setInnerHtml(element: HTMLElement) {
        if (this.defaultload) {
            return;
        }
        const viewContainerRef = this.pluginPlacement;
        viewContainerRef.clear();
        this.nonPluginHtml = element.outerHTML;
    }

    private createModule(moduleType: Type<unknown>) {
        if (this.currentModuleType === moduleType && this.currentModRef) {
            return this.currentModRef;
        }

        if (this.currentModRef) {
            this.currentModRef.destroy();
            this.currentModRef = undefined;
        }

        // Note: there are multiple ways to bootstrap a module with the components
        // 1. createNgModule - creates a new module with no platform -> mainly for child modules, prevents multiple platform instances
        //   * Seems to be fast and easy to bootstrap, but doesn't allow referencing e.g. BrowserModule => this seems to be intended for child modules
        // 2. PlatformRef.bootstrapModule - bootstraps a module as if it's an application module
        //   * Angular allows multiple bootstraps for the same platform
        //   * Seems to break change detection for plugins
        // For now we'll use createNgModule since eventually we'll likely move to use Angular Elements that seem to do the same thing
        this.currentModRef = createNgModule(moduleType, this.appRef.injector);
        this.currentModuleType = moduleType;
        return this.currentModRef;
    }

    async setComponent(registeredPlugin: IRegisteredPlugin, json: string) {
        const viewContainerRef = this.pluginPlacement;
        viewContainerRef.clear();

        const modRef = this.createModule(registeredPlugin.module);
        const componentRef = await viewContainerRef.createComponent<PluginJson>(
            registeredPlugin.component,
            {ngModuleRef: modRef}
        );
        componentRef.instance.json = json;
        componentRef.changeDetectorRef.detectChanges();
    }

    /**
     * Returns the non-lazy HTML for the plugin if the plugin is lazy.
     * If the plugin is not lazy, returns nothing.
     */
    getNonLazyHtml() {
        const pe = this.getPluginElement();
        const cns = pe[0].childNodes;
        let nonLazyHtml;
        for (const n of cns) {
            if (
                n.nodeType === Node.COMMENT_NODE &&
                n.nodeValue &&
                n.nodeValue.startsWith(LAZY_MARKER) &&
                n.nodeValue.endsWith(LAZY_MARKER)
            ) {
                nonLazyHtml = n.nodeValue.slice(
                    LAZY_MARKER_LENGTH,
                    n.nodeValue.length - LAZY_MARKER_LENGTH
                );
                break;
            } else if (
                isElement(n) &&
                n.tagName === "DIV" &&
                n.className === LAZY_MARKER
            ) {
                nonLazyHtml = n.getAttribute("data-html");
                break;
            }
        }
        return nonLazyHtml;
    }

    getPluginElement(): JQuery {
        return $(
            this.elementRef.nativeElement.querySelectorAll(
                "[plugin-type]"
            )[0] as HTMLElement
        );
    }

    unDimPlugin() {
        const e = this.getPluginElement();
        e.css("opacity", "1");
        e.removeClass("hidden-print");
    }

    dimPlugin() {
        if (!this.isInFormMode()) {
            const e = this.getPluginElement();
            e.css("opacity", "0.3");
            e.addClass("hidden-print");
        }
    }

    hidePlugin() {
        this.taskHidden = true;
    }

    unHidePlugin() {
        this.taskHidden = false;
    }

    async unlockTimedTask() {
        const r = await toPromise(
            this.http.get<{end_time: string; expired?: boolean}>(
                "/unlockTimedTask",
                {
                    params: {
                        task_id: this.taskId,
                    },
                }
            )
        );
        if (r.ok) {
            this.unlockable = false;
            this.endTime = moment(r.result.end_time);
            if (this.endTime.isBefore(moment.now())) {
                this.expireTask();
            } else {
                this.startTask();
            }
        }
    }

    informAboutLock(sourceTask: TaskId) {
        const prevInfo = this.pluginMarkup()?.previousTask;
        if (!prevInfo || !this.viewctrl?.docId || !prevInfo.requireLock) {
            return;
        }
        const tid = TaskIdWithDefaultDocId(
            prevInfo.taskid,
            this.viewctrl.docId
        );
        if (tid?.docTask() == sourceTask.docTask()) {
            if (prevInfo.count) {
                // todo
                return;
            }
            this.unlockHiddenTask();
        }
    }

    async unlockHiddenTask() {
        const r = await toPromise(
            this.http.get<{unlocked: boolean; error?: string}>(
                "/unlockHiddenTask",
                {
                    params: {
                        task_id: this.taskId,
                    },
                }
            )
        );
        if (r.ok) {
            if (r.result.unlocked) {
                this.lockedByPrerequisite = false;
                this.unHidePlugin();
                this.toggleLockedAreas();
            } else {
                this.lockedError =
                    r.result.error ??
                    $localize`You haven't unlocked this task yet`;
            }
        } else {
            this.lockedError = r.result.error.error;
        }
    }

    /**
     * Hide areas where hide-with attribute matches current taskid
     * TODO:
     *  This should be handled by the actual plugin containing the modelAnswer and the locks (and later
     *  be handled server-side), but the current implementation of modelAnswer lock query is expensive and unoptimized
     */
    toggleLockedAreas() {
        if (!this.parsedTaskId) {
            return;
        }
        const dataAreas = document.querySelectorAll(
            `[attrs*='"area"'][attrs*='"hide-with": "${this.parsedTaskId.name}"']`
        );
        for (const da of dataAreas) {
            const attrs = da.getAttribute("attrs");
            if (attrs) {
                try {
                    const attrObj = JSON.parse(attrs) as {
                        area: string;
                    };
                    const areaName = attrObj.area;
                    if (areaName) {
                        const area = document.querySelector(
                            `div.area.area_${areaName} > .areaContent`
                        );
                        if (area && area instanceof HTMLElement) {
                            area.style.setProperty(
                                "display",
                                this.lockedByPrerequisite ? "none" : "block",
                                "important"
                            );
                        }
                    }
                } catch {}
            }
        }
    }

    expireTask() {
        this.expired = true;
        this.running = false;
        if (this.accessEndText) {
            this.hidePlugin();
        }
    }

    startTask() {
        this.unHidePlugin();
        this.running = true;
    }

    isInFormMode() {
        if (this.viewctrl) {
            const timComp = this.viewctrl.getTimComponentByName(this.taskId);
            if (timComp) {
                return this.viewctrl.isTimComponentInFormMode(timComp);
            }
        }
        return false;
    }

    warnAboutMissingTaskId() {
        this.error = $localize`Plugin is missing task id`;
    }

    getPrerequisiteLockedText() {
        return this.lockedText ?? $localize`You haven't unlocked this task yet`;
    }

    getPrerequisiteUnlockText() {
        return this.lockedButtonText ?? $localize`Open task`;
    }

    getAnswerId() {
        return this.answerId;
    }
}
@NgModule({
    declarations: [PluginLoaderComponent],
    imports: [
        AnswerBrowserModule,
        BrowserModule,
        FormsModule,
        PurifyModule,
        TimUtilityModule,
        HttpClientModule,
    ],
    exports: [PluginLoaderComponent],
})
export class PluginLoaderModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                PluginLoaderModule
            )
        ),
        "timPluginLoader",
        PluginLoaderComponent
    ),
];
