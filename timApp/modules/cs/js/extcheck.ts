/* eslint no-underscore-dangle: ["error", { "allow": ["hide_"] }] */
import type {ComponentRef, Type} from "@angular/core";
import {
    ChangeDetectorRef,
    Compiler,
    Component,
    Directive,
    ElementRef,
    Injector,
    Input,
    NgModule,
    ViewChild,
    ViewContainerRef,
} from "@angular/core";
import {CommonModule} from "@angular/common";
import {HttpClient} from "@angular/common/http";
import {DomSanitizer} from "@angular/platform-browser";
import {CsController} from "./csPlugin";

interface IAngularComponent {
    template: string;
    component: string;
}

interface IAngularModule {
    components: Record<string, IAngularComponent>;
    entry: string;
}

interface IDivContent {
    classes: string;
    content: string;
}

interface IOutputContainer {
    title?: IDivContent;
    text?: IDivContent;
    angular?: IAngularModule;
    html?: IDivContent;
    hide?: boolean;
}

interface IRunResult {
    output_boxes: IOutputContainer[];
    penalties: string[];
}

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "cs-extcheck-runner",
    styleUrls: ["./extcheck.scss"],
    template: `
        <div [ngClass]="{'csRunDiv': markup.borders}" [class.cs-has-header]="header" class="type-{{rtype}}">
            <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <h4 *ngIf="header" [innerHTML]="header"></h4>
            <p *ngIf="stem" class="stem" [innerHTML]="stem"></p>
            <ng-container *ngIf="upload">
                <file-select-manager class="small"
                        [dragAndDrop]="markup.dragAndDrop"
                        [uploadUrl]="uploadUrl"
                        [stem]="uploadstem"
                        (file)="onFileLoad($event)"
                        (upload)="onUploadResponse($event)">
                </file-select-manager>
                <div class="form-inline small">
                    <span *ngFor="let item of uploadedFiles">
                        <cs-upload-result [src]="item.path" [type]="item.type"></cs-upload-result>
                    </span>
                </div>
            </ng-container>
            <pre *ngIf="viewCode && codeover">{{code}}</pre>
            <div class="csRunCode">
                <pre class="csRunPre" *ngIf="viewCode && !codeunder && !codeover">{{precode}}</pre>
                <div class="csEditorAreaDiv">
                    <cs-editor #mainEditor *ngIf="!noeditor || viewCode" class="csrunEditorDiv"
                            [base]="byCode"
                            [minRows]="markup.rows"
                            [maxRows]="markup.maxrows"
                            [wrap]="wrap"
                            [modes]="editorModes"
                            [editorIndex]="markup.editorMode"
                            [parsonsOptions]="markup.parsons"
                            (close)="onFileClose($event)"
                            (content)="onContentChange($event)">
                    </cs-editor>
                    <div class="csRunChanged" *ngIf="usercode !== byCode && !hide.changed"></div>
                    <div class="csRunNotSaved" *ngIf="isUnSaved()"></div>
                </div>
                <pre class="csRunPost" *ngIf="viewCode && !codeunder && !codeover">{{postcode}}</pre>
            </div>
            <div *ngIf="isSage" class="computeSage no-popup-menu"></div>
            <div class="csInputDiv" *ngIf="showInput && isInput">
                <p *ngIf="inputstem" class="stem">{{inputstem}}</p>
                <div class="csRunCode">
                    <textarea class="csRunArea csInputArea"
                            [rows]="inputrows"
                            [(ngModel)]="userinput"
                            placeholder="inputplaceholder">
                    </textarea>
                </div>
            </div>
            <div class="csArgsDiv" *ngIf="showArgs && isInput"><label>{{argsstem}} </label>
                <span><input type="text"
                            class="csArgsArea"
                            [(ngModel)]="userargs"
                            placeholder="argsplaceholder"></span>
            </div>
            <cs-count-board *ngIf="markup.count" [options]="markup.count"></cs-count-board>
            <p class="csRunSnippets" *ngIf="templateButtonsCount && !noeditor">
                <button *ngFor="let item of templateButtons;" (click)="addText(item)" title="{{item.expl}}">{{item.text}}</button>
                &nbsp;&nbsp;
            </p>
            <cs-editor #externalEditor *ngIf="externalFiles && externalFiles.length" class="csrunEditorDiv"
                    [maxRows]="markup.maxrows"
                    [disabled]="true">
            </cs-editor>
            <div class="csRunMenuArea" *ngIf="!forcedupload">
                <p class="csRunMenu">
                    <button *ngIf="isRun && buttonText()"
                            [attr.disabled]="isRunning || preventSave || (disableUnchanged && !isUnSaved() && isText)"
                            class="timButton btn-sm"
                            title="(Ctrl-S)"
                            (click)="runCode()"
                            [innerHTML]="buttonText()"></button>
                    &nbsp;
                    <button *ngIf="isExternalFetch"
                            [disabled]="isRunning"
                            class="timButton btn-sm"
                            (click)="fetchExternalFiles()" i18n>Fetch</button>
                    <a href="#" *ngIf="undoButton && isUnSaved()" [title]="undoTitle"
                            (click)="tryResetChanges($event)"> &nbsp;{{undoButton}}</a>
                    &nbsp;&nbsp;
                    <span *ngIf="savedText"
                            class="savedText"
                            [innerHTML]="savedText"></span>
                    &nbsp;&nbsp;
                    <tim-loading *ngIf="isRunning"></tim-loading>
                    &nbsp;&nbsp;
                    <span *ngIf="isDocument">
                        <a href="#" [attr.disabled]="isRunning"
                                (click)="runDocument(); $event.preventDefault()">{{docLink}}</a>&nbsp;&nbsp;
                    </span>
                    <a href="#" *ngIf="!nocode && (file || program)"
                            (click)="showCode(); $event.preventDefault()">{{showCodeLink}}</a>&nbsp;&nbsp;
                    <a href="#" *ngIf="editor && editor.modified"
                            (click)="editor.reset(); $event.preventDefault()">{{resetText}}</a>
                    <a href="#" *ngIf="toggleEditor"
                            (click)="hideShowEditor(); $event.preventDefault()">{{toggleEditorText[noeditor ? 0 : 1]}}</a>
                    <a href="#" *ngIf="!noeditor && editor && editor.nextModeText"
                            (click)="editor.showOtherEditor(); $event.preventDefault()">
                        {{editor.nextModeText}}
                    </a>&nbsp;&nbsp;
                    <a href="#" *ngIf="markup.copyLink"
                            (click)="copyCode(); $event.preventDefault()">{{markup.copyLink}}</a>
                    <span *ngIf="showRuntime"
                            class="inputSmall"
                            style="float: right;"
                            title="Run time in sec {{runtime}}">{{oneruntime}}</span>
                    <span *ngIf="editor && wrap && wrap.n!=-1 && !hide.wrap" class="inputSmall" style="float: right;" title="Put 0 to no wrap">
                        <button class="timButton" title="Click to reformat text for given line length" (click)="editor.doWrap()" style="font-size: x-small; height: 1.7em; padding: 1px; margin-top: -4px;">Wrap
                        </button>
                        <input type="checkbox" title="Check for automatic wrapping" [(ngModel)]="wrap.auto" style="position: relative;top: 0.3em;"/>
                        <input type="text" title="Choose linelength for text.  0=no wrap" pattern="/[-0-9]*/" [(ngModel)]="wrap.n" size="2"/>
                    </span>
                    <span *ngIf="connectionErrorMessage" class="error" style="font-size: 12px" [innerHTML]="connectionErrorMessage"></span>
                </p>
            </div>
            <pre *ngIf="viewCode && codeunder">{{code}}</pre>
            <div class="csRunErrorClass" *ngIf="runError">
                <p class="pull-right">
                    <tim-close-button (click)="closeError()"></tim-close-button>
                </p>
                <pre class="csRunError" >{{error}}</pre>
                <p class="pull-right" style="margin-top: -1em">
                    <tim-close-button (click)="closeError()"></tim-close-button>
                </p>
            </div>
            <div class="csRunErrorClass" *ngIf="fetchError">
                <p class="pull-right">
                    <tim-close-button (click)="fetchError=undefined"></tim-close-button>
                </p>
                <pre class="csRunError" >{{fetchError}}</pre>
                <p class="pull-right" style="margin-top: -1em">
                    <tim-close-button (click)="fetchError=undefined"></tim-close-button>
                </p>
            </div>
            <extcheck-output-container *ngIf="penalty_container" [data]="penalty_container"></extcheck-output-container>
            <extcheck-output-container *ngFor="let container of containers" [data]="container"></extcheck-output-container>
            <p class="footer" [innerHTML]="markup.footer"></p>
        </div>`,
})
export class ExtcheckComponent extends CsController {
    containers?: IOutputContainer[];
    penalty_container?: IOutputContainer;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer, cdr);
    }

    languageResponse(data: IRunResult) {
        if (data == null) {
            this.containers = undefined;
            this.penalty_container = undefined;
        } else {
            this.containers = data.output_boxes;
            if (this.containers && this.containers.length == 0) {
                this.containers = undefined;
            }
            if (data.penalties && data.penalties.length != 0) {
                this.penalty_container = {
                    title: {
                        classes: "",
                        content: "Penalties",
                    },
                    html: {
                        classes: "",
                        content: data.penalties
                            .map(
                                (e) =>
                                    `<p class="penalty-text centermargin">${e}</p>`
                            )
                            .join("\n"),
                    },
                };
            }
        }
    }
}

@Directive({
    selector: "[custom-data]",
})
export class CustomOutputDirective {
    constructor(public viewContainerRef: ViewContainerRef) {}
}

export class CustomOutputBase {
    data?: string;
}

@Component({
    selector: "extcheck-output-container",
    styleUrls: ["./extcheck.scss"],
    template: `
        <button (click)="hide=!hide" [ngClass]="{'collapsed-button': hide}" class="title-button {{title.classes}}">{{title.content}}<span class='caret'></span></button>
        <div *ngIf="angularContent" class="centermargin" custom-data></div>
        <ng-container *ngIf="!hide">
            <div *ngIf="htmlContent" class="centermargin {{htmlContent.classes}}" [innerHTML]="htmlContent.content | purify"></div>
            <pre *ngIf="textContent" class="centermargin {{textContent.classes}}">{{textContent.content}}</pre>
        </ng-container>`,
})
export class OutputContainerComponent implements IOutputContainer {
    private static components: Record<string, Type<unknown>>;
    static addComponent(name: string, type: Type<unknown>): Type<unknown> {
        if (!this.components.hasOwnProperty(name)) {
            this.components[name] = type;
        }
        return this.components[name];
    }

    title: IDivContent = {classes: "", content: ""};
    textContent?: IDivContent;
    angularContent?: IAngularModule;
    htmlContent?: IDivContent;
    componentRef?: ComponentRef<unknown>;
    hide_: boolean = false;
    @ViewChild(CustomOutputDirective) customOutput!: CustomOutputDirective;

    caret: string = "<span class='caret'></span>";

    constructor(private injector: Injector, private compiler: Compiler) {}

    async ngAfterViewInit() {
        if (!this.angularContent) {
            return;
        }
        if (
            !this.angularContent.entry ||
            !Object.keys(this.angularContent.components).length
        ) {
            this.error("Angular entry or components not specified");
            return;
        }

        const components = [];
        const module = this.angularContent;
        for (const [key, value] of Object.entries(module.components)) {
            const tmpCls = Function(`return ${value.component};`)();
            const tmpCmp = Component({selector: key, template: value.template})(
                tmpCls
            );
            components.push(tmpCmp);
        }
        const tmpModule = NgModule({
            declarations: components,
            imports: [CommonModule],
        })(class {});

        const factories =
            await this.compiler.compileModuleAndAllComponentsAsync(tmpModule);
        const m = factories.ngModuleFactory.create(this.injector);
        const factory = factories.componentFactories.find(
            (e) => e.selector == module.entry
        );
        if (!factory) {
            this.error("");
            return;
        }
        this.componentRef = factory.create(this.injector, [], null, m);
        this.customOutput.viewContainerRef.insert(this.componentRef.hostView);
        this.hide = this.hide; // refresh style.display
    }

    ngOnDestroy() {
        if (this.componentRef) {
            this.componentRef.destroy();
        }
    }

    get hide(): boolean {
        return this.hide_;
    }
    set hide(b: boolean) {
        this.hide_ = b;
        if (this.componentRef) {
            // ngIf destroys the component: use display
            (
                this.componentRef.location.nativeElement as HTMLElement
            ).style.display = b ? "none" : "";
        }
    }

    @Input()
    set data(data: IOutputContainer) {
        if (data.title) {
            this.title = data.title;
        }
        if (
            (data.angular ? 1 : 0) + (data.html ? 1 : 0) + (data.text ? 1 : 0) >
            1
        ) {
            this.error(
                "Only one of angular, text or html can be defined at once"
            );
            this.hide = false;
            return;
        }
        if (data.angular) {
            this.angularContent = data.angular;
        } else if (data.html) {
            this.htmlContent = data.html;
        } else {
            this.textContent = data.text;
        }

        this.hide = !!data.hide;
    }

    error(str: string) {
        this.angularContent = undefined;
        this.textContent = {classes: "", content: str};
    }
}
