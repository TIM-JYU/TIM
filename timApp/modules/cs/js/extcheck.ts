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
          @if (markupError) {
            <tim-markup-error [data]="markupError"></tim-markup-error>
          }
          @if (header) {
            <h4 [innerHTML]="header"></h4>
          }
          @if (stem) {
            <p class="stem" [innerHTML]="stem"></p>
          }
          @if (upload) {
            <file-select-manager class="small"
              [dragAndDrop]="markup.dragAndDrop"
              [uploadUrl]="uploadUrl"
              [stem]="uploadstem"
              (file)="onFileLoad($event)"
              (upload)="onUploadResponse($event)">
            </file-select-manager>
            <div class="form-inline small">
              @for (item of uploadedFiles; track item) {
                <span>
                  <cs-upload-result [src]="item.path" [type]="item.type"></cs-upload-result>
                </span>
              }
            </div>
          }
          @if (viewCode && codeover) {
            <pre>{{code}}</pre>
          }
          <div class="csRunCode">
            @if (viewCode && !codeunder && !codeover) {
              <pre class="csRunPre">{{precode}}</pre>
            }
            <div class="csEditorAreaDiv">
              @if (!noeditor || viewCode) {
                <cs-editor #mainEditor class="csrunEditorDiv"
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
              }
              @if (usercode !== byCode && !hide.changed) {
                <div class="csRunChanged"></div>
              }
              @if (isUnSaved()) {
                <div class="csRunNotSaved"></div>
              }
            </div>
            @if (viewCode && !codeunder && !codeover) {
              <pre class="csRunPost">{{postcode}}</pre>
            }
          </div>
          @if (isSage) {
            <div class="computeSage no-popup-menu"></div>
          }
          @if (showInput && isInput) {
            <div class="csInputDiv">
              @if (inputstem) {
                <p class="stem">{{inputstem}}</p>
              }
              <div class="csRunCode">
                <textarea class="csRunArea csInputArea"
                  [rows]="inputrows"
                  [(ngModel)]="userinput"
                  placeholder="inputplaceholder">
                </textarea>
              </div>
            </div>
          }
          @if (showArgs && isInput) {
            <div class="csArgsDiv"><label>{{argsstem}} </label>
            <span><input type="text"
              class="csArgsArea"
              [(ngModel)]="userargs"
            placeholder="argsplaceholder"></span>
          </div>
        }
        @if (markup.count) {
          <cs-count-board [options]="markup.count"></cs-count-board>
        }
        @if (templateButtonsCount && !noeditor) {
          <p class="csRunSnippets">
            @for (item of templateButtons; track item) {
              <button (click)="addText(item)" title="{{item.expl}}">{{item.text}}</button>
            }
            <ng-container> &nbsp;&nbsp; </ng-container>
          </p>
        }
        @if (externalFiles && externalFiles.length) {
          <cs-editor #externalEditor class="csrunEditorDiv"
            [maxRows]="markup.maxrows"
            [disabled]="true">
          </cs-editor>
        }
        @if (!forcedupload) {
          <div class="csRunMenuArea">
            <p class="csRunMenu">
              @if (isRun && buttonText()) {
                <button
                  [attr.disabled]="isRunning || preventSave || (disableUnchanged && !isUnSaved() && isText)"
                  class="timButton btn-sm"
                  title="(Ctrl-S)"
                  (click)="runCode()"
                [innerHTML]="buttonText()"></button>
              }
              <ng-container> &nbsp; </ng-container>
              @if (isExternalFetch) {
                <button
                  [disabled]="isRunning"
                  class="timButton btn-sm"
                (click)="fetchExternalFiles()" i18n>Fetch</button>
              }
              @if (undoButton && isUnSaved()) {
                <a href="#" [title]="undoTitle"
                (click)="tryResetChanges($event)"> &nbsp;{{undoButton}}</a>
              }
              <ng-container> &nbsp;&nbsp; </ng-container>
              @if (savedText) {
                <span
                  class="savedText"
                [innerHTML]="savedText"></span>
              }
              <ng-container> &nbsp;&nbsp; </ng-container>
              @if (isRunning) {
                <tim-loading></tim-loading>
              }
              <ng-container> &nbsp;&nbsp; </ng-container>
              @if (isDocument) {
                <span>
                  <a href="#" [attr.disabled]="isRunning"
                  (click)="runDocument(); $event.preventDefault()">{{docLink}}</a>&nbsp;&nbsp;
                </span>
              }
              @if (!nocode && (file || program)) {
                <a href="#"
                (click)="showCode(); $event.preventDefault()">{{showCodeLink}}</a>
                }<ng-container>&nbsp;&nbsp; </ng-container>
                @if (editor && editor.modified) {
                  <a href="#"
                  (click)="editor.reset(); $event.preventDefault()">{{resetText}}</a>
                }
                @if (toggleEditor) {
                  <a href="#"
                  (click)="hideShowEditor(); $event.preventDefault()">{{toggleEditorText[noeditor ? 0 : 1]}}</a>
                }
                @if (!noeditor && editor && editor.nextModeText) {
                  <a href="#"
                    (click)="editor.showOtherEditor(); $event.preventDefault()">
                    {{editor.nextModeText}}
                  </a>
                  }<ng-container>&nbsp;&nbsp; </ng-container>
                  @if (markup.copyLink) {
                    <a href="#"
                    (click)="copyCode(); $event.preventDefault()">{{markup.copyLink}}</a>
                  }
                  @if (showRuntime) {
                    <span
                      class="inputSmall"
                      style="float: right;"
                    title="Run time in sec {{runtime}}">{{oneruntime}}</span>
                  }
                  @if (editor && wrap && wrap.n!=-1 && !hide.wrap) {
                    <span class="inputSmall" style="float: right;" title="Put 0 to no wrap">
                      <button class="timButton" title="Click to reformat text for given line length" (click)="editor.doWrap()" style="font-size: x-small; height: 1.7em; padding: 1px; margin-top: -4px;">Wrap
                      </button>
                      <input type="checkbox" title="Check for automatic wrapping" [(ngModel)]="wrap.auto" style="position: relative;top: 0.3em;"/>
                      <input type="text" title="Choose linelength for text.  0=no wrap" pattern="/[-0-9]*/" [(ngModel)]="wrap.n" size="2"/>
                    </span>
                  }
                  @if (connectionErrorMessage) {
                    <span class="error" style="font-size: 12px" [innerHTML]="connectionErrorMessage"></span>
                  }
                </p>
              </div>
            }
            @if (viewCode && codeunder) {
              <pre>{{code}}</pre>
            }
            @if (runError) {
              <div class="csRunErrorClass">
                <p class="pull-right">
                  <tim-close-button (click)="closeError()"></tim-close-button>
                </p>
                <pre class="csRunError" >{{error}}</pre>
                <p class="pull-right" style="margin-top: -1em">
                  <tim-close-button (click)="closeError()"></tim-close-button>
                </p>
              </div>
            }
            @if (fetchError) {
              <div class="csRunErrorClass">
                <p class="pull-right">
                  <tim-close-button (click)="fetchError=undefined"></tim-close-button>
                </p>
                <pre class="csRunError" >{{fetchError}}</pre>
                <p class="pull-right" style="margin-top: -1em">
                  <tim-close-button (click)="fetchError=undefined"></tim-close-button>
                </p>
              </div>
            }
            @if (penalty_container) {
              <extcheck-output-container [data]="penalty_container"></extcheck-output-container>
            }
            @for (container of containers; track container) {
              <extcheck-output-container [data]="container"></extcheck-output-container>
            }
            <p class="footer" [innerHTML]="markup.footer"></p>
          </div>`,
    standalone: false,
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
    standalone: false,
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
        @if (angularContent) {
          <div class="centermargin" custom-data></div>
        }
        @if (!hide) {
          @if (htmlContent) {
            <div class="centermargin {{htmlContent.classes}}" [innerHTML]="htmlContent.content | purify"></div>
          }
          @if (textContent) {
            <pre class="centermargin {{textContent.classes}}">{{textContent.content}}</pre>
          }
        }`,
    standalone: false,
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
