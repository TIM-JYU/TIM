import {IScope, ICompileService, IAugmentedJQuery, IAttributes} from "angular";
import {pluginBindings} from "tim/plugin/util";
import {CsController, uploadTemplate} from "./csPlugin";
import {
        Input,
        Component,
        DoBootstrap,
        NgModule,
        StaticProvider,
        ViewChild,
        ChangeDetectorRef,
        ElementRef,
        ApplicationRef,
        Directive,
        Type,
        ViewContainerRef,
        Injector,
        Compiler,
        ComponentRef,
    } from "@angular/core"
import {CommonModule} from "@angular/common"
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {BrowserModule, DomSanitizer} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {EditorModule} from "./editor/module"

interface IAngularComponent {
    template: string;
}

interface IAngularModule {
    components: {[key: string]: IAngularComponent};
    entry: string;
}

interface IDivContent {
    classes: string;
    content: string;
    isHTML?: boolean;
}

interface IOutputContainer {
    title?: IDivContent;
    text?: IDivContent;
    angular?: IAngularModule;
    hide?: boolean;
}

interface IRunResult {
    output_boxes: IOutputContainer[];
    penalties: string[];
}

@Component({
    selector: "cs-extcheck-runner",
    template: `
        <div [ngClass]="{'csRunDiv': markup.borders}" class="type-{{rtype}}">
            <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <h4 *ngIf="header" [innerHTML]="header"></h4>
            <p *ngIf="stem" class="stem" [innerHTML]="stem"></p>`
            + uploadTemplate() +
            `<pre *ngIf="viewCode && codeover">{{code}}</pre>
            <div class="csRunCode">
                <pre class="csRunPre" *ngIf="viewCode && !codeunder && !codeover">{{precode}}</pre>
                <div class="csEditorAreaDiv">
                    <cs-editor *ngIf="!noeditor || viewCode" class="csrunEditorDiv"
                            [base]="byCode"
                            [cssPrint]="cssPrint"
                            [minRows]="markup.rows"
                            [maxRows]="markup.maxrows"
                            [wrap]="wrap"
                            [modes]="editorModes"
                            [editorIndex]="markup.editorMode"
                            [languageMode]="mode"
                            [parsonsShuffle]="initUserCode"
                            [parsonsMaxcheck]="markup.parsonsmaxcheck"
                            [parsonsNotordermatters]="markup.parsonsnotordermatters"
                            [parsonsStyleWords]="markup['style-words']"
                            [parsonsWords]="markup.words"
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
            <div *ngIf="countItems" class="csPluginCountItems">
                <span *ngIf="countLines">Lines: <span>{{lineCount}}</span></span>
                <span *ngIf="countWords">Words: <span>{{wordCount}}</span></span>
                <span *ngIf="countChars">Chars: <span>{{charCount}}</span></span>
            </div>    
            <div *ngIf="countError" class="csPluginCountError">
                <p>{{countError}}</p>
            </div>    
            <p class="csRunSnippets" *ngIf="buttons">
                <button *ngFor="let item of buttons" (click)="addText(item)">{{addTextHtml(item)}}</button>
                &nbsp;&nbsp;
            </p>
            <div class="csRunMenuArea" *ngIf="!forcedupload">
                <p class="csRunMenu">
                    <button *ngIf="isRun && buttonText()"
                            [attr.disabled]="isRunning || preventSave || (markup.disableUnchanged && !isUnSaved() && isText)"
                            class="timButton btn-sm"
                            title="(Ctrl-S)"
                            (click)="runCode()"
                            [innerHTML]="buttonText()"></button>
                    <a href="javascript:void(0)" *ngIf="undoButton && isUnSaved()" title="undoTitle"
                            (click)="tryResetChanges()"> &nbsp;{{undoButton}}</a>
                    &nbsp;&nbsp;
                    <span *ngIf="savedText"
                            class="savedText"
                            [innerHTML]="savedText"></span>
                    &nbsp;&nbsp;
                    <tim-loading *ngIf="isRunning"></tim-loading>
                    &nbsp;&nbsp;
                    <span *ngIf="isDocument">
                        <a href="javascript:void(0)" [attr.disabled]="isRunning"
                                (click)="runDocument()">{{docLink}}</a>&nbsp;&nbsp;
                    </span>
                    <a href="javascript:void(0)" *ngIf="!nocode && (file || program)"
                            (click)="showCode()">{{showCodeLink}}</a>&nbsp;&nbsp;
                    <a href="javascript:void(0)" *ngIf="editor && editor.modified"
                            (click)="editor?.reset()">{{resetText}}</a>
                    <a href="javascript:void(0)" *ngIf="toggleEditor"
                            (click)="hideShowEditor()">{{toggleEditorText[noeditor ? 0 : 1]}}</a>
                    <a href="javascript:void(0)" *ngIf="!noeditor && editor && editor.nextModeText"
                            (click)="editor?.showOtherEditor()">
                        {{editor.nextModeText}}
                    </a>&nbsp;&nbsp;
                    <a href="javascript:void(0)" *ngIf="markup.copyLink"
                            (click)="copyCode()">{{markup.copyLink}}</a>
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
            <extcheck-output-container *ngIf="penalty_container" [data]="penalty_container"></extcheck-output-container>
            <extcheck-output-container *ngFor="let container of containers" [data]="container"></extcheck-output-container>
            <p class="footer" [innerHTML]="markup.footer"></p>
        </div>`,
})
export class ExtcheckComponent extends CsController {
    containers?: IOutputContainer[];
    penalty_container?: IOutputContainer;
    
    constructor(el: ElementRef<HTMLElement>, http: HttpClient, domSanitizer: DomSanitizer, cdr: ChangeDetectorRef) {
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
                    text: {
                        classes: "",
                        content: data.penalties.map((e) => `<p class="penalty-text centermargin">${e}</p>`).join("\n"),
                        isHTML: true,
                    },
                };
            }
        }
    }
}

@Directive({
    selector: '[custom-data]',
})
export class CustomOutputDirective {
    constructor(public viewContainerRef: ViewContainerRef) { }
}

export class CustomOutputBase {
    data?: string;
}

@Component({
    selector: "extcheck-output-container",
    template: `
        <button *ngIf="title.isHTML" (click)="hide=!hide" [ngClass]="{'collapsed-button': hide}" class="title-button {{title.classes}}" [innerHTML]="title.content + caret"></button>
        <button *ngIf="!title.isHTML" (click)="hide=!hide" [ngClass]="{'collapsed-button': hide}" class="title-button {{title.classes}}">{{title.content}}<span class='caret'></span></button>
        <div *ngIf="angularContent" class="centermargin" custom-data></div>
        <ng-container *ngIf="!hide && textContent">
            <div *ngIf="textContent.isHTML" class="centermargin {{textContent.classes}}" [innerHTML]="textContent.content"></div>
            <div *ngIf="!textContent.isHTML">
                <pre class="centermargin {{textContent.classes}}">{{textContent.content}}</pre>
            </div>
        </ng-container>`,
})
export class OutputContainerComponent implements IOutputContainer {
    private static components: {[key: string]: Type<unknown>};
    static addComponent(name: string, type: Type<unknown>): Type<unknown> {
        if (!this.components.hasOwnProperty(name)) {
            this.components[name] = type;
        }
        return this.components[name];
    }
    
    title: IDivContent = {classes: "", content: ""};
    textContent?: IDivContent;
    angularContent?: IAngularModule;
    componentRef?: ComponentRef<unknown>;
    hide_: boolean = false;
    @ViewChild(CustomOutputDirective) customOutput!: CustomOutputDirective;

    caret: string = "<span class='caret'></span>";

    constructor(private _injector: Injector, private _compiler: Compiler) { }

    async ngAfterViewInit() {
        if (!this.angularContent) { return; }
        if (!this.angularContent.entry || !Object.keys(this.angularContent.components).length) {
            this.error("Angular entry or components not specified");
            return;
        }
        
        let components = [];
        const module = this.angularContent;
        for (const key in module.components) {
            const tmpCmp = Component({selector: key, template: module.components[key].template})(class {});
            components.push(tmpCmp);
        }
        const tmpModule = NgModule({declarations: components, imports: [CommonModule]})(class {});
        
        const factories = await this._compiler.compileModuleAndAllComponentsAsync(tmpModule);
        const m = factories.ngModuleFactory.create(this._injector);
        const factory = factories.componentFactories.find((e) => e.selector == module.entry);
        if (!factory) {
            this.error("");
            return;
        }
        this.componentRef = factory.create(this._injector, [], null, m);
        this.customOutput.viewContainerRef.insert(this.componentRef.hostView);
        this.hide = this.hide; // refresh style.display
    }
    
    ngOnDestroy() {
        if(this.componentRef) {
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
            this.componentRef.location.nativeElement.style.display = b ? "none" : null;
        }
    }
    
    @Input()
    set data(data: IOutputContainer) {
        if (data.title) {
            this.title = data.title;
        }
        if (data.angular && data.text) {
            this.error("Only either angular or text can be defined at once");
            this.hide = false;
            return;
        }
        if (data.angular) {
            this.angularContent = data.angular;
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

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        ExtcheckComponent,
        OutputContainerComponent,
        CustomOutputDirective,
    ],
    exports: [
        ExtcheckComponent,
    ],
    imports: [
        BrowserModule,
        TimUtilityModule,
        FormsModule,
        HttpClientModule,
        EditorModule,
    ],
})
export class ExtcheckModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
    }
}

const bootstrapFn = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(ExtcheckModule);
};

const angularJsModule = createDowngradedModule(bootstrapFn);
doDowngrade(angularJsModule, "csExtcheckRunner", ExtcheckComponent);
export const moduleDefs = [angularJsModule];
