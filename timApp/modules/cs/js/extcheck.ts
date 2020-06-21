import {IScope, ICompileService, IAugmentedJQuery, IAttributes} from "angular";
import {pluginBindings} from "tim/plugin/util";
import {csApp, CsController, uploadTemplate} from "./csPlugin";

interface IDivContent {
    classes: string;
    content?: string;
    isHTML?: boolean;
    isAngular?: boolean;
}

interface IOutputContainer {
    title?: IDivContent;
    content?: IDivContent;
    hide?: boolean;
}

interface IRunResult {
    output_boxes: IOutputContainer[];
    penalties: string[];
}

class ExtcheckController extends CsController {

    private containers?: IOutputContainer[];
    private penalty_container?: IOutputContainer;

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
                    content: {
                        classes: "",
                        content: data.penalties.map((e) => `<p class="penalty-text centermargin">${e}</p>`).join("\n"),
                        isHTML: true,
                    },
                };
            }
        }
    }
}

csApp.directive("compile", function($compile: ICompileService) {
    return function(scope: IScope, element: IAugmentedJQuery, attrs: IAttributes) {
        scope.$watch(
            function(scope2: IScope) {
                return scope2.$eval(attrs.compile as string) as string;
            },
            function(value: string) {
                element.html(value);
                // eslint-disable-next-line @typescript-eslint/no-explicit-any
                $compile(element.contents() as any)(scope);
            }
        );
    };
});

csApp.component("csExtcheckRunner", {
    bindings: pluginBindings,
    controller: ExtcheckController,
    require: {
        vctrl: "^timView",
    },
    template: `<div ng-class="::{'csRunDiv': $ctrl.attrs.borders}" class="type-{{::$ctrl.rtype}}">
    <tim-markup-error ng-if="$ctrl.markupError" [data]="$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="$ctrl.header" ng-bind-html="$ctrl.header"></h4>
    <p ng-if="$ctrl.stem" class="stem" ng-bind-html="$ctrl.stem"></p>`
    + uploadTemplate() +
    `<pre ng-if="$ctrl.viewCode && $ctrl.codeover">{{$ctrl.code}}</pre>
    <div class="csRunCode">
        <pre class="csRunPre" ng-if="$ctrl.viewCode && !$ctrl.codeunder && !$ctrl.codeover">{{$ctrl.precode}}</pre>
        <div class="csEditorAreaDiv">
            <div class="csrunEditorDiv" ng-if="!$ctrl.noeditor || $ctrl.viewCode">
            <textarea class="csRunArea csEditArea no-popup-menu"
                      rows="{{$ctrl.rows}}"
                      ng-model="$ctrl.usercode"
                      ng-trim="false"
                      ng-attr-placeholder="{{$ctrl.placeholder}}"> 
            </textarea>
            </div>
            <div class="csRunChanged" ng-if="$ctrl.usercode !== $ctrl.byCode && !$ctrl.hide.changed"></div>
            <div class="csRunNotSaved" ng-show="$ctrl.isUnSaved()"></div>
        </div>
        <pre class="csRunPost" ng-if="$ctrl.viewCode && !$ctrl.codeunder && !$ctrl.codeover">{{$ctrl.postcode}}</pre>
    </div>
    <div class="csInputDiv" ng-if="$ctrl.showInput && $ctrl.isInput">
        <p ng-if="$ctrl.inputstem" class="stem">{{$ctrl.inputstem}}</p>
        <div class="csRunCode">
            <textarea class="csRunArea csInputArea"
                                         rows={{::$ctrl.inputrows}}
                                         ng-model="$ctrl.userinput"
                                         ng-trim="false"
                                         placeholder="{{::$ctrl.inputplaceholder}}"></textarea>
        </div>
    </div>
    <div class="csArgsDiv" ng-if="$ctrl.showArgs && $ctrl.isInput"><label>{{::$ctrl.argsstem}} </label>
        <span><input type="text"
                     class="csArgsArea"
                     ng-model="$ctrl.userargs"
                     ng-trim="false"
                     placeholder="{{::$ctrl.argsplaceholder}}"></span>
    </div>
    <div ng-if="$ctrl.countItems" class="csPluginCountItems">
        <span ng-if="$ctrl.countLines">Lines: <span>{{$ctrl.lineCount}}</span></span>
        <span ng-if="$ctrl.countWords">Words: <span>{{$ctrl.wordCount}}</span></span>
        <span ng-if="$ctrl.countChars">Chars: <span>{{$ctrl.charCount}}</span></span>
    </div>
    <div ng-if="$ctrl.countError" class="csPluginCountError">
        <p>{{$ctrl.countError}}</p>
    </div>
    <p class="csRunSnippets" ng-if="$ctrl.buttons && $ctrl.buttons.length">
        <button ng-repeat="item in ::$ctrl.buttons" ng-click="$ctrl.addText(item)">{{$ctrl.addTextHtml(item)}}</button>
        &nbsp;&nbsp;
    </p>
    <div class="csRunMenuArea" ng-if="::!$ctrl.forcedupload">
        <p class="csRunMenu">
            <button ng-if="::$ctrl.isRun && $ctrl.buttonText()"
                    ng-disabled="$ctrl.isRunning || $ctrl.preventSave || ($ctrl.disableUnchanged && !$ctrl.isUnSaved() && $ctrl.isText)"
                    class="timButton btn-sm"
                    title="(Ctrl-S)"
                    ng-click="$ctrl.runCode()"
                    ng-bind-html="::$ctrl.buttonText()"></button>
            <a href="" ng-if="$ctrl.undoButton && $ctrl.isUnSaved()" title="{{::$ctrl.undoTitle}}"
            ng-click="$ctrl.tryResetChanges();">
                &nbsp;{{::$ctrl.undoButton}}
            </a>
            &nbsp&nbsp
            <span ng-if="$ctrl.savedText"
                    class="savedText"
                    ng-bind-html="$ctrl.savedText"></span>
            &nbsp&nbsp
            <tim-loading ng-if="$ctrl.isRunning"></tim-loading>
            &nbsp&nbsp<span ng-if="::$ctrl.isDocument">

            <a href="" ng-disabled="$ctrl.isRunning"
               ng-click="$ctrl.runDocument()">{{$ctrl.docLink}}</a>&nbsp&nbsp</span>
            <a href=""
               ng-if="::!$ctrl.nocode && ($ctrl.file || $ctrl.program)"
               ng-click="$ctrl.showCode()">{{$ctrl.showCodeLink}}</a>&nbsp&nbsp
            <a href=""
               ng-if="$ctrl.muokattu"
               ng-click="$ctrl.initCode()">{{::$ctrl.resetText}}</a>
            <a href=""
               ng-if="$ctrl.toggleEditor"
               ng-click="$ctrl.hideShowEditor()">{{$ctrl.toggleEditorText[$ctrl.noeditor ? 0 : 1]}}</a>
            <a href=""
               ng-if="!$ctrl.noeditor"
               ng-click="$ctrl.showOtherEditor()">
                {{$ctrl.editorText[$ctrl.editorModeIndecies[$ctrl.editorMode+1]]}}</a>&nbsp&nbsp
            <a href=""
               ng-if="::$ctrl.attrs.copyLink"
               ng-click="$ctrl.copyCode()">{{$ctrl.attrs.copyLink}}</a>
            <span ng-if="::$ctrl.showRuntime"
                  class="inputSmall"
                  style="float: right;"
                  title="Run time in sec {{$ctrl.runtime}}">{{$ctrl.oneruntime}}</span>
            <span ng-if="$ctrl.wrap.n!=-1 && !$ctrl.hide.wrap" class="inputSmall" style="float: right;" title="Put 0 to no wrap">
                <button class="timButton" title="Click to reformat text for given line length" ng-click="$ctrl.checkWrap()" style="font-size: x-small; height: 1.7em; padding: 1px; margin-top: -4px;">Wrap
                </button>
                <input type="checkbox" title="Check for automatic wrapping" ng-model="$ctrl.wrap.auto" style="position: relative;top: 0.3em;"/>
                <input type="text" title="Choose linelength for text.  0=no wrap" ng-pattern="/[-0-9]*/" ng-model="$ctrl.wrap.n" size="2"/>
            </span>
            <div ng-if="$ctrl.connectionErrorMessage" class="error" style="font-size: 12px" ng-bind-html="$ctrl.connectionErrorMessage"></div>
            </p>

    </div>
    <pre ng-if="$ctrl.viewCode && $ctrl.codeunder">{{$ctrl.code}}</pre>
    <div class="csRunErrorClass" ng-if="$ctrl.runError">
        <p class="pull-right">
            <tim-close-button ng-click="$ctrl.closeError()"></tim-close-button>
        </p>
        <pre class="csRunError" >{{$ctrl.error}}</pre>
        <p class="pull-right" style="margin-top: -1em">
            <tim-close-button ng-click="$ctrl.closeError()"></tim-close-button>
        </p>
    </div>
    <extcheck-output-container ng-if="$ctrl.penalty_container" data="$ctrl.penalty_container"></extcheck-output-container>
    <extcheck-output-container ng-repeat="container in $ctrl.containers" data="container"></extcheck-output-container>
    <p class="footer" ng-bind-html="$ctrl.footer"></p>
</div>`,
});

class OutputContainerController implements IOutputContainer {
    caret: string = "<span class='caret'></span>";

    title: IDivContent = {classes: ""};
    content: IDivContent = {classes: ""};
    hide: boolean = false;

    set data(data: IOutputContainer) {
        this.title = data.title ?? {classes: ""};
        this.content = data.content ?? {classes: ""};
        this.hide = !!data.hide;
    }
}

csApp.component("extcheckOutputContainer", {
    controller: OutputContainerController,
    bindings: {
        data: "<",
    },
    template: `
        <ng-container ng-if="$ctrl.title.content && $ctrl.content.content">
            <button ng-if="$ctrl.title.isAngular" ng-click="$ctrl.hide=!$ctrl.hide" ng-class="{'collapsed-button': $ctrl.hide}" class="title-button {{$ctrl.title.classes}}" compile="$ctrl.title.content + $ctrl.caret"></button>
            <button ng-if="!$ctrl.title.isAngular && $ctrl.title.isHTML" ng-click="$ctrl.hide=!$ctrl.hide" ng-class="{'collapsed-button': $ctrl.hide}" class="title-button {{$ctrl.title.classes}}" ng-bind-html="$ctrl.title.content + $ctrl.caret"></button>
            <button ng-if="!$ctrl.title.isAngular && !$ctrl.title.isHTML" ng-click="$ctrl.hide=!$ctrl.hide" ng-class="{'collapsed-button': $ctrl.hide}" class="title-button {{$ctrl.title.classes}}">{{$ctrl.title.content}}<span class='caret'></span></button>
            <ng-container ng-if="!$ctrl.hide">
                <div ng-if="$ctrl.content.isAngular" class="centermargin {{$ctrl.content.classes}}" compile="$ctrl.content.content"></div>
                <div ng-if="!$ctrl.content.isAngular && $ctrl.content.isHTML" class="centermargin {{$ctrl.content.classes}}" ng-bind-html="$ctrl.content.content"></div>
                <div ng-if="!$ctrl.content.isAngular && !$ctrl.content.isHTML">
                    <pre class="centermargin {{$ctrl.content.classes}}">{{$ctrl.content.content}}</pre>
                </div>
            </ng-container>
        </ng-container>`,
});

// TODO: replace extcheckOutputContainer with this. Requires a substitute for the compile directive
/*
@Component({
    selector: "extcheck-output-container",
    template: `
        <ng-container *ngIf="title.content && content.content">
            <button *ngIf="title.isAngular" (click)="hide=!hide" [ngClass]="{'collapsed-button': hide}" class="title-button {{title.classes}}" compile="title.content + caret"></button>
            <button *ngIf="!title.isAngular && title.isHTML" (click)="hide=!hide" [ngClass]="{'collapsed-button': hide}" class="title-button {{title.classes}}" [innerHTML]="title.content + caret"></button>
            <button *ngIf="!title.isAngular && !title.isHTML" (click)="hide=!hide" [ngClass]="{'collapsed-button': hide}" class="title-button {{title.classes}}">{{title.content}}<span class='caret'></span></button>
            <ng-container *ngIf="!hide">
                <div *ngIf="content.isAngular" class="centermargin {{content.classes}}" compile="content.content"></div>
                <div *ngIf="!content.isAngular && content.isHTML" class="centermargin {{content.classes}}" [innerHTML]="content.content"></div>
                <div *ngIf="!content.isAngular && !content.isHTML">
                    <pre class="centermargin {{content.classes}}">{{content.content}}</pre>
                </div>
            </ng-container>
        </ng-container>`,
})
class OutputContainerComponent implements IOutputContainer {
    title: IDivContent = {classes: ""};
    content: IDivContent = {classes: ""};
    hide: boolean = false;

    caret: string = "<span class='caret'></span>";

    /* tslint:disable-next-line:no-unsafe-any / <-- put * back in
    @Input()
    set data(data: IOutputContainer) {
        this.title = data.title ?? {classes: ""};
        this.content = data.content ?? {classes: ""};
        this.hide = !!data.hide;
    }
}

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        OutputContainerComponent,
    ],
    imports: [
        BrowserModule,
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
doDowngrade(angularJsModule, "extcheckOutputContainer", OutputContainerComponent);
export const moduleDefs = [angularJsModule];
*/
