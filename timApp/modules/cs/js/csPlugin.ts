import angular, {IAttributes, IController, IRootElementService, IScope} from "angular";
import * as t from "io-ts";
import $ from "jquery";
import {CellInfo} from "sagecell";
import {IAce, IAceEditor} from "tim/editor/ace-types";
import {ParCompiler} from "tim/editor/parCompiler";
import {IPluginAttributes} from "tim/plugin/util";
import {lazyLoadMany, lazyLoadTS} from "tim/util/lazyLoad";
import {$compile, $http, $interval, $sce, $timeout, $upload, $window} from "tim/util/ngimport";
import {getHeading, set} from "tim/util/timHelper";
import {Binding, fixDefExport, to} from "tim/util/utils";
import * as csparsons from "./cs-parsons/csparsons";

interface Simcir {
    setupSimcir(element: JQuery, data: {}): void;

    controller(element: JQuery): {data(): {connectors: any[], devices: any[]}};
}

interface GlowScriptWindow extends Window {
    runJavaScript?(text: string, args: string, input: string, wantsConsole: boolean): string;

    setDefLanguage?(language: string): void;

    getConsoleHeight?(): number;

    runWeScheme(s: string): void;
}

interface GlowScriptFrame extends HTMLIFrameElement {
    contentWindow: GlowScriptWindow;
}

// js-parsons is unused; just declare a stub to make TS happy
declare class ParsonsWidget {
    static _graders: any;

    constructor(data: {});

    init(a: string, b: string): void;

    show(): void;
}

interface AttrType {
    by: string;
    byCode: string;
    examples: string;
    type: string;
    path: string;
}

// TODO better name?
interface Vid {
    vid: string;
    w: any;
    h: any;
}

const csPluginStartTime = new Date();
/*
Sagea varten ks: https://github.com/sagemath/sagecell/blob/master/doc/embedding.rst#id3
*/

const csApp = angular.module("csApp", ["ngSanitize", "ngFileUpload"]);
const taunoPHIndex = 3;

function csLogTime(msg: string) {
    const d = new Date();
    const diff = d.getTime() - csPluginStartTime.getTime();
    console.log("cs: " + d.toLocaleTimeString() + " " + diff.valueOf() + " - " + msg);
}

csLogTime("directives done");

let taunoNr = 0;

// ==============================================================
// Global object to store every plugin that wants to
// know when pwd changes.  plugin must implement (or scope)
// setPWD method.  Also it should have property path = "user"
// to be able to register.

interface IPwd {
    savestate: string;
    path: string;
    attrs?: {path: string};

    setPWD(s: string): void;
}

class CWPD {
    pwdHolders: IPwd[] = [];
    currentPWD: {[i: string]: string} = {};

    constructor() {

    }

    register(scope: IPwd) {
        if (!this.isUser(scope)) {
            return;
        }
        this.pwdHolders.push(scope);
    }

    isUser(scope: IPwd) {
        return (scope.path === "user" || (scope.attrs && scope.attrs.path === "user"));
    }

    setPWD(pwd: string, scope: IPwd) {
        if (!this.isUser(scope) || !scope.savestate) {
            if (scope.setPWD) {
                scope.setPWD("/home/agent");
            }
            return;
        }

        this.currentPWD[scope.savestate] = pwd;
        for (const pwdHolder of this.pwdHolders) {
            if (pwdHolder.savestate === scope.savestate) {
                pwdHolder.setPWD(pwd);
            }
        }
    }

    getPWD(scope: IPwd) {
        if (scope.savestate) {
            return this.currentPWD[scope.savestate];
        }
        return "/home/agent";
    }
}

const ConsolePWD = new CWPD();

let browserName: string | undefined;

function getBrowserName() {
    return browserName = browserName || (() => {
        const userAgent = navigator ? navigator.userAgent.toLowerCase() : "other";

        if (userAgent.indexOf("chrome") > -1) {
            return "chrome";
        } else if (userAgent.indexOf("safari") > -1) {
            return "safari";
        } else if (userAgent.indexOf("msie") > -1) {
            return "ie";
        } else if (userAgent.indexOf("firefox") > -1) {
            return "firefox";
        }
        // if ( userAgent.match(/Trident.*rv\:11\./) ) return "ie";'
        if (userAgent.indexOf("trident/") >= 0) {
            return "ie";
        }
        return userAgent;
    })();
}

let isAcrobatInstalled: boolean | undefined;

function hasAcrobatInstalled() {
    if (isAcrobatInstalled !== undefined) {
        return isAcrobatInstalled;
    }

    function getActiveXObject(name: string) {
        try {
            return new ActiveXObject(name);
        } catch (e) {
        }
    }

    isAcrobatInstalled = getActiveXObject("AcroPDF.PDF") || getActiveXObject("PDF.PdfCtrl");
    return isAcrobatInstalled;
}

const csJSTypes = ["js", "glowscript", "vpython", "html", "processing", "wescheme"];

// =================================================================================================================
// Known upload files

const uploadFileTypes = ["pdf", "xml"];

function is(types: string[], file: string) {
    if (!file) {
        return false;
    }
    file = file.toLowerCase();
    for (let i = 0; i < types.length; i++) {
        const t = types[i];
        if (file.endsWith(t)) {
            if (t !== "pdf") {
                return true;
            }
            if (navigator.mimeTypes.namedItem("application/pdf") || hasAcrobatInstalled() || getBrowserName() === "ie") {
                return true;
            }
            return false;
        }
    }
    return false;
}

function uploadFileTypesName(file: string) {
    const s = file.split("\\").pop();
    if (!s) {
        return undefined;
    }
    return s.split("/").pop();
}

function resizeIframe(obj: HTMLFrameElement) {
    const contentWindow = obj.contentWindow;
    if (contentWindow) {
        obj.style.height = contentWindow.document.body.scrollHeight + "px";
    }
}

async function loadSimcir() {
    const modules = await lazyLoadMany(["simcir", "simcir/basicset", "simcir/library", "simcir/oma-kirjasto"]);
    return modules[0] as Simcir;
}

// =================================================================================================================
// Things for known languages

class LanguageTypes {
    // What are known language types (be careful not to include partial word):
    runTypes = ["pascal", "fortran", "css", "jypeli", "scala", "java", "graphics", "cc", "c++", "shell", "vpython", "py2", "py", "fs", "clisp",
        "jjs", "psql", "sql", "alloy", "text", "cs", "run", "md", "js", "glowscript", "sage", "simcir",
        "xml", "octave", "lua", "swift", "mathcheck", "html", "processing", "rust", "r", "wescheme", "ping", "kotlin",
        "smalltalk"];

    // For editor modes see: http://ace.c9.io/build/kitchen-sink.html ja sieltä http://ace.c9.io/build/demo/kitchen-sink/demo.js
    aceModes = ["pascal", "fortran", "css", "csharp", "scala", "java", "java", "c_cpp", "c_cpp", "sh", "python", "python", "python", "fsharp", "lisp",
        "javascript", "sql", "sql", "alloy", "text", "csharp", "run", "text", "javascript", "javascript", "python", "json",
        "xml", "matlab", "lua", "swift", "text", "html", "javascript", "text", "r", "scheme", "text", "kotlin",
        "text"];

    // What are known test types (be careful not to include partial word):
    testTypes = ["ccomtest", "jcomtest", "comtest", "scomtest"];
    unitTestTypes = ["junit", "unit"];

    // If test type is comtest, how to change it for specific languages
    impTestTypes: {[i: string]: string | undefined} = {
        "cs": "comtest",
        "console": "comtest",
        "cc": "ccomtest",
        "java": "jcomtest",
        "scala": "scomtest",
        "c++": "ccomtest",
    };
    // If test type is unit, how to change it for specific languages
    impUnitTestTypes: {[i: string]: string | undefined} = {
        "cs": "nunit",
        "console": "nunit",
        "cc": "cunit",
        "java": "junit",
        "scala": "junit",
        "c++": "cunit",
    };

    whatIsIn(types: string[], type: string, def: string) {

        if (!type) {
            return def;
        }
        type = type.toLowerCase();
        for (let i = 0; i < types.length; i++) {
            if (type.indexOf(types[i]) >= 0) {
                return types[i];
            }
        }
        return def;
    }

    whatIsInAce(types: string[], type: string, def: string) {

        if (!type) {
            return def;
        }
        type = type.toLowerCase();
        for (let i = 0; i < types.length; i++) {
            if (type.indexOf(types[i]) >= 0) {
                return this.aceModes[i];
            }
        }
        return def;
    }

    isAllType(type: string) {

        if (!type) {
            return false;
        }
        type = type.toLowerCase();
        if (!type.startsWith("all")) {
            return false;
        }
        if (type.match(/^all[^a-z0-9]/)) {
            return true;
        }
        return false;

    }

    getRunType(type: string, def: string) {

        return this.whatIsIn(this.runTypes, type, def);
    }

    getAceModeType(type: string, def: string) {
        if (def) {
            return def;
        }
        return this.whatIsInAce(this.runTypes, type, def);
    }

    getTestType(type: string, language: string, def: string) {

        const t = this.whatIsIn(this.testTypes, type, def);
        if (t !== "comtest") {
            return t;
        }
        const lt = this.whatIsIn(this.runTypes, language, "console");
        const impt = this.impTestTypes[lt];
        if (impt) {
            return impt;
        }
        return t;
    }

    getUnitTestType(type: string, language: string, def: string) {

        const t = this.whatIsIn(this.unitTestTypes, type, def);
        if (t !== "unit") {
            return t;
        }
        const lt = this.whatIsIn(this.runTypes, language, "console");
        const impt = this.impUnitTestTypes[lt];
        if (impt) {
            return impt;
        }
        return t;
    }

    isInArray(word: string, array: string[]) {
        for (let i = 0; i < array.length; i++) {
            if (word === array[i]) {
                return true;
            }
        }
        return false;
    }
}

const languageTypes = new LanguageTypes();

// Wrap given text to max n chars length lines spliting from space
function wrapText(s: string, n: number) {
    const lines = s.split("\n");
    let needJoin = false;
    for (let i = 0; i < lines.length; i++) {
        let line = lines[i];
        // lines[i] = "";
        let sep = "";
        if (line.length > n) {
            lines[i] = "";
            while (true) {
                let p = -1;
                if (line.length > n) {
                    p = line.lastIndexOf(" ", n);
                    if (p < 0) {
                        p = line.indexOf(" ");
                    } // long line
                }
                if (p < 0) {
                    lines[i] += sep + line;
                    break;
                }
                lines[i] += sep + line.substring(0, p);
                line = line.substring(p + 1);
                if (i + 1 < lines.length && (lines[i + 1].length > 0 && (" 0123456789-".indexOf(lines[i + 1][0]) < 0))) {
                    lines[i + 1] = line + " " + lines[i + 1];
                    needJoin = true;
                    break;
                }
                sep = "\n";
                needJoin = true;
            }
        }
    }
    if (needJoin) {
        return {modified: true, s: lines.join("\n")};
    }
    return {modified: false, s: s};
}

// =================================================================================================================

function removeXML(s: string) {
    s = s.replace(/^<\?xml [^>]*\?>/, "");
    s = s.replace(/(<svg [^>]*height="[0-9]+)pt/, "$1");
    s = s.replace(/(<svg [^>]*width="[0-9]+)pt/, "$1");
    return s;
}

function iotaPermutation(n: number) {
    const permutation = [];
    for (let i = 0; i < n; i++) {
        permutation.push(i);
    }
    return permutation;
}

function commentTrim(s: string) {
    if (!s) {
        return "";
    }
    const n = s.indexOf("//\n");
    if (n !== 0) {
        return s;
    }
    return s.substr(3);
}

function makeTemplate() {
    return `<div class="csRunDiv type-{{$ctrl.rtype}}">
    <p>Here comes header</p>
    <p ng-if="$ctrl.stem" class="stem" ng-bind-html="$ctrl.stem"></p>
    <div ng-if="$ctrl.isSimcir || $ctrl.isTauno">
    <p ng-if="$ctrl.taunoOn" class="pluginHide""><a ng-click="$ctrl.hideTauno()">{{$ctrl.hideTaunoText}}</a></p>
    <div><p></p></div>
    <p ng-if="!$ctrl.taunoOn" class="pluginShow"><a ng-click="$ctrl.showTauno()">{{$ctrl.showTaunoText}}</a></p>
    <p ng-if="$ctrl.taunoOn && $ctrl.isTauno"
       class="pluginHide">
        <a ng-click="$ctrl.copyTauno()">{{$ctrl.copyFromTaunoText}}</a> |
        <a ng-click="$ctrl.hideTauno()">{{$ctrl.hideTaunoText}}</a></p>
    <p ng-if="$ctrl.taunoOn && $ctrl.isTauno" class="taunoOhje">
        {{$ctrl.taunoOhjeText}}</a></p>
    <p ng-if="$ctrl.taunoOn && !$ctrl.noeditor && $ctrl.isSimcir" class="pluginHide"">
    <a ng-click="$ctrl.copyFromSimcir()">copy from SimCir</a>
    | <a ng-click="$ctrl.copyToSimcir()">copy to SimCir</a> | <a ng-click="$ctrl.hideTauno()">hide SimCir</a></p>
    </div>
    <div ng-if="$ctrl.upload" class="form-inline small">
        <div class="form-group small"> {{$ctrl.uploadstem}}: <input type="file" ngf-select="$ctrl.onFileSelect($file)">
            <span ng-show="$ctrl.file.progress >= 0 && !$ctrl.file.error"
                  ng-bind="$ctrl.file.progress < 100 ? 'Uploading... ' + $ctrl.file.progress + '%' : 'Done!'"></span>
        </div>
        <div class="error" ng-show="$ctrl.file.error" ng-bind="$ctrl.file.error"></div>
        <div ng-if="$ctrl.uploadresult"><span ng-bind-html="$ctrl.uploadresult"></span></div>
    </div>
    <div ng-show="$ctrl.isAll" style="float: right;">{{$ctrl.languageText}}
        <select ng-model="$ctrl.selectedLanguage" ng-options="$ctrl.progLanguages" ng-required></select>
    </div>
    <pre ng-if="$ctrl.viewCode && $ctrl.codeover">{{$ctrl.code}}</pre>
    <div class="csRunCode">
        <pre class="csRunPre" ng-if="$ctrl.viewCode && !$ctrl.codeunder && !$ctrl.codeover">{{$ctrl.precode}}</pre>
        <div class="csEditorAreaDiv">
            <div class="csrunEditorDiv"><textarea class="csRunArea csEditArea no-popup-menu"
                                                  ng-hide="$ctrl.noeditor && !$ctrl.viewCode" rows="{{$ctrl.rows}}"
                                                  ng-model="$ctrl.usercode"
                                                  ng-trim="false"
                                                  ng-attr-placeholder="{{$ctrl.placeholder}}"></textarea>
            </div>
            <div class="csRunChanged" ng-if="$ctrl.usercode !== $ctrl.byCode"></div>
        </div>
        <pre class="csRunPost" ng-if="$ctrl.viewCode && !$ctrl.codeunder && !$ctrl.codeover">{{$ctrl.postcode}}</pre>
    </div>
    <div ng-if="$ctrl.isSage" class="computeSage no-popup-menu"></div>
    <div class="csInputDiv" ng-hide="!$ctrl.showInput || !$ctrl.isInput"><p ng-show="$ctrl.inputstem" class="stem">
        {{$ctrl.inputstem}}</p>
        <div class="csRunCode"><textarea class="csRunArea csInputArea"
                                         rows={{$ctrl.inputrows}}
                                         ng-model="$ctrl.userinput"
                                         ng-trim="false"
                                         placeholder="{{$ctrl.inputplaceholder}}"></textarea></div>
    </div>
    <div class="csArgsDiv" ng-hide="!$ctrl.showArgs || !$ctrl.isInput"><label>{{$ctrl.argsstem}} </label>
        <span><input type="text"
                     class="csArgsArea"
                     ng-model="$ctrl.userargs"
                     ng-trim="false"
                     placeholder="{{$ctrl.argsplaceholder}}"></span>
    </div>
    <p class="csRunSnippets" ng-if="$ctrl.buttons">
        <button ng-repeat="item in $ctrl.buttons" ng-click="$ctrl.addText(item)">{{$ctrl.addTextHtml(item)}}</button>
        &nbsp;&nbsp;
    </p>
    <div class="csRunMenuArea" ng-if="!$ctrl.forcedupload">
        <p class="csRunMenu">
            <button ng-if="$ctrl.isRun" ng-disabled="$ctrl.isRunning" title="(Ctrl-S)" ng-click="$ctrl.runCode()"
                    ng-bind-html="$ctrl.buttonText"></button>
            &nbsp&nbsp
            <button ng-if="$ctrl.isTest" ng-disabled="$ctrl.isRunning" ng-click="$ctrl.runTest()">Test</button>
            &nbsp&nbsp
            <button ng-if="$ctrl.isUnitTest"
                    ng-disabled="$ctrl.isRunning"
                    ng-click="$ctrl.runUnitTest()">UTest
            </button>
            &nbsp&nbsp<span ng-if="$ctrl.isDocument">
            <a href="" ng-disabled="$ctrl.isRunning"
               ng-click="$ctrl.runDocument()">{{$ctrl.docLink}}</a>&nbsp&nbsp</span>
            <a href=""
               ng-if="!$ctrl.nocode && ($ctrl.file || $ctrl.attrs.program)"
               ng-click="$ctrl.showCode()">{{$ctrl.showCodeLink}}</a>&nbsp&nbsp
            <a href=""
               ng-if="$ctrl.muokattu"
               ng-click="$ctrl.initCode()">{{$ctrl.resetText}}</a>
            <a href=""
               ng-if="$ctrl.toggleEditor"
               ng-click="$ctrl.hideShowEditor()">{{$ctrl.toggleEditorText[$ctrl.noeditor ? 0 : 1]}}</a>
            <a href=""
               ng-if="!$ctrl.noeditor"
               ng-click="$ctrl.showOtherEditor()">
                {{$ctrl.editorText[$ctrl.editorModeIndecies[$ctrl.editorMode+1]]}}
            </a>
            <span ng-if="$ctrl.showRuntime"
                  class="inputSmall"
                  style="float: right;"
                  title="Run time in sec {{$ctrl.runtime}}">{{$ctrl.oneruntime}}</span>
            <span ng-if="$ctrl.wrap!=-1" class="inputSmall" style="float: right;"><label
                    title="Put 0 to no wrap">wrap: <input type="text"
                                                          ng-pattern="/[-0-9]*/"
                                                          ng-model="$ctrl.wrap"
                                                          size="2"/></label></span></p>
    </div>
    <div ng-if="$ctrl.isSage" class="outputSage no-popup-menu"></div>
    <pre ng-if="$ctrl.viewCode && $ctrl.codeunder">{{$ctrl.code}}</pre>
    <p class="unitTestGreen" ng-if="$ctrl.runTestGreen">&nbsp;ok</p>
    <pre class="unitTestRed" ng-if="$ctrl.runTestRed">{{$ctrl.comtestError}}</pre>
    <pre class="csRunError" ng-if="$ctrl.runError">{{$ctrl.error}}</pre>
    <pre class="console" ng-show="$ctrl.result">{{$ctrl.result}}</pre>
    <div class="htmlresult" ng-if="$ctrl.htmlresult"><span ng-bind-html="$ctrl.svgImageSnippet()"></span></div>
    <span class="csrunPreview"></span><img ng-if="$ctrl.imgURL" class="grconsole" ng-src="{{$ctrl.imgURL}}" alt=""/>
    <video ng-if="$ctrl.wavURL" ng-src="{{$ctrl.wavURL}}" type="video/mp4" controls="" autoplay="true" width="300"
           height="40"></video>
    <div ng-if="$ctrl.docURL" class="docurl"><p align="right" style="position: absolute; margin-left: 790px;">
        <a ng-click="$ctrl.closeDocument()">X</a></p>
        <iframe width="800" height="600" ng-src="{{$ctrl.docURL}}" target="csdocument" allowfullscreen/>
    </div>
    <p class="plgfooter">Here comes footer</p>
</div>
`;
}

function insertAtCaret(txtarea: HTMLTextAreaElement, text: string) {
    const doc = document as any;
    const scrollPos = txtarea.scrollTop;
    let strPos = 0;
    const br = ((txtarea.selectionStart || txtarea.selectionStart === 0) ?
        "ff" : (doc.selection ? "ie" : false));
    if (br === "ie") {
        txtarea.focus();
        const range = doc.selection.createRange();
        range.moveStart("character", -txtarea.value.length);
        strPos = range.text.length;
    } else if (br === "ff") {
        strPos = txtarea.selectionStart;
    }

    const front = (txtarea.value).substring(0, strPos);
    const back = (txtarea.value).substring(strPos, txtarea.value.length);
    txtarea.value = front + text + back;
    strPos = strPos + text.length;
    if (br === "ie") {
        txtarea.focus();
        const range = doc.selection.createRange();
        range.moveStart("character", -txtarea.value.length);
        range.moveStart("character", strPos);
        range.moveEnd("character", 0);
        range.select();
    } else if (br === "ff") {
        txtarea.selectionStart = strPos;
        txtarea.selectionEnd = strPos;
        txtarea.focus();
    }
    txtarea.scrollTop = scrollPos;
}

const mathcheckLoaded = false;

async function loadMathcheck() {
    if (mathcheckLoaded) {
        return;
    }
    await $.ajax({
        dataType: "script",
        cache: true,
        url: "//cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=AM_HTMLorMML",
    });
}

function getInt(s: string) {
    const n = parseInt(s);
    if (isNaN(n)) {
        return 0;
    }
    return n;
}

function countChars(s: string, c: string) {
    let n = 0;
    for (let i = 0; i < s.length; n += +(c === s[i++])) {
    }
    return n;
}

function updateEditSize(scope: CsController) {
    if (!scope) {
        return;
    }
    if (!scope.usercode) {
        return;
    }
    let n = countChars(scope.usercode, "\n") + 1;
    if (n < scope.minRows) {
        n = scope.minRows;
    }
    if (n > scope.maxRows) {
        n = scope.maxRows;
    }
    if (n < 1) {
        n = 1;
    }
    scope.rows = n;
}

function ifIs(value: string, name: string, def: string | number) {
    if (!value && !def) {
        return "";
    }
    if (!value) {
        return `${name}="${def}" `;
    }
    return `${name}="${value}" `;
}

function doVariables(v: string, name: string) {
    if (!v) {
        return "";
    }
    let r = "";
    const va = v.split(";");
    for (const n of va) {
        const nv = n.trim();
        if (nv) {
            r += name + nv + "&";
        }
    }
    return r.replace(/ /g, "");
}

interface IExtraMarkup {
    document?: boolean;
}

// from https://github.com/teamdigitale/italia-ts-commons/blob/de4d85a2a1502da54f78aace8c6d7b263803f115/src/types.ts
export function withDefault<T extends t.Any>(
    type: T,
    defaultValue: t.TypeOf<T>,
): t.Type<t.TypeOf<T>, any> {
    return new t.Type(
        type.name,
        (v: any): v is T => type.is(v),
        (v: any, c: any) =>
            type.validate(v !== undefined && v !== null ? v : defaultValue, c),
        (v: any) => type.encode(v),
    );
}

/**
 * This defines the required format for the csPlugin YAML markup.
 * All fields in the markup are optional (as indicated by t.partial function).
 *
 * If a field does not match the type, an error will be shown to the user
 * and the plugin won't work until the problem is fixed.
 */
const CsMarkupOptional = t.partial({
    answerLimit: t.number,
    buttonText: t.string,
    by: t.string,
    byCode: t.string,
    file: t.string,
    filename: t.string,
    footer: t.string,
    fullhtml: t.string,
    header: t.string,
    height: t.string,
    html: t.string,
    indices: t.string,
    lazy: t.boolean,
    mode: t.string,
    parsonsmaxcheck: t.number,
    path: t.string,
    program: t.string,
    replace: t.string,
    runeverytime: t.boolean,
    savestate: t.string,
    scripts: t.string,
    stem: t.string,
    table: t.string,
    taunotype: t.string,
    treplace: t.string,
    uploadbycode: t.boolean,
    uploadstem: t.string,
    variables: t.string,
    width: t.string,
});

const CsMarkupDefaults = t.type({
    argsplaceholder: withDefault(t.string, "Write your program args here"),
    argsstem: withDefault(t.string, "Args:"),
    autoupdate: withDefault(t.boolean, false),
    parsonsnotordermatters: withDefault(t.boolean, false),
    blind: withDefault(t.boolean, false),
    button: withDefault(t.string, ""),
    canvasHeight: withDefault(t.number, 300),
    canvasWidth: withDefault(t.number, 700),
    codeover: withDefault(t.boolean, false),
    codeunder: withDefault(t.boolean, false),
    cols: withDefault(t.number, 10),
    cssPrint: withDefault(t.boolean, false),
    editorMode: withDefault(t.number, -1),
    editorModes: withDefault(t.union([t.string, t.number]), "01"),
    highlight: withDefault(t.string, "Highlight"),
    iframe: withDefault(t.boolean, false),
    iframeopts: withDefault(t.string, ""),
    indent: withDefault(t.number, -1),
    initSimcir: withDefault(t.string, ""),
    "style-args": withDefault(t.string, ""),
    "style-words": withDefault(t.string, ""),
    inputplaceholder: withDefault(t.string, "Write your input here"),
    inputrows: withDefault(t.number, 1),
    inputstem: withDefault(t.string, ""),
    isHtml: withDefault(t.boolean, false),
    jsparsons: withDefault(t.string, "JS-Parsons"),
    justSave: withDefault(t.boolean, false),
    lang: withDefault(t.string, "fi"),
    maxrows: withDefault(t.number, 100),
    noConsoleClear: withDefault(t.boolean, false),
    nocode: withDefault(t.boolean, false),
    noeditor: withDefault(t.boolean, false),
    normal: withDefault(t.string, "Normal"),
    norun: withDefault(t.boolean, false),
    nosave: withDefault(t.boolean, false),
    open: withDefault(t.boolean, false),
    parsons: withDefault(t.string, "Parsons"),
    placeholder: withDefault(t.string, ""),
    resetText: withDefault(t.string, "Reset"),
    rows: withDefault(t.number, 1),
    selectedLanguage: withDefault(t.string, "text"),
    showCodeOff: withDefault(t.string, "Hide extra code"),
    showCodeOn: withDefault(t.string, "Show all code"),
    showRuntime: withDefault(t.boolean, false),
    toggleEditor: withDefault(t.boolean, false),
    type: withDefault(t.string, "cs"),
    upload: withDefault(t.boolean, false),
    userargs: withDefault(t.string, ""),
    usercode: withDefault(t.string, ""),
    userinput: withDefault(t.string, ""),
    validityCheck: withDefault(t.string, ""),
    validityCheckMessage: withDefault(t.string, ""),
    viewCode: withDefault(t.boolean, false),
    words: withDefault(t.boolean, false),
    wrap: withDefault(t.number, -1),
});

const CsMarkup = t.intersection([CsMarkupOptional, CsMarkupDefaults]);

interface ICsAttributes extends t.TypeOf<typeof CsMarkup> {
}

type ICsPluginAttributes = IPluginAttributes<ICsAttributes, null>;

type ICsAttributesImpl = Required<ICsAttributes>;

// from io-ts readme
function getPaths<A>(v: t.Validation<A>): string[] {
    return v.fold((errors) => errors.map((error) => error.context.map(({key}) => key).join(".")), () => ["no errors"]);
}

class CsController implements IController {
    private static $inject = ["$scope", "$element", "$attrs"];

    private aceEditor?: IAceEditor;
    private aelement: object;
    private attrs: ICsAttributes;
    // autoupdate: number;
    // byCode: string; method
    private canvas?: HTMLCanvasElement;
    private canvasConsole: {log: (...args: string[]) => void};
    // canvasHeight: number;
    // canvasWidth: number;
    private code: string;
    private codeInitialized: boolean;
    private comtestError?: string;
    private contentWindow: GlowScriptWindow;
    private copyingFromTauno: boolean;
    private csparson: any;
    // cssPrint: boolean;
    private cursor: string;
    private docLink: string;
    private docURL?: string;
    private edit: HTMLTextAreaElement;
    private editArea?: Element;
    private editorIndex: number;
    private editorMode: number;
    private editorModeIndecies: number[];
    // editorModes: string;
    // element0: HTMLElement;
    // english: boolean; method
    private error?: string;
    private errors: string[];
    // private file: {};
    private fileError?: string;
    private fileProgress?: number;
    private fullhtml: string;
    private glowscript: boolean;
    private gsDefaultLanguage: string;
    // private height: string;
    private htmlresult: string;
    private iframe: boolean;
    private iframeClientHeight: number;
    private iframeLoadTries: number;
    // private iframeopts: string;
    private imgURL: string;
    private indent: number;
    // private indices: string;
    private initUserCode: boolean;
    private irrotaKiinnita: string;
    // private isAll: boolean; method
    private isFirst: boolean;
    // private isHtml: boolean;
    // private isMathCheck: boolean; method
    private isRunning: boolean = false;
    // private isSage: boolean; method
    // private isSimcir: boolean; method
    // private isText: boolean; method
    // private jstype: string; unused
    // private lang: string;
    private lastJS: string;
    private lastMD: string;
    private lastUserargs?: string;
    private lastUserinput?: string;
    private localcode?: string;
    private maxRows: number;
    // private minRows: number; method
    // private mode: string; method
    private muokattu: boolean;
    // private noConsoleClear: boolean;
    private noeditor: boolean;
    // private nosave: boolean;
    private oneruntime: string;
    private out: {write: Function, writeln: Function, canvas: Element};
    private parson?: ParsonsWidget;
    private parsonsId?: Vid;
    private plugin?: string; // maybe method
    private postcode: string = "";
    private precode: string = "";
    private preview: HTMLElement; // TODO optional?
    // private replace: string;
    private result?: string;
    // private rows: number;
    // private rtype: string; method
    private runError?: string | boolean;
    private runned: boolean = false;
    private runSuccess: boolean;
    private runTestGreen: boolean = false;
    private runTestRed: boolean = false;
    private runtime?: string;
    private sageArea?: Element;
    private sageButton?: HTMLElement;
    private sagecellInfo?: CellInfo;
    private sageInput?: HTMLInputElement;
    private sageOutput?: Element;
    private selectedLanguage!: string;
    // private showCodeLink: string; // method
    // private showCodeOff: string;
    // private showCodeOn: string;
    private simcir?: JQuery;
    // private table: string;
    private taskId?: string;
    private taunoElem: HTMLElement;
    private taunoId: string; // TODO optional?
    private taunoOn: boolean;
    // private taunotype: string;
    // private tiny: boolean; method
    private tinyErrorStyle: Partial<CSSStyleDeclaration> = {};
    // private type: string;
    // private upload: boolean; method
    private uploadedFile?: string;
    private uploadedType?: string;
    private uploadresult?: string;
    private userargs: string = "";
    private usercode: string = "";
    private userinput: string = "";
    // private validityCheck: string;
    // private validityCheckMessage: string;
    // private variables: string;
    private viewCode: boolean;
    private wavURL: string = "";
    // private width: string;
    // private words: boolean;
    // private wrap: number;

    // These are used only in $doCheck to keep track of the old values.
    private dochecks: {
        userinput?: string;
        userargs?: string;
        usercode?: string
    } = {};

    // Binding that has all the data as a JSON string.
    private json!: Binding<string, "@">;

    constructor(private scope: IScope, private element: IRootElementService, private attributes: IAttributes) {
        this.byCode = "";
        this.errors = [];
        this.taunoOn = false;
        this.result = "";
        this.htmlresult = "";
        this.imgURL = "";
        this.viewCode = false;
        this.runSuccess = false;
        this.copyingFromTauno = false;
        this.lastMD = "";
        this.canvasConsole = {
            log: (...args: string[]) => {
                let res = "";
                let sep = "";
                for (const a of args) {
                    res += sep + a;
                    sep = " ";
                }
                this.writeln(res);
            },
        };

        this.lastJS = "";
        this.iframeClientHeight = -1;
        this.cursor = "⁞"; // \u0383"; //"\u0347"; // "\u02FD";
        this.isFirst = true;
        this.docLink = "Document";
        this.muokattu = false;
    }

    svgImageSnippet() {
        return $sce.trustAsHtml(this.htmlresult);
    }

    $onInit() {
        const parsed = JSON.parse(this.json);
        const validated = CsMarkup.decode(parsed.markup);
        if (validated.isLeft()) {
            console.log(parsed);
            console.log(getPaths(validated));
            throw new Error("faillllled");
        }
        let x: string = validated.value.selectedLanguage;
        this.attrs = validated.value;
        console.log(this.attrs);
        throw new Error("asdasdasd");
        this.byCode = this.attrs.by || this.attrs.byCode;
        const element = this.element;
        const attrs = this.attrs;
        let kind;
        switch (element[0].tagName.toLowerCase()) {
            case "cs-runner":
                kind = "console";
                break;
            case "cs-jypeli-runner":
                kind = "jypeli";
                break;
            case "cs-comtest-runner":
                kind = "comtest";
                break;
            case "cs-runner-input":
                kind = "console";
                break;
            case "cs-jypeli-runner-input":
                kind = "jypeli";
                break;
            case "cs-comtest-runner-input":
                kind = "comtest";
                break;
            case "cs-tauno-runner":
                kind = "tauno";
                break;
            case "cs-tauno-runner-input":
                kind = "tauno";
                break;
            case "cs-parsons-runner":
                kind = "parsons";
                break;
            case "cs-sage-runner":
                kind = "sage";
                break;
            case "cs-simcir-runner":
                kind = "simcir";
                break;
            case "cs-text-runner":
                kind = "text";
                break;
            case "cs-wescheme-runner":
                kind = "wescheme";
                break;
            default:
                console.warn("Unrecognized csplugin tag type, falling back to 'console'");
                kind = "console";
                break;
        }
        this.taunoElem = element[0].children[taunoPHIndex] as HTMLElement; // Check this carefully, where is Tauno placeholder
        this.plugin = element.parent().attr("data-plugin");
        this.taskId = element.parent().attr("id");
        this.element = element;
        if (this.scope.$parent.$$prevSibling) {
            this.isFirst = false;
        }
        set(this, attrs, "lang", "fi");
        const english = this.lang === "en";
        this.english = english;

        if ((kind === "tauno" || kind === "simcir")) { // Tauno translations
            let taunoText = "Tauno";
            if (kind === "simcir") {
                taunoText = "SimCir";
            }
            this.hideTaunoText = (english ? "hide " : "piilota ") + taunoText;
            this.showTaunoText = (english ? "Click here to show " : "Näytä ") + taunoText;
            this.taunoOhjeText = english ?
                'Copy the code you made by Tauno by pressing the link "copy from Tauno". Then press Run button. Note that the running code may have different code than in Tauno!' :
                'Kopioi Taunolla tekemäsi koodi "kopioi Taunosta"-linkkiä painamalla. Sitten paina Aja-painiketta. Huomaa, että ajossa voi olla eri taulukko kuin Taunossa!';
            this.copyFromTaunoText = english ? "copy from Tauno" : "kopioi Taunosta";
            this.copyFromSimCirText = english ? "copy from SimCir" : "kopioi SimCiristä";
            this.copyToSimCirText = english ? "copy to SimCir" : "kopioi SimCiriin";
        }

        this.languageText = english ? "language: " : "kieli: ";

        set(this, attrs, "type", "cs");
        const rt = languageTypes.getRunType(this.type, "text");
        let iupload = false;
        let inoeditor = false;
        let inocode = false;
        if (this.type === "upload") {
            iupload = true;
            inoeditor = true;
            inocode = true;
            this.forcedupload = true;
        }

        this.isText = rt === "text" || rt === "xml" || rt === "css";
        this.rtype = rt;
        this.isSage = rt === "sage";
        this.isMathCheck = rt === "mathcheck";
        this.isSimcir = kind === "simcir";
        this.tiny = this.type.indexOf("tiny") >= 0;
        const isArgs = this.type.indexOf("args") >= 0;

        set(this, attrs, "file");
        set(this, attrs, "viewCode", false);
        set(this, attrs, "filename");
        set(this, attrs, "nosave", false);
        set(this, attrs, "upload", iupload);
        if (this.attrs.uploadbycode) {
            this.upload = true;
        }
        set(this, attrs, "uploadstem");
        set(this, attrs, "nocode", inocode);
        set(this, attrs, "lang");
        set(this, attrs, "width");
        set(this, attrs, "height");
        set(this, attrs, "table");
        set(this, attrs, "variables");
        set(this, attrs, "indices");
        set(this, attrs, "replace");
        set(this, attrs, "taunotype");
        set(this, attrs, "stem");
        set(this, attrs, "iframe", false);
        if (languageTypes.isInArray(rt, ["glowscript", "vpython"])) {
            this.glowscript = true;
            this.iframe = true;
        }
        set(this, attrs, "codeunder", false);
        set(this, attrs, "codeover", false);
        set(this, attrs, "open", !this.isFirst);
        set(this, attrs, "rows", 1);
        set(this, attrs, "cols", 10);
        set(this, attrs, "maxrows", 100);
        set(this, attrs, "cssPrint", false); // For changing code editors to pre-defined paragraphs.
        set(this, attrs, "attrs.bycode");
        set(this, attrs, "placeholder", this.tiny ? "" : english ? "Write your code here" : "Kirjoita koodi tähän:");
        set(this, attrs, "inputplaceholder", english ? "Write your input here" : "Kirjoita syöte tähän");
        set(this, attrs, "argsplaceholder", this.isText ? (english ? "Write file name here" : "Kirjoita tiedoston nimi tähän") : (english ? "Write your program args here" : "Kirjoita ohjelman argumentit tähän"));
        set(this, attrs, "argsstem", this.isText ? (english ? "File name:" : "Tiedoston nimi:") : (english ? "Args:" : "Args"));
        set(this, attrs, "userinput", "");
        set(this, attrs, "userargs", this.isText && isArgs ? this.filename : "");
        set(this, attrs, "selectedLanguage", rt);
        set(this, attrs, "inputstem", "");
        set(this, attrs, "inputrows", 1);
        set(this, attrs, "toggleEditor", this.isSimcir ? "True" : false);
        set(this, attrs, "indent", -1);
        set(this, attrs, "user_id");
        set(this, attrs, "isHtml", false);
        set(this, attrs, "autoupdate", false);
        set(this, attrs, "canvasWidth", 700);
        set(this, attrs, "canvasHeight", 300);
        set(this, attrs, "button", "");
        set(this, attrs, "noeditor", this.isSimcir ? "True" : inoeditor);
        set(this, attrs, "norun", false);
        set(this, attrs, "normal", english ? "Normal" : "Tavallinen");
        set(this, attrs, "highlight", "Highlight");
        set(this, attrs, "parsons", "Parsons");
        set(this, attrs, "jsparsons", "JS-Parsons");
        set(this, attrs, "editorMode", -1);
        set(this, attrs, "showCodeOn", english ? "Show all code" : "Näytä koko koodi");
        set(this, attrs, "showCodeOff", english ? "Hide extra code" : "Piilota muu koodi");
        set(this, attrs, "resetText", english ? "Reset" : "Alusta");
        set(this, attrs, "blind", false);
        set(this, attrs, "words", false);
        set(this, attrs, "editorModes", "01");
        set(this, attrs, "justSave", false);
        set(this, attrs, "validityCheck", "");
        set(this, attrs, "validityCheckMessage", "");
        set(this, attrs, "savestate");
        set(this, attrs, "mode");
        set(this, attrs, "iframeopts", "");
        set(this, attrs, "noConsoleClear", false);
        set(this, attrs, "showRuntime", false);
        set(this, attrs, "wrap", this.isText ? 70 : -1);
        set(this, attrs, "usercode", "");
        this.editorMode = parseInt(this.editorMode);
        this.editorText = [this.normal, this.highlight, this.parsons, this.jsparsons];
        this.editorModeIndecies = [];
        for (const c of this.editorModes) {
            this.editorModeIndecies.push(parseInt(c));
        }
        this.editorModeIndecies.push(parseInt(this.editorModes[0]));
        if (this.editorModes.length <= 1) {
            this.editorText = ["", "", "", "", "", "", ""];
        }
        this.checkEditorModeLocalStorage();

        this.showCodeLink = this.showCodeOn;
        this.minRows = getInt(this.rows);
        this.maxRows = getInt(this.maxrows);

        this.toggleEditorText = [english ? "Edit" : "Muokkaa", english ? "Hide" : "Piilota"];

        if (this.toggleEditor && this.toggleEditor !== "True") {
            this.toggleEditorText = this.toggleEditor.split("|");
        }

        if (this.usercode === "" && this.byCode) {
            this.usercode = this.byCode;
            this.initUserCode = true;
        }
        this.usercode = commentTrim(this.usercode);
        if (this.blind) {
            this.usercode = this.usercode.replace(/@author.*/, "@author XXXX");
        }
        this.byCode = commentTrim(this.byCode);

        if (this.usercode) {
            const rowCount = countChars(this.usercode, "\n") + 1;
            if (this.maxRows < 0 && this.maxRows < rowCount) {
                this.maxRows = rowCount;
            }
        } else if (this.maxRows < 0) {
            this.maxRows = 10;
        }

        this.isAll = languageTypes.isAllType(this.type);
        this.isRun = (languageTypes.getRunType(this.type, false) !== false || this.isAll) && this.norun === false;
        this.isTest = languageTypes.getTestType(this.type, this.selectedLanguage, false) !== false;
        this.isUnitTest = languageTypes.getUnitTestType(this.type, this.selectedLanguage, false) !== false;
        this.isDocument = (this.type.indexOf("doc") >= 0);

        this.showInput = (this.type.indexOf("input") >= 0);
        this.showArgs = (this.type.indexOf("args") >= 0);
        if (!this.uploadstem) {
            this.uploadstem = english ? "Upload image/file" : "Lataa kuva/tiedosto";
        }
        this.buttonText = english ? "Run" : "Aja";
        if (this.type.indexOf("text") >= 0 || this.isSimcir || this.justSave) { // || scope.isSage ) {
            this.isRun = true;
            this.buttonText = english ? "Save" : "Tallenna";
        }
        if (this.button) {
            this.isRun = true;
            this.buttonText = this.button;
        }

        this.indent = getInt(this.indent);
        if (this.indent < 0) {
            if (this.file) {
                this.indent = 8;
            } else {
                this.indent = 0;
            }
        }

        this.edit = element.find("textarea")[0]; // angular.element(e); // $("#"+scope.editid);
        this.preview = element.find(".csrunPreview")[0]; // angular.element(e); // $("#"+scope.editid);

        this.element0 = element[0];
        element[0].childNodes[0].outerHTML = getHeading(attrs, "header", this, "h4");
        const n = element[0].childNodes.length;
        if (n > 1) {
            element[0].childNodes[n - 1].outerHTML = getHeading(attrs, "footer", this, 'p class="footer"');
        }
        if (this.open) {
            if (kind === "tauno" || kind === "simcir") {
                this.showTauno();
            }
            // if (kind === 'wescheme') scope.showWeScheme();
        }

        let b = attrs.buttons || this.attrs.buttons;
        if (b) {
            const helloButtons = "public \nclass \nHello \n\\n\n{\n}\n" +
                "static \nvoid \n Main\n(\n)\n" +
                '        Console.WriteLine(\n"\nworld!\n;\n ';
            const typeButtons = "bool \nchar\n int \ndouble \nstring \nStringBuilder \nPhysicsObject \n[] \nreturn \n, ";
            const charButtons = "a\nb\nc\nd\ne\ni\nj\n.\n0\n1\n2\n3\n4\n5\nfalse\ntrue\nnull\n=";
            b = b.replace("$hellobuttons$", helloButtons);
            b = b.replace("$typebuttons$", typeButtons);
            b = b.replace("$charbuttons$", charButtons);
            b = b.trim();
            b = b.replace("$space$", " ");
            this.buttons = b.split("\n");
        }

        if (this.isAll) {
            const langs = attrs.languages || this.attrs.languages;
            if (langs) {
                this.progLanguages = langs.split(/[\n;\/]/);
            } else {
                this.progLanguages = languageTypes.runTypes;
            }
        }

        if (this.attrs.autorun) {
            this.runCodeLink(true);
        }
        this.editorIndex = 0;
        if (this.editorMode !== 0 || this.editorModes !== "01" || this.cssPrint) {
            this.showOtherEditor(this.editorMode);
        } // Forces code editor to change to pre
        this.mode = languageTypes.getAceModeType(this.type, this.mode);

        this.changeCodeLink();
        this.processPluginMath();

        csLogTime(this.taskId);

        this.showUploaded(this.attrs.uploadedFile, this.attrs.uploadedType);
    }

    $postLink() {
        const styleArgs = getParam(this, "style-args", "");
        if (styleArgs) {
            const argsEdit = this.element[0].getElementsByClassName("csArgsArea");
            if (argsEdit.length > 0) {
                argsEdit[0].setAttribute("style", styleArgs);
            }
        }
        this.initEditorKeyBindings();
        $(this.element[0]).bind("keydown", (event) => {
            if (event.ctrlKey || event.metaKey) {
                switch (String.fromCharCode(event.which).toLowerCase()) {
                    case "s":
                        event.preventDefault();
                        if (this.isRun) {
                            this.runCode();
                        }
                        break;
                }
            }
        });
    }

    async $doCheck() {
        let anyChanged = false;
        if (this.usercode !== this.dochecks.usercode) {
            if (this.aceEditor && this.aceEditor.getSession().getValue() !== this.usercode) {
                this.aceEditor.getSession().setValue(this.usercode);
            }
            this.dochecks.usercode = this.usercode;
            if (!this.copyingFromTauno && this.usercode !== this.byCode) {
                this.muokattu = true;
            }
            this.copyingFromTauno = false;
            if (this.minRows < this.maxRows) {
                updateEditSize(this);
            }
            if (this.viewCode) {
                this.pushShowCodeNow();
            }
            if (this.wrap > 0) {
                this.checkWrap();
            }
            anyChanged = true;
        }

        if (this.userargs !== this.dochecks.userargs) {
            this.dochecks.userargs = this.userargs;
            anyChanged = true;
        }
        if (this.userinput !== this.dochecks.userinput) {
            this.dochecks.userinput = this.userinput;
            anyChanged = true;
        }

        if (anyChanged) {
            const currUsercode = this.usercode;
            const currUserargs = this.userargs;
            const currUserinput = this.userinput;
            if (this.runned && this.autoupdate) {
                await $timeout(this.autoupdate);
                if (currUsercode === this.usercode &&
                    currUserargs === this.userargs &&
                    currUserinput === this.userinput) {
                    this.runCodeAuto();
                }
            }
        }
    }

    onFileSelect(file: File) {
        if (!this.taskId) {
            console.log("taskId missing");
            return;
        }
        console.log(file);

        if (file) {
            if (this.attrs.uploadbycode) {
                console.log("bycode");
                const reader = new FileReader();
                reader.onload = ((e) => {
                    // showTrack(theFile.target.result,type);
                    // console.log(theFile.target.result);
                    this.scope.$evalAsync(() => {
                        this.usercode = reader.result as string;
                    });
                });
                reader.readAsText(file);
                return;
            }

            this.fileProgress = 0;
            this.fileError = undefined;
            this.uploadedFile = undefined;
            this.uploadresult = undefined;
            this.docURL = undefined;
            const ti = this.taskId.split(".");
            if (ti.length < 2) {
                return;
            }
            const upload = $upload.upload<{file: string, type: string}>({
                url: `/pluginUpload/${ti[0]}/${ti[1]}/`,
                data: {
                    file: file,
                },
                method: "POST",
            });

            upload.then((response) => {
                $timeout(() => {
                    this.showUploaded(response.data.file, response.data.type);
                    this.doRunCode("upload", false);
                });
            }, (response) => {
                if (response.status > 0) {
                    this.fileError = response.data.error;
                }
            }, (evt) => {
                this.fileProgress = Math.min(100, Math.floor(100.0 *
                    evt.loaded / evt.total));
            });

            upload.finally(() => {
            });
        }
    }

    async processPluginMath() {
        if (!this.isMathCheck) {
            return;
        }
        await loadMathcheck();
        $timeout(() => {
            // MathJax.Hub.Queue(["Typeset", MathJax.Hub, $scope.element[0]]); // TODO
        }, 0);
    }

    showUploaded(file: string, type: string) {
        if (!file || !type) {
            return;
        }
        this.uploadedFile = file;
        this.uploadedType = type;
        const name = uploadFileTypesName(file);
        let html = `<p class="smalllink"><a href="${file}" title="${type}">${name}</a></p>`; // (' + type + ')</p>';
        if (type.indexOf("image") === 0) {
            html += '<img src="' + this.uploadedFile + '"/>';
            this.uploadresult = $sce.trustAsHtml(html);
            return;
        }
        if (type.indexOf("video") === 0) {
            html += '<video src="' + this.uploadedFile + '" controls/>';
            this.uploadresult = $sce.trustAsHtml(html);
            return;
        }
        if (type.indexOf("audio") === 0) {
            html += '<audio src="' + this.uploadedFile + '" controls/>';
            this.uploadresult = $sce.trustAsHtml(html);
            return;
        }
        if (type.indexOf("text") === 0) {
            html += '<div style="overflow: auto; -webkit-overflow-scrolling: touch; max-height:900px; -webkit-box-pack: center; -webkit-box-align: center; display: -webkit-box;"  width:1200px>';
            // html += '<iframe  width="800" src="' + file +'" target="csdocument" allowfullscreen  onload="resizeIframe(this)" '+$scope.iframeopts+' />';
            html += `<iframe  width="800" src="${file}" target="csdocument" allowfullscreen   ${this.iframeopts} />`;
            html += "</div>";
            this.uploadresult = $sce.trustAsHtml(html);
            return;

        }

        if (is(uploadFileTypes, file)) {
            html += '<div style="overflow: auto; -webkit-overflow-scrolling: touch; max-height:1200px; -webkit-box-pack: center; -webkit-box-align: center; display: -webkit-box;"  width:1200px>';
            // html += '<iframe width="800" height="900"  src="' + file +'" target="csdocument" allowfullscreen onload="resizeIframe(this)" '+$scope.iframeopts+' />';
            html += `<iframe width="800" height="900"  src="${file}" target="csdocument" allowfullscreen " ${this.iframeopts} />`;
            // html += '<embed  width="800" height="16000"  src="' + file +'" />';
            // html += '<object width="800" height="600"   data="' + file +'" type="' + type +'"  ></object>';
            html += "</div>";
            this.uploadresult = $sce.trustAsHtml(html);
            return;
        }

        html = '<p></p><p>Ladattu: <a href="' + file + '" title="' + type + '">' + name + "</a></p>";
        this.uploadresult = $sce.trustAsHtml(html);
        return;
    }

    checkWrap() {
        const r = wrapText(this.usercode, this.wrap);
        if (r.modified) {
            if (this.editorIndex === 0) {
                const start = this.edit.selectionStart;

                this.usercode = r.s;
                $timeout(() => {
                    this.edit.selectionStart = start;
                    this.edit.selectionEnd = start;
                });
            }
            if (this.editorIndex === 1) { // ACE
                const editor = this.aceEditor;
                let cursor = editor.selection.getCursor();
                const index = editor.session.doc.positionToIndex(cursor, 0);
                this.usercode = r.s;
                $timeout(() => {
                    cursor = editor.session.doc.indexToPosition(index, 0);
                    editor.selection.moveCursorToPosition(cursor);
                    editor.selection.clearSelection();
                });
            }

        }

    }

    logTime(msg: string) {
        csLogTime(msg + " " + this.taskId);
        return true;
    }

    runCodeIfCR(event: KeyboardEvent) {
        this.runError = "";
        if (event.keyCode === 13) {
            this.runCode();
        }
    }

    runCodeCommon(nosave: boolean, extraMarkUp?: IExtraMarkup) {
        this.runned = true;
        const t = languageTypes.getRunType(this.selectedLanguage, "cs");
        if (t === "md") {
            this.showMD();
            if (nosave || this.nosave) {
                return;
            }
        }
        if (languageTypes.isInArray(t, csJSTypes)) {
            // this.jstype = t;
            this.showJS();
            if (nosave || this.nosave) {
                return;
            }
        }
        this.doRunCode(t, nosave || this.nosave);
    }

    runCodeAuto() {
        this.runCodeCommon(true);
    }

    runCodeLink(nosave: boolean) {
        this.runCodeCommon(nosave || this.nosave);
    }

    runCode() {
        this.runCodeCommon(false);
    }

    runTest() {
        const t = languageTypes.getTestType(this.type, this.selectedLanguage, "comtest");
        this.doRunCode(t, false);
    }

    runUnitTest() {
        const t = languageTypes.getUnitTestType(this.type, this.selectedLanguage, "junit");
        this.doRunCode(t, false);
    }

    runDocument() {
        if (this.docURL) {
            this.closeDocument();
            this.docLink = "Document";
            return;
        }
        this.docLink = "Hide document";
        const t = languageTypes.getRunType(this.selectedLanguage, "cs");
        this.doRunCode(t, false, {document: true});
    }

    closeDocument() {
        this.docURL = "";
    }

    hideShowEditor() {
        this.noeditor = !this.noeditor;
    }

    async doRunCode(runType: string, nosave: boolean, extraMarkUp?: IExtraMarkup) {
        if (this.isRunning) {
            return;
        } // do not run if previuos is still running
        this.closeDocument();
        if (this.isSage) {
            await this.initSage(true);
            if (this.sageButton) {
                this.sageButton.click();
            }
        }

        if (this.simcir) {
            this.usercode = await this.getCircuitData();
        } else if (this.taunoOn && (!this.muokattu || !this.usercode)) {
            this.copyTauno();
        }

        if (this.parson) {
            const fb = this.parson.getFeedback();
            this.usercode = this.getJsParsonsCode();
        }

        if (this.csparson) {
            this.usercode = this.csparson.join("\n");
            this.csparson.check(this.usercode);
        }

        this.checkIndent();
        if (!this.autoupdate) {
            this.tinyErrorStyle = {};
            this.error = "... running ...";
            this.runError = true;
            this.isRunning = true;
        }
        this.isRunning = true;
        this.imgURL = "";
        this.wavURL = "";
        this.runSuccess = false;
        if (!(languageTypes.isInArray(runType, csJSTypes) || this.noConsoleClear)) {
            this.result = "";
        }
        this.runTestGreen = false;
        this.runTestRed = false;
        this.oneruntime = "";
        let isInput = false;
        if (this.type.indexOf("input") >= 0) {
            isInput = true;
        }

        let ucode = "";
        let uinput = "";
        let uargs = "";
        if (this.usercode) {
            ucode = this.usercode.replace(this.cursor, "");
        }
        ucode = ucode.replace(/\r/g, "");
        if (this.userinput) {
            uinput = this.userinput;
        }
        if (this.userargs) {
            uargs = this.userargs;
        }
        if (this.validityCheck) {
            const re = new RegExp(this.validityCheck);
            if (!ucode.match(re)) {
                this.tinyErrorStyle = {color: "red"};
                let msg = this.validityCheckMessage;
                if (!msg) {
                    msg = "Did not match to " + this.validityCheck;
                }
                this.error = msg;
                this.isRunning = false;
                return;
            }
        }

        const params = {
            input: {
                usercode: ucode,
                userinput: uinput,
                isInput: isInput,
                userargs: uargs,
                uploadedFile: this.uploadedFile,
                uploadedType: this.uploadedType,
                nosave: false,
                type: runType,
                ...extraMarkUp,
                ...(this.isAll ? {selectedLanguage: this.selectedLanguage} : {}),
            },
        };
        if (nosave || this.nosave) {
            params.input.nosave = true;
        }
        let url = "/cs/answer";
        if (this.plugin) {
            url = this.plugin;
            const i = url.lastIndexOf("/");
            if (i > 0) {
                url = url.substring(i);
            }
            url += "/" + this.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        }
        const t0run = performance.now();
        const r = await to($http<{
            web: {
                error?: string,
                pwd?: string,
                image?: string,
                wav?: string,
                testGreen?: boolean,
                testRed?: boolean,
                comtestError?: string,
                docurl?: string,
                console?: string,
                runtime?: string,
                "-replyImage"?: string,
                "-replyHTML"?: string,
                "-replyMD"?: string,
            },
        }>({method: "PUT", url: url, data: params, timeout: 20000},
        ));
        if (r.ok) {
            this.isRunning = false;
            const data = r.result.data;
            const tsruntime = ((performance.now() - t0run) / 1000).toFixed(3);
            const runtime = (data.web.runtime || "").trim();
            this.oneruntime = "" + tsruntime + " " + runtime.split(" ", 1)[0];
            this.runtime = "\nWhole: " + tsruntime + "\ncsPlugin: " + runtime;
            if (data.web.pwd) {
                ConsolePWD.setPWD(data.web.pwd, this);
            }
            this.error = data.web.error;
            this.runSuccess = true;

            this.runError = this.error;

            const imgURL = data.web.image;
            // if ( !imgURL ) imgURL = data.web["-replyImage"];
            this.imgURL = data.web["-replyImage"] || "";
            this.htmlresult = (data.web["-replyHTML"] || "") + (data.web["-replyMD"] || "");
            const wavURL = data.web.wav;
            if (data.web.testGreen) {
                this.runTestGreen = true;
            }
            if (data.web.testRed) {
                this.runTestRed = true;
            }
            this.comtestError = data.web.comtestError;
            if (this.runError) {
                this.runTestGreen = false;
            }

            const docURL = data.web.docurl;

            if (docURL) {
                this.docURL = docURL;
                this.error = data.web.console.trim();
            }

            if (wavURL) {
                // <video src="https://tim.jyu.fi/csgenerated/vesal/sinewave.wav" type="video/mp4" controls="" autoplay="true" ></video>
                this.wavURL = wavURL;
                this.result = data.web.console.trim();
            }

            if (imgURL) {
                this.imgURL = imgURL + this.imgURL;
                this.result = data.web.console.trim();
            } else {
                if (this.runSuccess) {
                    if (this.isHtml) {
                        this.htmlresult = removeXML(data.web.console || "") + this.htmlresult;
                    } else if (!languageTypes.isInArray(runType, csJSTypes)) {
                        this.result = data.web.console;
                    } else {
                        this.error = data.web.error;
                    }
                }
            }
            this.processPluginMath();

        } else {
            this.isRunning = false;
            const data = r.result.data;
            this.error = "Ikuinen silmukka tai jokin muu vika?";
            if (data && data.error) {
                this.error = data.error;
                this.errors.push(data.error);
            }
        }
    }

    hideTauno() {
        if (this.simcir) {
            this.simcir.children().remove();
            this.simcir = undefined;
        }
        this.taunoOn = false;
        this.taunoElem.innerHTML = "<p></p>";
    }

    copyTauno() {
        const f = document.getElementById(this.taunoId) as any;
        // var s = $scope.taunoElem.contentWindow().getUserCodeFromTauno();
        let s = f.contentWindow.getUserCodeFromTauno();
        this.copyingFromTauno = true;
        const treplace = this.attrs.treplace || "";
        if (treplace) {
            const treps = treplace.split("&");
            for (let i = 0; i < treps.length; i++) {
                const reps = (treps[i] + "|").split("|");
                s = s.replace(new RegExp(reps[0], "g"), reps[1]);
                s = s.replace(new RegExp("\n\n", "g"), "\n");
            }
        }
        this.usercode = s;
        this.checkIndent();
        this.muokattu = false;
    }

    async addText(s: string) {
        // $scope.usercode += s;
        if (this.noeditor) {
            this.userargs += s + " ";
            return;
        }
        let tbox;
        let editor: IAceEditor | undefined;
        let i = 0;
        if (this.editorIndex === 1) {
            editor = this.aceEditor;
            i = editor.session.doc.positionToIndex(editor.selection.getCursor(), 0);
        } else {
            tbox = this.edit;
            i = tbox.selectionStart || 0;
        }
        let uc = (this.usercode || "");
        const ci = uc.indexOf(this.cursor);
        if (ci >= 0) {
            if (ci < i) {
                i--;
            }
            uc = uc.replace(this.cursor, "");
        }
        const text = s.replace(/\\n/g, "\n");
        const cur = ""; // $scope.cursor;  // this would be needed by iPad because it does not show cursor
        this.usercode = uc.substring(0, i) + text + cur + uc.substring(i);
        // $scope.usercode = (uc + s.replace(/\\n/g,"\n")).replace($scope.cursor,"")+$scope.cursor;
        // $scope.insertAtCursor(tbox, s);
        // tbox.selectionStart += s.length;
        // tbox.selectionEnd += s.length;
        i += text.length;
        await $timeout();
        if (editor) {
            const cursor = editor.session.doc.indexToPosition(i, 0);
            editor.selection.moveCursorToPosition(cursor);
            editor.selection.clearSelection();
        } else if (tbox) {
            tbox.selectionStart = i;
            tbox.selectionEnd = i;
            tbox.focus();
        }
    }

    addTextHtml(s: string) {
        let ret = s.trim();
        if (ret.length === 0) {
            ret = "\u00A0";
        }
        return ret;
    }

    insertAtCursor(myField: HTMLTextAreaElement, myValue: string) {
        // IE support
        const doc = document as any;
        if (doc.selection) {
            myField.focus();
            const sel = doc.selection.createRange();
            sel.text = myValue;
        } else if (myField.selectionStart || myField.selectionStart === 0) {
            const startPos = myField.selectionStart;
            const endPos = myField.selectionEnd;
            myField.value = myField.value.substring(0, startPos) +
                myValue +
                myField.value.substring(endPos, myField.value.length);
        } else {
            myField.value += myValue;
        }
    }

    // Returns the visible index for next item and the desired size
    getVid(dw?: number | string, dh?: number | string): Vid {
        taunoNr++;
        const vid = "tauno" + taunoNr;
        this.taunoId = vid;
        if (!dw) {
            dw = "100%";
        }
        if (!dh) {
            dh = 500;
        }
        const w = ifIs(this.width, "width", dw);
        const h = ifIs(this.height, "height", dh);
        return {vid: vid, w: w, h: h};
    }

    async setCircuitData() {
        let data: {width: any, height: any};
        this.runError = false;
        try {
            if (this.usercode) {
                data = JSON.parse(this.usercode);
            }
        } catch (err) {
            this.error = err.message;
            this.runError = true;
        }
        try {
            const initstr = getParam(this, "initSimcir", "") as string;
            if (initstr) {
                const initdata = JSON.parse(initstr);
                $.extend(data, initdata);
            }
        } catch (err) {
            this.error = err.message;
            this.runError = true;
        }

        data.width = getParam(this, "width", 800);
        data.height = getParam(this, "height", 400);
        this.simcir.children().remove();
        const simcir = await loadSimcir();
        simcir.setupSimcir(this.simcir, data);
    }

    async getCircuitData() {
        const simcir = await loadSimcir();
        const d = simcir.controller(this.simcir.find(".simcir-workspace"));
        const data = d.data();

        let buf = "";
        const print = (s: string) => {
            buf += s;
        };
        const println = (s: string) => {
            print(s);
            buf += "\r\n";
        };
        const printArray = (array: any[]) => {
            $.each(array, (i, item) => {
                println("    " + JSON.stringify(item) +
                    (i + 1 < array.length ? "," : ""));
            });
        };
        println("{");
        println('  "devices":[');
        printArray(data.devices);
        println("  ],");
        println('  "connectors":[');
        printArray(data.connectors);
        println("  ]");
        print("}");
        return buf;
        // return JSON.stringify(result);
    }

    copyToSimcir() {
        this.setCircuitData();
    }

    async copyFromSimcir() {
        this.usercode = await this.getCircuitData();
    }

    showSimcir() {
        const v = this.getVid();
        this.taunoOn = true;
        this.taunoElem.innerHTML = `<div id=${v.vid}></div>`;
        const jqTauno = $(this.taunoElem);
        this.simcir = $("#" + v.vid);
        this.simcir = jqTauno.find("#" + v.vid);
        this.setCircuitData();
        return true;
    }

    showTauno() {
        if (this.isSimcir) {
            return this.showSimcir();
        }
        /*
        csApp.taunoNr++;
        var vid = 'tauno'+csApp.taunoNr;
        $scope.taunoId = vid;
        var w = csApp.ifIs($scope.width,"width",800);
        var h = csApp.ifIs($scope.height,"height",500);
        */
        const v = this.getVid();
        let p = "";
        let tt = "/cs/tauno/index.html?lang=" + this.lang + "&";
        if (this.taunotype && this.taunotype === "ptauno") {
            tt = "/cs/tauno/index.html?lang=" + this.lang + "&s&";
        }
        let taunoUrl = tt; // +"?"; // t=1,2,3,4,5,6&ma=4&mb=5&ialku=0&iloppu=5";
        const s = this.table;
        if (s && s.length > 0) {
            if (s[0] === "s") {
                p = "ts=" + s.substring(1) + "&";
            } else {
                p = "t=" + s.trim() + "&";
            }                      // table by it's items
        }

        p += doVariables(this.variables, "m");
        p += doVariables(this.indices, "i");

        taunoUrl = taunoUrl + p;
        this.iframe = true;
        if (this.iframe) {
            this.taunoElem.innerHTML =
                // '<p class="pluginHide"" ><a ng-click="hideTauno()">hide Tauno</a></p>' + // ng-click ei toimi..
                '<iframe id="' + v.vid + '" class="showTauno" src="' + taunoUrl + '" ' + v.w + v.h + " " + this.iframeopts + " ></iframe>";
        } else {
            this.taunoElem.innerHTML = '<div class="taunoNaytto" id="' + v.vid + '" />';
        }
        this.taunoOn = true;
    }

    runWeScheme(s: string) {
        // var f = document.getElementById($scope.taunoId) as any;
        // var s = $scope.taunoElem.contentWindow().getUserCodeFromTauno();
        // $scope.contentWindow.runWeScheme(s);
    }

    showWeScheme() {
        const v = this.getVid();
        const p = "";
        const tt = "/csstatic/WeScheme/WeSchemeEditor.html";
        const weSchemeUrl = tt;
        const s = this.table;
        this.iframe = true;
        // $scope.sageOutput = $scope.element0.getElementsByClassName('outputSage')[0];
        if (this.iframe) {
            this.taunoElem.innerHTML =
                // '<p class="pluginHide"" ><a ng-click="hideTauno()">hide Tauno</a></p>' + // ng-click ei toimi..
                '<iframe id="' + v.vid + '" class="showWeScheme" src="' + weSchemeUrl + '" ' + v.w + v.h + " " + this.iframeopts + " ></iframe>";
        } else {
            this.taunoElem.innerHTML = '<div class="taunoNaytto" id="' + v.vid + '" />';
        }
        this.taunoOn = true;
    }

    initCode() {
        this.muokattu = false;
        this.usercode = this.byCode;
        this.imgURL = "";
        this.runSuccess = false;
        this.runError = false;
        this.result = "";
        this.viewCode = false;
        if (this.parson || this.editorModeIndecies[this.editorMode] > 1) {
            this.initUserCode = true;
            this.showOtherEditor(this.editorMode);
        }
        if (this.isSage) {
            this.initSage(false);
        }
        if (this.simcir) {
            this.setCircuitData();
        }
    }

    async initSage(firstTime: boolean) {
        // TODO: lisää kentätkin vasta kun 1. kerran alustetaan.
        // TODO: kielien valinnan tallentaminen
        // TODO: kielien valinta kunnolla float.
        // ks: https://github.com/sagemath/sagecell/blob/master/doc/embedding.rst
        const sagecell = fixDefExport(await import("sagecell"));
        if (this.sagecellInfo) {
            this.sagecellInfo.editor = "textarea";
            // cs.sagecellInfo.inputLocation = null;
            // cs.sagecellInfo.outputLocation = null;
            // sagecell.deleteSagecell(cs.sagecellInfo);
            // cs.sagecellInfo = null;
        }
        const types = this.type.split("/");
        let languages = sagecell.allLanguages;
        if (types.length > 1) {
            languages = types.slice(1);
        }
        // if ( cs.sagecellInfo ) {
        if (this.sageInput && this.sageOutput) {
            const outputLocation = $(this.sageOutput);
            outputLocation.find(".sagecell_output_elements").hide();
            // cs.sagecellInfo.code = cs.usercode;
            // cs.sagecellInfo.code = cs.getReplacedCode();
            // cs.sagecellInfo.session.code = cs.sagecellInfo.code;
            // cs.sagecellInfo.inputLocation.innerText = cs.sagecellInfo.code;
            // cs.sagecellInfo.inputLocation.children[0].children[0].children[0].value = cs.sagecellInfo.code;
            this.sageInput.value = this.getReplacedCode();
            return;
        }
        this.sageArea = this.element0.getElementsByClassName("computeSage")[0];
        this.editArea = this.element0.getElementsByClassName("csEditArea")[0];
        this.sageOutput = this.element0.getElementsByClassName("outputSage")[0];

        this.sagecellInfo = sagecell.makeSagecell({
            inputLocation: this.sageArea,
            replaceOutput: true,
            // inputLocation: cs.editArea,
            editor: "textarea",
            // hide: ["evalButton"],
            hide: ["editor", "evalButton"],
            outputLocation: this.sageOutput,
            requires_tos: false,
            // code: cs.usercode,
            code: this.getReplacedCode(),
            getCode: () => this.getReplacedCode(),
            autoeval: this.attrs.autorun || firstTime,
            callback: () => {
                this.sageButton = this.sageArea.getElementsByClassName("sagecell_evalButton")[0] as HTMLElement;
                this.sageInput = this.sageArea.getElementsByClassName("sagecell_commands")[0] as HTMLInputElement;

                this.sageButton.onclick = () => {
                    // cs.checkSageSave();
                    this.sagecellInfo.code = this.getReplacedCode();
                    // cs.sagecellInfo.session.code = cs.sagecellInfo.code;
                };
                const sagecellOptions = this.element0.getElementsByClassName("sagecell_options")[0];
                const csRunMenuArea = this.element0.getElementsByClassName("csRunMenuArea")[0];
                if (csRunMenuArea && sagecellOptions) {
                    csRunMenuArea.appendChild(sagecellOptions);
                }
                sagecellOptions.style.marginTop = "-2em";
            },
            languages: languages, // sagecell.allLanguages
        });
    }

    pushShowCodeNow() {
        if (!this.viewCode) {
            return;
        }
        this.showCodeNow();
    }

    changeCodeLink() {
        if (this.viewCode) {
            this.showCodeLink = this.showCodeOff;
        } else {
            this.showCodeLink = this.showCodeOn;
        }

    }

    showCode() {
        this.viewCode = !this.viewCode;
        this.changeCodeLink();
        this.localcode = undefined;
        this.showCodeNow();
    }

    checkIndent() {
        if (!this.indent || !this.usercode) {
            return;
        }
        let start = this.edit.selectionStart;
        let spaces = "";
        for (let j1 = 0; j1 < this.indent; j1++) {
            spaces += " ";
        }
        let n = 0;
        let len = 0;
        const st = this.usercode.split("\n");
        for (const i in st) {
            if (st.hasOwnProperty(i)) {
                let s = st[i];
                const l = s.length;
                len += l;
                let j = 0;
                for (; j < s.length; j++) {
                    if (s[j] !== " ") {
                        break;
                    }
                }
                // if ( s.lastIndexOf(spaces,0) === 0 ) continue;
                if (j >= spaces.length) {
                    continue;
                }
                if (s.trim() === "") {
                    continue;
                } // do not indent empty lines
                s = spaces + s.substring(j);
                const dl = s.length - l;
                if (len - l < start) {
                    start += dl;
                }
                len += dl;
                st[i] = s;
                n++;
            }
        }
        if (!n) {
            return;
        }
        this.usercode = st.join("\n");
    }

    getReplacedCode() {
        // $scope.code = $scope.localcode;
        if (!this.attrs.program) {
            this.code = this.usercode;
            return this.code;
        }
        const st = this.attrs.program.split("\n");
        let r = "";
        const rp = ["", ""]; // alkuosa, loppuosa
        let step = 0;
        let nl = "";
        let nls = "";
        const needReplace = !!this.replace;
        const regexp = new RegExp(this.replace);
        for (const s of st) {
            // if ( s.indexOf($scope.replace) >= 0 ) {
            if (needReplace && regexp.test(s)) {
                r += nl + this.usercode + "\n";
                if (step === 0) {
                    step++;
                    nls = "";
                    continue;
                }
            } else {
                r += nl + s;
                rp[step] += nls + s;
            }
            nl = nls = "\n";
        }
        this.code = r;
        return this.code;
    }

    getCodeFromLocalCode(f?: () => void) {
        // f: function to call after ready
        // $scope.code = $scope.localcode;
        if (!this.localcode) {
            this.code = this.usercode;
            this.precode = "";
            this.postcode = "";
            if (f) {
                f();
            }
            return;
        }
        const st = this.localcode.split("\n");
        let r = "";
        const rp = ["", ""]; // alkuosa, loppuosa
        let step = 0;
        let nl = "";
        let nls = "";
        const needReplace = !!this.replace;
        const regexp = new RegExp(this.replace);
        for (const s of st) {
            // if ( s.indexOf($scope.replace) >= 0 ) {
            if (needReplace && regexp.test(s)) {
                r += nl + this.usercode;
                if (step === 0) {
                    step++;
                    nls = "";
                    continue;
                }
            } else {
                r += nl + s;
                rp[step] += nls + s;
            }
            nl = nls = "\n";
        }
        this.code = r;
        this.precode = rp[0];
        this.postcode = rp[1];
        if (f) {
            f();
        }
    }

    showCodeNow() {
        if (!this.viewCode) {
            return;
        }
        this.getAllCode(null);
    }

    async getAllCode(f) {
        // f: function to call after ready
        if (this.localcode !== null) {
            this.getCodeFromLocalCode(f);
            return;
        }
        if (!this.file && !this.attrs.program) {
            this.localcode = "";
            this.getCodeFromLocalCode(f);
            return;
        }

        const params = this.attrs;
        const r = await to($http<{msg: string, error: string} | string>({
                method: "POST",
                url: "/cs/",
                params: {
                    print: 1,
                    replace: "",
                },
                data: params,
            },
        ));
        if (r.ok) {
            const data = r.result.data;

            // Server always seems to give text/plain as result, so prepare for it.
            if (typeof data === "string") {
                this.localcode = data;
                this.getCodeFromLocalCode(f);
            } else if (data.msg !== "") {
                this.localcode = data.msg;
                this.getCodeFromLocalCode(f);
            } else {
                this.errors.push(data.error);
                this.precode = "";
                this.postcode = "";
            }
        } else {
            const status = r.result.data.error;
            this.errors.push(status);
            this.precode = "";
            this.postcode = "";
        }
    }

    async showMD() {
        if (!this.usercode) {
            return;
        }
        if (!this.taskId) {
            console.log("taskId missing");
            return;
        }
        if (this.precode == undefined) {
            this.getAllCode(this.showMD);
            return;
        }
        const text = this.precode + "\n" + this.usercode.replace(this.cursor, "") + "\n" + this.postcode;
        if (text === this.lastMD) {
            return;
        }
        this.lastMD = text;
        const r = await to($http.post<{texts: string | Array<{html: string}>}>(
            `/preview/${this.taskId.split(".")[0]}`, {
                text: text,
            }));
        if (r.ok) {
            const data = r.result.data;
            let s = "";
            const $previewDiv = angular.element(this.preview);

            if (typeof data.texts === "string") {
                s = data.texts;
            } else {
                const len = data.texts.length;
                for (let i = 0; i < len; i++) {
                    s += data.texts[i].html;
                }
            }
            // s = '<div class="par"  id="f2AP4FHbBIkB"  t="MHgzMGJlMDIxNw=="  attrs="{}" ng-non-bindable>  <div class=""> <p>Vesa MD 2</p>  </div>    <div class="editline" title="Click to edit this paragraph"></div>    <div class="readline"          title="Click to mark this paragraph as read"></div>     </div>    ';
            s = s.replace(/parContent/, ""); // Tämä piti ottaa pois ettei scope pilaannu seuraavaa kertaa varten???
            s = s.replace(/<div class="editline".*<.div>/, "");
            s = s.replace(/<div class="readline"[\s\S]*?<.div>/, "");
            const html = $compile(s)(this.scope);
            $previewDiv.empty().append(html);

            ParCompiler.processAllMath($previewDiv);
            // $scope.outofdate = false;
            // $scope.parCount = len;

        } else {
            const data = r.result.data;
            $window.alert("Failed to show preview: " + data.error);
        }
    }

    getJsParsonsCode() {
        const gr = new ParsonsWidget._graders.VariableCheckGrader(this.parson);
        let result = gr._codelinesAsString();
        const len = result.length;
        if (result[len - 1] === "\n") {
            result = result.slice(0, -1);
        }
        return result;
    }

    async showJsParsons(parsonsEditDiv: Element) {
        let v = this.parsonsId;
        if (!v) {
            v = this.getVid();
            this.parsonsId = v;
        }
        parsonsEditDiv.setAttribute("id", v.vid);
        if ($("#" + v.vid).length === 0) {
            console.log("wait 300 ms " + v.vid);
            await $timeout(300);
            this.showJsParsons(parsonsEditDiv);
            return;
        }
        let classes = "csrunEditorDiv sortable-code";
        let canIndent = true;
        if (this.words) {
            classes += " jssortable-words";
            canIndent = false;
        }
        parsonsEditDiv.setAttribute("class", classes);
        // parsonsEditDiv.setAttribute('style',"float: none;");

        // parsonsEditDiv.innerHTML = "";
        this.parson = new ParsonsWidget({
            sortableId: v.vid,
            max_wrong_lines: 1,
            // 'prettyPrint': false,
            // 'x_indent': 0,
            can_indent: canIndent,
            // 'vartests': [{initcode: "output = ''", code: "", message: "Testing...", variables: {output: "I am a Java program I am a Java program I am a Java program "}},
            //    ],
            // 'grader': ParsonsWidget._graders.LanguageTranslationGrader,
        });
        // $scope.parson.init($scope.byCode,$scope.usercode);
        this.parson.init(this.usercode);
        if (!this.initUserCode) {
            this.parson.options.permutation = iotaPermutation;
        }
        this.parson.shuffleLines();
    }

    async showCsParsons(sortable: Element) {
        const csp = await lazyLoadTS<typeof csparsons>("./cs-parsons/csparsons.js", __moduleName);
        const parson = new csp.CsParsonsWidget({
            sortable: sortable,
            words: this.words,
            minWidth: "40px",
            shuffle: this.initUserCode,
            styleWords: getParam(this, "style-words", "") as string,
            maxcheck: getParam(this, "parsonsmaxcheck", "") as number,
            notordermatters: getParam(this, "parsonsnotordermatters", false) as boolean,
            onChange: (p) => {
                const s = p.join("\n");
                this.usercode = s;
            },
        });
        parson.init(this.byCode, this.usercode);
        parson.show();
        this.csparson = parson;
    }

    initEditorKeyBindings() {
        let eindex = this.editorModeIndecies[this.editorMode];
        if (eindex !== 0) {
            return;
        }
        $(this.edit).bind("keydown", (event) => {
            eindex = this.editorModeIndecies[this.editorMode];
            if (eindex !== 0) {
                return;
            }
            if (this.editorMode !== 0) {
                return;
            }
            if (event.which === 9) {
                event.preventDefault();
                if (event.shiftKey) {
                    return;
                }
                insertAtCaret(this.edit, "    ");
                this.usercode = this.edit.value;
                return;
            }
        });
    }

    checkEditorModeLocalStorage() {
        if (this.editorMode >= 0) {
            return;
        }
        this.editorMode = 0;
        const eindexStr = localStorage.getItem("editorIndex");
        if (!eindexStr) {
            return;
        }
        const eindex = parseInt(eindexStr, 10);
        if (this.editorModes.indexOf("0") < 0) {
            return;
        }
        if (this.editorModes.indexOf("1") < 0) {
            return;
        }
        for (let em = 0; em < this.editorModeIndecies.length; em++) {
            const ein = this.editorModeIndecies[em];
            if (ein === eindex) {
                this.editorMode = em;
                break;
            }
        }
    }

    async showOtherEditor(editorMode: number) {
        if (this.parson) {
            this.usercode = this.getJsParsonsCode();
        }

        this.parson = null;
        this.csparson = null;

        const editorHtml = '<textarea class="csRunArea csrunEditorDiv" ng-hide="noeditor" rows={{rows}} ng-model="usercode" ng-trim="false" placeholder="{{placeholder}}"></textarea>';

        const aceHtml = '<div class="no-popup-menu"><div ng-show="mode"' +
            // var aceHtml = '<div ng-show="mode" ui-ace="{  mode: \'{{mode}}\',    require: [\'/static/scripts/bower_components/ace-builds/src-min-noconflict/ext-language_tools.js\'],  advanced: {enableSnippets: true,enableBasicAutocompletion: true,enableLiveAutocompletion: true}}"'+
            // ' style="left:-6em; height:{{rows*1.17}}em;" class="csRunArea csEditArea" ng-hide="noeditor"  ng-model="usercode" ng-trim="false" placeholder="{{placeholder}}"></div>'+
            // ' style="left:-5px; width: 101% !important;'+
            ' " class="csRunArea csEditArea csAceEditor" ng-hide="noeditor" ng-trim="false" placeholder="{{placeholder}}"></div>' +
            /*
            '<div style="right:0px;">'+
            '<button ng-click="moveCursor(-1, 0);">&#x21d0;</button>'+
            '<button ng-click="moveCursor( 0,-1);">&#x21d1;</button>'+
            '<button ng-click="moveCursor( 1, 0);">&#x21d2;</button>'+
            '<button ng-click="moveCursor( 0, 1);">&#x21d3;</button>'+
            '</div>'+
            */
            "</div>";

        const cssHtml = "<pre>{{usercode}}</pre>";

        const parsonsHtml = '<div class="no-popup-menu"></div>';

        let html;
        if (this.cssPrint) {
            html = [cssHtml, cssHtml, cssHtml, cssHtml];
        } else {
            html = [editorHtml, aceHtml, parsonsHtml, parsonsHtml];
        }

        this.mode = languageTypes.getAceModeType(this.type, this.mode);
        if (editorMode != undefined) {
            this.editorMode = editorMode;
        } else {
            this.editorMode++;
        }
        if (this.editorMode >= this.editorModeIndecies.length - 1) {
            this.editorMode = 0;
        }
        const eindex = this.editorModeIndecies[this.editorMode];
        this.editorIndex = eindex;
        const otherEditDiv = this.element0.getElementsByClassName("csrunEditorDiv")[0];
        const editorDiv = angular.element(otherEditDiv) as JQuery;
        this.edit = $compile(html[eindex])(this.scope)[0] as HTMLTextAreaElement; // TODO unsafe cast
        // don't set the html immediately in case of Ace to avoid ugly flash because of lazy load
        if (eindex === 1) {
            const ace = (await import("tim/editor/ace")).ace;
            editorDiv.empty().append(this.edit);
            const editor = ace.edit(editorDiv.find(".csAceEditor")[0]) as IAceEditor;

            this.aceLoaded(ace, editor);
            if (this.mode) {
                editor.getSession().setMode("ace/mode/" + this.mode);
            }
            editor.setOptions({
                enableBasicAutocompletion: true,
                enableLiveAutocompletion: false,
                enableSnippets: true,
                maxLines: this.maxRows,
                // showTokenInfo: true
            });
            editor.setFontSize(15);
            if (editorDiv.parents(".reveal").length > 0) {
                editor.setFontSize(25);
            }
            editor.getSession().setUseWorker(false); // syntax check away
            editor.renderer.setScrollMargin(12, 12, 0, 0);
            editor.getSession().setValue(this.usercode);
            editor.getSession().on("change", () => {
                this.scope.$evalAsync(() => {
                    this.usercode = editor.getSession().getValue();
                });
            });
        } else {
            editorDiv.empty().append(this.edit);
            if (eindex === 2) {
                this.showCsParsons(otherEditDiv.children[0]);
            }
            if (eindex === 3) {
                this.showJsParsons(otherEditDiv.children[0]);
            }
        }
        this.initEditorKeyBindings();
        if (eindex <= 1) {
            localStorage.setItem("editorIndex", eindex.toString());
        }
    }

    moveCursor(dx: number, dy: number) {
        const p = this.aceEditor.getCursorPosition();
        p.row += dy;
        p.column += dx;
        this.aceEditor.moveCursorToPosition(p);
    }

    // Runs when editor loads
    aceLoaded(ace: IAce, editor: IAceEditor) {
        this.aceEditor = editor;
        console.log("Ace editor loaded successfully");
        const session = editor.getSession();
        session.setUndoManager(new ace.UndoManager());
    }

    write(s: string) {
        this.result += s;
    }

    writeln(s: string) {
        this.write(s + "\n");
    }

    toggleFixed() {
        if (this.canvas.style.position === "fixed") {
            this.canvas.style.position = "";
            this.irrotaKiinnita = this.english ? "Release" : "Irrota";
        } else {
            this.canvas.style.position = "fixed";
            this.canvas.style.width = "900px";
            this.irrotaKiinnita = this.english ? "Fix" : "Kiinnitä";
        }
    }

    getCode() {
        if (this.attrs.program && !this.codeInitialized) {
            this.localcode = this.attrs.program;
            this.getCodeFromLocalCode();
        }
        this.codeInitialized = true;
        let text = this.usercode.replace(this.cursor, "");
        if (this.precode || this.postcode) {
            text = this.precode + "\n" + text + "\n" + this.postcode;
        }
        return text;
    }

    closeFrame() {
        if (!this.canvas) {
            return;
        }
        this.canvas.remove();
        this.canvas = undefined;
        this.lastJS = "";
    }

    async showJS() {
        let isProcessing = false;
        if (this.type.indexOf("processing") >= 0) {
            this.iframe = true;
            isProcessing = true;
        }
        let wescheme = false;
        if (this.type === "wescheme") {
            this.iframe = true;
            wescheme = true;
        }

        let wantsConsole = false;
        if (this.type.indexOf("/c") >= 0) {
            wantsConsole = true;
        }
        if (!this.attrs.runeverytime && !this.usercode && !this.userargs && !this.userinput) {
            return;
        }
        if (!this.canvas) { // create a canvas on first time
            let html = "";
            let scripts = "";
            this.fullhtml = (this.attrs.fullhtml || "");
            if (this.fullhtml) {
                this.iframe = true;
            }  // fullhtml always to iframe
            if (this.type.indexOf("html") >= 0) {
                this.iframe = true; // html always iframe
                if (!this.fullhtml) {
                    this.fullhtml = "REPLACEBYCODE";
                }

            }  // html always to iframe
            if (this.type.indexOf("/vis") >= 0) {
                this.iframe = true;  // visjs always to iframe
                html = '<div id="myDiv" class="mydiv" width="800" height="400" ></div>';
                scripts = "https://cdnjs.cloudflare.com/ajax/libs/vis/4.20.0/vis.min.js";
            }
            let fsrc = "/cs/gethtml/canvas.html";
            if (wescheme) {
                // fsrc = "/csstatic/WeScheme/WeSchemeEditor.html";
                fsrc = "/csstatic/WeScheme/openEditor.html";
            }
            if (this.iframe) {
                let dw, dh;
                if (this.glowscript) {
                    fsrc = "/cs/gethtml/GlowScript.html";
                    dh = "430";
                    dw = "800";
                    if (this.type === "glowscript") {
                        this.gsDefaultLanguage = "GlowScript 2.1 JavaScript";
                    }
                }
                if (isProcessing) {
                    fsrc = "/cs/gethtml/processing.html";
                    if (!this.fullhtml) {
                        this.fullhtml = "REPLACEBYCODE";
                    }
                }
                const v = this.getVid(dw, dh);
                this.irrotaKiinnita = this.english ? "Release" : "Irrota";
                html = (this.attrs.html || html);
                html = encodeURI(html);
                let opts = 'seamless="seamless" sandbox="allow-scripts allow-forms allow-same-origin"';
                if (this.iframeopts) {
                    opts = this.iframeopts;
                }
                const angularElement = `
<div tim-draggable-fixed class="no-popup-menu" style="top: 91px; right: 0px; z-index: 20">
    <span class="csRunMenu">
        <div class="csFixRelease"><a href ng-click="$ctrl.toggleFixed()">{{$ctrl.irrotaKiinnita}}</a>
        <a href
           ng-click="$ctrl.closeFrame()"
           style="float: right">[X]</a></div></span>
    <iframe ng-if="$ctrl.fullhtml" id="${v.vid}" class="jsCanvas"
            src="${fsrc}?scripts=${this.attrs.scripts || scripts}&html=${html}" ${v.w}${v.h} style="border:0"
            ${opts}>
        <iframe ng-if="!$ctrl.fullhtml" id="${v.vid}" class="jsCanvas" ${v.w}${v.h} style="border:0"
                ${opts}></iframe>
</div>
`;
                this.aelement = angular.element(angularElement);
                this.canvas = this.aelement[0] as HTMLCanvasElement; // TODO this seems wrong
                // $scope.canvas = angular.element('<iframe id="'+v.vid+'" class="jsCanvas" src="/cs/gethtml/canvas.html" ' + v.w + v.h + ' style="border:0" seamless="seamless" ></iframe>');
                this.iframeLoadTries = 10;
            } else {
                this.canvas = angular.element(// '<div class="userlist" tim-draggable-fixed="" style="top: 91px; right: -375px;">'+
                    '<canvas id="csCanvas" width="' + this.canvasWidth + '" height="' + this.canvasHeight + '" class="jsCanvas"></canvas>' +
                    // '<canvas id="csCanvas" width="'+$scope.canvasWidth+'" height="'+$scope.canvasHeight+'" class="jsCanvas userlist" tim-draggable-fixed="" style="top: 91px; right: -375px;"></canvas>' +
                    "")[0] as HTMLCanvasElement; // '</div>');
            }
            const $previewDiv = angular.element(this.preview);
            // $previewDiv.html($scope.canvas);
            $previewDiv.empty().append($compile(this.canvas)(this.scope));
            // $scope.canvas = $scope.preview.find(".csCanvas")[0];
        }
        let text = this.usercode.replace(this.cursor, "");
        // if ( text === $scope.lastJS && $scope.userargs === $scope.lastUserargs && $scope.userinput === $scope.lastUserinput ) return;
        if (!this.attrs.runeverytime && text === this.lastJS && this.userargs === this.lastUserargs && this.userinput === this.lastUserinput) {
            return;
        }
        this.lastJS = text;
        this.lastUserargs = this.userargs;
        this.lastUserinput = this.userinput;

        text = this.getCode();

        if (this.iframe) { // in case of iframe, the text is send to iframe
            const f = document.getElementById(this.taunoId) as GlowScriptFrame; // but on first time it might be not loaded yet
            // var s = $scope.taunoElem.contentWindow().getUserCodeFromTauno();
            // wait for contentWindow ready and the callback function also
            if (!f || !f.contentWindow || (!f.contentWindow.runJavaScript && !this.fullhtml && !wescheme)
                || (wescheme && !f.contentWindow.runWeScheme)) {
                this.lastJS = "";
                this.lastUserargs = "";
                this.lastUserinput = "";
                this.iframeLoadTries--;
                if (this.iframeLoadTries <= 0) {
                    return;
                }
                console.log("Odotetaan 300 ms");
                await $timeout(300);
                this.showJS();
                return;
            }
            this.contentWindow = f.contentWindow;
            if (this.iframeClientHeight < 0) {
                this.iframeClientHeight = f.clientHeight;
            }
            if (this.gsDefaultLanguage) {
                f.contentWindow.setDefLanguage(this.gsDefaultLanguage);
            }
            if (this.fullhtml) {
                let fhtml = this.fullhtml.replace("REPLACEBYCODE", text);
                if (isProcessing) {
                    fhtml = '<script src="/cs/static/processing/processing.js"></script>\n' +
                        '<script type="text/processing" data-processing-target="mycanvas">\n' +
                        fhtml + "\n" +
                        "</script>\n" +
                        '<canvas id="mycanvas"></canvas>';
                }
                f.contentWindow.document.open();
                f.contentWindow.document.write(fhtml);
                f.contentWindow.document.close();
            } else if (wescheme) {
                // $scope.runWeScheme($scope.usercode);
                try {
                    f.contentWindow.runWeScheme(this.usercode);
                } catch (e) {
                    console.log(e);
                }
            } else {
                const s = f.contentWindow.runJavaScript(text, this.userargs, this.userinput, wantsConsole);
            }
            if (f.contentWindow.getConsoleHeight) {
                let ch = f.contentWindow.getConsoleHeight();
                if (ch < this.iframeClientHeight) {
                    ch = this.iframeClientHeight;
                }
                f.height = "";
                f.height = "" + ch + "px";
            }

            return;
        }
        this.error = "";
        this.runError = false;
        try {
            const ctx = this.canvas.getContext("2d");
            ctx.save();
            this.result = "";
            const beforeCode = "function paint(ctx,out, userargs, userinput, console) { ";
            const afterCode = "\n}\n";
            let a = "";
            let b = "";
            let cons;
            if (wantsConsole) {
                b = beforeCode;
                a = afterCode;
                cons = this.canvasConsole;
            }

            const paint = new Function("return (" + b + text + a + ")")();
            if (!this.out) {
                this.out = this.element0.getElementsByClassName("console")[0] as any;
                this.out.write = (s: string) => this.write(s);
                this.out.writeln = (s: string) => this.writeln(s);
                this.out.canvas = this.canvas;
            }
            paint(ctx, this.out, this.userargs, this.userinput, cons);
            ctx.restore();
        } catch (exc) {
            let rivi = "";
            let sarake = "";
            if (exc.column) {
                sarake = " Col " + exc.column.toString() + ": ";
            }
            if (exc.line) {
                rivi = "Row " + exc.line + ": ";
            } else if (exc.lineNumber) {
                rivi = "Row " + exc.lineNumber + ": ";
            } // Safari has lineNUmber
            this.error = rivi + sarake + exc.toString();
            this.runError = this.error;
        }
        this.safeApply();
    }

    safeApply(fn?: () => any) {
        const phase = this.scope.$root.$$phase;
        if (phase === "$apply" || phase === "$digest") {
            if (fn && (typeof (fn) === "function")) {
                fn();
            }
        } else {
            this.scope.$apply();
        }
    }
}

/* Add fillCircle to canvas context */
Object.getPrototypeOf(document.createElement("canvas").getContext("2d")).fillCircle =
    function(this: CanvasRenderingContext2D, x: number, y: number, r: number) {
        this.beginPath();
        this.arc(x, y, r, 0, Math.PI * 2, false);
        this.closePath();
        this.fill();
        this.stroke();
    };

const commonComponentOptions = {
    bindings: {
        json: "@",
    },
    controller: CsController,
};

csApp.component("csRunner", {
    ...commonComponentOptions,
    template: makeTemplate(),
});
csApp.component("csJypeliRunner", {
    ...commonComponentOptions,
    template: makeTemplate(),
});
csApp.component("csComtestRunner", {
    ...commonComponentOptions,
    template: makeTemplate(),
});
csApp.component("csRunnerInput", {
    ...commonComponentOptions,
    template: makeTemplate(),
});
csApp.component("csJypeliRunnerInput", {
    ...commonComponentOptions,
    template: makeTemplate(),
});
csApp.component("csComtestRunnerInput", {
    ...commonComponentOptions,
    template: makeTemplate(),
});
csApp.component("csTaunoRunner", {
    ...commonComponentOptions,
    template: makeTemplate(),
});
csApp.component("csTaunoRunnerInput", {
    ...commonComponentOptions,
    template: makeTemplate(),
});
csApp.component("csParsonsRunner", {
    ...commonComponentOptions,
    template: makeTemplate(),
});
csApp.component("csSageRunner", {
    ...commonComponentOptions,
    template: makeTemplate(),
});
csApp.component("csSimcirRunner", {
    ...commonComponentOptions,
    template: makeTemplate(),
});
csApp.component("csTextRunner", {
    ...commonComponentOptions,
    template: `
<div class="csRunDiv csTinyDiv" style="text-align: left;">
    <p>Here comes header</p>
    <span ng-if="$ctrl.stem"
          class="stem"
          ng-bind-html="$ctrl.stem"></span>
    <input class="csTinyText no-popup-menu"
           ng-hide="$ctrl.noeditor && !$ctrl.viewCode"
           size="{{$ctrl.cols}}"
           ng-model="$ctrl.usercode"
           ng-trim="false"
           ng-attr-placeholder="{{$ctrl.placeholder}}"
           ng-keypress="$ctrl.runCodeIfCR($event);"/>
    <button ng-if="$ctrl.isRun"
            ng-disabled="$ctrl.isRunning"
            title="(Ctrl-S)"
            ng-click="$ctrl.runCode();"
            ng-bind-html="$ctrl.buttonText"></button>
    &nbsp;&nbsp;<a href=""
                   ng-if="$ctrl.muokattu"
                   ng-click="$ctrl.initCode();">{{$ctrl.resetText}}</a>&nbsp;&nbsp;
    <pre class="console"
         ng-show="$ctrl.result">{{$ctrl.result}}</pre>
    <span class="csRunError"
          ng-if="$ctrl.runError"
          ng-style="$ctrl.tinyErrorStyle">{{$ctrl.error}}</span>
    <div class="htmlresult"
         ng-if="$ctrl.htmlresult">
        <span ng-bind-html="$ctrl.svgImageSnippet()"></span>
    </div>
</div>
`,
});
csApp.component("csWeschemeRunner", {
    ...commonComponentOptions,
    template: makeTemplate(),
});

const csConsoleApp = angular.module("csConsoleApp", ["ngSanitize"]);

class CsConsoleController implements IController {
    private static $inject = ["$element"];

    isShell: boolean;
    cursor: number;
    currentSize: string;
    isHtml: boolean;
    oldpwd: string;
    currentInput: string;
    pwd: string;
    attrs: AttrType;
    byCode: string;
    taskId: string;
    plugin: string;
    ident: string;
    content: AttrType;
    examples: Array<{expr: string, title: string}>;
    history: Array<{istem: string, ostem: string, input: string, response: string}>;
    savestate: string;
    path: string;
    type: string;

    // Binding that has all the data as a JSON string.
    private json!: Binding<string, "@">;

    constructor(private element: IRootElementService) {

    }

    $postLink() {
        const attrs = this.attrs;
        set(this, attrs, "usercode", "");
        set(this, attrs, "type", "cs");
        set(this, attrs, "path");
        set(this, attrs, "savestate");
        this.pwd = ConsolePWD.getPWD(this);
        this.oldpwd = this.pwd;
        this.isShell = languageTypes.getRunType(this.type, "") === "shell";
        if (this.usercode === "" && this.byCode) {
            this.usercode = this.byCode.split("\n")[0];
        }
        this.currentInput = this.usercode;
        if (this.isShell) {
            ConsolePWD.register(this);
        }
    }

    $onInit() {
        this.attrs = JSON.parse(this.data);
        this.byCode = this.attrs.by || this.attrs.byCode;

        // This block could be re-used
        this.taskId = this.element.parent().attr("id");
        this.plugin = this.element.parent().attr("data-plugin");
        const reqPath = this.plugin + "/" + this.ident + "/";
        this.content = this.attrs;
        // End of generally re-usable TIM stuff

        this.examples = [];

        if (this.content.examples) {
            const s = this.content.examples.replace(/'/g, '"');
            this.examples = JSON.parse(s);
        }

        this.history = [];

        this.currentSize = "normal";
        this.currentInput = "";
        this.cursor = this.history.length; // $scope.history.length means new input is last command.
    }

    setPWD(pwd: string) {
        this.pwd = pwd;
    }

    loadExample(i: number) {
        const $scope = this;
        $scope.currentInput = $scope.examples[i].expr;
        $scope.focusOnInput();
    }

    focusOnInput() {
        const el = this.element[0].querySelector(".console-input") as HTMLInputElement | null;
        if (el) {
            el.focus();
        }
    }

    async handler() {
        let url = "/cs/answer";
        if (this.plugin) {
            url = this.plugin;
            const i = url.lastIndexOf("/");
            if (i > 0) {
                url = url.substring(i);
            }
            url += "/" + this.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        }
        const t = languageTypes.getRunType(this.content.type, "shell");
        const ucode = this.currentInput;
        const isInput = false;
        const uargs = "";
        const uinput = "";

        const r = await to($http<{web: {pwd?: string, error?: string, console?: string}}>({
            method: "PUT",
            url: url,
            data: {
                input: {
                    usercode: ucode,
                    userinput: uinput,
                    isInput: isInput,
                    userargs: uargs,
                    type: t,
                },
            },
        }));
        if (r.ok) {
            const data = r.result.data;
            let s = "";
            this.oldpwd = this.pwd;
            if (data.web.pwd) {
                ConsolePWD.setPWD(data.web.pwd, this);
            }
            if (data.web.error) {
                s = data.web.error;
                s = "<pre>" + s + "</pre>";
            } else {
                s = data.web.console || "";
                if (!this.isHtml) {
                    s = "<pre>" + s + "</pre>";
                }
            }
            this.submit(s);
            // console.log(["data", data.web]);
        } else {
            console.log(["protocol error", r.result.data]);
            this.submit("Endless loop?");
        }
    }

    toggleSize() {
        if (this.currentSize === "normal") {
            this.currentSize = "enlarged";
        } else {
            this.currentSize = "normal";
        }
    }

    async submit(result: string) {
        this.history.push({
            istem: this.isShell ? this.history.length + " " + this.oldpwd + "$" : "in_" + this.history.length + ": ",
            ostem: this.isShell ? "" : "out_" + this.history.length + ": ",
            input: this.currentInput,
            response: result,
        });
        this.currentInput = "";
        this.cursor = this.history.length;
        await $timeout();
        const el = this.element[0].querySelector(".console-output");
        if (el) {
            el.scrollTop = el.scrollHeight;
        }
    }

    load() {
        if (this.cursor >= this.history.length) {
            this.currentInput = "";
            this.cursor = this.history.length;
            return;
        }
        const norm = Math.min(this.history.length - 1, Math.max(0, this.cursor));
        this.currentInput = this.history[norm].input;
        this.cursor = norm;
    }

    up() {
        if (!this.cursor) {
            return;
        }
        this.cursor--;
        this.load();
    }

    down() {
        this.cursor++;
        this.load();
    }

    handleKey(ev: KeyboardEvent) {
        if (ev.which === 13) {
            this.handler();
        }// submit();
        if (ev.which === 40) {
            this.down();
        }
        if (ev.which === 38) {
            this.up();
        }
    }
}

csConsoleApp.component("csConsole", {
    bindings: {
        json: "@",
    },
    controller: CsConsoleController,
    template: `
<div class="web-console no-popup-menu {{$ctrl.currentSize}} " ng-keydown="$ctrl.handleKey($event)"><code
        class="console-output">
    <div class="console-output-elem"
         ng-repeat="item in $ctrl.history track by $index"><span class="console-oldinput">  <span
            class="console-in">{{item.istem}}</span>  <span class="console-userInput">{{item.input}}</span> </span>
        <span class="console-oldresponse"><span ng-if="!$ctrl.isShell">  <br/>  <span
                class="console-out">{{item.ostem}}</span></span>  <span class="console-response"
                                                                        ng-class="{error:item.error}"><span
                ng-bind-html="item.response"></span></span>
            <!-- Double span since ng-bind eats the innermost one -->
            </span></div>
    <span class="console-expander-sym" ng-click="$ctrl.toggleSize()"></span></code>
    <div class="console-examples-box">
        <span class="examples-title"
              ng-click="$ctrl.examplesVisible=!$ctrl.examplesVisible">    ▼ example expressions ▲</span>
        <div>Click to load:</div>
        <ul>
            <li ng-repeat="example in $ctrl.examples track by $index">
                <a ng-click="$ctrl.loadExample($index)"
                   title="{{example.expr}}">{{example.title||example.expr}}</a>
            </li>
            <ul>
    </div>
    <div class="console-curIndex" ng-if="$ctrl.isShell">{{$ctrl.pwd}}</div>
    <span class="console-curIndex">in_{{$ctrl.cursor}}</span><input type="text" placeholder="type expressions here"
                                                                    class="console-input"
                                                                    ng-model="$ctrl.currentInput"/>&nbsp;<div
            class="console-buttons">
        <button ng-click="$ctrl.up()">↑</button>&nbsp;<button ng-click="$ctrl.down()">↓</button>&nbsp;<button
            ng-click="$ctrl.handler()">
        Enter
    </button>&nbsp;
    </div>
</div>
`,
});

export function truthTable(sentence: string, topbottomLines: boolean) {

    let result = "";
    try {
        if (!sentence) {
            return "";
        }
        if (!sentence.trim()) {
            return "";
        }
        const replace = "v ||;^ &&;~ !;∧ &&;∨ ||;∼ !;xor ^;and &&;not !;or ||;ja &&;ei !;tai ||";
        const abcde = "abcdefghijklmnopqrstuxy";
        let header = "";
        let vals = '""';
        let count = 0;
        let cnt2 = 1;

        let input = sentence.toLowerCase();

        const repls = replace.split(";");
        for (const i of repls) {
            const r = i.split(" ");
            input = input.split(r[0]).join(r[1]);
        }

        for (let i = 0; i < abcde.length; i++) {
            if (input.indexOf(abcde[i]) >= 0) {
                header += abcde[i] + " ";
                const zv = "z[" + count + "]";
                input = input.split(abcde[i]).join(zv);
                vals += "+" + zv + '+" "';
                count++;
                cnt2 *= 2;
            }
        }

        const sents = sentence.split(";");
        const lens = [];
        const fills = [];
        for (let i = 0; i < sents.length; i++) {
            sents[i] = sents[i].trim();
            lens[i] = sents[i].length;
            fills[i] = "                                                               ".substring(0, lens[i]);
        }
        header += "  " + sents.join("  ");
        const line = "---------------------------------------".substring(0, header.length);
        // result += input + "\n";
        if (topbottomLines) {
            result += line + "\n";
        }
        result += header + "\n";
        result += line + "\n";
        for (let n = 0; n < cnt2; n++) {
            const z = [];
            for (let i = 0; i < count; i++) {
                z[i] = (n >> (count - 1 - i)) & 1;
            }
            result += eval(vals) + "= ";
            const inp = input.split(";");
            for (let i = 0; i < inp.length; i++) {
                const tulos = " " + (eval(inp[i]) ? 1 : 0) + fills[i];
                result += tulos;
            }
            result += "\n";
        }
        if (topbottomLines) {
            result += line + "\n";
        }
        return result;
    } catch (err) {
        return result + "\n" + err + "\n";
    }
}
