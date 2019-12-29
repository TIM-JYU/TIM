/* eslint-disable @typescript-eslint/no-explicit-any,@typescript-eslint/tslint/config,no-underscore-dangle */
import {Ace} from "ace-builds/src-noconflict/ace";
import angular, {IController, IScope} from "angular";
import * as t from "io-ts";
import $ from "jquery";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {IAce} from "tim/editor/ace";
import {IPluginInfoResponse, ParCompiler} from "tim/editor/parCompiler";
import {GenericPluginMarkup, Info, nullable, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$compile, $http, $sce, $timeout, $upload} from "tim/util/ngimport";
import {copyToClipboard, getClipboardHelper, to, valueDefu, valueOr} from "tim/util/utils";
import {CellInfo} from "./embedded_sagecell";
import IAceEditor = Ace.Editor;

interface GlowScriptWindow extends Window {
    runJavaScript(text: string, args: string, input: string, wantsConsole: boolean): string;

    setDefLanguage(language: string): void;

    getConsoleHeight?(): number;

    runWeScheme(s: string): void;
}

interface TaunoWindow extends Window {
    getUserCodeFromTauno(): string;
}

interface CustomFrame<T extends Window> extends HTMLIFrameElement {
    contentWindow: T;
}

// js-parsons is unused; just declare a stub to make TS happy
declare class ParsonsWidget {
    static _graders: any;

    constructor(data: {});

    options: {permutation: (n: number) => number[]};

    init(a: string): void;

    show(): void;

    getFeedback(): unknown;

    shuffleLines(): void;
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
    w: string;
    h: string;
}

const csPluginStartTime = new Date();
/*
Sagea varten ks: https://github.com/sagemath/sagecell/blob/master/doc/embedding.rst#id3
*/

const csApp = angular.module("csApp", ["ngSanitize", "ngFileUpload"]);

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
    savestate?: string;
    path?: string;
    attrs?: {path?: string};

    setPWD(s: string): void;
}

type IPwdWithoutSetPWD = Pick<IPwd, "savestate" | "path" | "attrs"> & {setPWD?(s: string): void};

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

    isUser(scope: IPwdWithoutSetPWD) {
        return (scope.path === "user" || (scope.attrs && scope.attrs.path === "user"));
    }

    setPWD(pwd: string, scope: IPwdWithoutSetPWD) {
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
    for (const ty of types) {
        if (file.endsWith(ty)) {
            if (ty !== "pdf") {
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
    const load = await import("../simcir/simcir-all");
    return load.simcir;
}

// =================================================================================================================
// Things for known languages

class LanguageTypes {
    // What are known language types (be careful not to include partial word):
    runTypes = ["pascal", "fortran", "css", "jypeli", "scala", "java", "graphics", "cc", "c++", "shell", "vpython", "py2", "py", "fs", "clisp",
        "jjs", "psql", "sql", "alloy", "text", "cs", "run", "md", "js", "glowscript", "sage", "simcir",
        "xml", "octave", "lua", "quorum", "swift", "mathcheck", "html", "processing", "rust", "r", "wescheme", "ping", "kotlin",
        "smalltalk", "upload"];

    // For editor modes see: http://ace.c9.io/build/kitchen-sink.html ja sieltä http://ace.c9.io/build/demo/kitchen-sink/demo.js
    aceModes = ["pascal", "fortran", "css", "csharp", "scala", "java", "java", "c_cpp", "c_cpp", "sh", "python", "python", "python", "fsharp", "lisp",
        "javascript", "sql", "sql", "alloy", "text", "csharp", "run", "text", "javascript", "javascript", "python", "json",
        "xml", "matlab", "lua", "quorum", "swift", "text", "html", "javascript", "text", "r", "scheme", "text", "kotlin",
        "text", "text"];

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
        for (const ty of types) {
            if (type.indexOf(ty) >= 0) {
                return ty;
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
        if (type.match(/^all[^a-z0-9]?/)) {
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

        const ty = this.whatIsIn(this.testTypes, type, def);
        if (ty !== "comtest") {
            return ty;
        }
        const lt = this.whatIsIn(this.runTypes, language, "console");
        const impt = this.impTestTypes[lt];
        if (impt) {
            return impt;
        }
        return ty;
    }

    getUnitTestType(type: string, language: string, def: string) {

        const ty = this.whatIsIn(this.unitTestTypes, type, def);
        if (ty !== "unit") {
            return ty;
        }
        const lt = this.whatIsIn(this.runTypes, language, "console");
        const impt = this.impUnitTestTypes[lt];
        if (impt) {
            return impt;
        }
        return ty;
    }

    isInArray(word: string, array: string[]) {
        return array.includes(word);
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
    if (!s || s === "//") {
        return "";
    }
    const n = s.indexOf("//\n");
    if (n !== 0) {
        return s;
    }
    return s.substr(3);
}

function makeTemplate() {
    return `<div ng-class="::{'csRunDiv': $ctrl.attrs.borders}" class="type-{{::$ctrl.rtype}}">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" class="stem" ng-bind-html="::$ctrl.stem"></p>
    <div ng-if="::$ctrl.isSimcir || $ctrl.isTauno">
        <p ng-if="$ctrl.taunoOn" class="pluginHide"><a ng-click="$ctrl.hideTauno()">{{$ctrl.hideTaunoText}}</a></p>
        <div class="taunoContainer"><p></p></div>
        <p ng-if="!$ctrl.taunoOn" class="pluginShow"><a ng-click="$ctrl.showTauno()">{{$ctrl.showTaunoText}}</a></p>
        <p ng-if="$ctrl.taunoOn && $ctrl.isTauno"
           class="pluginHide">
            <a ng-click="$ctrl.copyTauno()">{{::$ctrl.copyFromTaunoText}}</a> |
            <a ng-click="$ctrl.hideTauno()">{{::$ctrl.hideTaunoText}}</a></p>
        <p ng-if="$ctrl.taunoOn && $ctrl.isTauno" class="taunoOhje">
            {{::$ctrl.taunoOhjeText}}</a></p>
        <p ng-if="$ctrl.taunoOn && !$ctrl.noeditor && $ctrl.isSimcir" class="pluginHide">
            <a ng-click="$ctrl.copyFromSimcir()">copy from SimCir</a>
            | <a ng-click="$ctrl.copyToSimcir()">copy to SimCir</a> | <a ng-click="$ctrl.hideTauno()">hide SimCir</a>
        </p>
    </div>
    <div ng-if="::$ctrl.upload" class="form-inline small">
        <div class="form-group small"> {{::$ctrl.uploadstem}}:
            <input type="file" ngf-select="$ctrl.onFileSelect($file)">
            <span ng-show="$ctrl.fileProgress >= 0 && !$ctrl.fileError"
                  ng-bind="$ctrl.fileProgress < 100 ? 'Uploading... ' + $ctrl.fileProgress + '%' : 'Done!'"></span>
        </div>
        <div class="error" ng-show="$ctrl.fileError" ng-bind="$ctrl.fileError"></div>
        <div ng-if="$ctrl.uploadresult"><span ng-bind-html="$ctrl.uploadresult"></span></div>
    </div>
    <div ng-show="::$ctrl.isAll" style="float: right;">{{::$ctrl.languageText}}
        <select ng-model="$ctrl.selectedLanguage" ng-options="o for o in ::$ctrl.progLanguages" ng-required></select>
    </div>
    <pre ng-if="$ctrl.viewCode && $ctrl.codeover">{{$ctrl.code}}</pre>
    <div class="csRunCode">
        <pre class="csRunPre" ng-if="$ctrl.viewCode && !$ctrl.codeunder && !$ctrl.codeover">{{$ctrl.precode}}</pre>
        <div class="csEditorAreaDiv">
            <div class="csrunEditorDiv" ng-hide="$ctrl.noeditor && !$ctrl.viewCode">
            <textarea class="csRunArea csEditArea no-popup-menu"
                      rows="{{$ctrl.rows}}"
                      ng-model="$ctrl.usercode"
                      ng-trim="false"
                      ng-attr-placeholder="{{$ctrl.placeholder}}"></textarea>
            </div>
            <div class="csRunChanged" ng-if="$ctrl.usercode !== $ctrl.byCode"></div>
            <div class="csRunNotSaved" ng-show="$ctrl.notSaved"></div>
        </div>
        <pre class="csRunPost" ng-if="$ctrl.viewCode && !$ctrl.codeunder && !$ctrl.codeover">{{$ctrl.postcode}}</pre>
    </div>
    <div ng-if="::$ctrl.isSage" class="computeSage no-popup-menu"></div>
    <div class="csInputDiv" ng-hide="::!$ctrl.showInput || !$ctrl.isInput"><p ng-show="::$ctrl.inputstem" class="stem">
        {{::$ctrl.inputstem}}</p>
        <div class="csRunCode"><textarea class="csRunArea csInputArea"
                                         rows={{::$ctrl.inputrows}}
                                         ng-model="$ctrl.userinput"
                                         ng-trim="false"
                                         placeholder="{{::$ctrl.inputplaceholder}}"></textarea></div>
    </div>
    <div class="csArgsDiv" ng-hide="::!$ctrl.showArgs || !$ctrl.isInput"><label>{{::$ctrl.argsstem}} </label>
        <span><input type="text"
                     class="csArgsArea"
                     ng-model="$ctrl.userargs"
                     ng-trim="false"
                     placeholder="{{::$ctrl.argsplaceholder}}"></span>
    </div>
    <p class="csRunSnippets" ng-if="::$ctrl.buttons">
        <button ng-repeat="item in ::$ctrl.buttons" ng-click="$ctrl.addText(item)">{{$ctrl.addTextHtml(item)}}</button>
        &nbsp;&nbsp;
    </p>
    <div class="csRunMenuArea" ng-if="::!$ctrl.forcedupload">
        <p class="csRunMenu">
            <button ng-if="::$ctrl.isRun && $ctrl.buttonText()"
                    ng-disabled="$ctrl.isRunning"
                    title="(Ctrl-S)"
                    ng-click="$ctrl.runCode()"
                    ng-bind-html="::$ctrl.buttonText()"></button>
            &nbsp&nbsp
            <button ng-if="::$ctrl.isTest" ng-disabled="$ctrl.isRunning" ng-click="$ctrl.runTest()">Test</button>
            &nbsp&nbsp
            <button ng-if="::$ctrl.isUnitTest"
                    ng-disabled="$ctrl.isRunning"
                    ng-click="$ctrl.runUnitTest()">UTest
            </button>
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
            <span ng-if="$ctrl.wrap!=-1" class="inputSmall" style="float: right;"><label
                    title="Put 0 to no wrap">wrap: <input type="text"
                                                          ng-pattern="/[-0-9]*/"
                                                          ng-model="$ctrl.wrap"
                                                          size="2"/></label></span></p>

    </div>
    <div ng-if="::$ctrl.isSage" class="outputSage no-popup-menu"></div>
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
    <p class="footer" ng-bind-html="::$ctrl.footer"></p>
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

function getInt(s: string | number) {
    if (typeof s === "number") {
        return s;
    }
    const n = parseInt(s, 10);
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

function ifIs(value: number | string | undefined, name: string, def: string | number) {
    if (!value && !def) {
        return "";
    }
    if (!value) {
        return `${name}="${def}" `;
    }
    return `${name}="${value}" `;
}

function doVariables(v: string | undefined, name: string) {
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

/**
 * This defines the required format for the csPlugin YAML markup.
 * All fields in the markup are optional (as indicated by t.partial function).
 *
 * If a field does not match the type, an error will be shown to the user
 * and the plugin won't work until the problem is fixed.
 */

const Example = t.type({
    expr: t.string,
    title: t.string,
});

const CsMarkupOptional = t.partial({
    // TODO: this gets deleted in server but only conditionally,
    //  decide if this should be here.
    //  Seems yes; GlowScript uses it at least.
    program: t.string,

    argsplaceholder: t.string,
    argsstem: t.string,
    autoupdate: t.number,
    buttons: t.string,
    byCode: t.string,
    docurl: t.string,
    examples: t.array(Example),
    file: t.string,
    filename: t.string,
    fullhtml: t.string,
    height: t.union([t.number, t.string]),
    highlight: nullable(t.string),
    html: t.string,
    indices: t.string,
    inputplaceholder: t.string,
    languages: t.string, // not used in any plugin? // TODO: should be used to give set of languages that can be used
    mode: t.string,
    noeditor: t.boolean,
    normal: nullable(t.string),
    parsonsmaxcheck: t.number,
    path: t.string,
    placeholder: nullable(t.string),
    replace: t.string,
    runeverytime: t.boolean,
    savestate: t.string,
    scripts: t.string,
    selectedLanguage: t.string,
    showCodeOff: t.string,
    showCodeOn: nullable(t.string),
    table: t.string,
    taunotype: t.string,
    treplace: t.string,
    uploadbycode: t.boolean,
    uploadautosave: t.boolean,
    uploadstem: t.string,
    userargs: t.union([t.string, t.number]),
    userinput: t.union([t.string, t.number]),
    variables: t.string,
    width: t.union([t.number, t.string]),
    wrap: t.Integer,
    borders: withDefault(t.boolean, true),
});

const CsMarkupDefaults = t.type({
    /* eslint-disable quote-props */
    autorun: withDefault(t.boolean, false),
    parsonsnotordermatters: withDefault(t.boolean, false),
    blind: withDefault(t.boolean, false),
    canvasHeight: withDefault(t.number, 300),
    canvasWidth: withDefault(t.number, 700),
    codeover: withDefault(t.boolean, false),
    codeunder: withDefault(t.boolean, false),
    cols: withDefault(t.Integer, 10),
    copyLink: withDefault(t.string, "Copy"),
    cssPrint: withDefault(t.boolean, false),
    editorMode: withDefault(t.Integer, -1),
    editorModes: withDefault(t.union([t.string, t.Integer]), "01"),
    iframe: withDefault(t.boolean, false), // TODO this maybe gets deleted on server
    iframeopts: withDefault(t.string, ""),
    indent: withDefault(t.Integer, -1),
    initSimcir: withDefault(t.string, ""),
    "style-args": withDefault(t.string, ""), // TODO get rid of "-"
    "style-words": withDefault(t.string, ""), // TODO get rid of "-"
    inputrows: withDefault(t.Integer, 1),
    inputstem: withDefault(t.string, ""),
    isHtml: withDefault(t.boolean, false),
    jsparsons: withDefault(t.string, "JS-Parsons"),
    justSave: withDefault(t.boolean, false),
    lang: withDefault(t.string, "fi"),
    maxrows: withDefault(t.Integer, 100),
    noConsoleClear: withDefault(t.boolean, false),
    nocode: withDefault(t.boolean, false),
    norun: withDefault(t.boolean, false),
    nosave: withDefault(t.boolean, false),
    open: withDefault(t.boolean, false),
    parsons: withDefault(t.string, "Parsons"),
    rows: withDefault(t.Integer, 1),
    showRuntime: withDefault(t.boolean, false),
    toggleEditor: withDefault(t.union([t.boolean, t.string]), false),
    type: withDefault(t.string, "cs"),
    upload: withDefault(t.boolean, false),
    validityCheck: withDefault(t.string, ""),
    validityCheckMessage: withDefault(t.string, ""),
    viewCode: withDefault(t.boolean, false),
    words: withDefault(t.boolean, false),
    /* eslint-enable quote-props */
});

const CsMarkup = t.intersection([CsMarkupOptional, CsMarkupDefaults, GenericPluginMarkup]);

const CsAll = t.intersection([
    t.partial({
        by: t.string,
        docurl: t.string,
        program: t.string,
        replace: t.string,
        uploadedFile: t.string,
        uploadedType: t.string,
        userargs: t.string,
        usercode: t.string,
        userinput: t.string,
        selectedLanguage: t.string,
    }),
    t.type({
        // anonymous: t.boolean,
        // doLazy: t.boolean,
        info: Info,
        markup: t.readonly(CsMarkup),
        preview: t.boolean,
        // review: t.boolean,
        // targetFormat: t.literal("latex"),
        // taskID: t.string,
        // taskIDExt: t.string,
        // userPrint: t.boolean,
    })]);

class CsBase extends PluginBase<t.TypeOf<typeof CsMarkup>, t.TypeOf<typeof CsAll>, typeof CsAll> {
    protected usercode: string = "";

    get byCode() {
        return commentTrim(this.attrsall.by || this.attrs.byCode || "");
    }

    get type() {
        return this.attrs.type;
    }

    get path() {
        return this.attrs.path;
    }

    protected getAttributeType() {
        return CsAll;
    }

    getDefaultMarkup() {
        return {norun: true}; // prevent running broken plugin accidentally
    }
}

function numOrDef(val: string | number | undefined, def: number) {
    if (typeof val === "number") {
        return val;
    }
    return def;
}

class CsController extends CsBase implements ITimComponent {
    private vctrl!: ViewCtrl;

    private notSaved: boolean = false;
    private aceEditor?: IAceEditor;
    private canvas?: HTMLCanvasElement;
    private canvasConsole: {log: (...args: string[]) => void};
    private code?: string;
    private codeInitialized: boolean = false;
    private comtestError?: string;
    private contentWindow?: GlowScriptWindow;
    private copyingFromTauno: boolean;
    private csparson: any;
    private cursor: string;
    private docLink: string;
    private docURL?: string;
    private edit!: HTMLTextAreaElement;
    private editArea?: Element;
    private editorIndex: number;
    private editorMode!: number;
    private editorModeIndecies: number[];
    private error?: string;
    private errors: string[];
    private fileError?: string;
    private fileProgress?: number;
    private gsDefaultLanguage?: string;
    private htmlresult: string;
    private iframeClientHeight: number;
    private iframeLoadTries: number = 10;
    private imgURL: string;
    private indent!: number;
    private initUserCode: boolean = false;
    private irrotaKiinnita?: string;
    private isRunning: boolean = false;
    private lastJS: string;
    private lastMD: string;
    private lastUserargs?: string;
    private lastUserinput?: string;
    private localcode?: string;
    private maxRows!: number;
    private muokattu: boolean;
    private noeditor!: boolean;
    private oneruntime?: string;
    private out?: {write: () => void, writeln: () => void, canvas: Element};
    private parson?: ParsonsWidget;
    private parsonsId?: Vid;
    private postcode?: string;
    private precode?: string;
    private preview!: JQuery<HTMLElement>;
    private result?: string;
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
    private simcir?: JQuery;
    private taunoElem!: HTMLElement;
    private taunoOn: boolean;
    private tinyErrorStyle: Partial<CSSStyleDeclaration> = {};
    private uploadedFile?: string;
    private uploadedType?: string;
    private uploadresult?: string;
    private userargs: string = "";
    private userinput: string = "";
    private viewCode!: boolean;
    private wavURL: string = "";
    private wrap!: number;
    private buttons: string[] = [];

    // These are used only in $doCheck to keep track of the old values.
    private dochecks: {
        userinput?: string;
        userargs?: string;
        usercode?: string;
        savedcode?: string;
        savedargs?: string;
        savedinput?: string;
    } = {};

    private editorText: string[] = [];
    private rows: number = 1;

    constructor(scope: IScope, element: JQLite) {
        super(scope, element);
        this.errors = [];
        this.taunoOn = false;
        this.result = "";
        this.htmlresult = "";
        this.imgURL = "";
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
        this.docLink = "Document";
        this.muokattu = false;
        this.editorIndex = 0;
        this.editorModeIndecies = [];
    }

    getContent(): string {
        return this.usercode;
    }

    async save() {
        this.runCode();
        return {saved: true, message: undefined};
    }

    isUnSaved() {
        return this.notSaved && this.pluginMeta.getTaskId() !== undefined && !this.nosave;
    }

    resetField(): undefined {
        this.initCode();
        return undefined;
    }

    svgImageSnippet() {
        return $sce.trustAsHtml(this.htmlresult);
    }

    get english() {
        return this.attrs.lang === "en";
    }

    get kind() {
        let kind;
        switch (this.getRootElement().tagName.toLowerCase()) {
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
            case "cs-geogebra-runner":
                kind = "geogebra";
                break;
            case "cs-jsav-runner":
                kind = "jsav";
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
        return kind;
    }

    get isInput() {
        return this.getRootElement().tagName.toLowerCase().endsWith("-input");
    }

    get isSimcir() {
        return this.kind === "simcir";
    }

    get isTauno() {
        return this.kind === "tauno";
    }

    getTaunoOrSimcir() {
        if (this.isSimcir) {
            return "SimCir";
        } else {
            return "Tauno";
        }
    }

    get program() {
        return this.attrsall.program || this.attrs.program;
    }

    get hideTaunoText() {
        return (this.english ? "hide " : "piilota ") + this.getTaunoOrSimcir();
    }

    get showTaunoText() {
        return (this.english ? "Click here to show " : "Näytä ") + this.getTaunoOrSimcir();
    }

    get taunoOhjeText() {
        return this.english ?
            'Copy the code you made by Tauno by pressing the link "copy from Tauno". Then press Run button. Note that the running code may have different code than in Tauno!' :
            'Kopioi Taunolla tekemäsi koodi "kopioi Taunosta"-linkkiä painamalla. Sitten paina Aja-painiketta. Huomaa, että ajossa voi olla eri taulukko kuin Taunossa!';
    }

    get copyFromTaunoText() {
        return this.english ? "copy from Tauno" : "kopioi Taunosta";
    }

    get copyFromSimCirText() {
        return this.english ? "copy from SimCir" : "kopioi SimCiristä";
    }

    get copyToSimCirText() {
        return this.english ? "copy to SimCir" : "kopioi SimCiriin";
    }

    get languageText() {
        return this.english ? "language: " : "kieli: ";
    }

    get forcedupload() {
        return this.type === "upload" && !this.attrs.button ;
    }

    get upload() {
        return this.type === "upload" || this.attrs.upload || this.attrs.uploadbycode;
    }

    get rtype() {
        return languageTypes.getRunType(this.type, "text");
    }

    get isSage() {
        return this.rtype === "sage";
    }

    get isMathCheck() {
        return this.rtype === "mathcheck";
    }

    get nocode() {
        return this.type === "upload" || this.attrs.nocode;
    }

    get placeholder() {
        const tiny = this.type.indexOf("tiny") >= 0;
        return valueDefu(this.attrs.placeholder, (tiny ? "" : this.english ? "Write your code here" : "Kirjoita koodi tähän:"));
    }

    get inputplaceholder() {
        return valueOr(this.attrs.inputplaceholder, (this.english ? "Write your input here" : "Kirjoita syöte tähän"));
    }

    get isText() {
        const rt = this.rtype;
        return rt === "text" || rt === "xml" || rt === "css";
    }

    get argsplaceholder() {
        return valueOr(this.attrs.argsplaceholder, (this.isText ? (this.english ? "Write file name here" : "Kirjoita tiedoston nimi tähän") : (this.english ? "Write your program args here" : "Kirjoita ohjelman argumentit tähän")));
    }

    get argsstem() {
        return valueOr(this.attrs.argsstem, (this.isText ? (this.english ? "File name:" : "Tiedoston nimi:") : (this.english ? "Args:" : "Args")));
    }

    get iframe() {
        return this.attrs.iframe
            || languageTypes.isInArray(this.rtype, ["glowscript", "vpython"])
            || this.isTauno
            || this.isProcessing
            || this.type === "wescheme"
            || this.fullhtml
            || this.type.indexOf("html") >= 0
            || this.type.indexOf("/vis") >= 0;
    }

    get fullhtml() {
        const r = this.attrs.fullhtml || "";
        if (!r && this.type.indexOf("html") >= 0 || this.isProcessing) {
            return "REPLACEBYCODE";
        }
        return r;
    }

    get isProcessing() {
        return this.type.indexOf("processing") >= 0;
    }

    get toggleEditor() {
        return this.attrs.toggleEditor || this.isSimcir;
    }

    get toggleEditorText() {
        if (typeof this.toggleEditor === "string" && this.toggleEditor.indexOf("|") >= 0) {
            return this.toggleEditor.split("|");
        } else {
            return this.english ? ["Edit", "Hide"] : ["Muokkaa", "Piilota"];
        }
    }

    get minRows() {
        return getInt(this.attrs.rows);
    }

    get isAll() {
        return languageTypes.isAllType(this.type);
    }

    get glowscript() {
        return languageTypes.isInArray(this.rtype, ["glowscript", "vpython"]);
    }

    get isRun() {
        return ((languageTypes.getRunType(this.type, "") !== "" || this.isAll) && !this.attrs.norun)
            || (this.type.indexOf("text") >= 0 || this.isSimcir || this.attrs.justSave)
            || this.attrs.button; // or this.buttonText()?
    }

    buttonText() {
        const txt = super.buttonText();
        if (txt) {
            return txt;
        }
        if (this.attrs.button === null || this.attrs.buttonText === null) {
            return null;
        }
        if (this.type.indexOf("text") >= 0 || this.isSimcir || this.attrs.justSave) {
            return this.english ? "Save" : "Tallenna";
        }
        return this.english ? "Run" : "Aja";
    }

    get isTest() {
        return languageTypes.getTestType(this.type, this.selectedLanguage, "") !== "";
    }

    get isUnitTest() {
        return languageTypes.getUnitTestType(this.type, this.selectedLanguage, "") !== "";
    }

    get isDocument() {
        return this.type.indexOf("doc") >= 0;
    }

    get showInput() {
        return this.type.indexOf("input") >= 0;
    }

    get showArgs() {
        return this.type.indexOf("args") >= 0;
    }

    get uploadstem() {
        return valueOr(this.attrs.uploadstem, (this.english ? "Upload image/file" : "Lataa kuva/tiedosto"));
    }

    get file() {
        return this.attrs.file;
    }

    get showCodeOn() {
        return valueDefu(this.attrs.showCodeOn, (this.english ? "Show all code" : "Näytä koko koodi"));
    }

    get showCodeOff() {
        return valueOr(this.attrs.showCodeOff, (this.english ? "Hide extra code" : "Piilota muu koodi"));
    }

    get resetText() {
        return valueDefu(this.attrs.resetText, (this.english ? "Reset" : "Alusta"));
    }

    get editorModes() {
        return this.attrs.editorModes.toString();
    }

    getTemplateButtons(): string[] {
        let b = this.attrs.buttons;
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
            return b.split("\n");
        }
        return [];
    }

    get progLanguages() {
        if (this.isAll) {
            const langs = this.attrs.languages;
            if (langs) {
                return langs.split(/[\n;, \/]/);
            } else {
                return languageTypes.runTypes.sort();
            }
        }
    }

    get cssPrint() {
        return this.attrs.cssPrint;
    }

    get mode() {
        return languageTypes.getAceModeType(this.type, this.attrs.mode || "");
    }

    get iframeopts() {
        return this.attrs.iframeopts;
    }

    get nosave() {
        return this.attrs.nosave;
    }

    get cols() {
        return this.attrs.cols;
    }

    $onInit() {
        super.$onInit();
       //  if ( typeof this.attrs.borders !== 'undefined' ) this.attrs.borders = true;
        this.buttons = this.getTemplateButtons();
        const rt = this.rtype;
        const isText = this.isText;
        const isArgs = this.type.indexOf("args") >= 0;
        if ( this.attrsall.markup.docurl ) {
            this.docURL = this.attrsall.markup.docurl;
            this.docLink = "Hide document";
        }

        this.userinput = valueOr(this.attrsall.userinput, (this.attrs.userinput || "").toString());
        this.userargs = valueOr(this.attrsall.userargs, (this.attrs.userargs || (isText && isArgs ? this.attrs.filename || "" : "")).toString());
        this.selectedLanguage = this.attrsall.selectedLanguage || rt;
        this.noeditor = valueOr(this.attrs.noeditor, this.isSimcir || (this.type === "upload"));
        this.wrap = this.attrs.wrap || (isText ? 70 : -1);
        this.editorMode = this.attrs.editorMode;
        this.viewCode = this.attrs.viewCode;
        this.editorText = [
            valueDefu(this.attrs.normal, this.english ? "Normal" : "Tavallinen"),
            valueDefu(this.attrs.highlight, "Highlight"),
            this.attrs.parsons,
            this.attrs.jsparsons,
        ];
        for (const c of this.editorModes) {
            this.editorModeIndecies.push(parseInt(c, 10));
        }
        this.editorModeIndecies.push(parseInt(this.editorModes[0], 10));
        if (this.editorModes.length <= 1) {
            this.editorText = ["", "", "", "", "", "", ""];
        }
        this.checkEditorModeLocalStorage();

        this.maxRows = getInt(this.attrs.maxrows);
        this.rows = this.minRows;

        if (this.attrsall.usercode == null) {
            if (this.byCode) {
                this.usercode = this.byCode;
                this.initUserCode = true;
            }
        } else {
            this.usercode = this.attrsall.usercode;
        }
        this.usercode = commentTrim(this.usercode);
        if (this.attrs.blind) {
            this.usercode = this.usercode.replace(/@author.*/, "@author XXXX");
        }

        if (this.usercode) {
            const rowCount = countChars(this.usercode, "\n") + 1;
            if (this.maxRows < 0 && this.maxRows < rowCount) {
                this.maxRows = rowCount;
            }
        } else if (this.maxRows < 0) {
            this.maxRows = 10;
        }

        if (this.indent < 0) {
            if (this.file) {
                this.indent = 8;
            } else {
                this.indent = 0;
            }
        }

        if (this.editorMode !== 0 || this.editorModes !== "01" || this.cssPrint) {
            this.showOtherEditor(this.editorMode);
        } // Forces code editor to change to pre

        this.processPluginMath();
        const tid = this.pluginMeta.getTaskId();
        if (tid) {
            csLogTime(tid.docTask());
        }

        this.showUploaded(this.attrsall.uploadedFile, this.attrsall.uploadedType);
        this.initSaved();
        this.vctrl.addTimComponent(this);
    }

    async $postLink() {
        await $timeout(); // because of ng-if, tauno would not be found until Angular has processed everything
        this.taunoElem = this.element.find(".taunoContainer")[0];
        this.edit = this.element.find("textarea")[0] as HTMLTextAreaElement;
        this.preview = this.element.find(".csrunPreview");
        const styleArgs = this.attrs["style-args"];
        if (styleArgs) {
            const argsEdit = this.getRootElement().getElementsByClassName("csArgsArea");
            if (argsEdit.length > 0) {
                argsEdit[0].setAttribute("style", styleArgs);
            }
        }
        this.initEditorKeyBindings();
        this.element.bind("keydown", (event) => {
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

        if (this.attrs.open) {
            const kind = this.kind;
            if (kind === "tauno" || kind === "simcir") {
                this.showTauno();
            }
        }
        if (this.attrs.autorun) {
            this.runCodeLink(true);
        }
    }

    updateEditSize() {
        if (!this.usercode) {
            return;
        }
        let n = countChars(this.usercode, "\n") + 1;
        if (n < this.minRows) {
            n = this.minRows;
        }
        if (n > this.maxRows) {
            n = this.maxRows;
        }
        if (n < 1) {
            n = 1;
        }
        this.rows = n;
    }

    initSaved() {
        this.dochecks.savedcode = this.usercode;
        this.dochecks.savedargs = this.userargs;
        this.dochecks.savedinput = this.userinput;
        this.notSaved = false;
    }

    async $doCheck() {
        let anyChanged = false;
        this.notSaved = this.usercode !== this.dochecks.savedcode ||
                        this.userinput != this.dochecks.savedinput ||
                        this.userargs != this.dochecks.savedargs;
        if ( this.notSaved )  {
            anyChanged = true;
        }
        if (this.usercode !== this.dochecks.usercode) {

            this.checkByCodeRemove();
            if (this.aceEditor && this.aceEditor.getSession().getValue() !== this.usercode) {
                this.aceEditor.getSession().setValue(this.usercode);
            }
            this.dochecks.usercode = this.usercode;
            if (!this.copyingFromTauno && this.usercode !== this.byCode) {
                this.muokattu = true;
            }
            this.copyingFromTauno = false;
            if (this.minRows < this.maxRows) {
                this.updateEditSize();
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
            if (this.runned && this.attrs.autoupdate) {
                await $timeout(this.attrs.autoupdate);
                if (currUsercode === this.usercode &&
                    currUserargs === this.userargs &&
                    currUserinput === this.userinput) {
                    this.runCodeAuto();
                }
            }
        }
    }

    onFileSelect(file: File) {
        const taskId = this.pluginMeta.getTaskId();
        if (!taskId) {
            console.log("taskId missing");
            return;
        }

        if (file) {
            if (this.attrs.uploadbycode) {
                const reader = new FileReader();
                reader.onload = ((e) => {
                    this.scope.$evalAsync(() => {
                        this.usercode = reader.result as string;
                        if ( this.attrs.uploadautosave ) { this.runCode(); }
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
            const upload = $upload.upload<{file: string, type: string}>({
                url: `/pluginUpload/${taskId.docId}/${taskId.name}/`,
                data: {
                    file: file,
                },
                method: "POST",
            });

            upload.then((response) => {
                $timeout(() => {
                    this.showUploaded(response.data.file, response.data.type);
                    if ( this.attrs.uploadautosave || !this.attrs.button ) {
                        this.doRunCode("upload", false);
                    }
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

        await $timeout();
        await ParCompiler.processMathJax(this.element[0]);
    }

    showUploaded(file: string | undefined, type: string | undefined) {
        if (!file || !type) {
            return;
        }
        this.uploadedFile = file;
        this.uploadedType = type;
        const name = uploadFileTypesName(file);
        let html = `<p class="smalllink"><a href="${file}" title="${type}">${name}</a></p>`; // (' + type + ')</p>';
        if (type.indexOf("image") === 0) {
            html += `<img src="${this.uploadedFile}"/>`;
            this.uploadresult = $sce.trustAsHtml(html);
            return;
        }
        if (type.indexOf("video") === 0) {
            html += `<video src="${this.uploadedFile}" controls/>`;
            this.uploadresult = $sce.trustAsHtml(html);
            return;
        }
        if (type.indexOf("audio") === 0) {
            html += `<audio src="${this.uploadedFile}" controls/>`;
            this.uploadresult = $sce.trustAsHtml(html);
            return;
        }
        if (type.indexOf("text") === 0) {
            html = '<div style="overflow: auto; -webkit-overflow-scrolling: touch; max-height:900px; -webkit-box-pack: center; -webkit-box-align: center; display: -webkit-box;"  width:1200px>';
            html += `<iframe  width="800" src="${file}" target="csdocument" allowfullscreen   ${this.iframeopts} />`;
            html += "</div>";
            this.uploadresult = $sce.trustAsHtml(html);
            return;

        }

        if (is(uploadFileTypes, file)) {
            html += '<div style="overflow: auto; -webkit-overflow-scrolling: touch; max-height:1200px; -webkit-box-pack: center; -webkit-box-align: center; display: -webkit-box;"  width:1200px>';
            html += `<iframe width="800" height="900"  src="${file}" target="csdocument" allowfullscreen " ${this.iframeopts} />`;
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
            if (this.editorIndex === 0 && this.edit) {
                const start = this.edit.selectionStart;

                this.usercode = r.s;
                $timeout(() => {
                    this.edit.selectionStart = start;
                    this.edit.selectionEnd = start;
                });
            }
            if (this.editorIndex === 1 && this.aceEditor) {
                const editor = this.aceEditor;
                let cursor = editor.selection.getCursor();
                const index = editor.session.getDocument().positionToIndex(cursor, 0);
                this.usercode = r.s;
                $timeout(() => {
                    cursor = editor.session.getDocument().indexToPosition(index, 0);
                    editor.selection.moveCursorToPosition(cursor);
                    editor.selection.clearSelection();
                });
            }

        }

    }

    logTime(msg: string) {
        const tid = this.pluginMeta.getTaskId();
        csLogTime(msg + " " + (tid && tid.docTask()));
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
        const ty = languageTypes.getRunType(this.selectedLanguage, "cs");
        if (ty === "md") {
            this.showMD();
            if (nosave || this.nosave) {
                return;
            }
        }
        if (languageTypes.isInArray(ty, csJSTypes)) {
            // this.jstype = ty;
            this.showJS();
            if (nosave || this.nosave) {
                return;
            }
        }
        this.doRunCode(ty, nosave || this.nosave);
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
        const ty = languageTypes.getTestType(this.type, this.selectedLanguage, "comtest");
        this.doRunCode(ty, false);
    }

    runUnitTest() {
        const ty = languageTypes.getUnitTestType(this.type, this.selectedLanguage, "junit");
        this.doRunCode(ty, false);
    }

    runDocument() {
        if (this.docURL) {
            this.closeDocument();
            return;
        }
        this.docLink = "Hide document";
        const ty = languageTypes.getRunType(this.selectedLanguage, "cs");
        this.doRunCode(ty, false, {document: true});
    }

    closeDocument() {
        this.docURL = "";
        this.docLink = "Document"
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
            this.usercode = await this.getCircuitData(this.simcir);
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
        if (!this.attrs.autoupdate) {
            this.tinyErrorStyle = {};
            this.error = "... running ...";
            this.runError = true;
            this.isRunning = true;
        }
        this.isRunning = true;
        this.imgURL = "";
        this.wavURL = "";
        this.runSuccess = false;
        if (!(languageTypes.isInArray(runType, csJSTypes) || this.attrs.noConsoleClear)) {
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
        if (this.attrs.validityCheck) {
            const re = new RegExp(this.attrs.validityCheck);
            if (!ucode.match(re)) {
                this.tinyErrorStyle = {color: "red"};
                let msg = this.attrs.validityCheckMessage;
                if (!msg) {
                    msg = "Did not match to " + this.attrs.validityCheck;
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
        const url = this.pluginMeta.getAnswerUrl();
        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            this.runError = this.error;
            this.isRunning = false;
            return;
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
            this.initSaved();
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

            const err = data.web.console || "";
            if (docURL) {
                this.docURL = docURL;
                this.docLink = "Hide document";
                this.error = err.trim();
            }

            if (wavURL) {
                // <video src="https://tim.jyu.fi/csgenerated/vesal/sinewave.wav" type="video/mp4" controls="" autoplay="true" ></video>
                this.wavURL = wavURL;
                this.result = err.trim();
            }

            if (imgURL) {
                this.imgURL = imgURL + this.imgURL;
                this.result = err.trim();
            } else {
                if (this.runSuccess) {
                    if (this.attrs.isHtml) {
                        this.htmlresult = removeXML(err) + this.htmlresult;
                    } else if (!languageTypes.isInArray(runType, csJSTypes)) {
                        this.result = err;
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
        const f = this.taunoElem.firstChild as CustomFrame<TaunoWindow>;
        let s = f.contentWindow.getUserCodeFromTauno();
        this.copyingFromTauno = true;
        const treplace = this.attrs.treplace || "";
        if (treplace) {
            const treps = treplace.split("&");
            for (const trep of treps) {
                const reps = (trep + "|").split("|");
                s = s.replace(new RegExp(reps[0], "g"), reps[1]);
                s = s.replace(new RegExp("\n\n", "g"), "\n");
            }
        }
        this.usercode = s;
        this.checkIndent();
        this.muokattu = false;
    }

    async addText(s: string) {
        if (this.noeditor) {
            this.userargs += s + " ";
            return;
        }
        let tbox;
        let editor: IAceEditor | undefined;
        let i = 0;
        if (this.editorIndex === 1) {
            editor = this.aceEditor!;
            i = editor.session.getDocument().positionToIndex(editor.selection.getCursor(), 0);
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
            const cursor = editor.session.getDocument().indexToPosition(i, 0);
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
        if (!dw) {
            dw = "100%";
        }
        if (!dh) {
            dh = 500;
        }
        const w = ifIs(this.attrs.width, "width", dw);
        const h = ifIs(this.attrs.height, "height", dh);
        return {vid: vid, w: w, h: h};
    }

    async setCircuitData() {
        if (!this.simcir) {
            console.warn("setCircuitData: simcir not loaded");
            return;
        }
        let data: {width: number, height: number} = {width: 0, height: 0};
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
            const initstr = this.attrs.initSimcir;
            if (initstr) {
                const initdata = JSON.parse(initstr);
                data = {...data, ...initdata};
            }
        } catch (err) {
            this.error = err.message;
            this.runError = true;
        }

        // width and height are passed to svg viewBox attribute that needs numbers
        data.width = numOrDef(this.attrs.width, 800);
        data.height = numOrDef(this.attrs.height, 400);
        this.simcir.children().remove();
        const simcir = await loadSimcir();
        simcir.setupSimcir(this.simcir, data);
    }

    async getCircuitData(simcirElem: JQuery) {
        const simcir = await loadSimcir();
        const d = simcir.controller(simcirElem.find(".simcir-workspace"));
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
    }

    copyToSimcir() {
        this.setCircuitData();
    }

    async copyFromSimcir() {
        if (this.simcir) {
            this.usercode = await this.getCircuitData(this.simcir);
        }
    }

    showSimcir() {
        const v = this.getVid();
        this.taunoOn = true;
        this.taunoElem.innerHTML = `<div id=${v.vid}></div>`;
        this.simcir = $(this.taunoElem).children().first();
        this.setCircuitData();
        return true;
    }

    showTauno() {
        if (this.isSimcir) {
            return this.showSimcir();
        }
        const v = this.getVid();
        let p = "";
        let tt = "/cs/tauno/index.html?lang=" + this.attrs.lang + "&";
        if (this.attrs.taunotype && this.attrs.taunotype === "ptauno") {
            tt = "/cs/tauno/index.html?lang=" + this.attrs.lang + "&s&";
        }
        let taunoUrl = tt; // +"?"; // t=1,2,3,4,5,6&ma=4&mb=5&ialku=0&iloppu=5";
        const s = this.attrs.table;
        if (s && s.length > 0) {
            if (s[0] === "s") {
                p = "ts=" + s.substring(1) + "&";
            } else {
                p = "t=" + s.trim() + "&";
            }                      // table by it's items
        }

        p += doVariables(this.attrs.variables, "m");
        p += doVariables(this.attrs.indices, "i");

        taunoUrl = taunoUrl + p;
        if (this.iframe) {
            this.taunoElem.innerHTML =
                `<iframe id="${v.vid}" class="showTauno" src="${taunoUrl}" ${v.w}${v.h} ${this.iframeopts} ></iframe>`;
        } else {
            this.taunoElem.innerHTML = `<div class="taunoNaytto" id="${v.vid}" />`;
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
        this.initSaved();
    }

    async initSage(firstTime: boolean) {
        // TODO: lisää kentätkin vasta kun 1. kerran alustetaan.
        // TODO: kielien valinnan tallentaminen
        // TODO: kielien valinta kunnolla float.
        // ks: https://github.com/sagemath/sagecell/blob/master/doc/embedding.rst
        const sagecell = (await import("./embedded_sagecell")).default;
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
        this.sageArea = this.getRootElement().getElementsByClassName("computeSage")[0];
        this.editArea = this.getRootElement().getElementsByClassName("csEditArea")[0];
        this.sageOutput = this.getRootElement().getElementsByClassName("outputSage")[0];

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
                this.sageButton = this.sageArea!.getElementsByClassName("sagecell_evalButton")[0] as HTMLElement;
                this.sageInput = this.sageArea!.getElementsByClassName("sagecell_commands")[0] as HTMLInputElement;

                this.sageButton.onclick = () => {
                    // cs.checkSageSave();
                    this.sagecellInfo!.code = this.getReplacedCode();
                    // cs.sagecellInfo.session.code = cs.sagecellInfo.code;
                };
                const sagecellOptions = this.getRootElement().getElementsByClassName("sagecell_options")[0] as HTMLElement;
                const csRunMenuArea = this.getRootElement().getElementsByClassName("csRunMenuArea")[0];
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

    get showCodeLink() {
        if (this.viewCode) {
            return this.showCodeOff;
        } else {
            return this.showCodeOn;
        }
    }

    showCode() {
        this.viewCode = !this.viewCode;
        this.localcode = undefined;
        this.showCodeNow();
    }

    getFromClipboard() {  // This does not work, it is not possible to get user clp contents
        const e1 = getClipboardHelper();
        e1.select();
        document.execCommand("paste");
        e1.select();
        return e1.value;
    }

    getSameIndent(s: string, beg: number): string {
        let n = 0;
        let b = beg;
        for (let i = b; i < s.length; i++) {
            const c = s[i];
            if (c == " ") { n++; } else if (c == "\n") { b = i + 1; n = 0; } else { break; }
        }
        return s.substr(b, n);
    }

    findLastNonEmpty(s: string): number {
        let i = s.length - 1;
        let foundChars = false;

        for ( ; i >= 0; i--) {
            const c = s[i];
            if ( c == "\n") {
                if ( foundChars ) { i++; break; }
            } else if ( c != " ") { foundChars = true; }
        }
        return i;
    }

    copyCode() {
        let pre = "";
        let post = "";
        let extra = false;
        if ( this.viewCode && this.precode ) { // TODO: get if not present?
            pre = this.precode + "\n";
            extra = true;
        }

        if ( this.viewCode && this.postcode ) { // TODO: get if not present?
            post = this.postcode + "\n";
            extra = true;
        }

        const usercode = this.usercode;

        // TODO: begin and end texts as a parameter and then indext picked there
        let ind = "";
        if ( extra ) {
            ind = this.getSameIndent(this.usercode, 0);
            pre += ind + "// BYCODEBEGIN\n";  // TODO: ask comment string from language
            const i = this.findLastNonEmpty(usercode);
            ind = this.getSameIndent(this.usercode, i);
            post = "\n" + ind + "// BYCODEEND\n" + post;  // TODO: ask comment string from language
        }
        const s = pre + this.usercode + post;
        copyToClipboard(s);
    }

    checkByCodeRemove() {
        // TODO: begin and end texts as a parameter and then indext picked there
        if ( this.nocode || !(this.file || this.program) ) { return; }
        const BEGINCODE = "BYCODEBEGIN";
        const ENDCODE = "BYCODEEND";
        let code = this.usercode;
        let i = code.indexOf(BEGINCODE);
        if ( i >= 0) {
            const endl = code.indexOf("\n", i);
            if (endl < 0) { return; } // NO user code
            code = code.substr(endl + 1);
        }
        i = code.indexOf(ENDCODE);
        if ( i >= 0 ) {
            let endl = code.lastIndexOf("\n", i);
            if ( endl > 0 && code[endl - 1] == "\r") { endl--; } // if there are linefeeds like cr lf
            if ( endl >= 0 ) { code = code.substr(0, endl); }
        }
        if ( code.length == this.usercode.length ) { return; }
        this.usercode = code;

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
        for (let i = 0; i < st.length; ++i) {
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
        if (!n) {
            return;
        }
        this.usercode = st.join("\n");
    }

    getReplacedCode() {
        if (!this.program) {
            this.code = this.usercode;
            return this.code;
        }
        const st = this.program.split("\n");
        [this.code] = this.maybeReplace(st);
        return this.code;
    }

    get replace() {
        return this.attrs.replace || this.attrsall.replace;
    }

    private maybeReplace(st: string[]): [string, string, string] {
        let r = "";
        const rp = ["", ""]; // alkuosa, loppuosa
        let step = 0;
        let nl = "";
        let nls = "";
        const needReplace = !!this.replace;
        const regexp = new RegExp(this.replace || "");
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
        return [r, rp[0], rp[1]];
    }

    getCodeFromLocalCode() {
        if (!this.localcode) {
            this.code = this.usercode;
            this.precode = "";
            this.postcode = "";
        } else {
            const st = this.localcode.split("\n");
            [this.code, this.precode, this.postcode] = this.maybeReplace(st);
        }
    }

    showCodeNow() {
        if (!this.viewCode) {
            return;
        }
        this.getAllCode();
    }

    async getAllCode() {
        if (this.localcode != null) {
            this.getCodeFromLocalCode();
            return;
        }
        if (!this.file && !this.program) {
            this.localcode = "";
            this.getCodeFromLocalCode();
            return;
        }

        const params = this.attrsall;
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
                this.getCodeFromLocalCode();
            } else if (data.msg !== "") {
                this.localcode = data.msg;
                this.getCodeFromLocalCode();
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
        const taskId = this.pluginMeta.getTaskId();
        if (!taskId) {
            console.log("taskId missing");
            return;
        }
        if (this.precode == undefined) {
            await this.getAllCode();
        }
        const text = this.precode + "\n" + this.usercode.replace(this.cursor, "") + "\n" + this.postcode;
        if (text === this.lastMD) {
            return;
        }
        this.lastMD = text;
        const r = await to($http.post<IPluginInfoResponse>(
            `/preview/${taskId.docId}`, {
                text: text,
            }));
        if (r.ok) {
            await ParCompiler.compileAndAppendTo(this.preview, r.result.data, this.scope);
        } else {
            const data = r.result.data;
            alert("Failed to show preview: " + data.error);
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
            console.log("Wait 300 ms " + v.vid);
            await $timeout(300);
            this.showJsParsons(parsonsEditDiv);
            return;
        }
        let classes = "csrunEditorDiv sortable-code";
        let canIndent = true;
        if (this.attrs.words) {
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
        const csp = await import("./cs-parsons/csparsons");
        const parson = new csp.CsParsonsWidget({
            sortable: sortable,
            words: this.attrs.words,
            minWidth: "40px",
            shuffle: this.initUserCode,
            styleWords: this.attrs["style-words"],
            maxcheck: this.attrs.parsonsmaxcheck,
            notordermatters: this.attrs.parsonsnotordermatters,
            onChange: (p) => {
                this.usercode = p.join("\n");
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

    async showOtherEditor(editorMode?: number) {
        if (this.parson) {
            this.usercode = this.getJsParsonsCode();
        }

        this.parson = undefined;
        this.csparson = null;

        const editorHtml = `
<textarea class="csRunArea csrunEditorDiv"
          rows={{$ctrl.rows}}
          ng-model="$ctrl.usercode"
          ng-trim="false"
          placeholder="{{$ctrl.placeholder}}"></textarea>
`;

        const aceHtml = `
<div class="no-popup-menu">
    <div ng-show="$ctrl.mode"
         class="csRunArea csEditArea csAceEditor"></div>
</div>
`;

        const cssHtml = `<pre>{{$ctrl.usercode}}</pre>`;

        const parsonsHtml = `<div class="no-popup-menu"></div>`;

        let html;
        if (this.cssPrint) {
            html = [cssHtml, cssHtml, cssHtml, cssHtml];
        } else {
            html = [editorHtml, aceHtml, parsonsHtml, parsonsHtml];
        }

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
        const otherEditDiv = this.getRootElement().getElementsByClassName("csrunEditorDiv")[0];
        const editorDiv = angular.element(otherEditDiv) as JQuery;
        // editorDiv.empty();
        this.edit = $compile(html[eindex])(this.scope)[0] as HTMLTextAreaElement; // TODO unsafe cast
        // don't set the html immediately in case of Ace to avoid ugly flash because of lazy load
        if (eindex === 1) {
            const ace = (await import("tim/editor/ace")).ace;
            editorDiv.empty();
            editorDiv.append(this.edit);
            // const editor = ace.edit(editorDiv.find(".csAceEditor")[0]) as IAceEditor;
            const editor = ace.edit(this.edit);

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
            await 1; // TODO:  Miksi tässä pitää olla tämä?  Muuten tuo editorDiv.empty() aiheuttaa poikkeuksen
            editorDiv.empty();
            editorDiv.append(this.edit);
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

    // Runs when editor loads
    aceLoaded(ace: IAce, editor: IAceEditor) {
        this.aceEditor = editor;
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
        if (!this.canvas) {
            console.warn("toggleFixed: canvas not initialized");
            return;
        }
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
        if (this.program && !this.codeInitialized) {
            this.localcode = this.program;
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
            isProcessing = true;
        }
        let wescheme = false;
        if (this.type === "wescheme") {
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
            if (this.type.indexOf("/vis") >= 0) {
                html = '<div id="myDiv" class="mydiv" width="800" height="400" ></div>';
                scripts = "https://cdnjs.cloudflare.com/ajax/libs/vis/4.20.0/vis.min.js";
            }
            let fsrc = "/cs/gethtml/canvas.html";
            if (wescheme) {
                // fsrc = "/csstatic/WeScheme/WeSchemeEditor.html";
                fsrc = "/csstatic/WeScheme/openEditor.html";
            }
            if (this.iframe) {
                let dw;
                let dh;
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
    <iframe ng-if="!$ctrl.fullhtml" id="${v.vid}" class="jsCanvas"
            src="${fsrc}?scripts=${this.attrs.scripts || scripts}&html=${html}" ${v.w}${v.h} style="border:0"
            ${opts}></iframe>
    <iframe ng-if="$ctrl.fullhtml" id="${v.vid}" class="jsCanvas" ${v.w}${v.h} style="border:0"
            ${opts}></iframe>
</div>
`;
                const e = angular.element(angularElement);
                this.canvas = e[0] as HTMLCanvasElement; // TODO this seems wrong, it isn't canvas
                this.iframeLoadTries = 10;
            } else {
                this.canvas = angular.element(// '<div class="userlist" tim-draggable-fixed="" style="top: 91px; right: -375px;">'+
                    `<canvas id="csCanvas" width="${this.attrs.canvasWidth}" height="${this.attrs.canvasHeight}" class="jsCanvas"></canvas>`)[0] as HTMLCanvasElement;
            }
            const previewDiv = this.preview;
            previewDiv.empty().append($compile(this.canvas)(this.scope));
        }
        let text = this.usercode.replace(this.cursor, "");
        if (!this.attrs.runeverytime && text === this.lastJS && this.userargs === this.lastUserargs && this.userinput === this.lastUserinput) {
            return;
        }
        this.lastJS = text;
        this.lastUserargs = this.userargs;
        this.lastUserinput = this.userinput;

        text = this.getCode();

        if (this.iframe) { // in case of iframe, the text is send to iframe
            const f = this.preview.find("iframe")[0] as CustomFrame<GlowScriptWindow>; // but on first time it might be not loaded yet
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
                console.log("Wait 300 ms");
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
                    fhtml = `
<script src="/cs/static/processing/processing.js"></script>
<script type="text/processing" data-processing-target="mycanvas">
${fhtml}
</script>
<canvas id="mycanvas"></canvas>`;
                }
                f.contentWindow.document.open();
                f.contentWindow.document.write(fhtml);
                f.contentWindow.document.close();
            } else if (wescheme) {
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
            const ctx = this.canvas.getContext("2d")!;
            ctx.save();
            this.result = "";
            const beforeCode = "function paint(ctx, out, userargs, userinput, console) { ";
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
                const elem = this.getRootElement().getElementsByClassName("console")[0] as any;
                this.out = elem;
                elem.write = (s: string) => this.write(s);
                elem.writeln = (s: string) => this.writeln(s);
                elem.canvas = this.canvas;
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

    get showRuntime() {
        return this.attrs.showRuntime;
    }

    get codeover() {
        return this.attrs.codeover;
    }

    get codeunder() {
        return this.attrs.codeunder;
    }

    get inputstem() {
        return this.attrs.inputstem;
    }

    get inputrows() {
        return this.attrs.inputrows;
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
    bindings: pluginBindings,
    controller: CsController,
        require: {
        vctrl: "^timView",
    },
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
<div ng-class="::{'csRunDiv': $ctrl.attrs.borders}" class="csTinyDiv" style="text-align: left;">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <span ng-if="::$ctrl.stem"
          class="stem"
          ng-bind-html="::$ctrl.stem"></span>
    <input class="csTinyText no-popup-menu"
           ng-hide="$ctrl.noeditor && !$ctrl.viewCode"
           size="{{::$ctrl.cols}}"
           ng-model="$ctrl.usercode"
           ng-trim="false"
           ng-attr-placeholder="{{$ctrl.placeholder}}"
           ng-keypress="$ctrl.runCodeIfCR($event);"/>
    <button ng-if="::$ctrl.isRun"
            ng-disabled="$ctrl.isRunning"
            class = "timButton"
            title="(Ctrl-S)"
            ng-click="$ctrl.runCode();"
            ng-bind-html="::$ctrl.buttonText()"></button>
    &nbsp;&nbsp;<a href=""
                   ng-if="$ctrl.muokattu"
                   ng-click="$ctrl.initCode();">{{::$ctrl.resetText}}</a>&nbsp;&nbsp;
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

class CsConsoleController extends CsBase implements IController {
    // isShell: boolean; method
    cursor: number;
    currentSize: string;
    // isHtml: boolean;
    oldpwd!: string;
    currentInput: string;
    pwd!: string;
    // byCode: string; method
    // content: AttrType;
    examples: Array<t.TypeOf<typeof Example>>;
    history: Array<{istem: string, ostem: string, input: string, response: string}>;
    // savestate: string;
    // path: string;
    // type: string;

    constructor(scope: IScope, element: JQLite) {
        super(scope, element);
        this.examples = [];
        this.history = [];
        this.currentSize = "normal";
        this.currentInput = "";
        this.cursor = this.history.length; // this.history.length means new input is last command.
    }

    $postLink() {
        // nothing to do
    }

    get isShell() {
        return languageTypes.getRunType(this.type, "") === "shell";
    }

    get savestate() {
        return this.attrs.savestate;
    }

    $onInit() {
        super.$onInit();

        // This block could be re-used

        // End of generally re-usable TIM stuff
        if (this.attrs.examples) {
            this.examples = this.attrs.examples;
        }

        this.pwd = ConsolePWD.getPWD(this);
        this.oldpwd = this.pwd;
        if (this.usercode === "" && this.byCode) {
            this.usercode = this.byCode.split("\n")[0];
        }
        this.currentInput = this.usercode;
        if (this.isShell) {
            ConsolePWD.register(this);
        }
    }

    setPWD(pwd: string) {
        this.pwd = pwd;
    }

    loadExample(i: number) {
        this.currentInput = this.examples[i].expr;
        this.focusOnInput();
    }

    focusOnInput() {
        const el: HTMLInputElement | null = this.getRootElement().querySelector(".console-input");
        if (el) {
            el.focus();
        }
    }

    async handler() {
        const url = this.pluginMeta.getAnswerUrl();
        const ty = languageTypes.getRunType(this.type, "shell");
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
                    type: ty,
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
                if (!this.attrs.isHtml) {
                    s = "<pre>" + s + "</pre>";
                }
            }
            this.submit(s);
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
        const el = this.getRootElement().querySelector(".console-output");
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
        }
        if (ev.which === 40) {
            this.down();
        }
        if (ev.which === 38) {
            this.up();
        }
    }
}

csConsoleApp.component("csConsole", {
    bindings: pluginBindings,
    controller: CsConsoleController,
    template: `
<div class="web-console no-popup-menu {{$ctrl.currentSize}} " ng-keydown="$ctrl.handleKey($event)"><code
        class="console-output">
    <div class="console-output-elem"
         ng-repeat="item in $ctrl.history track by $index"><span class="console-oldinput">  <span
            class="console-in">{{item.istem}}</span>  <span class="console-userInput">{{item.input}}</span> </span>
        <span class="console-oldresponse"><span ng-if="::!$ctrl.isShell">  <br/>  <span
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
            <li ng-repeat="example in ::$ctrl.examples track by $index">
                <a ng-click="$ctrl.loadExample($index)"
                   title="{{example.expr}}">{{example.title||example.expr}}</a>
            </li>
            <ul>
    </div>
    <div class="console-curIndex" ng-if="::$ctrl.isShell">{{$ctrl.pwd}}</div>
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

        for (const c of abcde) {
            if (input.indexOf(c) >= 0) {
                header += c + " ";
                const zv = "z[" + count + "]";
                input = input.split(c).join(zv);
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
                // eslint-disable-next-line no-bitwise
                z[i] = (n >> (count - 1 - i)) & 1;
            }
            // eslint-disable-next-line no-eval
            result += eval(vals) + "= ";
            const inp = input.split(";");
            for (let i = 0; i < inp.length; i++) {
                // eslint-disable-next-line no-eval
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

export const moduleDefs = [csApp, csConsoleApp];
