import angular, {IAttributes, IController, IRootElementService, IScope} from "angular";
import $ from "jquery";
import {CellInfo} from "sagecell";
import {IAce, IAceEditor} from "tim/editor/ace-types";
import {ParCompiler} from "tim/editor/parCompiler";
import {lazyLoadMany, lazyLoadTS} from "tim/util/lazyLoad";
import {$compile, $http, $interval, $sce, $timeout, $upload, $window} from "tim/util/ngimport";
import {getHeading, set} from "tim/util/timHelper";
import {fixDefExport} from "tim/util/utils";
import * as csparsons from "./cs-parsons/csparsons";

interface Simcir {
    setupSimcir(element: JQuery, data: {}): void;

    controller(element: JQuery): void;
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
    user_id: string;
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
let taunoPHIndex = 3;

function csLogTime(msg: string) {
    const d = new Date();
    const diff = d.getTime() - csPluginStartTime.getTime();
    console.log("cs: " + d.toLocaleTimeString() + " " + diff.valueOf() + " - " + msg);
}

csLogTime("directives done");

const TESTWITHOUTPLUGINS = true && false;
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

function directiveTemplateCS(t: string, isInput: boolean) {
    taunoPHIndex = 3;
    csLogTime("dir templ " + t);
    if (TESTWITHOUTPLUGINS) {
        return "";
    }

    if (t === "text") {
        return '<div class="csRunDiv csTinyDiv" ng-cloak style="text-align: left;">' +
            "<p>Here comes header</p>" +
            '<span ng-if="stem" class="stem"  ng-bind-html="stem"></span>' +
            '<input class="csTinyText no-popup-menu" ng-hide="noeditor && !viewCode" size="{{cols}}" ng-model="usercode" ng-trim="false" ng-attr-placeholder="{{placeholder}}" ng-keypress="runCodeIfCR($event);" />' +
            '<button ng-if="isRun"  ng-disabled="isRunning" title="(Ctrl-S)" ng-click="runCode();" ng-bind-html="buttonText"></button>&nbsp&nbsp' +
            '<a href="" ng-if="muokattu" ng-click="initCode();">{{resetText}}</a>&nbsp&nbsp' +
            '<pre  class="console ng-hide" ng-show="result" ng-cloak>{{result}}</pre>' +
            '<span class="csRunError"  ng-if="runError" ng-style="tinyErrorStyle">{{error}}</span>' +
            '<div  class="htmlresult" ng-if="htmlresult" ><span ng-bind-html="svgImageSnippet()"></span></div>' +
            "</div>";
    }

    return '<div class="csRunDiv type-{{rtype}}" ng-cloak>' +

        "<p>Here comes header</p>" +
        //  '<p ng-bind-html="getHeader()"></p>
        '<p ng-if="stem" class="stem" ng-bind-html="stem"></p>' +
        (t === "tauno" || t === "simcir" ?
            '<p ng-if="taunoOn" class="pluginHide""><a ng-click="hideTauno()">{{hideTaunoText}}</a></p>' +
            "<div ><p></p></div>" + // Tauno code place holder nr 3!!
            '<p ng-if="!taunoOn" class="pluginShow" ><a ng-click="showTauno()">{{showTaunoText}}</a></p>' +
            (t === "tauno" ? '<p ng-if="taunoOn" class="pluginHide"" ><a ng-click="copyTauno()">{{copyFromTaunoText}}</a> | <a ng-click="hideTauno()">{{hideTaunoText}}</a></p>' +
                '<p ng-if="taunoOn" class="taunoOhje">{{taunoOhjeText}}</a></p>'
                : '<p ng-if="taunoOn && !noeditor" class="pluginHide"" ><a ng-click="copyFromSimcir()">copy from SimCir</a> | <a ng-click="copyToSimcir()">copy to SimCir</a> | <a ng-click="hideTauno()">hide SimCir</a></p>') +
            "" : "") +
        '<div ng-if="upload" class="form-inline small">' +
        '<div class="form-group small">' +
        '    {{uploadstem}}: <input type="file" ngf-select="onFileSelect($file)" >' +
        '            <span ng-show="file.progress >= 0 && !file.error"' +
        '                  ng-bind="file.progress < 100 ? \'Uploading... \' + file.progress + \'%\' : \'Done!\'"></span>' +
        "</div>" +
        '    <div class="error" ng-show="file.error" ng-bind="file.error"></div>' +
        '    <div  class="úploadresult" ng-if="uploadresult"  ><span ng-bind-html="uploadresult"></span></div>' +
        "</div>" +
        '<div ng-show="isAll" style="float: right;">{{languageText}} ' +
        '<select ng-model="selectedLanguage" ng-required ng-init="progLanguag=\'java\'">' +
        '<option ng-repeat="item in progLanguages" value="{{item}}">{{item}}</option>' +
        "</select>" +
        "</div>" +
        '<pre ng-if="viewCode && codeover">{{code}}</pre>' +
        '<div class="csRunCode">' +
        // '<p></p>'+
        '<pre class="csRunPre" ng-if="viewCode &&!codeunder &&!codeover">{{precode}}</pre>' +
        '<div class="csEditorAreaDiv">' +
        '<div  class="csrunEditorDiv">' +
        '<textarea class="csRunArea csEditArea no-popup-menu" ng-hide="noeditor && !viewCode" rows="{{rows}}" ng-model="usercode" ng-trim="false" ng-attr-placeholder="{{placeholder}}"></textarea>' +
        "</div>" +
        // (t=="sage" ? '</div>' : '') +

        '<div class="csRunChanged" ng-if="usercode!=byCode"></div>' +
        // '<div class="csRunChanged" ng-if="muokattu"></div>'+
        "</div>" +
        // '<div class="csRunArea" contentEditable ng-model="usercode" ng-trim="false" "></div>'+
        '<pre class="csRunPost" ng-if="viewCode &&!codeunder &&!codeover">{{postcode}}</pre>' +
        "</div>" +
        // '<br />'+
        (t === "sage" ? '<div class="computeSage no-popup-menu"></div>' : "") +

        (isInput ?
            '<div class="csInputDiv" ng-hide="!showInput">' +
            '<p ng-show="inputstem" class="stem" >{{inputstem}}</p>' +
            '<div class="csRunCode" >' +
            '<textarea class="csRunArea csInputArea"  rows={{inputrows}} ng-model="userinput" ng-trim="false" placeholder="{{inputplaceholder}}"></textarea>' +
            "</div>" +
            "</div>" +
            '<div class="csArgsDiv" ng-hide="!showArgs">' +
            '<label>{{argsstem}} </label><span><input type ="text" class="csArgsArea" ng-model="userargs" ng-trim="false" placeholder="{{argsplaceholder}}"></span>' +
            "</div>" +
            ""
            : "") + // end of isInput

        '<p class="csRunSnippets" ng-if="buttons">' + // && viewCode">' +
        '<button ng-repeat="item in buttons" ng-click="addText(item);">{{addTextHtml(item)}}</button> &nbsp;&nbsp;' +
        "</p>" +
        '<div class="csRunMenuArea" ng-if="!forcedupload">' +
        '<p class="csRunMenu" >' +
        '<button ng-if="isRun"  ng-disabled="isRunning" title="(Ctrl-S)" ng-click="runCode();" ng-bind-html="buttonText"></button>&nbsp&nbsp' +
        '<button ng-if="isTest" ng-disabled="isRunning" ng-click="runTest();">Test</button>&nbsp&nbsp' +
        '<button ng-if="isUnitTest" ng-disabled="isRunning" ng-click="runUnitTest();">UTest</button>&nbsp&nbsp' +
        '<span ng-if="isDocument"><a href="" ng-disabled="isRunning" ng-click="runDocument();">{{docLink}}</a>&nbsp&nbsp</span>' +
        '<a href="" ng-if="!nocode && (file || attrs.program)" ng-click="showCode();">{{showCodeLink}}</a>&nbsp&nbsp' +
        '<a href="" ng-if="muokattu" ng-click="initCode();">{{resetText}}</a>' +
        ' <a href="" ng-if="toggleEditor" ng-click="hideShowEditor();">{{toggleEditorText[noeditor?0:1]}}</a>' +
        ' <a href="" ng-if="!noeditor" ng-click="showOtherEditor();">{{editorText[editorModeIndecies[editorMode+1]]}}</a>' +
        // ' <label class="font-weight-normal" ng-show="aceEnabled"><input type="checkbox" ng-model="autocomplete"/> Autocomplete</label>' +
        ' <span ng-if="showRuntime" class="inputSmall" style="float: right;" title="Run time in sec {{runtime}}">{{oneruntime}}</span>' +
        ' <span ng-if="wrap.n!=-1" class="inputSmall" style="float: right;"><label title="Put 0 to no wrap">wrap: <input type="text"  ng-pattern="/[-0-9]*/" ng-model="wrap.n" size="2" /></label></span>' +
        "</p>" +
        "</div>" +
        (t === "sage" ? '<div class="outputSage no-popup-menu"></div>' : "") +

        '<pre ng-if="viewCode && codeunder">{{code}}</pre>' +
        (t === "comtest" || t === "tauno" || t === "parsons" || true ? '<p class="unitTestGreen"  ng-if="runTestGreen" >&nbsp;ok</p>' : "") +
        (t === "comtest" || t === "tauno" || t === "parsons" || true ? '<pre class="unitTestRed"    ng-if="runTestRed">{{comtestError}}</pre>' : "") +
        // '<p>{{resImage}}</p>'+
        // '<p>Testi valituksesta</p>' +
        '<pre class="csRunError" ng-if="runError">{{error}}</pre>' +
        '<pre  class="console ng-hide" ng-show="result" ng-cloak>{{result}}</pre>' +
        '<div  class="htmlresult" ng-if="htmlresult" ><span ng-bind-html="svgImageSnippet()"></span></div>' +
        // '<div  class="userlist" tim-draggable-fixed style="top: 39px; right: 408px;"><span>Raahattava</span>'+
        '<span  class="csrunPreview"></span>' +
        // '</div>'+
        // '<div class="previewcontent"></div>' +

        // '<div  class="htmlresult" ng-if="htmlresult" ><span ng-bind-html="htmlresult"></span></div>'+
        // '<div  class="htmlresult" ng-if="htmlresult" >{{htmlresult}}</span></div>'+

        (t === "jypeli" || true ? '<img ng-if="imgURL" class="grconsole" ng-src="{{imgURL}}" alt=""  />' +
            '<video ng-if="wavURL" ng-src="{{wavURL}}" type="video/mp4" controls="" autoplay="true" width="300" height="40"></video>'
            : "") +
        // '<a ng-if="docURL" class="docurl" href="{{docURL}}" target="csdocument" >Go to document</a>' +
        '<div ng-if="docURL" class="docurl">' +
        '<p align="right" style="position: absolute; margin-left: 790px;"><a ng-click="closeDocument()" >X</a></p>' +
        '<iframe width="800" height="600"  ng-src="{{docURL}}" target="csdocument" allowfullscreen/>' +
        "</div>" +
        // (t === "jypeli" ? '<img  class="grconsole" ng-src="{{imgURL}}" alt=""  ng-if="runSuccess"  />' : "") +
        //  '<div class="userlist" tim-draggable-fixed="" style="top: 39px; right: 408px;">' +
        //  'Raahattava' +
        //  '</div>' +
        '<p class="plgfooter">Here comes footer</p>' +
        // '<p ng-show="logTime()"></p>'+
        "</div>";
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

function runSage(scope: CsController) {
    if (scope.sageButton) {
        scope.sageButton.click();
    }
}

async function initSage(cs: CsController, firstTime: boolean) {
// TODO: lisää kentätkin vasta kun 1. kerran alustetaan.
// TODO: kielien valinnan tallentaminen
// TODO: kielien valinta kunnolla float.
// ks: https://github.com/sagemath/sagecell/blob/master/doc/embedding.rst
    const sagecell = fixDefExport(await import("sagecell"));
    if (cs.sagecellInfo) {
        cs.sagecellInfo.editor = "textarea";
        // cs.sagecellInfo.inputLocation = null;
        // cs.sagecellInfo.outputLocation = null;
        // sagecell.deleteSagecell(cs.sagecellInfo);
        // cs.sagecellInfo = null;
    }
    const types = cs.type.split("/");
    let languages = sagecell.allLanguages;
    if (types.length > 1) {
        languages = types.slice(1);
    }
    // if ( cs.sagecellInfo ) {
    if (cs.sageInput && cs.sageOutput) {
        const outputLocation = $(cs.sageOutput);
        outputLocation.find(".sagecell_output_elements").hide();
        // cs.sagecellInfo.code = cs.usercode;
        // cs.sagecellInfo.code = cs.getReplacedCode();
        // cs.sagecellInfo.session.code = cs.sagecellInfo.code;
        // cs.sagecellInfo.inputLocation.innerText = cs.sagecellInfo.code;
        // cs.sagecellInfo.inputLocation.children[0].children[0].children[0].value = cs.sagecellInfo.code;
        cs.sageInput.value = cs.getReplacedCode();
        return;
    }
    cs.sageArea = cs.element0.getElementsByClassName("computeSage")[0];
    cs.editArea = cs.element0.getElementsByClassName("csEditArea")[0];
    cs.sageOutput = cs.element0.getElementsByClassName("outputSage")[0];

    cs.sagecellInfo = sagecell.makeSagecell({
        inputLocation: cs.sageArea,
        replaceOutput: true,
        // inputLocation: cs.editArea,
        editor: "textarea",
        // hide: ["evalButton"],
        hide: ["editor", "evalButton"],
        outputLocation: cs.sageOutput,
        requires_tos: false,
        // code: cs.usercode,
        code: cs.getReplacedCode(),
        getCode: () => cs.getReplacedCode(),
        autoeval: cs.attrs.autorun || firstTime,
        callback: () => {
            cs.sageButton = cs.sageArea.getElementsByClassName("sagecell_evalButton")[0] as HTMLElement;
            cs.sageInput = cs.sageArea.getElementsByClassName("sagecell_commands")[0] as HTMLInputElement;

            cs.sageButton.onclick = () => {
                // cs.checkSageSave();
                cs.sagecellInfo.code = cs.getReplacedCode();
                // cs.sagecellInfo.session.code = cs.sagecellInfo.code;
            };
            const sagecellOptions = cs.element0.getElementsByClassName("sagecell_options")[0];
            const csRunMenuArea = cs.element0.getElementsByClassName("csRunMenuArea")[0];
            if (csRunMenuArea && sagecellOptions) {
                csRunMenuArea.appendChild(sagecellOptions);
            }
            sagecellOptions.style.marginTop = "-2em";
        },
        languages: languages, // sagecell.allLanguages
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

// return;
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
        return name + '="' + def + '" ';
    }
    return name + '="' + value + '" ';
}

function doVariables(v: string, name: string) {

    if (!v) {
        return "";
    }
    let r = "";
    const va = v.split(";");
    for (const n in va) {
        if (va.hasOwnProperty(n)) {
            const nv = va[n].trim();
            if (nv) {
                r += name + nv + "&";
            }
        }
    }
    return r.replace(/ /g, "");
}

interface IExtraMarkup {
    document?: boolean;
}

class CsController implements IController {
    private static $inject = ["$scope", "$element", "$attrs"];

    cssPrint: boolean;
    out: {write: Function, writeln: Function, canvas: Element};
    canvasHeight: number;
    canvasWidth: number;
    previewIFrame: JQuery;
    lastUserinput: string;
    lastUserargs: string;
    iframeLoadTries: number;
    gsDefaultLanguage: string;
    glowscript: boolean;
    fullhtml: string;
    contentWindow: GlowScriptWindow;
    aelement: object;
    iframeClientHeight: number;
    lastJS: string;
    codeInitialized: boolean;
    irrotaKiinnita: string;
    english: boolean;
    canvas?: HTMLCanvasElement;
    canvasConsole: {log: (...args: string[]) => void};
    element0: HTMLElement;
    iframeopts: string;
    mode: string;
    editorModes: string;
    words: boolean;
    parsonsId: Vid;
    preview: HTMLElement;
    previewUrl: string;
    lastMD: string;
    file: {};
    postcode: string;
    precode?: string;
    replace: string;
    code: string;
    carretPos: number;
    indent: number;
    localcode?: string;
    showCodeLink: string;
    showCodeOn: string;
    showCodeOff: string;
    stop?: angular.IPromise<{}>;
    editorModeIndecies: number[];
    initUserCode: boolean;
    editorMode: number;
    height: string;
    width: string;
    taunoId: string;
    isHtml: boolean;
    comtestError?: string;
    plugin?: string;
    isAll: boolean;
    validityCheckMessage: string;
    validityCheck: string;
    userargs: string;
    userinput: string;
    cursor: string;
    tinyErrorStyle: {};
    runTestRed: boolean;
    runTestGreen: boolean;
    isRunning: boolean;
    error?: string;
    csparson: any;
    parson: any;
    iframe: boolean;
    indices: string;
    variables: string;
    table: string;
    lang: string;
    taunotype: string;
    isSimcir: boolean;
    simcir2: JQuery;
    taunoHtml: HTMLElement;
    simcir?: JQuery;
    isSage: boolean;
    noeditor: boolean;
    docLink: string;
    type: string;
    jstype: string;
    nosave: boolean;
    selectedLanguage: string;
    runError?: string | boolean;
    muokattu: boolean;
    autoupdate: boolean;
    runned: boolean;
    runTimer: number;
    aceEditor: IAceEditor;
    edit: HTMLTextAreaElement;
    editorIndex: number;
    wrap: {n: number};
    isMathCheck: boolean;
    uploadedType: string;
    user_id: string;
    taskId?: string;
    docURL?: string;
    uploadresult?: string;
    uploadedFile?: string;
    ufile?: File;
    copyingFromTauno: boolean;
    runSuccess: boolean;
    viewCode: boolean;
    imgURL: string;
    oneruntime: string;
    runtime: string;
    resImage: string;
    result?: string;
    taunoOn: boolean;
    errors: string[];
    htmlresult: string;
    byCode: string;
    attrs: {
        path: string,
        by: string,
        byCode: string,
        uploadbycode: boolean,
        treplace: string,
        program: string,
        fullhtml: string,
        html: string,
        runeverytime: boolean,
        scripts: string,
    };
    usercode: string;
    minRows: number;
    maxRows: number;
    rows: number;
    wavURL: string;
    noConsoleClear: boolean;
    isFirst: boolean;
    upload: boolean;
    isText: boolean;
    rtype: string;
    tiny: boolean;
    sagecellInfo?: CellInfo;
    sageArea?: Element;
    editArea?: Element;
    sageOutput?: Element;
    sageButton?: HTMLElement;
    sageInput?: HTMLInputElement;
    fileProgress?: number;
    fileError?: string;

    constructor(private scope: IScope, private element: IRootElementService, private attributes: IAttributes) {

    }

    svgImageSnippet() {
        const s = $sce.trustAsHtml(this.htmlresult);
        return s;
        // return $scope.htmlresult;
    }

    $postLink() {
        if (TESTWITHOUTPLUGINS) {
            return;
        }
        const element = this.element;
        const attrs = this.attrs;
        let t;
        switch (element[0].tagName.toLowerCase()) {
            case "cs-runner":
                t = "console";
                break;
            case "cs-jypeli-runner":
                t = "jypeli";
                break;
            case "cs-comtest-runner":
                t = "comtest";
                break;
            case "cs-runner-input":
                t = "console";
                break;
            case "cs-jypeli-runner-input":
                t = "jypeli";
                break;
            case "cs-comtest-runner-input":
                t = "comtest";
                break;
            case "cs-tauno-runner":
                t = "tauno";
                break;
            case "cs-tauno-runner-input":
                t = "tauno";
                break;
            case "cs-parsons-runner":
                t = "parsons";
                break;
            case "cs-sage-runner":
                t = "sage";
                break;
            case "cs-simcir-runner":
                t = "simcir";
                break;
            case "cs-text-runner":
                t = "text";
                break;
            case "cs-wescheme-runner":
                t = "wescheme";
                break;
            default:
                console.warn("Unrecognized csplugin tag type, falling back to 'console'");
                t = "console";
                break;
        }

        this.cursor = "⁞"; // \u0383"; //"\u0347"; // "\u02FD";
        this.taunoHtml = element[0].children[taunoPHIndex] as HTMLElement; // Check this carefully, where is Tauno placeholder
        this.plugin = element.parent().attr("data-plugin");
        this.taskId = element.parent().attr("id");
        this.isFirst = true;
        this.element = element;
        if (this.scope.$parent.$$prevSibling) {
            this.isFirst = false;
        }
        set(this, attrs, "lang", "fi");
        const english = this.lang === "en";
        this.english = english;

        if ((t === "tauno" || t === "simcir")) { // Tauno translations
            let taunoText = "Tauno";
            if (t === "simcir") {
                taunoText = "SimCir";
            }
            this.hideTaunoText = (english ? "hide " : "piilota ") + taunoText;
            this.showTaunoText = (english ? "Click here to show " : "Näytä ") + taunoText;
            this.taunoOhjeText = english ?
                'Copy the code you made by Tauno by pressing the link "copy from Tauno". Then press Run-button. Note that the running code may have different code than in Tauno!' :
                'Kopioi Taunolla tekemäsi koodi "kopioi Taunosta"-linkkiä painamalla. Sitten paina Aja-painiketta. Huomaa että ajossa voi olla eri taulukko kuin Taunossa!';
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
        this.isSimcir = t === "simcir";
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
        // csApp.set(scope,attrs,"usercode","");
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
        this.wrap = {n: this.wrap}; // to avoid child scope problems
        // csApp.set(scope,attrs,"program");

        this.docLink = "Document";
        this.editorMode = parseInt(this.editorMode);
        this.editorText = [this.normal, this.highlight, this.parsons, this.jsparsons];
        this.editorModeIndecies = [];
        for (let i = 0; i < this.editorModes.length; i++) {
            this.editorModeIndecies.push(parseInt(this.editorModes[i]));
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

        if (this.toggleEditor && this.toggleEditor != "True") {
            this.toggleEditorText = this.toggleEditor.split("|");
        }

        set(this, attrs, "usercode", "");
        if (this.usercode === "" && this.byCode) {
            this.usercode = this.byCode;
            this.initUserCode = true;
        }
        this.usercode = commentTrim(this.usercode);
        if (this.blind) {
            this.usercode = this.usercode.replace(/@author.*/, "@author XXXX");
        } // blind
        this.byCode = commentTrim(this.byCode);
        // scope.usercode = csApp.commentTrim(decodeURIComponent(escape(scope.usercode)));
        // scope.byCode = csApp.commentTrim(decodeURIComponent(escape(scope.byCode)));

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
        this.muokattu = false;
        if (this.open) {
            if (t === "tauno" || t === "simcir") {
                this.showTauno();
            }
            // if (t === 'wescheme') scope.showWeScheme();
        }

        // attrs.buttons = "$hellobuttons$\nMunOhjelma\n$typebuttons$\n$charbuttons$";
        let b = attrs.buttons || this.attrs.buttons;
        if (b) {
            const helloButtons = "public \nclass \nHello \n\\n\n{\n}\n" +
                "static \nvoid \n Main\n(\n)\n" +
                '        Console.WriteLine(\n"\nworld!\n;\n ';
            const typeButtons = "bool \nchar\n int \ndouble \nstring \nStringBuilder \nPhysicsObject \n[] \nreturn \n, ";
            const charButtons = "a\nb\nc\nd\ne\ni\nj\n.\n0\n1\n2\n3\n4\n5\nfalse\ntrue\nnull\n=";
            // var b = attrs.buttons;
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

        /*
        var editArea = element[0].getElementsByClassName('csEditArea');
        var editor = ace.edit(editArea);
        editor.setTheme("ace/theme/monokai");
        editor.getSession().setMode("ace/mode/javascript");
        */
        // scope.out = element[0].getElementsByClassName('console');
        if (this.attrs.autorun) {
            this.runCodeLink(true);
        }
        this.editorIndex = 0;
        if (this.editorMode != 0 || this.editorModes !== "01" || this.cssPrint) {
            this.showOtherEditor(this.editorMode);
        } // Forces code editor to change to pre
        this.mode = languageTypes.getAceModeType(this.type, this.mode);

        const styleArgs = getParam(this, "style-args", "");
        if (styleArgs) {
            const argsEdit = element[0].getElementsByClassName("csArgsArea"); // element.find("csArgsArea")[0]
            if (argsEdit.length > 0) {
                argsEdit[0].setAttribute("style", styleArgs);
            }
        }

        this.changeCodeLink();
        this.processPluginMath();

        csLogTime(this.taskId);

        this.showUploaded(this.attrs.uploadedFile, this.attrs.uploadedType);

        // if ( scope.isSage ) initSage(scope);
        /*
        scope.element0.keydown(function (e) {
              if (e.ctrlKey) {
                   if (e.keyCode === 0x53) {
                       if (!scope.isRunning) scope.runCode();
                   }
              }
        });
        */
        this.initEditorKeyBindings();
        $(element[0]).bind("keydown", (event) => {
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

    $onInit() {
        csLogTime("controller");
        this.wavURL = "";
        this.byCode = "";

        this.attrs = JSON.parse(this.data);
        this.byCode = this.attrs.by || this.attrs.byCode;
        this.errors = [];
        this.taunoOn = false;
        // $scope.replace = "INSERT YOUR CODE HERE";
        // $scope.file = "https://svn.cc.jyu.fi/srv/svn/ohj1/luentomonistecs/esimerkit/Pohja/Jypeli/Jypeli.cs";
        this.result = "";
        this.htmlresult = "";
        this.resImage = "";
        this.imgURL = "";
        this.viewCode = false;
        this.runSuccess = false;
        this.copyingFromTauno = false;

        this.scope.$watch("usercode", () => {
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
            if (this.wrap.n > 0) {
                this.checkWrap();
            }
            window.clearInterval(this.runTimer);
            if (this.runned && this.autoupdate) {
                this.runTimer = setInterval(this.runCodeAuto, this.autoupdate);
            }

            /* // tällä koodilla on se vika, että ei voi pyyhkiä alusta välilyönteä ja yhdistää rivejä
            if ( $scope.carretPos && $scope.carretPos >= 0 ) {
                $scope.edit.selectionStart = $scope.carretPos;
                $scope.edit.selectionEnd = $scope.carretPos;
                $scope.carretPos = -1;
            }
            else $scope.checkIndent(); // ongelmia saada kursori paikalleen
            */
        }, true);

        this.scope.$watch("userargs", () => {
            window.clearInterval(this.runTimer);
            if (this.runned && this.autoupdate) {
                this.runTimer = setInterval(this.runCodeAuto, this.autoupdate);
            }
        }, true);

        this.scope.$watch("userinput", () => {
            window.clearInterval(this.runTimer);
            if (this.runned && this.autoupdate) {
                this.runTimer = setInterval(this.runCodeAuto, this.autoupdate);
            }
        }, true);

        this.lastMD = "";

        this.canvasConsole = {log: (...args: string[]) => {
            let res = "";
            let sep = "";
            for (const a of args) {
                res += sep + a;
                sep = " ";
            }
            this.writeln(res);
        }};

        this.lastJS = "";
        this.iframeClientHeight = -1;
    }

    onFileSelect(file: File) {
        if (!this.taskId) {
            console.log("taskId missing");
            return;
        }
        this.ufile = file;
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
                url: `/pluginUpload/${ti[0]}/${ti[1]}/${this.user_id}/`,
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
        const r = wrapText(this.usercode, this.wrap.n);
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
        const t = languageTypes.getRunType(this.selectedLanguage, "cs") as string;
        if (t === "md") {
            this.showMD();
            if (nosave || this.nosave) {
                return;
            }
        }
        if (languageTypes.isInArray(t, csJSTypes)) {
            this.jstype = t;
            this.showJS();
            if (nosave || this.nosave) {
                return;
            }
        }
        this.doRunCode(t, nosave || this.nosave);
    }

    runCodeAuto() {
        window.clearInterval(this.runTimer);
        this.runCodeCommon(true);
    }

    runCodeLink(nosave: boolean) {
        this.runCodeCommon(nosave || this.nosave);
    }

    runCode() {
        this.runCodeCommon(false);
    }

    runTest() {
        const t = languageTypes.getTestType(this.type, this.selectedLanguage, "comtest") as string;
        this.doRunCode(t, false);
    }

    runUnitTest() {
        const t = languageTypes.getUnitTestType(this.type, this.selectedLanguage, "junit") as string;
        this.doRunCode(t, false);
    }

    runDocument() {
        if (this.docURL) {
            this.closeDocument();
            this.docLink = "Document";
            return;
        }
        this.docLink = "Hide document";
        const t = languageTypes.getRunType(this.selectedLanguage, "cs") as string;
        this.doRunCode(t, false, {document: true});
    }

    closeDocument() {
        this.docURL = "";
    }

    hideShowEditor() {
        this.noeditor = !this.noeditor;
    }

    async doRunCode(runType: string, nosave: boolean, extraMarkUp?: IExtraMarkup) {
        // $scope.viewCode = false;
        if (this.isRunning) {
            return;
        } // do not run if previuos is still running
        window.clearInterval(this.runTimer);
        this.closeDocument();
        if (this.isSage) {
            await initSage(this, true);
            runSage(this);
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
            // var fb = $scope.parson.getFeedback();
            this.usercode = this.csparson.join("\n");
            this.csparson.check(this.usercode);
        }

        // if ( runType === "md" ) { $scope.showMD(); return; }
        this.checkIndent();
        if (!this.autoupdate) {
            this.tinyErrorStyle = {};
            this.error = "... running ...";
            this.runError = true;
            this.isRunning = true;
        }
        this.isRunning = true;
        this.resImage = "";
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
        const t = runType;
        // if ( t === "tauno" ) t = "comtest";
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

        // params = 'type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&replace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.usercode);
        // $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
        const params = {
            //   'input': 1
            input: {
                usercode: ucode,
                userinput: uinput,
                isInput: isInput,
                userargs: uargs,
                uploadedFile: this.uploadedFile,
                uploadedType: this.uploadedType,
                nosave: false,
                // 'markup': {'type':t, 'file': $scope.file, 'replace': $scope.replace, 'lang': $scope.lang, 'taskId': $scope.taskId, 'user_id': $scope.user_id},
                markup: {type: t, taskId: this.taskId, user_id: this.user_id},
            },
        };
        // 		  alert($scope.usercode);
        if (nosave || this.nosave) {
            params.input.nosave = true;
        }
        if (extraMarkUp) {
            jQuery.extend(params.input.markup, extraMarkUp);
        }
        if (this.isAll) {
            jQuery.extend(params.input, {selectedLanguage: this.selectedLanguage});
        }
        let url = "/cs/answer";
        // url = "http://tim-beta.it.jyu.fi/cs/answer";
        if (this.plugin) {
            // url = "/csPlugin" + /*$scope.plugin + */ "/" + $scope.taskId + "/answer/"; // Häck to get same origin
            url = this.plugin;
            const i = url.lastIndexOf("/");
            if (i > 0) {
                url = url.substring(i);
            }
            url += "/" + this.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        }
        const t0run = performance.now();
        $http<{
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
        }>({method: "PUT", url: url, data: params, headers: {"Content-Type": "application/json"}, timeout: 20000},
        ).then((response) => {
            this.isRunning = false;
            const data = response.data;
            if (data.web.error && false) {
                this.error = data.web.error;
                // return;
            }

            const tsruntime = ((performance.now() - t0run) / 1000).toFixed(3);
            const runtime = (data.web.runtime || "").trim();
            this.oneruntime = "" + tsruntime + " " + runtime.split(" ", 1)[0];
            this.runtime = "\nWhole: " + tsruntime + "\ncsPlugin: " + runtime;
            if (data.web.pwd) {
                ConsolePWD.setPWD(data.web.pwd, this);
            }
            this.error = data.web.error;
            let imgURL;
            let wavURL;
            this.runSuccess = true;

            this.runError = this.error; // !$scope.runSuccess;

            imgURL = data.web.image;
            // if ( !imgURL ) imgURL = data.web["-replyImage"];
            this.imgURL = data.web["-replyImage"] || "";
            this.htmlresult = (data.web["-replyHTML"] || "") + (data.web["-replyMD"] || "");
            wavURL = data.web.wav;
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
                // $scope.resImage = '<img src="' + imgURL + ' " alt="Result image" />';
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

        }, (response) => {
            this.isRunning = false;
            const data = response.data;
            this.errors.push(status);
            this.error = "Ikuinen silmukka tai jokin muu vika?";
            // $scope.error = "TIMIssä ongelmia, odota vikaa tutkitaan...";
            if (data && data.error) {
                this.error = data.error;
            }
        });
    }

    hideTauno() {
        if (this.simcir) {
            this.simcir.children().remove();
            this.simcir = undefined;
        }
        this.taunoOn = false;
        this.taunoHtml.innerHTML = "<p></p>";
    }

    copyTauno() {
        const f = document.getElementById(this.taunoId) as any;
        // var s = $scope.taunoHtml.contentWindow().getUserCodeFromTauno();
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
        let data = simcir.controller(this.simcir.find(".simcir-workspace"));
        data = data.data();

        let buf = "";
        const print = (s) => {
            buf += s;
        };
        const println = (s) => {
            print(s);
            buf += "\r\n";
        };
        const printArray = (array) => {
            $.each(array, (i, item) => {
                println("    " + JSON.stringify(item) +
                    (i as string + 1 < array.length ? "," : ""));
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
        this.taunoHtml.innerHTML = `<div id=${v.vid}></div>`;
        const jqTauno = $(this.taunoHtml);
        this.simcir2 = $(this.taunoHtml.innerHTML);
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
            this.taunoHtml.innerHTML =
                // '<p class="pluginHide"" ><a ng-click="hideTauno()">hide Tauno</a></p>' + // ng-click ei toimi..
                '<iframe id="' + v.vid + '" class="showTauno" src="' + taunoUrl + '" ' + v.w + v.h + " " + this.iframeopts + " ></iframe>";
        } else {
            this.taunoHtml.innerHTML = '<div class="taunoNaytto" id="' + v.vid + '" />';
        }
        this.taunoOn = true;
    }

    runWeScheme(s: string) {
        // var f = document.getElementById($scope.taunoId) as any;
        // var s = $scope.taunoHtml.contentWindow().getUserCodeFromTauno();
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
            this.taunoHtml.innerHTML =
                // '<p class="pluginHide"" ><a ng-click="hideTauno()">hide Tauno</a></p>' + // ng-click ei toimi..
                '<iframe id="' + v.vid + '" class="showWeScheme" src="' + weSchemeUrl + '" ' + v.w + v.h + " " + this.iframeopts + " ></iframe>";
        } else {
            this.taunoHtml.innerHTML = '<div class="taunoNaytto" id="' + v.vid + '" />';
        }
        this.taunoOn = true;
    }

    initCode() {
        this.muokattu = false;
        this.usercode = this.byCode;
        this.resImage = "";
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
            initSage(this, false);
        }
        if (this.simcir) {
            this.setCircuitData();
        }
    }

    stopShow() {
        if (this.stop) {
            $interval.cancel(this.stop);
            this.stop = undefined;
        }
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
        // $scope.edit[0].selectionStart = start; // aiheuttaa uuden tapahtuman
        this.carretPos = start; // seuraava tapahtuma nappaa tämän ja siirtää vain kursorin.
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
        for (const i in st) {
            if (st.hasOwnProperty(i)) {
                const s = st[i];
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

    getAllCode(f) {
        // f: function to call after ready
        // var copyEvent = new ClipboardEvent('copy', { dataType: 'text/plain', data: 'kissa' } );
        // document.dispatchEvent(copyEvent);
        if (this.localcode != null) {
            this.getCodeFromLocalCode(f);
            return;
        }
        if (!this.file && !this.attrs.program) {
            this.localcode = "";
            this.getCodeFromLocalCode(f);
            return;
        }

        // params = 'print=1&type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&keplace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.usercode);
        const params = this.attrs;
        // $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
        $http<{msg: string, error: string} | string>({
                method: "POST",
                url: "/cs/?print=1&replace=",
                data: params,
                headers: {"Content-Type": "application/json"},
            },
            // $http({method: 'POST', url:"/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
        ).then((response) => {
            const data = response.data;

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
        }, (response) => {
            const status = response.status;
            this.errors.push(status);
            this.precode = "";
            this.postcode = "";
        });
    }

    showMD() {
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
        this.previewUrl = "/preview/" + this.taskId.split(".")[0]; // 12971"
        $http.post<{texts: string | Array<{html: string}>}>(this.previewUrl, {
            text: text,
        }).then((response) => {
            const data = response.data;
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
            const html = $compile(s)(this as any);
            $previewDiv.empty().append(html);

            ParCompiler.processAllMath($previewDiv);
            // $scope.outofdate = false;
            // $scope.parCount = len;

        }, (response) => {
            const data = response.data;
            $window.alert("Failed to show preview: " + data.error);
        });
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

    showJsParsons(parsonsEditDiv: Element) {
        let v = this.parsonsId;
        if (!v) {
            v = this.getVid();
            this.parsonsId = v;
        }
        parsonsEditDiv.setAttribute("id", v.vid);
        if ($("#" + v.vid).length === 0) {
            setTimeout(() => {
                    this.showJsParsons(parsonsEditDiv);
                }
                , 300);
            console.log("wait 300 ms " + v.vid);
            return;
        }
        let classes = "csrunEditorDiv sortable-code";
        let can_indent = true;
        if (this.words) {
            classes += " jssortable-words";
            can_indent = false;
            const a = $("#" + v.vid);
        }
        parsonsEditDiv.setAttribute("class", classes);
        // parsonsEditDiv.setAttribute('style',"float: none;");

        // parsonsEditDiv.innerHTML = "";
        this.parson = new ParsonsWidget({
            sortableId: v.vid,
            max_wrong_lines: 1,
            // 'prettyPrint': false,
            // 'x_indent': 0,
            can_indent: can_indent,
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
        if (eindex != 0) {
            return;
        }
        $(this.edit).bind("keydown", (event) => {
            eindex = this.editorModeIndecies[this.editorMode];
            if (eindex != 0) {
                return;
            }
            if (this.editorMode != 0) {
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
        this.edit = $compile(html[eindex])(this as any)[0] as HTMLTextAreaElement; // TODO unsafe cast
        // don't set the html immediately in case of Ace to avoid ugly flash because of lazy load
        // $scope.aceEnabled = eindex === 1;
        if (eindex === 1) {
            const ace = (await import("tim/editor/ace")).ace;
            editorDiv.empty().append(this.edit);
            const editor = ace.edit(editorDiv.find(".csAceEditor")[0]);

            this.aceLoaded(ace, editor as IAceEditor);
            if (this.mode) {
                this.aceEditor.getSession().setMode("ace/mode/" + this.mode);
            }
            this.aceEditor.setOptions({
                enableBasicAutocompletion: true,
                enableLiveAutocompletion: false,
                enableSnippets: true,
                maxLines: this.maxRows,
                // showTokenInfo: true
            });
            // $scope.aceEditor.setOptions({enableLiveAutocompletion: $scope.autocomplete});
            this.aceEditor.setFontSize(15);
            if (editorDiv.parents(".reveal").length > 0) {
                this.aceEditor.setFontSize(25);
            }
            this.aceEditor.getSession().setUseWorker(false); // syntax check away
            this.aceEditor.renderer.setScrollMargin(12, 12, 0, 0);
            this.aceEditor.getSession().setValue(this.usercode);
            this.aceEditor.getSession().on("change", () => {
                this.scope.$evalAsync(() => {
                    this.usercode = this.aceEditor.getSession().getValue();
                });
            });
            this.scope.$watch(() => this.usercode, (newValue, oldValue) => {
                if (newValue === oldValue || this.aceEditor.getSession().getValue() === newValue) {
                    return;
                }
                this.aceEditor.getSession().setValue(newValue);
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

    showJS() {
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
                const angularElement = '<div tim-draggable-fixed class="no-popup-menu" style="top: 91px; right: 0px; z-index: 20" >' +
                    '<span class="csRunMenu"><div class="csFixRelease"><a href ng-click="toggleFixed()" >{{irrotaKiinnita}}</a><a href ng-click="closeFrame()" style="float: right" >[X]</a></div></span>' +
                    (!this.fullhtml ? '<iframe id="' + v.vid + '" class="jsCanvas" src="' + fsrc + "?scripts=" + (this.attrs.scripts || scripts) + "&html=" + html + '" ' + v.w + v.h + ' style="border:0" ' + opts + ">" :
                        '<iframe id="' + v.vid + '" class="jsCanvas" ' + v.w + v.h + ' style="border:0" ' + opts + " >") +
                    "</iframe>" +
                    "</div>";
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
            $previewDiv.empty().append(this.previewIFrame = $compile(this.canvas)(this as any));
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
            // var s = $scope.taunoHtml.contentWindow().getUserCodeFromTauno();
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
                setTimeout(this.showJS, 300);
                console.log("Odotetaan 300 ms");
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
                this.out.write = this.write;
                this.out.writeln = this.writeln;
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
            if (fn && (typeof(fn) === "function")) {
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
    controller: CsController,
};

csApp.component("csRunner", {
    ...commonComponentOptions,
    template: directiveTemplateCS("console", false),
});
csApp.component("csJypeliRunner", {
    ...commonComponentOptions,
    template: directiveTemplateCS("jypeli", false),
});
csApp.component("csComtestRunner", {
    ...commonComponentOptions,
    template: directiveTemplateCS("comtest", false),
});
csApp.component("csRunnerInput", {
    ...commonComponentOptions,
    template: directiveTemplateCS("console", true),
});
csApp.component("csJypeliRunnerInput", {
    ...commonComponentOptions,
    template: directiveTemplateCS("jypeli", true),
});
csApp.component("csComtestRunnerInput", {
    ...commonComponentOptions,
    template: directiveTemplateCS("comtest", true),
});
csApp.component("csTaunoRunner", {
    ...commonComponentOptions,
    template: directiveTemplateCS("tauno", false),
});
csApp.component("csTaunoRunnerInput", {
    ...commonComponentOptions,
    template: directiveTemplateCS("tauno", true),
});
csApp.component("csParsonsRunner", {
    ...commonComponentOptions,
    template: directiveTemplateCS("parsons", false),
});
csApp.component("csSageRunner", {
    ...commonComponentOptions,
    template: directiveTemplateCS("sage", true),
});
csApp.component("csSimcirRunner", {
    ...commonComponentOptions,
    template: directiveTemplateCS("simcir", false),
});
csApp.component("csTextRunner", {
    ...commonComponentOptions,
    template: directiveTemplateCS("text", false),
});
csApp.component("csWeschemeRunner", {
    ...commonComponentOptions,
    template: directiveTemplateCS("wescheme", false),
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
        // $scope.content = JSON.parse($element.attr("data-content"));
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

    handler() {
        let url = "/cs/answer";
        if (this.plugin) {
            // url = "/csPlugin" + /*$scope.plugin + */ "/" + $scope.taskId + "/answer/"; // Häck to get same origin
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

        $http<{web: {pwd?: string, error?: string, console?: string}}>({
            method: "PUT",
            url: url,
            data: {
                input: {
                    usercode: ucode, userinput: uinput, isInput: isInput, userargs: uargs,
                    // "input": $scope.currentInput,
                    markup: {type: t, taskId: this.taskId, user_id: this.content.user_id},
                },
            },
        })
            .then((response) => {
                const data = response.data;
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
            }, (response) => {
                console.log(["protocol error", response]);
                this.submit("Endless loop?");
            });
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
        data: "<",
    },
    controller: CsConsoleController,
    template: `
<div class="web-console no-popup-menu {{$ctrl.currentSize}} " ng-keydown="$ctrl.handleKey($event)"><code
        class="console-output">
    <div class="console-output-elem" ng-repeat="item in $ctrl.history track by $index"><span class="console-oldinput">  <span
            class="console-in">{{item.istem}}</span>  <span class="console-userInput">{{item.input}}</span> </span>
        <span class="console-oldresponse"><span ng-if="!$ctrl.isShell">  <br/>  <span
                class="console-out">{{item.ostem}}</span></span>  <span class="console-response"
                                                                        ng-class="{error:item.error}"><span
                ng-bind-html="item.response"></span></span>
            <!-- Double span since ng-bind eats the innermost one -->
            </span></div>
    <span class="console-expander-sym" ng-click="$ctrl.toggleSize()"></span></code>
    <div class="console-examples-box"><span class="examples-title"
                                            ng-click="$ctrl.examplesVisible=!$ctrl.examplesVisible">    ▼ example expressions ▲</span>
        <div>Click to load:</div>
        <ul>
            <li ng-repeat="example in $ctrl.examples track by $index"><a ng-click="$ctrl.loadExample($index)"
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
