/* eslint-disable @typescript-eslint/no-unsafe-member-access,no-underscore-dangle */
/**
 * csPlugin component
 *
 * @author Daniel Juola
 * @author Denis Zhidkikh
 * @author Jaakko Palm
 * @author Juha Reinikainen
 * @author Mika Lehtinen
 * @author Simo Lehtinen
 * @author Tuomas Laine
 * @author Vesa Lappalainen
 * @license MIT
 * @date 3.8.2014
 */
import {
    ChangeDetectorRef,
    Component,
    Directive,
    ElementRef,
    HostListener,
    ViewChild,
} from "@angular/core";

import {HttpClient, HttpHeaders} from "@angular/common/http";
import type {SafeResourceUrl} from "@angular/platform-browser";
import {DomSanitizer} from "@angular/platform-browser";
import * as t from "io-ts";
import $ from "jquery";
import type {
    ISetAnswerResult,
    ITimComponent,
    ViewCtrl,
} from "tim/document/viewctrl";
import {ChangeType, FormModeOption} from "tim/document/viewctrl";
import type {IPluginInfoResponse} from "tim/editor/parCompiler";
import {ParCompiler} from "tim/editor/parCompiler";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {getFormBehavior} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {
    copyToClipboard,
    defaultErrorMessage,
    defaultTimeout,
    timeout,
    to,
    toPromise,
    valueDefu,
    valueOr,
} from "tim/util/utils";
import {TimDefer} from "tim/util/timdefer";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import type {InvalidMarkerState} from "tim/plugin/angular-plugin-base.directive";
import deepEqual from "deep-equal";
import type {ITemplateParam} from "tim/ui/showTemplateReplaceDialog";
import {
    showTemplateReplaceDialog,
    TemplateParam,
} from "tim/ui/showTemplateReplaceDialog";
import {InputDialogKind} from "tim/ui/input-dialog.kind";
import {showInputDialog} from "tim/ui/showInputDialog";
import html2canvas from "html2canvas";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import type {
    SimcirConnectorDef,
    SimcirDeviceInstance,
} from "../simcir/simcir-all";
import type {ICsParsonsOptions} from "./cs-parsons/csparsons";
import {CsParsonsOptions} from "./cs-parsons/csparsons";
import type {CellInfo} from "./embedded_sagecell";
import {getIFrameDataUrl} from "./iframeutils";
import type {EditorComponent} from "./editor/editor";
import {CURSOR, EditorFile, Mode} from "./editor/editor";
import {CountBoardComponent} from "./editor/countboard";
import {getInt} from "./util/util";
import type {IFile, IFileSpecification} from "./util/file-select";
import {FileSelectManagerComponent} from "./util/file-select";
import {OrderedSet, Set} from "./util/set";
import type {FormulaEvent} from "./editor/math-editor/symbol-button-menu.component";
import {selectFormulaFromPreview} from "./editor/math-editor/formula-utils";
import {LATEX_BUTTONS} from "./editor/math-editor/default-symbol-buttons";
import {FormulaEditorComponent} from "./editor/math-editor/formula-editor.component";

// TODO better name?
interface Vid {
    vid: string;
    width: number;
    height: number;
}

/*
Sagea varten ks: https://github.com/sagemath/sagecell/blob/master/doc/embedding.rst#id3
*/

let taunoNr = 0;

const globalFileUrlCache: Record<string, string> = {};

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

type IPwdWithoutSetPWD = Pick<IPwd, "savestate" | "path" | "attrs"> & {
    setPWD?(s: string): void;
};

interface ICsSimcirData {
    devices: SimcirDeviceInstance[];
    connectors: SimcirConnectorDef[];
}

class CWPD {
    pwdHolders: IPwd[] = [];
    currentPWD: Record<string, string> = {};

    constructor() {}

    register(scope: IPwd) {
        if (!this.isUser(scope)) {
            return;
        }
        this.pwdHolders.push(scope);
    }

    isUser(scope: IPwdWithoutSetPWD) {
        return (
            scope.path === "user" ||
            (scope.attrs && scope.attrs.path === "user")
        );
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
            const pwd = this.currentPWD[scope.savestate];
            if (pwd) {
                return pwd;
            }
        }
        // return "/home/agent";
        return "";
    }
}

export const ConsolePWD = new CWPD();

const csJSTypes = [
    "js",
    "glowscript",
    "vpython",
    "html",
    "processing",
    "wescheme",
];

async function loadSimcir() {
    const load = await import("../simcir/simcir-all");
    return load.simcir;
}

// =================================================================================================================
// Things for known languages

interface LanguageType {
    ace: string; // ACE-editor type
    comment: string; // one line comment for language, if needs
    // beg and end symbols, separate by space
    // for example "/* */"
}

class LanguageTypes {
    // For ACE editor mode check
    // * ACE editor preview http://ace.c9.io/build/kitchen-sink.html
    // * ACE editor demo sourcecode http://ace.c9.io/build/demo/kitchen-sink/demo.js

    languages: Record<string, LanguageType> = {
        go: {ace: "golang", comment: "//"},
        pascal: {ace: "pascal", comment: "//"},
        fortran: {ace: "fortran", comment: "c"},
        css: {ace: "css", comment: "/* */"},
        jypeli: {ace: "csharp", comment: "//"},
        scala: {ace: "scala", comment: "//"},
        java: {ace: "java", comment: "//"},
        graphics: {ace: "java", comment: "//"},
        cc: {ace: "c_cpp", comment: "//"},
        "c++": {ace: "c_cpp", comment: "//"},
        coq: {ace: "coq", comment: "(* *)"},
        shell: {ace: "sh", comment: "#"},
        vpython: {ace: "python", comment: "#"},
        py2: {ace: "python", comment: "#"},
        py: {ace: "python", comment: "#"},
        fs: {ace: "fsharp", comment: "//"},
        clisp: {ace: "lisp", comment: ";;"},
        jjs: {ace: "javascript", comment: "//"},
        nodejs: {ace: "javascript", comment: "//"},
        psql: {ace: "sql", comment: "--"},
        sql: {ace: "sql", comment: "--"},
        mongodb: {ace: "javascript", comment: "//"},
        cql: {ace: "sql", comment: "--"},
        alloy: {ace: "alloy", comment: ""},
        text: {ace: "text", comment: ""},
        cs: {ace: "csharp", comment: "//"},
        run: {ace: "sh", comment: "#"},
        md: {ace: "text", comment: "<!-- -->"},
        js: {ace: "javascript", comment: "//"},
        glowscript: {ace: "javascript", comment: "//"},
        sage: {ace: "python", comment: "#"},
        simcir: {ace: "json", comment: ""},
        xml: {ace: "xml", comment: "<!-- -->"},
        ocaml: {ace: "ocaml", comment: "(* *)"},
        octave: {ace: "matlab", comment: "#"},
        lua: {ace: "lua", comment: "--"},
        quorum: {ace: "quorum", comment: "//"},
        swift: {ace: "swift", comment: "//"},
        mathcheck: {ace: "text", comment: ""},
        html: {ace: "html", comment: "<!-- -->"},
        ihtml: {ace: "html", comment: "<!-- -->"},
        processing: {ace: "javascript", comment: "//"},
        rust: {ace: "rust", comment: "//"},
        wescheme: {ace: "scheme", comment: "--"},
        ping: {ace: "text", comment: ""},
        kotlin: {ace: "kotlin", comment: "//"},
        smalltalk: {ace: "text", comment: '" "'},
        upload: {ace: "text", comment: "//"},
        extcheck: {ace: "c_cpp", comment: ""},
        gitreg: {ace: "text", comment: ""},
        viz: {ace: "text", comment: "//"},
        vars: {ace: "text", comment: "//"},
        r: {ace: "r", comment: "#"},
        racket: {ace: "scheme", comment: ";"},
        scheme: {ace: "scheme", comment: ";"},
        ts: {ace: "typescript", comment: "//"},
        maxima: {ace: "matlab", comment: "/* */"},
        elixir: {ace: "elixir", comment: "#"},
    };

    // The known language types
    runTypes: string[] = [];
    aceModes: string[] = [];

    constructor() {
        // Sort by length (shortest last) to account for fuzzy run type selection
        const languageEntries = Object.entries(this.languages).sort(
            (a, b) => b[0].length - a[0].length
        );
        for (const [lt, language] of languageEntries) {
            this.runTypes.push(lt);
            this.aceModes.push(language.ace);
        }
    }

    // What are known test types (be careful not to include partial word):
    testTypes = ["ccomtest", "jcomtest", "comtest", "scomtest", "runtest"];
    testAceModes = ["c_cpp", "java", "csharp", "scala"];
    unitTestTypes = ["junit", "unit"];

    // If test type is comtest, how to change it for specific languages
    impTestTypes: Record<string, string> = {
        cs: "comtest",
        console: "comtest",
        cc: "ccomtest",
        java: "jcomtest",
        scala: "scomtest",
        "c++": "ccomtest",
        jypeli: "comtest",
    };
    // If test type is unit, how to change it for specific languages
    impUnitTestTypes: Record<string, string> = {
        cs: "nunit",
        console: "nunit",
        cc: "cunit",
        java: "junit",
        scala: "junit",
        "c++": "cunit",
        jypeli: "nunit",
    };

    getCommentMarkers(type: string) {
        type = type.toLowerCase();
        // eslint-disable-next-line @typescript-eslint/ban-ts-comment
        // @ts-ignore
        const c = this.languages[type].comment;
        const c2 = c.split(" ");
        let b = c2[0];
        if (b !== "") {
            b = b + " ";
        }
        let e = "";
        if (c2.length >= 2) {
            e = c2[1];
        }
        if (e !== "") {
            e = " " + e;
        }
        return [b, e];
    }

    getPureRunType(type: string) {
        const lang = this.languages[type];
        if (lang) {
            return type;
        }
        return "";
    }

    getTestRunType(type: string) {
        for (const [tlang, tname] of Object.entries(this.impTestTypes)) {
            if (tname === type) {
                return tlang;
            }
        }
        return "";
    }

    whatIsIn(types: string[], type: string, def: string) {
        if (!type) {
            return def;
        }
        type = type.toLowerCase();
        for (const ty of types) {
            if (type.includes(ty)) {
                return ty;
            }
        }
        return def;
    }

    whatIsInAce(type: string) {
        if (!type) {
            return undefined;
        }
        type = type.toLowerCase();
        const mod = this.languages[type];
        if (mod) {
            return mod.ace;
        }
        // If not any of languages, is it any of test's?
        for (let i = 0; i < this.testTypes.length; i++) {
            if (type.includes(this.testTypes[i])) {
                return this.testAceModes[i];
            }
        }
        return undefined;
    }

    isAllType(type: string) {
        if (!type) {
            return false;
        }
        type = type.toLowerCase();
        if (!type.startsWith("all")) {
            return false;
        }
        return !!type.match(/^all[^a-z0-9]?/);
    }

    getRunType(type: string, def: string) {
        return this.whatIsIn(this.runTypes, type, def);
    }

    getAceModeType(type: string): string | undefined;
    getAceModeType(type: string, def: string): string;
    getAceModeType(type: string, def?: string) {
        if (def) {
            return def;
        }
        return this.whatIsInAce(type) ?? (def as string);
    }

    getTestType(type: string, language: string, def: string) {
        const ty = this.whatIsIn(this.testTypes, type, def);
        if (ty !== "comtest") {
            // specific text name, use it
            return ty;
        }
        const lang = this.languages[language];
        if (!lang) {
            // main lagnguage not know => no test
            return "";
        }
        const impt = this.impTestTypes[language];
        if (impt) {
            // no test know for this language
            return impt;
        }
        return "";
    }

    getUnitTestType(type: string, language: string, def: string) {
        const ty = this.whatIsIn(this.unitTestTypes, type, def);
        if (ty !== "unit") {
            return ty;
        }
        const lang = this.languages[language];
        if (!lang) {
            // main lagnguage not know => no test
            return "";
        }
        const impt = this.impUnitTestTypes[language];
        if (impt) {
            // no unit test know for this language
            return impt;
        }
        return "";
    }

    isInArray(word: string, array: string[]) {
        return array.includes(word);
    }
}

export const languageTypes = new LanguageTypes();

// =================================================================================================================

function removeXML(s: string) {
    s = s.replace(/^<\?xml [^>]*\?>/, "");
    s = s.replace(/(<svg [^>]*height="[0-9]+)pt/, "$1");
    s = s.replace(/(<svg [^>]*width="[0-9]+)pt/, "$1");
    return s;
}

function commentTrim(s: string) {
    if (!s || s === "//") {
        return "";
    }
    const n = s.indexOf("//\n");
    if (n !== 0) {
        return s;
    }
    return s.slice(3);
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

const CspluginAnswer = t.type({usercode: t.string});

interface IUploadResponse {
    file: string;
    type: string;
    block: number;
}

export const TemplateButton = t.intersection([
    t.type({
        data: t.string,
        text: t.string,
    }),
    t.partial({
        expl: t.string,
        hasMath: t.boolean,
        placeholders: t.array(TemplateParam),
        type: t.string,
    }),
]);

export interface ITemplateButton extends t.TypeOf<typeof TemplateButton> {}

/**
 * This defines the required format for the csPlugin YAML markup.
 * All fields in the markup are optional (as indicated by t.partial function).
 *
 * If a field does not match the type, an error will be shown to the user
 * and the plugin won't work until the problem is fixed.
 */

export const Example = t.type({
    expr: t.string,
    title: t.string,
});

const CountLimit = t.partial({
    show: t.boolean,
    min: t.number,
    max: t.number,
    text: t.string,
});

const CountType = t.partial({
    preventSave: t.boolean,
    tooManyWord: t.string,
    tooFewWord: t.string,
    lines: CountLimit,
    words: CountLimit,
    chars: CountLimit,
});

const oneOrArray = <T extends t.Mixed>(type: T) =>
    t.union([type, t.array(type)]);
const listify = <T>(e: T | T[]) => (Array.isArray(e) ? e : [e]);

const CommonMarkup = t.intersection([
    t.union([
        t.type({program: t.string}), // TODO
        t.type({fullprogram: t.string}), // TODO
        t.type({file: t.string}), // TODO
        t.type({fullfile: t.string}), // TODO
        t.type({}),
    ]),
    t.union([
        t.type({byCode: t.string}),
        t.type({byFile: t.string}), // TODO
        t.type({}),
    ]),
    t.partial({maxSize: t.number}),
]);

const EditorMarkupFields = t.intersection([
    // no source attribute
    t.type({
        path: t.string,
        canClose: withDefault(t.boolean, false),
        canRename: withDefault(t.boolean, false),
        canModify: withDefault(t.boolean, true),
    }),
    t.partial({
        mode: t.string,
        placeholder: t.string,
    }),
    CommonMarkup,
]);

const EditorMarkup = t.intersection([
    // with source attribute
    t.type({
        source: t.literal("editor"),
    }),
    EditorMarkupFields,
]);
type IEditorMarkup = t.TypeOf<typeof EditorMarkup>;

const UploadMarkupFields = t.intersection([
    // no source or paths attribute
    t.partial({
        maxSize: t.number,
        extensions: oneOrArray(t.string),
    }),
    CommonMarkup,
]);

const UploadMarkup = t.intersection([
    // with source and paths attribute
    t.type({
        paths: oneOrArray(t.string),
        source: t.literal("upload"),
    }),
    UploadMarkupFields,
]);
type IUploadMarkup = t.TypeOf<typeof UploadMarkup>;

const UploadByCodeMarkup = t.intersection([
    t.type({
        source: t.literal("uploadByCode"),
        show: withDefault(
            t.union([
                t.boolean,
                t.literal("loaded"), // show after load
            ]),
            false
        ),
    }),
    EditorMarkupFields,
    UploadMarkupFields,
]);
type IUploadByCodeMarkup = t.TypeOf<typeof UploadByCodeMarkup>;

const ExternalSourceMarkup = t.intersection([
    t.type({
        path: t.string,
        source: t.string,
    }),
    t.partial({
        maxSize: t.number,
        maxTotalSize: t.number,
    }),
]);

const FileMarkup = t.union([
    t.type({source: withDefault(t.string, "editor")}),
    EditorMarkup,
    UploadMarkup,
    UploadByCodeMarkup,
    ExternalSourceMarkup,
]);

const FileSubmission = t.intersection([
    t.type({
        source: t.string,
        path: t.string,
    }),
    t.partial({
        content: nullable(t.string),
        type: t.string,
    }),
]);
export type IFileSubmission = t.TypeOf<typeof FileSubmission>;

const UploadedFile = t.type({
    path: t.string,
    type: t.string,
});

interface IUploadedFile extends t.TypeOf<typeof UploadedFile> {}

const GitDefaultsMarkup = t.partial({
    url: t.string,
    user: t.string,
    branch: t.string,
    library: t.string,
    glob: t.string,
    cache: t.number,
    apiProtocol: t.string,
    librarySpecific: t.unknown,
});

const GitMarkup = t.partial({
    onError: t.keyof({
        raise: null,
        remove: null,
        removeall: null,
        create: null,
    }),
    repo: t.intersection([
        t.type({name: t.string}),
        t.partial({
            owner: t.string,
            fork: t.boolean,
            oldName: t.string,
            oldOwner: t.string,
            librarySpecific: t.unknown,
        }),
    ]),
    library: t.string,
    fields: t.record(
        t.string,
        t.intersection([
            t.type({
                value: t.unknown,
            }),
            t.partial({
                onError: t.keyof({
                    raise: null,
                    ask: null,
                    none: null,
                }),
            }),
        ])
    ),
    askFields: t.array(t.string),
});

const CsMarkupOptional = t.partial({
    // TODO: this gets deleted in server but only conditionally,
    //  decide if this should be here.
    //  Seems yes; GlowScript uses it at least.
    program: t.string,

    // "*"" for everything
    // TODO: add wildcard support to path options
    allowedPaths: t.union([t.literal("*"), t.array(t.string)]),
    argsplaceholder: t.string,
    argsstem: t.string,
    autosave: t.boolean,
    autoupdate: t.number,
    buttons: t.string,
    mdButtons: nullable(t.array(TemplateButton)),
    byCode: t.string,
    docurl: t.string,
    editorreadonly: t.boolean,
    examples: t.array(Example),
    file: t.string,
    filename: t.string,
    fullhtml: t.string,
    srchtml: t.string,
    fullhtmlurl: t.string,
    fullhtmlparams: t.record(t.string, t.string),
    git: GitMarkup,
    gitDefaults: GitDefaultsMarkup,
    height: t.union([t.number, t.string]),
    highlight: nullable(t.string),
    html: t.string,
    indices: t.string,
    inputplaceholder: t.string,
    jsparams: t.record(t.string, t.unknown),
    languages: t.string, // not used in any plugin? // TODO: should be used to give set of languages that can be used
    mode: t.string,
    noeditor: t.boolean,
    normal: nullable(t.string),
    parsons: CsParsonsOptions,
    formulaEditor: t.boolean,
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
    useSameFrame: t.boolean,
    variables: t.string,
    width: t.union([t.number, t.string]),
    wrap: t.number,
    borders: withDefault(t.boolean, true),
    iframeopts: t.string,
    iframescroll: t.boolean,
    count: CountType,
    hide: t.partial({wrap: t.boolean, changed: t.boolean}),
    savedText: t.string,
    testText: t.string,
    runningText: t.string,
    rootPath: t.string,
    masterPath: t.string,
    files: oneOrArray(FileMarkup),
    moreFiles: oneOrArray(ExternalSourceMarkup),
    jsFiles: t.array(t.string),
    cssFiles: t.array(t.string),
    deleteFiles: t.array(t.string),
    jsBrowserConsole: t.boolean,
    editorOrder: t.array(t.string),
    editorOrderForceAccessible: t.boolean,
    resetUserInput: t.boolean,
    uploadAcceptPattern: t.string,
    uploadAcceptMaxSize: t.number,
    showAlwaysSavedText: t.boolean,
    startLineNumber: t.number,
    targetCanvas: t.string,
    previewExtraSettings: t.UnknownRecord,
});

const CsMarkupDefaults = t.type({
    autorun: withDefault(t.boolean, false),
    blind: withDefault(t.boolean, false),
    canvasHeight: withDefault(t.number, 300),
    canvasWidth: withDefault(t.number, 700),
    codeover: withDefault(t.boolean, false),
    codeunder: withDefault(t.boolean, false),
    cols: withDefault(t.number, 10),
    copyLink: withDefault(t.string, "Copy"),
    copyConsoleLink: withDefault(t.string, "⧉"),
    copyErrorLink: withDefault(t.string, "⧉"),
    dragAndDrop: withDefault(t.boolean, true),
    editorMode: withDefault(t.number, -1),
    editorModes: withDefault(t.union([t.string, t.number]), "01"),
    formulaEditor: withDefault(t.boolean, false),
    iframe: withDefault(t.boolean, false), // TODO this maybe gets deleted on server
    indent: withDefault(t.number, -1),
    initSimcir: withDefault(t.string, ""),
    "style-args": withDefault(t.string, ""), // TODO get rid of "-"
    inputrows: withDefault(t.number, 1),
    inputstem: withDefault(t.string, ""),
    isHtml: withDefault(t.boolean, false),
    jsparsons: withDefault(t.string, "JS-Parsons"),
    justSave: withDefault(t.boolean, false),
    justCompile: withDefault(t.boolean, false),
    lang: withDefault(t.string, "fi"),
    maxrows: withDefault(t.number, 100),
    maxSize: withDefault(t.number, 50),
    uploadAcceptMaxSize: withDefault(t.number, -1), // Differs from maxSize because this is a global maxSize instead of default
    noConsoleClear: withDefault(t.boolean, false),
    nocode: withDefault(t.boolean, false),
    norun: withDefault(t.boolean, false),
    nosave: withDefault(t.boolean, false),
    open: withDefault(t.boolean, false),
    rows: withDefault(t.number, 1),
    showRuntime: withDefault(t.boolean, false),
    toggleEditor: withDefault(t.union([t.boolean, t.string]), false),
    type: withDefault(t.string, "cs"),
    upload: withDefault(t.boolean, false),
    validityCheck: withDefault(t.string, ""),
    validityCheckMessage: withDefault(t.string, ""),
    validityCheckForceSave: withDefault(t.boolean, false),
    viewCode: withDefault(t.boolean, false),
    allowMultipleFiles: withDefault(t.boolean, true),
    multipleUploadElements: withDefault(t.boolean, true),
    mayAddFiles: withDefault(t.boolean, false),
    norunmenu: withDefault(t.boolean, false),
    noargs: withDefault(t.boolean, false),
    noclose: withDefault(t.boolean, false),
});

const CsMarkup = t.intersection([
    CsMarkupOptional,
    CsMarkupDefaults,
    GenericPluginMarkup,
]);

const CsAllPart = t.partial({
    uploadedFile: t.string,
    uploadedType: t.string,
    uploadedFiles: t.array(UploadedFile),
    userargs: t.string,
    usercode: t.string,
    userinput: t.string,
    submittedFiles: t.array(FileSubmission),
    selectedLanguage: t.string,
    by: t.string,
    docurl: t.string,
    program: t.string,
    replace: t.string,
    timeout: t.number,
    error: t.string,
    own_error: t.string,
    gitRegistered: t.boolean,
    isTauno: t.boolean,
});

const SetData = CsAllPart; // TODO maybe make an own type for this instead of using CsAllPart

const CsAll = t.intersection([
    CsAllPart,
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
    }),
]);

export class CsBase extends AngularPluginBase<
    t.TypeOf<typeof CsMarkup>,
    t.TypeOf<typeof CsAll>,
    typeof CsAll
> {
    usercode_: string = "";
    languageType: string = "cs";
    origLanguageType: string = "cs";
    rtype: string = "cs";
    isRun: boolean = true;
    isAll: boolean = false;
    isTest: boolean = false;
    isUnitTest: boolean = false;
    byCode: string = "";
    runChanged: boolean = false;
    canReset: boolean = false;
    firstTime: boolean = true; // force showJS on first time even usercode === ""
    // otherwise clears for some reason the codearea???

    get usercode(): string {
        return this.usercode_;
    }

    set usercode(str: string) {
        this.usercode_ = str;
    }

    get type() {
        return this.languageType;
    }

    get path() {
        return this.markup.path;
    }

    getAttributeType() {
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

interface IFrameLoad {
    iframe: HTMLIFrameElement & {contentWindow: WindowProxy};
    channel: MessageChannel;
}

type IFrameType = {
    filename?: string;
    content: string;
    width?: number;
    height?: number;
    id?: string;
    style?: string;
    sandbox?: string;
};

type IFramesType = {
    files: IFrameType[];
    style?: string;
    defaults?: {
        width?: number;
        height?: number;
        style?: string;
        sandbox?: string;
    };
};

interface IRunResponseWeb {
    error?: string;
    pwd?: string;
    image?: string;
    video?: string;
    wav?: string;
    testGreen?: boolean;
    testRed?: boolean;
    comtestError?: string;
    docurl?: string;
    console?: string;
    runtime?: string;
    language?: unknown; // determined by language
    md?: string;
    "-replyImage"?: string;
    "-replyHTML"?: string;
    "-replyMD"?: string;
    parsons_correct?: number[];
    parsons_styles?: string[];
    iframes?: IFramesType;
}

export interface IRunResponse {
    web: IRunResponseWeb;
    savedNew: number;
}

interface IFetchResponse {
    error?: string;
    files: IFileSubmission[];
}

type IRunRequestInput = Partial<IExtraMarkup> & {
    usercode?: string;
    submittedFiles?: IFileSubmission[];
    userinput: string;
    isInput: boolean;
    userargs: string;
    uploadedFile?: string;
    uploadedType?: string;
    uploadedFiles?: {path: string; type: string}[];
    nosave: boolean;
    type: string;
    selectedLanguage?: string;
};

export interface IRunRequest {
    input: IRunRequestInput;
}

function getButtonTextHtml(s: string) {
    let ret = s.trim();
    ret = ret.replace("\\n", "");
    ret = ret.replace(CURSOR, "");
    if (ret.length === 0) {
        ret = "\u00A0";
    }
    return ret;
}

/**
 * Creates a list of template buttons based on syntax specified at
 * https://tim.jyu.fi/view/tim/ohjeita/csPlugin#buttons.
 * Also merges existing markdown template buttons into the parsed list.
 * @param b string containing an array of buttons
 * @param mdButtons array of markdown buttons to add
 * @return list of template buttons
 */
export function createTemplateButtons(
    b: string | undefined,
    mdButtons: ITemplateButton[] | undefined | null
) {
    if (!b && !mdButtons) {
        return [];
    }
    if (!b) {
        b = "";
    }
    if (!mdButtons) {
        mdButtons = [];
    }
    const helloButtons =
        "public \nclass \nHello \n\\n\n{\n}\n" +
        "static \nvoid \n Main\n(\n)\n" +
        '        Console.WriteLine(\n"\nworld!\n;\n ';
    const typeButtons =
        'bool \nchar\n int \ndouble \nstring \nStringBuilder \nPhysicsObject \n["[] "] \nreturn \n, ';
    const charButtons =
        "a\nb\nc\nd\ne\ni\nj\n.\n0\n1\n2\n3\n4\n5\nfalse\ntrue\nnull\n=";

    b = b.replace("$hellobuttons$", helloButtons);
    b = b.replace("$typebuttons$", typeButtons);
    b = b.replace("$charbuttons$", charButtons);
    b = b.trim();
    b = b.replace("$space$", " ");
    const lines = b.split("\n");

    // removes all " or ' respectively if line starts with them
    for (let i = 0; i < lines.length; i++) {
        let s = lines[i];
        if (s.length < 1) {
            continue;
        }

        if (s.startsWith('"') || s.startsWith("'")) {
            s = s.replace(new RegExp(s[0], "g"), "");
            lines[i] = s;
        }
    }
    const templateButtons: ITemplateButton[] = [];

    for (const line of lines) {
        if (line === "") {
            continue;
        }
        // just string
        if (!line.startsWith("[")) {
            // add all latex buttons
            if (line === "$latexbuttons$") {
                for (const button of LATEX_BUTTONS) {
                    templateButtons.push(button);
                }
                continue;
            }
            templateButtons.push({
                text: getButtonTextHtml(line),
                data: line,
            });
            continue;
        }
        // maybe array
        try {
            const parsed = JSON.parse(line);
            let defaultData = parsed[0].replace("\\square", "⁞");
            defaultData = defaultData.replace(
                /\\\[|\\]|\\square|\s|\\\(|\\\)/g,
                ""
            );
            const item: ITemplateButton = {
                text: parsed[0],
                data: defaultData,
                expl: defaultData,
            };
            const mathAttributes = ["", "math", "s", "q", "t", "e"];
            if (parsed.length > 1) {
                for (const part of parsed) {
                    if (mathAttributes.includes(part)) {
                        switch (part) {
                            case mathAttributes[1]:
                                item.hasMath = true;
                                break;
                            case mathAttributes[2]:
                            case mathAttributes[3]:
                            case mathAttributes[4]:
                            case mathAttributes[5]:
                                item.type = part;
                                break;
                        }
                    }
                }
                if (!mathAttributes.includes(parsed[1])) {
                    item.data = parsed[1];
                }
                item.expl = item.data.replace("⁞", "");
                if (parsed.length > 2) {
                    if (!mathAttributes.includes(parsed[2])) {
                        item.expl = parsed[2];
                    }
                    if (parsed.length > 3) {
                        if (!mathAttributes.includes(parsed[3])) {
                            item.placeholders = [];
                            for (let i = 3; i < parsed.length; i++) {
                                const p = parsed[i];
                                if (!(p instanceof Array)) {
                                    continue;
                                }
                                const param: ITemplateParam = {
                                    default: p[0] ?? "",
                                    text: p[1] ?? "",
                                    pattern: p[2] ?? "",
                                    error: p[3] ?? "",
                                };
                                if (p.length > 0) {
                                    item.placeholders?.push(param);
                                }
                            }
                        }
                    }
                }
            }
            templateButtons.push(item);
        } catch (e) {
            templateButtons.push({
                text: "error",
                data: "",
                expl: "" + e,
            });
        }
    }
    for (const btn of mdButtons) {
        templateButtons.push(btn);
    }
    return templateButtons;
}

@Directive() // needs this or compiler complains
export class CsController extends CsBase implements ITimComponent {
    vctrl!: ViewCtrl;

    autoupdateHandle?: number;
    canvasConsole: {log: (...args: string[]) => void};
    code?: string;
    codeInitialized: boolean = false;
    comtestError?: string;
    connectionErrorMessage?: string;
    copyingFromTauno: boolean;
    docLink: string;
    docURL?: SafeResourceUrl;
    edited: boolean = false;
    editArea?: Element;
    editorIndex: number;
    error?: string;
    errors: string[];
    externalFiles?: IFileSubmission[];
    fetchError?: string;
    fileError?: string;
    fileProgress?: number;
    fullCode: string = "";
    height?: string | number;
    csRunDivStyle: Record<string, string> = {};
    htmlresult: string;
    iframeClientHeight: number;
    imgURL: string;
    videoURL: string;
    indent!: number;
    initUserCode: boolean = false;
    isRunning: boolean = false;
    jsparams?: Record<string, unknown>;
    lastJS: string;
    lastMD: string;
    lastUserargs?: string;
    lastUserinput?: string;
    localcode?: string;
    keepErrors: boolean = false;
    muokattu: boolean;
    noeditor!: boolean;
    formulaEditorOpen = false;
    @ViewChild("formulaEditorComponent")
    formulaEditorComponent?: FormulaEditorComponent;
    currentSymbol: FormulaEvent = {text: ""};
    oneruntime?: string;
    out?: {write: () => void; writeln: () => void; canvas: Element};
    parsons?: ICsParsonsOptions;
    postcode?: string;
    precode?: string;
    preview!: JQuery<HTMLElement>;
    result?: string;
    runError?: string | boolean;
    hasBeenRun: boolean = false;
    runSuccess: boolean;
    runTestGreen: boolean = false;
    runTestRed: boolean = false;
    runtime?: string;
    sageArea?: Element;
    sageButton?: HTMLElement;
    sagecellInfo?: CellInfo;
    sageInput?: HTMLInputElement;
    sageOutput?: Element;
    selectedLanguage!: string;
    simcir?: JQuery;
    tinyErrorStyle: Partial<CSSStyleDeclaration> = {};
    uploadedFiles = new Set((o: IUploadedFile) =>
        this.uploadedFileName(o.path)
    );
    uploadUrl?: string;
    userargs_: string = "";
    userinput_: string = "";
    isViz?: boolean;
    isVars?: boolean;
    viewCode!: boolean;
    wavURL: string = "";
    wrap!: {n: number; auto: boolean};
    templateButtons: ITemplateButton[] = [];
    templateButtonsCount: number = 0;
    mdHtml?: string;
    @ViewChild("mdHtmlDiv") mdHtmlDiv?: ElementRef<HTMLDivElement>;

    iframesettings?: {
        src?: SafeResourceUrl;
        width: number;
        id: string;
        height: number;
    };
    loadedIframe?: IFrameLoad;
    taunoFrame?: IFrameLoad;
    simcirElem?: HTMLElement;
    taunoCopy?: TimDefer<string>;
    iframedefer?: TimDefer<IFrameLoad>;
    iframemessageHandler?: (e: MessageEvent) => void;
    savedvals?: {
        usercode: string;
        args: string;
        input: string;
        files: string[];
    };
    preventSave: boolean = false;
    hide: {wrap?: boolean; changed?: boolean} = {};
    savedText: string = "";
    timeout: number = 0;
    editorModes: Mode[] = [];
    editor?: EditorComponent;
    externalEditor?: EditorComponent;
    hasExternalSources: boolean = false;
    fileSelect?: FileSelectManagerComponent;
    upload?: boolean;
    uploadByCodeFiles: {path: string; show: boolean | "loaded"}[] = [];
    @ViewChild(CountBoardComponent) countBoard?: CountBoardComponent;
    private isSimcirUnsaved?: boolean;
    private canResetSimcir?: boolean;
    private clearSaved: boolean = false;
    private originalUserInput?: string;
    exportingMD = false;
    invalidMarker?: InvalidMarkerState;

    @ViewChild("externalEditor")
    set externalEditorViewSetter(newValue: EditorComponent | undefined) {
        if (newValue == this.externalEditor) {
            return;
        }
        this.externalEditor = newValue;
        this.updateExternalEditor();
    }

    updateExternalEditor() {
        const files: EditorFile[] = [];
        if (!this.externalEditor) {
            return;
        }

        if (this.externalFiles) {
            for (const f of this.externalFiles) {
                const file = new EditorFile(f.path);
                file.content = f.content ?? "";
                files.push(file);
            }
        }

        this.externalEditor.modes = [new Mode(Mode.Normal)];
        this.externalEditor.setFiles(files);
    }

    @ViewChild("mainEditor")
    set editorViewSetter(new_value: EditorComponent | undefined) {
        if (this.editor == new_value) {
            return;
        }
        this.editor = new_value;
        if (!this.editor) {
            return;
        }

        this.editor.setReadOnly(
            this.markup.editorreadonly === true || this.readonly
        );
        if (this.attrsall.submittedFiles || this.markup.files) {
            const files = new OrderedSet<EditorFile>((f) => f.path);
            const defaultMode =
                this.markup.mode ?? languageTypes.getAceModeType(this.type);
            if (this.markup.files) {
                const markupFiles = listify(this.markup.files).filter(
                    (f) =>
                        f.source == "editor" ||
                        (f.source == "uploadByCode" &&
                            (f as IUploadByCodeMarkup).show === true)
                ) as (IEditorMarkup | IUploadByCodeMarkup)[];
                for (const f of markupFiles) {
                    let base: string | undefined;
                    if ("byCode" in f) {
                        this.initUserCode = true;
                        base = f.byCode;
                    }
                    const file = new EditorFile(
                        f.path,
                        base,
                        f.mode ?? defaultMode,
                        f.canClose,
                        f.canRename,
                        f.canModify
                    );
                    files.push(file);
                    file.placeholder = f.placeholder ?? this.placeholder;
                }
            }

            if (this.attrsall.submittedFiles) {
                for (const file of this.attrsall.submittedFiles) {
                    let include = false;
                    if (file.source == "editor") {
                        include = true;
                    } else if (file.source == "uploadByCode") {
                        if (this.markup.files) {
                            if (
                                (
                                    listify(this.markup.files).find(
                                        (f) =>
                                            f.source == "uploadByCode" &&
                                            (f as IUploadByCodeMarkup).path ==
                                                file.path
                                    ) as IUploadByCodeMarkup
                                )?.show
                            ) {
                                if (this.editor.findFile(file.path)) {
                                    const f = this.createUploadByCodeEditorFile(
                                        file.path,
                                        file.content
                                    );
                                    if (f) {
                                        files.push(f);
                                    }
                                }
                                include = true;
                            }
                        } else if (
                            this.markup.uploadbycode &&
                            file.path == ""
                        ) {
                            include = true;
                        }
                    }
                    if (include) {
                        const f =
                            files.getByKey(file.path) ??
                            new EditorFile(file.path, "", defaultMode);
                        f.content = file.content ?? "";
                        files.push(f);
                    }
                }
            }

            for (const file of files) {
                let code = file.content;
                code = commentTrim(code);
                if (this.markup.blind) {
                    code = code.replace(/@author.*/, "@author XXXX");
                }
                file.content = code;
            }

            this.editor.setFiles(files.toArray());

            if (this.savedvals) {
                this.savedvals.files = files.toArray().map((f) => f.content);
            }
        } else {
            let usercode: string = this.attrsall.usercode ?? "";
            if (this.attrsall.usercode == null) {
                if (this.byCode) {
                    usercode = this.byCode;
                    this.initUserCode = true;
                }
            }
            usercode = commentTrim(usercode);
            if (this.markup.blind) {
                usercode = usercode.replace(/@author.*/, "@author XXXX");
            }
            this.editor.content = usercode;
            this.editor.languageMode = this.mode;

            if (this.savedvals) {
                this.savedvals.usercode = this.usercode;
            }
        }
        this.editor.placeholder = this.placeholder;
        this.editor.mayAddFiles = this.markup.mayAddFiles;
        if (this.markup.allowedPaths != "*") {
            this.editor.allowedPaths = this.editor.files
                .map((f) => f.path)
                .concat(this.markup.allowedPaths ?? []);
        }
        if (this.parsons) {
            this.parsons.shuffle = this.initUserCode;
        }
        if (this.editor.addFormulaEditorOpenHandler) {
            this.editor.addFormulaEditorOpenHandler(() => {
                this.toggleFormulaEditor();
            });
        }

        this.editor.startLineNumber = this.markup.startLineNumber;
    }

    @ViewChild(FileSelectManagerComponent)
    set fileSelectSetter(component: FileSelectManagerComponent | undefined) {
        if (component == this.fileSelect) {
            return;
        }
        this.fileSelect = component;
        if (!component || !this.upload) {
            return;
        }

        const files: IFileSpecification[] = [];
        if (this.markup.files) {
            const markupFiles = listify(this.markup.files).filter(
                (f) => f.source == "upload" || f.source == "uploadByCode"
            ) as (IUploadMarkup | IUploadByCodeMarkup)[];
            for (const fs of markupFiles) {
                const paths = listify("path" in fs ? fs.path : fs.paths);
                let extensions: string[] | undefined;
                if (fs.extensions) {
                    extensions = listify(fs.extensions);
                } else {
                    const exts = paths
                        .map((p) => "." + (p.split(".").slice(1)[0] ?? ""))
                        .filter((ext) => ext != ".");
                    if (exts.length != 0) {
                        extensions = exts;
                    }
                }
                files.push({
                    paths: paths, // TODO: handle no filename
                    extensions: extensions,
                    maxSize: fs.maxSize ?? this.markup.maxSize,
                    upload: fs.source == "upload",
                });
                if (fs.source == "uploadByCode") {
                    this.uploadByCodeFiles.push(
                        ...paths.map((p) => ({path: p, show: fs.show ?? false}))
                    );
                }
            }
        } else if (
            this.origLanguageType.includes("upload") ||
            this.markup.upload ||
            this.markup.uploadbycode
        ) {
            const isByCode = !!this.markup.uploadbycode;
            const path = this.markup.filename ?? "";
            files.push({
                paths: [path],
                maxSize: this.markup.maxSize,
                upload: !isByCode,
            });
            if (isByCode) {
                this.uploadByCodeFiles.push({path: path, show: !this.noeditor});
            }
        }

        if (this.attrsall.submittedFiles) {
            const uploadByCodeFiles = this.attrsall.submittedFiles.filter(
                (p) => p.source == "uploadByCode"
            );
            this.fileSelect?.loadFiles(...uploadByCodeFiles);
        }

        component.allowMultiple = this.markup.allowMultipleFiles;
        component.accept = this.markup.uploadAcceptPattern;
        component.maxSize = this.markup.uploadAcceptMaxSize;
        component.multipleElements = this.markup.multipleUploadElements;
        component.files = files;
    }

    get usercode(): string {
        return this.editor?.content ?? super.usercode;
    }

    set usercode(str: string) {
        super.usercode = str;
        if (this.editor) {
            this.editor.content = str;
        }
    }

    get userinput() {
        return this.userinput_;
    }

    set userinput(str: string) {
        const tmp = this.userinput_;
        this.userinput_ = str;
        if (tmp != str) {
            this.anyChanged();
            this.updateCanReset();
        }
    }

    get userargs() {
        return this.userargs_;
    }

    set userargs(str: string) {
        const tmp = this.userargs_;
        this.userargs_ = str;
        if (tmp != str) {
            this.anyChanged();
        }
    }

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        public cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer);

        this.errors = [];
        this.result = "";
        this.htmlresult = ""; // '<span class="math display">\\[-\\]</span>';
        this.imgURL = "";
        this.videoURL = "";
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
        this.docLink = "Document";
        this.muokattu = false;
        this.editorIndex = 0;
    }

    onIframeLoad(e: Event) {
        const fr = e.target as HTMLIFrameElement & {contentWindow: WindowProxy};
        // onIframeLoad gets called twice on chrome, on the first time src is empty
        if (fr.src == "") {
            return;
        }

        const channel = new MessageChannel();
        if (this.iframemessageHandler) {
            channel.port1.onmessage = this.iframemessageHandler;
        }

        fr.contentWindow.postMessage(
            {msg: "init", scroll: !!this.markup.iframescroll},
            "*",
            [channel.port2]
        );
        this.iframedefer?.resolve({iframe: fr, channel});
    }

    getContent(): string {
        return this.usercode;
    }

    async save() {
        if (this.preventSave) {
            return {saved: false, message: undefined};
        }
        await this.runCode();
        this.cdr.detectChanges();
        return {saved: !this.edited, message: this.connectionErrorMessage};
    }

    formBehavior(): FormModeOption {
        // if (!this.isText) {
        //    return FormModeOption.NoForm;
        // }
        return getFormBehavior(this.markup.form, FormModeOption.NoForm);
        // return getFormBehavior(this.markup.form, FormModeOption.Undecided);
    }

    setAnswer(content: Record<string, unknown>): ISetAnswerResult {
        this.error = undefined;
        let message;
        let ok = true;
        if (CspluginAnswer.is(content)) {
            // TODO: add support for multiple files
            // TODO: Add support for userArgs/userInput
            this.usercode = content.usercode;
            this.initSaved();
        } else {
            this.usercode = "";
            ok = false;
            message = `Couldn't find related content ("usercode") from ${JSON.stringify(
                content
            )}`;
            this.error = message;
        }
        return {ok: ok, message: message};
    }

    isUnSaved() {
        return this.edited;
    }

    log(s: string) {
        console.log(
            `csPlugin ${
                this.getTaskId()?.docTask().toString() ?? "(no taskId)"
            }, edited: ${this.edited.toString()}, editor: ${
                this.editor ? "true" : "false"
            }, ${s}`
        );
    }

    hasUnSavedInput(): boolean {
        if (this.savedvals == null) {
            return false;
        }
        if (this.editor) {
            const allFiles = this.editor.allFiles;
            if (allFiles.length != this.savedvals.files.length) {
                return true;
            }
            for (let i = 0; i < allFiles.length; ++i) {
                if (allFiles[i].content != this.savedvals.files[i]) {
                    return true;
                }
            }
        }
        return (
            (this.savedvals.args !== this.userargs ||
                this.savedvals.input !== this.userinput ||
                this.savedvals.usercode !== this.usercode ||
                this.isSimcirUnsaved === true) &&
            this.pluginMeta.getTaskId() !== undefined &&
            !this.nosave
        );
    }

    /**
     * Checks whether usercode/args/input differ from previously saved values
     */
    textChanged(): void {
        if (!this.keepErrors) {
            this.runError = undefined;
        }
        const nowUnsaved = this.hasUnSavedInput();
        if (!this.edited && nowUnsaved) {
            this.edited = true;
            if (this.clearSaved) {
                this.savedText = "";
            }
            this.updateListeners(ChangeType.Modified);
        } else if (this.edited && !nowUnsaved) {
            this.edited = false;
            this.updateListeners(ChangeType.Saved);
        }
    }

    languageChange(): void {
        this.languageType = this.selectedLanguage;
        if (this.editor) {
            this.editor.languageMode = this.mode;
        }
        this.rtype = this.languageType;
        this.isTest = this.getIsTest();
        this.isUnitTest = this.getIsUnitTest();
    }

    resetChanges(): void {
        this.usercode = this.savedvals?.usercode ?? "";
        this.userargs = this.savedvals?.args ?? "";
        this.userinput = this.savedvals?.input ?? "";
        this.edited = false;
        if (this.isSimcir) {
            (async () => {
                await this.setCircuitData();
                await this.initSimcirCircuitListener();
            })();
        }
        this.updateListeners(ChangeType.Saved);
        this.cdr.detectChanges();
    }

    get english() {
        return this.markup.lang === "en";
    }

    get isInput() {
        return (
            this.origLanguageType.includes("input") ||
            this.origLanguageType.includes("args")
        );
    }

    get isSimcir() {
        return this.origLanguageType.includes("simcir");
    }

    get isTauno() {
        return !!this.attrsall.isTauno;
    }

    get program() {
        const prg = this.attrsall.program ?? this.markup.program;
        // IF just replace by code, then no need to show full code
        if (prg?.trim() === "REPLACEBYCODE") {
            return "";
        }
        return prg;
    }

    get hideText() {
        return this.english ? "Hide " : "Piilota ";
    }

    get showText() {
        return this.english ? "Show " : "Näytä ";
    }

    get taunoOhjeText() {
        return this.english
            ? 'Copy the code you made by Tauno by pressing the link "copy from Tauno". Then press Run button. Note that the running code may have different code than in Tauno!'
            : 'Kopioi Taunolla tekemäsi koodi "kopioi Taunosta"-linkkiä painamalla. Sitten paina Aja-painiketta. Huomaa, että ajossa voi olla eri taulukko kuin Taunossa!';
    }

    get copyFromTaunoText() {
        return this.english ? "copy from Tauno" : "kopioi Taunosta";
    }

    get copyFromSimCirText() {
        return this.english ? "copy from SimCir" : "kopioi SimCiristä";
    }

    public get copyToSimCirText() {
        return this.english ? "copy to SimCir" : "kopioi SimCiriin";
    }

    get languageText() {
        return this.english ? "language: " : "kieli: ";
    }

    get forcedupload() {
        return this.type === "upload" && !this.markup.button;
    }

    get oldrtype() {
        return languageTypes.getRunType(this.type, "text");
    }

    get isSage() {
        return this.rtype === "sage";
    }

    get isMathCheck() {
        return this.rtype === "mathcheck";
    }

    get nocode() {
        return this.type === "upload" || this.markup.nocode;
    }

    get placeholder() {
        const tiny = this.origLanguageType.includes("tiny");
        return valueDefu(
            this.markup.placeholder,
            tiny
                ? ""
                : this.english
                ? "Write your code here"
                : "Kirjoita koodi tähän:"
        );
    }

    get inputplaceholder() {
        return valueOr(
            this.markup.inputplaceholder,
            this.english ? "Write your input here" : "Kirjoita syöte tähän"
        );
    }

    get isText() {
        const rt = this.rtype;
        return rt === "text" || rt === "xml" || rt === "css" || rt === "md";
    }

    get argsplaceholder() {
        return valueOr(
            this.markup.argsplaceholder,
            this.isText
                ? this.english
                    ? "Write file name here"
                    : "Kirjoita tiedoston nimi tähän"
                : this.english
                ? "Write your program args here"
                : "Kirjoita ohjelman argumentit tähän"
        );
    }

    get argsstem() {
        return valueOr(
            this.markup.argsstem,
            this.isText
                ? this.english
                    ? "File name:"
                    : "Tiedoston nimi:"
                : this.english
                ? "Args:"
                : "Args"
        );
    }

    async getFileFromUrl(url: string) {
        // const response = await fetch(url);
        // const data = await response.blob();
        // const html: string = data;
        // return html;
        const result = await this.httpGet<string>(url);
        if (result.ok) {
            return result.result;
        }
    }

    handleHTML(s: string): string {
        const regex = /<!-- DELETEBEGIN -->(.|\n)*?<!-- DELETEEND -->/gm;
        s = s.replace(regex, "");
        s = s.replace(
            /http:\/\/localhost\/csstatic\//g,
            window.origin + "/csstatic/"
        );
        if (this.markup.fullhtmlparams) {
            for (const [key, value] of Object.entries(
                this.markup.fullhtmlparams
            )) {
                s = s.replace(
                    new RegExp(`/\\*\s*htmlparam-${key}\s*\\*/`, "g"),
                    value
                );
            }
        }
        return s;
    }

    // `await` can only be used in an async body, but showing it here for simplicity.
    // const file = await getFileFromUrl('https://example.com/image.jpg', 'example.jpg');

    fullhtmlCache?: string;

    async getFullhtml(): Promise<string | undefined> {
        if (this.fullhtmlCache) {
            return this.fullhtmlCache;
        }
        let r = this.markup.fullhtml;
        if (!r) {
            r = this.markup.srchtml;
        }
        if ((!r && this.type.includes("html")) || this.isProcessing) {
            return "REPLACEBYCODE";
        }

        if (r) {
            return this.handleHTML(r);
        }

        r = this.markup.fullhtmlurl;
        if (!r) {
            return r;
        }

        if (globalFileUrlCache[r]) {
            return globalFileUrlCache[r];
        }
        const result = await this.httpGetText(r);
        if (result.ok) {
            let html = result.result;
            html = this.handleHTML(html);
            globalFileUrlCache[r] = html;
            this.fullhtmlCache = html;
            return html;
        } else {
            // TODO get the real error
            const html = "Same origin error " + r;
            this.fullhtmlCache = html;
            return html;
        }
    }

    get borders() {
        return this.markup.borders;
    }

    get dragAndDrop() {
        return this.markup.dragAndDrop;
    }

    get rows() {
        return this.markup.rows;
    }

    get maxrows() {
        return this.markup.maxrows;
    }

    get editorMode() {
        return this.markup.editorMode;
    }

    get formulaEditor() {
        return this.markup.formulaEditor;
    }

    /**
     * Changes formula editor visibility and puts
     * focus to main editor if closed and
     * sets cursor position if given.
     * @param cursorIndex position in editor.content to
     * move cursor to
     */
    toggleFormulaEditor(cursorIndex: number = -1) {
        if (this.formulaEditor) {
            this.formulaEditorOpen = !this.formulaEditorOpen;
            if (!this.formulaEditorOpen) {
                // without setTimeout editor focus doesn't work
                setTimeout(() => {
                    if (cursorIndex !== -1) {
                        this.editor?.moveCursorToContentIndex(cursorIndex);
                    }
                    this.editor?.focus();
                }, 0);
            }
        }
    }

    /**
     * Moves cursor inside clicked formula in preview in editor
     * and opens formula editor. Closes editor if it was open.
     * @param event mouse click event
     * @param div root element
     */
    async handleSelectFormulaFromPreview(
        event: MouseEvent,
        div: HTMLDivElement
    ) {
        if (!this.formulaEditor || !this.editor) {
            return;
        }
        // Open different formula
        if (this.formulaEditorOpen) {
            // close formula editor
            if (!this.formulaEditorComponent) {
                return;
            }
            const cancelSuccess =
                await this.formulaEditorComponent.handleFormulaCancel();
            // editing wasn't cancelled so do nothing
            if (!cancelSuccess) {
                return;
            }
            await timeout();
            // move to chosen formula
            const success = selectFormulaFromPreview(event, this.editor, div);
            // open formula editor again
            if (success) {
                this.toggleFormulaEditor();
            }
        } else {
            // open chosen formula
            const success = selectFormulaFromPreview(event, this.editor, div);
            if (success) {
                this.toggleFormulaEditor();
            }
        }
    }

    get count() {
        return this.markup.count;
    }

    get copyLink() {
        return this.markup.copyLink;
    }

    get footer() {
        return this.markup.footer;
    }

    async getfullhtmlext(text: string) {
        const fh = await this.getFullhtml();
        if (!fh) {
            return undefined;
        }
        let fhtml = fh.replace("REPLACEBYCODE", text);
        if (this.isProcessing) {
            fhtml = `
<script src="${location.origin}/cs/static/processing/processing.js"></script>
<script type="text/processing" data-processing-target="mycanvas">
${fhtml}
</script>
<canvas id="mycanvas"></canvas>`;
        }
        return fhtml;
    }

    get isProcessing() {
        return this.type.includes("processing");
    }

    get toggleEditor() {
        return this.markup.toggleEditor || this.isSimcir;
    }

    get toggleEditorText() {
        if (
            typeof this.toggleEditor === "string" &&
            this.toggleEditor.includes("|")
        ) {
            return this.toggleEditor.split("|");
        } else {
            return this.english ? ["Edit", "Hide"] : ["Muokkaa", "Piilota"];
        }
    }

    get minRows() {
        return getInt(this.markup.rows) ?? 0;
    }

    getIsAll() {
        return languageTypes.isAllType(this.origLanguageType);
    }

    get glowscript() {
        return languageTypes.isInArray(this.rtype, ["glowscript", "vpython"]);
    }

    getIsRun() {
        return (
            ((languageTypes.getRunType(this.type, "") !== "" || this.isAll) &&
                !this.markup.norun) ||
            this.type.includes("text") ||
            this.isSimcir ||
            this.markup.justSave ||
            this.markup.button
        ); // or this.buttonText()?
    }

    get testText() {
        return this.markup.testText ?? "Test";
    }

    get runningText() {
        return this.markup.runningText;
    }

    buttonText() {
        const txt = super.buttonText();
        if (txt) {
            return txt;
        }
        if (this.markup.button === null || this.markup.buttonText === null) {
            return null;
        }
        if (
            this.type.includes("text") ||
            this.isSimcir ||
            this.markup.justSave ||
            this.isVars
        ) {
            return this.english ? "Save" : "Tallenna";
        }
        if (this.markup.justCompile) {
            return this.english ? "Compile" : "Käännä";
        }
        return this.english ? "Run" : "Aja";
    }

    get isExternalFetch(): boolean {
        return !(!this.isRun || !this.hasExternalSources);
    }

    externalFetchText() {
        return this.english ? "Fetch" : "Nouda";
    }

    async fetchExternalFiles() {
        if (this.isRunning) {
            return;
        }
        this.isRunning = true;
        this.fetchError = undefined;

        const r = await toPromise(
            this.http.post<IFetchResponse>(
                `/plugin${this.pluginMeta.getTaskIdUrl()}/fetchExternal`,
                {},
                {headers: new HttpHeaders({timeout: `${defaultTimeout}`})}
            )
        );
        if (r.ok) {
            if (r.result.error) {
                this.fetchError = "Failed to fetch files: " + r.result.error;
                this.externalFiles = undefined;
            } else if (r.result.files.length == 0) {
                this.fetchError =
                    "No files were received. Make sure they are in the correct place";
                this.externalFiles = undefined;
            } else {
                this.externalFiles = r.result.files;
                this.updateExternalEditor();
            }
        } else {
            this.fetchError = "Failed to fetch files: " + r.result.error.error;
            this.externalFiles = undefined;
        }
        this.isRunning = false;
        this.cdr.detectChanges();
    }

    getIsTest() {
        return (
            languageTypes.getTestType(
                this.origLanguageType,
                this.selectedLanguage,
                ""
            ) !== ""
        );
    }

    getIsUnitTest() {
        return (
            languageTypes.getUnitTestType(
                this.origLanguageType,
                this.selectedLanguage,
                ""
            ) !== ""
        );
    }

    get isDocument() {
        return this.origLanguageType.includes("doc");
    }

    get showInput() {
        return this.origLanguageType.includes("input");
    }

    get showArgs() {
        return this.origLanguageType.includes("args");
    }

    get uploadstem(): string {
        return valueOr(
            this.markup.uploadstem,
            this.english ? "Upload image/file" : "Lataa kuva/tiedosto"
        );
    }

    get file() {
        return this.markup.file;
    }

    get showCodeOn() {
        return valueDefu(
            this.markup.showCodeOn,
            this.english ? "Show all code" : "Näytä koko koodi"
        );
    }

    get showCodeOff() {
        return valueOr(
            this.markup.showCodeOff,
            this.english ? "Hide extra code" : "Piilota muu koodi"
        );
    }

    get resetText() {
        return valueDefu(
            this.markup.resetText,
            this.english ? "Reset" : "Alusta"
        );
    }

    /**
     * Initialize template buttons.
     */
    createTemplateButtons() {
        const b = this.markup.buttons;
        const mdButtons = this.markup.mdButtons;
        this.templateButtons = createTemplateButtons(b, mdButtons);
        this.templateButtonsCount = this.templateButtons.length;
    }

    get progLanguages() {
        if (this.isAll) {
            const langs = this.markup.languages;
            if (langs) {
                return langs.split(/[\n;, \/]/);
            } else {
                return [...languageTypes.runTypes].sort();
            }
        }
    }

    get mode() {
        return languageTypes.getAceModeType(this.type, this.markup.mode ?? "");
    }

    get nosave() {
        return this.markup.nosave;
    }

    get cols() {
        return this.markup.cols;
    }

    ngOnInit() {
        super.ngOnInit();
        // First find out what language we are using
        const type = this.markup.type.split(/[/,; ]/)[0];

        this.byCode = commentTrim(this.attrsall.by ?? this.markup.byCode ?? "");

        this.languageType = this.markup.type; // user may change
        this.origLanguageType = this.markup.type;
        this.rtype = languageTypes.getPureRunType(type);

        if (this.getIsAll()) {
            this.rtype = "cs";
            this.isAll = true;
        }

        if (this.rtype) {
            this.isRun = !!this.getIsRun();
        } else {
            this.rtype = languageTypes.getTestRunType(type);
            this.isRun = false; // just test, no run type
        }
        if (!this.rtype) {
            this.rtype = "cs";
        }

        // Order: selectedLanguage in current answer, selected language in markup, selected language parsed from type
        this.selectedLanguage = this.rtype;

        if (this.isAll) {
            this.selectedLanguage =
                this.attrsall.selectedLanguage ??
                this.markup.selectedLanguage ??
                this.rtype;
        }
        this.languageChange();
        // Language find out

        this.clearSaved = !!this.attrsall.markup.savedText;

        this.upload =
            this.origLanguageType === "upload" ||
            this.markup.upload ||
            this.markup.uploadbycode;
        if (!this.upload) {
            this.upload = listify(this.markup.files).some(
                (f) => f?.source == "upload" || f?.source == "uploadByCode"
            );
        }

        if (this.markup.files) {
            this.hasExternalSources = listify(this.markup.files).some(
                (f) => !["upload", "uploadByCode", "editor"].includes(f.source)
            );
            if (this.attrsall.submittedFiles) {
                this.externalFiles = this.attrsall.submittedFiles.filter(
                    (f) =>
                        !["upload", "uploadByCode", "editor"].includes(f.source)
                );
            }
        }

        this.hide = this.attrsall.markup.hide ?? {};
        //  if ( typeof this.markup.borders !== 'undefined' ) this.markup.borders = true;
        this.createTemplateButtons();
        const isText = this.isText;
        const isArgs = this.type.includes("args");
        if (this.attrsall.markup.docurl) {
            this.docURL = this.domSanitizer.bypassSecurityTrustResourceUrl(
                this.attrsall.markup.docurl
            );
            this.docLink = "Hide document";
        }

        const taskId = this.pluginMeta.getTaskId();
        if (this.upload && !this.markup.uploadbycode && taskId?.docId) {
            this.uploadUrl = `/pluginUpload/${taskId.docId}/${taskId.name}/`;
        }

        this.timeout = valueOr(this.attrsall.timeout, 0) * 1000;
        this.originalUserInput = (this.markup.userinput ?? "").toString();
        this.userinput = valueOr(
            this.attrsall.userinput,
            this.originalUserInput
        );
        this.userargs = valueOr(
            this.attrsall.userargs,
            (
                this.markup.userargs ??
                (isText && isArgs ? this.markup.filename ?? "" : "")
            ).toString()
        );
        if (this.editor) {
            this.editor.languageMode = this.mode;
        }

        this.noeditor = valueOr(
            this.markup.noeditor,
            this.isSimcir || this.type === "upload"
        );

        const wn = this.markup.wrap ?? (isText ? 70 : -1);
        this.wrap = {n: wn == -1 ? -1 : Math.abs(wn), auto: wn > 0};

        this.viewCode = this.markup.viewCode;

        const editorText = [
            valueDefu(
                this.markup.normal,
                this.english ? "Normal" : "Tavallinen"
            ),
            valueDefu(this.markup.highlight, "Highlight"),
            this.parsons?.menuText ?? "Parsons",
            this.markup.jsparsons,
        ];
        for (const c of this.markup.editorModes.toString()) {
            const mode = parseInt(c, 10);
            this.editorModes.push(new Mode(mode, editorText[mode]));
        }

        if (
            this.markup.editorMode != -1 &&
            this.editorModes.findIndex((m) => m.id == this.markup.editorMode) ==
                -1
        ) {
            this.editorModes.push(
                new Mode(
                    this.markup.editorMode,
                    editorText[this.markup.editorMode]
                )
            );
        }

        if (this.indent < 0) {
            if (this.file) {
                this.indent = 8;
            } else {
                this.indent = 0;
            }
        }

        this.processPluginMath();
        if (this.attrsall.uploadedFiles) {
            this.uploadedFiles.push(...this.attrsall.uploadedFiles);
        } else if (this.attrsall.uploadedFile || this.attrsall.uploadedType) {
            this.uploadedFiles.push({
                path: this.attrsall.uploadedFile ?? "",
                type: this.attrsall.uploadedType ?? "",
            });
        }

        if (!this.usercode) {
            this.usercode = this.attrsall.usercode ?? this.byCode ?? "";
        }
        this.initSaved();
        this.isViz = this.type.startsWith("viz");
        this.isVars = this.type.startsWith("vars");
        if (!this.attrsall.preview) {
            this.vctrl.addTimComponent(this);
        }
        this.height = this.markup.height;
        if (this.markup.width) {
            this.csRunDivStyle = {width: this.markup.width.toString()};
        }
        this.jsparams = this.markup.jsparams;
        // if (this.isText) {
        //     this.preventSave = true;
        // }
        this.fullCode = this.getCode();
        this.parsons = this.markup.parsons ?? {};
        // TODO: showCodeNow() is required for viewCode: true to work.
        //  Otherwise precode and postcode won't show up until user clicks hide + show code.
        //  It's unclear if getCode should handle this already.
        this.showCodeNow();
        this.updateRunChanged();

        this.getInvalidMarkerState().then((r) => {
            if (r) {
                this.invalidMarker = r;
            }
        });
    }

    ngOnDestroy() {
        if (!this.attrsall.preview) {
            this.vctrl.removeTimComponent(this);
        }
    }

    async ngAfterViewInit() {
        if (this.markup.editorOrder) {
            const style = this.element[0].style;
            const classOrder: Record<string, number> = {
                csHeader: -9999,
            };
            for (let i = 0; i < this.markup.editorOrder.length; i++) {
                const key = this.markup.editorOrder[i];
                style.setProperty(`--csplugin-${key}`, i.toString());
                classOrder[key] = i;
            }

            // TODO: Instead of forcing reorder, maybe it would be better to create something that uses
            //  Angular templates to conditionally render elements in desired order. This solution is temporary.
            if (this.markup.editorOrderForceAccessible) {
                const runDiv = this.element.find(".csRunDiv");
                const els = runDiv.children().toArray();
                els.sort((a, b) => {
                    const ca = classOrder[a.className] ?? 9999;
                    const cb = classOrder[b.className] ?? 9999;
                    return ca - cb;
                });
                for (const e of els) {
                    runDiv[0].appendChild(e);
                }
            }
        }

        this.preview = this.element.find(".csrunPreview");
        const styleArgs = this.markup["style-args"];
        if (styleArgs) {
            const argsEdit =
                this.getRootElement().getElementsByClassName("csArgsArea");
            if (argsEdit.length > 0) {
                argsEdit[0].setAttribute("style", styleArgs);
            }
        }
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

        if (this.markup.open) {
            if (this.isTauno) {
                await this.showTauno();
            }
            if (this.isSimcir) {
                await this.showSimcir();
            }
        }
        if (this.markup.autorun) {
            this.runCodeLink(true);
        }

        // TODO: Snippets should be processed via Dumbo
        ParCompiler.processAllMath(this.element.find(".csRunSnippets"));
    }

    uploadedFileName(url: string) {
        return url.split("/").slice(6).join("/");
    }

    initSaved(clear = false) {
        if (!this.savedvals || (this.savedvals && !clear)) {
            this.savedvals = {
                files: this.editor?.files.map((f) => f.content) ?? [
                    this.usercode,
                ],
                args: this.userargs,
                input: this.userinput,

                // NOTE: "type: text/tiny" needs this because there is no editor in that case.
                usercode: this.usercode,
            };
        }
        this.edited = false;
        this.isSimcirUnsaved = false;
        this.updateListeners(ChangeType.Saved);
    }

    onContentChange(str: string) {
        super.usercode = str;
        this.checkByCodeRemove();
        if (!this.copyingFromTauno && str !== this.byCode) {
            this.muokattu = true;
        }
        this.copyingFromTauno = false;
        if (this.viewCode) {
            this.showCodeNow();
        }
        this.countBoard?.count(str);
        if (this.isText) {
            this.preventSave = this.countBoard?.preventSave ?? false;
        }
        if (this.isText || this.clearSaved) {
            this.savedText = "";
        }

        this.anyChanged();
        this.cdr.detectChanges();
        this.updateRunChanged();
        if (this.editor?.parsonsEditor) {
            this.autosave();
        }
    }

    anyChanged() {
        this.textChanged();
        this.fullCode = this.getCode();
        if (this.hasBeenRun && this.markup.autoupdate) {
            if (this.autoupdateHandle) {
                window.clearTimeout(this.autoupdateHandle);
            }
            this.autoupdateHandle = window.setTimeout(() => {
                this.autoupdateHandle = undefined;
                this.runCodeAuto();
            }, this.markup.autoupdate);
        }
    }

    createUploadByCodeEditorFile(
        path: string,
        content?: string | null
    ): EditorFile | undefined {
        if (this.markup.files) {
            const markupfile = listify(this.markup.files).find(
                (f) =>
                    f.source == "uploadByCode" &&
                    (f as IUploadByCodeMarkup).path == path
            ) as IUploadByCodeMarkup | undefined;
            if (markupfile) {
                const defaultMode =
                    this.markup.mode ?? languageTypes.getAceModeType(this.type);
                const f = new EditorFile(
                    markupfile.path,
                    content ?? undefined,
                    markupfile.mode ?? defaultMode,
                    markupfile.canClose,
                    markupfile.canRename,
                    markupfile.canModify
                );
                f.source = markupfile.source;
                return f;
            }
        }
        return undefined;
    }

    onFileLoad(file: IFile) {
        if (this.markup.files) {
            let bycodefile = this.uploadByCodeFiles.find(
                (f) => f.path == file.path
            );
            if (
                !bycodefile &&
                (this.uploadByCodeFiles.length == 1 || this.markup.uploadbycode)
            ) {
                bycodefile = this.uploadByCodeFiles[0];
            }

            if (bycodefile) {
                if (bycodefile.show && this.editor) {
                    if (this.editor.findFile(file.path) == -1) {
                        const f = this.createUploadByCodeEditorFile(
                            bycodefile.path,
                            file.content
                        );
                        if (f) {
                            this.editor.addFile(f);
                        }
                    }
                    this.editor.setFileContent(bycodefile.path, file.content);
                    this.editor.activeFile = file.path;
                }
            }
        } else {
            this.usercode = file.content;
        }
        if (this.markup.uploadautosave) {
            this.runCode();
        }
    }

    onFileClose(data: {file: EditorFile; index: number}) {
        if (data.file.source != "uploadByCode") {
            return;
        }
        this.fileSelect?.removeFile(data.file.path);
    }

    onUploadResponse(resp: unknown) {
        if (!resp) {
            return;
        }

        const resps = resp as [IUploadResponse];
        if (!this.markup.files) {
            this.uploadedFiles.clear();
        }
        for (const response of resps) {
            this.uploadedFiles.push({path: response.file, type: response.type});
        }

        // Add reference to image to markdown
        if (this.formulaEditor) {
            for (const response of resps) {
                // ask user to type in caption text for image
                showInputDialog<string>({
                    isInput: InputDialogKind.InputAndValidator,
                    defaultValue: "Image 1",
                    validator: (s) => {
                        return new Promise((resolve) => {
                            resolve({ok: true, result: s});
                        });
                    },
                    text: "Enter image caption",
                    title: "Caption",
                    okValue: "",
                })
                    .then((caption) => {
                        // write image tag to editor
                        const url = response.file;
                        const markdownImageTag = `![${caption}](${url})`;
                        this.editor?.insert(markdownImageTag);
                        this.editor?.focus();
                    })
                    .catch((_) => {}); // nothing to catch
            }
        }
    }

    onUploadDone(success: boolean) {
        if (
            success &&
            (this.markup.uploadautosave ||
                (this.origLanguageType.includes("upload") &&
                    !this.markup.button) ||
                !(this.isRun && this.buttonText()))
        ) {
            this.doRunCode("upload", false);
        }
    }

    async processPluginMath() {
        if (this.isMathCheck) {
            await timeout();
            await ParCompiler.processMathJaxAsciiMath(this.element[0]);
        } else if (this.type === "maxima") {
            await timeout();
            await ParCompiler.processMathJaxTeX(this.element[0]);
        }
    }

    runCodeIfCR(event: KeyboardEvent) {
        if (event.keyCode === 13) {
            this.runCode();
        }
    }

    get mdSaveButton() {
        if (!this.markup.targetCanvas) {
            return undefined;
        }
        const ty = languageTypes.getRunType(this.selectedLanguage, "cs");
        if (ty !== "md") {
            return undefined;
        }
        return $localize`Copy image to task ${this.markup.targetCanvas}`;
    }

    async runCodeCommon(nosave: boolean, _extraMarkUp?: IExtraMarkup) {
        this.hasBeenRun = true;
        const ty = languageTypes.getRunType(this.selectedLanguage, "cs");
        if (ty === "md") {
            await this.showMD();
            if (nosave || this.nosave) {
                return;
            }
        }
        if (languageTypes.isInArray(ty, csJSTypes)) {
            // this.jstype = ty;
            await this.showJS();
            if (nosave || this.nosave) {
                return;
            }
        }
        if (this.preventSave) {
            return;
        }
        await this.doRunCode(ty, nosave || this.nosave);
    }

    runCodeAuto() {
        this.runCodeCommon(true);
    }

    runCodeLink(nosave: boolean) {
        this.runCodeCommon(nosave || this.nosave);
    }

    async runCode() {
        await this.runCodeCommon(false);
    }

    runTest() {
        const ty = languageTypes.getTestType(
            this.origLanguageType,
            this.selectedLanguage,
            "comtest"
        );
        this.doRunCode(ty, false);
    }

    runUnitTest() {
        const ty = languageTypes.getUnitTestType(
            this.origLanguageType,
            this.selectedLanguage,
            "junit"
        );
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
        this.docURL = undefined;
        this.docLink = "Document";
    }

    closeError() {
        this.runError = false;
    }

    hideShowEditor() {
        this.noeditor = !this.noeditor;
    }

    async doRunCode(
        runType: string,
        nosave: boolean,
        extraMarkUp?: IExtraMarkup
    ) {
        this.connectionErrorMessage = undefined;
        this.error = undefined;
        if (this.isRunning) {
            return;
        } // do not run if previuos is still running
        let noErrorClear = false;
        this.closeDocument();
        if (this.isSage) {
            await this.initSage(true);
            if (this.sageButton) {
                this.sageButton.click();
            }
        }

        if (this.simcir) {
            this.usercode = await this.getSimcirDataString(this.simcir);
        } else if (this.taunoFrame && (!this.muokattu || !this.usercode)) {
            await this.copyTauno();
        }

        if (this.editor?.parsonsEditor) {
            this.editor.parsonsEditor.check();
        }

        this.checkIndent();
        if (!this.markup.autoupdate) {
            this.tinyErrorStyle = {};
        }
        this.isRunning = true;
        this.imgURL = "";
        this.videoURL = "";
        this.wavURL = "";
        this.runSuccess = false;
        if (
            !(
                languageTypes.isInArray(runType, csJSTypes) ||
                this.markup.noConsoleClear
            )
        ) {
            this.result = "";
        }
        this.runTestGreen = false;
        this.runTestRed = false;
        this.oneruntime = "";
        let isInput = false;
        if (this.type.includes("input")) {
            isInput = true;
        }

        const validityCheck = (ucode2: string) => {
            let msg2 = "";
            if (this.markup.validityCheck) {
                const re = new RegExp(this.markup.validityCheck);
                if (!ucode2.match(re)) {
                    this.tinyErrorStyle = {color: "red"};
                    msg2 = this.markup.validityCheckMessage;
                    if (!msg2) {
                        msg2 = "Did not match to " + this.markup.validityCheck;
                    }
                    this.error = msg2;
                    this.isRunning = false;
                    this.runError = true;
                    if (!this.markup.validityCheckForceSave) {
                        return msg2;
                    }
                    noErrorClear = true;
                }
            }
            return msg2;
        };

        const editorFiles: IFileSubmission[] =
            this.editor?.allFiles
                .filter((f) => f.source != "uploadByCode")
                .map((f) => ({...f, source: "editor"})) ?? [];
        const fileSelectFiles: IFileSubmission[] =
            this.fileSelect?.loadedFiles
                .toArray()
                .filter((f) =>
                    this.uploadByCodeFiles.find((f2) => f2.path == f.path)
                )
                .map((f) => ({source: "uploadByCode", ...f})) ?? [];
        const uploadedFiles: IFileSubmission[] = this.uploadedFiles
            .toArray()
            .map((f) => ({
                source: "upload:" + f.path,
                path: this.uploadedFileName(f.path),
                type: f.type,
            }));
        const externalFiles = this.externalFiles ?? [];

        let allFiles: IFileSubmission[] = editorFiles
            .concat(fileSelectFiles)
            .concat(externalFiles);
        if (allFiles.length == 0) {
            if (!this.markup.files) {
                allFiles = [
                    {source: "editor", path: "", content: this.usercode},
                ];
            } else {
                this.error = "No files to submit";
                this.runError = this.error;
                this.isRunning = false;
                return;
            }
        }

        let msg = "";
        for (const file of allFiles) {
            const m = validityCheck((file.content ?? "").replace(/\r/g, ""));
            if (m) {
                msg += m + "\n";
            }
        }
        if (msg) {
            this.error = msg;
            if (!this.markup.validityCheckForceSave) {
                return;
            }
        }
        allFiles.push(...uploadedFiles);

        let ucode = "";
        if (this.usercode) {
            ucode = this.usercode.replace(/\r/g, "");
            msg = validityCheck(ucode);
            if (msg) {
                this.error = msg;
                if (!this.markup.validityCheckForceSave) {
                    return;
                }
            }
        }

        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            this.runError = this.error;
            this.isRunning = false;
            return;
        }

        this.languageResponse(null);

        const params: IRunRequest = {
            input: {
                usercode: ucode,
                userinput: this.userinput || "",
                isInput: isInput,
                userargs: this.userargs || "",
                uploadedFiles: this.uploadedFiles.toArray(),
                nosave: nosave || this.nosave,
                type: runType,
                ...extraMarkUp,
                ...(this.isAll
                    ? {selectedLanguage: this.selectedLanguage}
                    : {}),
            },
        };

        if (this.markup.files) {
            // TODO: add byCode replacing support to multifile submissions so that this if isn't needed.
            // For now, only include allFiles when no byCode replacing is needed
            if (!this.file && !this.program) {
                params.input.submittedFiles = allFiles;
            }
        }

        const t0run = performance.now();
        const r = await this.postAnswer<IRunResponse>(
            params,
            new HttpHeaders({timeout: `${this.timeout + defaultTimeout}`})
        );
        if (r.ok) {
            this.isRunning = false;

            this.initSaved();
            const data = r.result;
            const tsruntime = ((performance.now() - t0run) / 1000).toFixed(3);
            const runtime = (data.web.runtime ?? "").trim();
            this.oneruntime = "" + tsruntime + " " + runtime.split(" ", 1)[0];
            this.runtime = "\nWhole: " + tsruntime + "\ncsPlugin: " + runtime;
            if (
                (this.isText || this.attrsall.markup.savedText) &&
                (data.savedNew || this.attrsall.markup.showAlwaysSavedText)
            ) {
                // let savedText = "saved";
                // this.savedText = data.web.error ?? "saved";
                this.savedText = this.attrsall.markup.savedText ?? "Saved";
                // this.preventSave = true;
                if (data.web.error === this.savedText) {
                    data.web.error = "";
                }
            }
            if (data.web.error === this.savedText) {
                // } && data.web.console) {
                data.web.error = "";
            }
            if (data.web.pwd) {
                ConsolePWD.setPWD(data.web.pwd, this);
            }
            if (!noErrorClear) {
                this.error = data.web.error;
            }
            if (data.web.parsons_correct || data.web.parsons_styles) {
                if (this.editor?.parsonsEditor) {
                    this.editor.parsonsEditor.checkHost(
                        data.web.parsons_correct,
                        data.web.parsons_styles
                    );
                }
            }
            this.runSuccess = true;

            this.runError = this.error; // TODO: TÄMÄ AIHEUTTAA TEKSTIN PALAUTTAMISEN

            const imgURL = data.web.image;
            const videoURL = data.web.video;
            // if ( !imgURL ) imgURL = data.web["-replyImage"];
            this.imgURL = data.web["-replyImage"] ?? "";
            this.htmlresult =
                (data.web.md ?? "") +
                (data.web["-replyHTML"] ?? "") +
                (data.web["-replyMD"] ?? "");
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

            const err = data.web.console ?? "";
            if (docURL) {
                this.docURL =
                    this.domSanitizer.bypassSecurityTrustResourceUrl(docURL);
                this.docLink = "Hide document";
                this.error = err.trim();
            }

            if (wavURL) {
                // <video src="https://tim.jyu.fi/csgenerated/vesal/sinewave.wav" type="video/mp4" controls="" autoplay="true" ></video>
                this.wavURL = wavURL;
                this.result = err.trim();
            }

            if (videoURL) {
                this.videoURL = videoURL;
            }

            if (data.web.iframes) {
                this.showIframes(data.web.iframes);
            }

            if (imgURL) {
                this.imgURL = imgURL + this.imgURL; // TODO: What's the point to catanate ULR's?
                this.result = err.trim();
            } else {
                if (this.runSuccess) {
                    if (this.markup.isHtml) {
                        this.htmlresult = removeXML(err) + this.htmlresult;
                    } else if (
                        !languageTypes.isInArray(runType, csJSTypes) ||
                        (err && !data.web.error)
                    ) {
                        this.result = err;
                    } else {
                        this.error = data.web.error;
                    }
                }
            }
            this.languageResponse(data.web.language);

            this.processPluginMath();
        } else {
            this.isRunning = false;
            const data = r.result.error;
            if (data?.error) {
                this.error = data.error;
                this.errors.push(data.error);
            }
            this.connectionErrorMessage =
                this.error ??
                this.markup.connectionErrorMessage ??
                defaultErrorMessage;
        }
    }

    languageResponse(data: unknown) {}

    hideTauno() {
        this.taunoFrame = undefined;
        this.iframesettings = undefined;
    }

    hideSimcir() {
        this.simcir?.children().remove();
        this.simcir = undefined;
    }

    get simcirOn() {
        return this.simcir != undefined;
    }

    async copyTauno() {
        this.taunoCopy = new TimDefer<string>();
        this.taunoFrame!.channel.port1.postMessage({msg: "getData"});
        let s = await this.taunoCopy.promise;
        this.copyingFromTauno = true;
        const treplace = this.markup.treplace ?? "";
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

    async addText(item: ITemplateButton) {
        let s = item.data;
        if (this.noeditor) {
            this.userargs += s + " ";
            return;
        }
        let ip = 0;
        while (s.includes("\\?")) {
            let param: ITemplateParam = {
                default: "",
                text: "Value",
                pattern: ".*",
                error: "",
            };
            if (item.placeholders && ip < item.placeholders.length) {
                param = item.placeholders[ip];
            }
            s = await showTemplateReplaceDialog(s, param);
            if (!s) {
                return;
            }
            ip++;
        }
        let text = s.replace(/\\n/g, "\n");
        // Don't replace in latex as commands like \ne wouldn't work
        if (this.formulaEditor) {
            text = s;
        }
        // write the text to formulaeditor if its enabled and open
        if (this.formulaEditor && this.formulaEditorOpen) {
            this.currentSymbol = {text: text};
        } else {
            this.editor?.insert?.(text);
            this.editor?.focus();
        }
    }

    // Returns the visible index for next item and the desired size
    getVid(dw?: number, dh?: number): Vid {
        taunoNr++;
        const vid = "tauno" + taunoNr;
        if (!dw) {
            dw = 800;
        }
        if (!dh) {
            dh = 500;
        }
        return {
            vid: vid,
            width: this.markup.width ? getInt(this.markup.width) ?? dw : dw,
            height: this.markup.height ? getInt(this.markup.height) ?? dh : dh,
        };
    }

    async setCircuitData() {
        if (!this.simcir) {
            console.warn("setCircuitData: simcir not loaded");
            return;
        }
        let data: {width: number; height: number} = {width: 0, height: 0};
        this.runError = false;
        try {
            if (this.usercode) {
                data = JSON.parse(this.usercode);
            }
        } catch (err) {
            this.error = (err as Error).message;
            this.runError = true;
        }
        try {
            const initstr = this.markup.initSimcir;
            if (initstr) {
                const initdata = JSON.parse(initstr);
                data = {...data, ...initdata};
            }
        } catch (err) {
            this.error = (err as Error).message;
            this.runError = true;
        }

        // width and height are passed to svg viewBox attribute that needs numbers
        data.width = numOrDef(this.markup.width, 800);
        data.height = numOrDef(this.markup.height, 400);
        this.simcir.children().remove();
        const simcir = await loadSimcir();
        simcir.setupSimcir(this.simcir, data);

        const saved = this.getSavedSimcirCircuit();
        const original = this.getOriginalSimcirCircuit();
        this.canResetSimcir = original && !deepEqual(saved, original);
        this.updateCanReset();
    }

    async getSimcirData(simcirElem: JQuery): Promise<ICsSimcirData> {
        const simcir = await loadSimcir();
        const d = simcir.controller(simcirElem.find(".simcir-workspace"));
        const data = d.data();
        return {
            devices: data.devices as SimcirDeviceInstance[],
            connectors: data.connectors as SimcirConnectorDef[],
        };
    }

    async getSimcirDataString(simcirElem: JQuery) {
        return JSON.stringify(await this.getSimcirData(simcirElem));
    }

    copyToSimcir() {
        this.setCircuitData();
    }

    async copyFromSimcir() {
        if (this.simcir) {
            this.usercode = JSON.stringify(
                await this.getSimcirData(this.simcir),
                undefined,
                4
            );
        }
    }

    private cleanSimcirData(data: ICsSimcirData): ICsSimcirData {
        data.devices.forEach((d) => delete d.state);
        return data;
    }

    private getOriginalSimcirCircuit() {
        let originalCircuit: ICsSimcirData | undefined;
        try {
            if (this.byCode) {
                originalCircuit = this.cleanSimcirData(
                    JSON.parse(this.byCode) as ICsSimcirData
                );
            }
        } catch (e) {
            // Ignore errors
        }

        return originalCircuit;
    }

    private getSavedSimcirCircuit() {
        return this.cleanSimcirData(JSON.parse(this.usercode) as ICsSimcirData);
    }

    private async getCurrentSimcirCircuit() {
        if (!this.simcir) {
            return undefined;
        }
        return this.cleanSimcirData(await this.getSimcirData(this.simcir));
    }

    private initSimcirCircuitListener() {
        if (!this.simcir) {
            return;
        }

        const originalCircuit = this.getOriginalSimcirCircuit();

        this.simcir.find(".simcir-workspace").on("mouseup", async () => {
            // Simcir's own mouseup hasn't happened yet - timeout hack is for that.
            await timeout();

            const saved = this.getSavedSimcirCircuit();
            const current = (await this.getCurrentSimcirCircuit())!;

            this.isSimcirUnsaved = !deepEqual(saved, current);
            this.canResetSimcir =
                originalCircuit && !deepEqual(originalCircuit, current);
            this.anyChanged();
            this.updateCanReset();
        });
    }

    async showSimcir() {
        const v = this.getVid();
        this.simcirElem = this.element.find(".simcirContainer")[0];
        this.simcirElem.textContent = "";
        const div = document.createElement("div");
        div.id = v.vid;
        this.simcirElem.appendChild(div);
        this.simcir = $(this.simcirElem).children().first();
        await this.setCircuitData();
        await this.initSimcirCircuitListener();
        return true;
    }

    async showTauno() {
        const v = this.getVid();
        let p = "";
        let tt = "/cs/tauno/index.html?lang=" + this.markup.lang + "&";
        if (this.markup.taunotype && this.markup.taunotype === "ptauno") {
            tt = "/cs/tauno/index.html?lang=" + this.markup.lang + "&s&";
        }
        let taunoUrl = tt; // +"?"; // t=1,2,3,4,5,6&ma=4&mb=5&ialku=0&iloppu=5";
        const s = this.markup.table;
        if (s && s.length > 0) {
            if (s.startsWith("s")) {
                p = "ts=" + s.substring(1) + "&";
            } else {
                p = "t=" + s.trim() + "&";
            } // table by it's items
        }

        p += doVariables(this.markup.variables, "m");
        p += doVariables(this.markup.indices, "i");

        taunoUrl = taunoUrl + p;
        this.iframesettings = {
            id: v.vid,
            width: v.width,
            height: v.height,
            src: this.domSanitizer.bypassSecurityTrustResourceUrl(taunoUrl),
        };
        this.taunoFrame = await this.waitIframeLoad((e) => {
            if (e.data.data && this.taunoCopy) {
                this.taunoCopy.resolve(e.data.data);
            }
        });
    }

    get taunoOn() {
        return this.taunoFrame != undefined;
    }

    updateCanReset() {
        this.canReset = !!(
            (this.editor?.modified ?? false) ||
            this.isSage ||
            this.canResetSimcir ||
            (this.markup.resetUserInput &&
                this.userinput !== this.originalUserInput)
        );
        return this.canReset;
    }

    async initCode(clear = false) {
        this.muokattu = false;
        this.imgURL = "";
        this.videoURL = "";
        this.runSuccess = false;
        this.runError = false;
        this.result = "";
        this.viewCode = this.markup.viewCode;
        if (this.editor) {
            this.editor?.reset();
        } else {
            this.usercode = this.byCode;
        }
        if (this.markup.resetUserInput) {
            this.userinput_ = this.originalUserInput ?? "";
        }
        if (this.isSage) {
            await this.initSage(false);
        }
        if (this.simcir) {
            await this.setCircuitData();
            await this.initSimcirCircuitListener();
        }
        this.initSaved(clear);
        this.updateCanReset();
    }

    async initSage(firstTime: boolean) {
        // TODO: lisää kentätkin vasta kun 1. kerran alustetaan.
        // TODO: kielien valinnan tallentaminen
        // TODO: kielien valinta kunnolla float.
        // ks: https://github.com/sagemath/sagecell/blob/master/doc/embedding.rst
        const sagecell = (await import("./embedded_sagecell")).default;
        sagecell.mathRender = async (e, cb) => {
            await ParCompiler.processMathJaxTeX(e);
            if (cb) {
                cb();
            }
        };
        if (this.sagecellInfo) {
            this.sagecellInfo.editor = "textarea";
            // cs.sagecellInfo.inputLocation = null;
            // cs.sagecellInfo.outputLocation = null;
            // sagecell.deleteSagecell(cs.sagecellInfo);
            // cs.sagecellInfo = null;
        }
        let languages = sagecell.allLanguages;
        /*
        const types = this.type.split("/");
        if (types.length > 1) {
            languages = types.slice(1);
        }
        */
        if (this.attrsall.markup.languages) {
            languages = this.attrsall.markup.languages.split("/");
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
        this.sageArea =
            this.getRootElement().getElementsByClassName("computeSage")[0];
        this.editArea =
            this.getRootElement().getElementsByClassName("csEditArea")[0];
        this.sageOutput =
            this.getRootElement().getElementsByClassName("outputSage")[0];

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
            autoeval: this.markup.autorun || firstTime,
            callback: () => {
                this.sageButton = this.sageArea!.getElementsByClassName(
                    "sagecell_evalButton"
                )[0] as HTMLElement;
                this.sageInput = this.sageArea!.getElementsByClassName(
                    "sagecell_commands"
                )[0] as HTMLInputElement;

                this.sageButton.onclick = () => {
                    // cs.checkSageSave();
                    this.sagecellInfo!.code = this.getReplacedCode();
                    // cs.sagecellInfo.session.code = cs.sagecellInfo.code;
                };
                const sagecellOptions =
                    this.getRootElement().getElementsByClassName(
                        "sagecell_options"
                    )[0] as HTMLElement;
                const csRunMenuArea =
                    this.getRootElement().getElementsByClassName(
                        "csRunMenuArea"
                    )[0];
                if (csRunMenuArea && sagecellOptions) {
                    csRunMenuArea.appendChild(sagecellOptions);
                }
                sagecellOptions.style.marginTop = "-2em";
            },
            languages: languages, // sagecell.allLanguages
        });
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

    getSameIndent(s: string, beg: number): string {
        let n = 0;
        let b = beg;
        for (let i = b; i < s.length; i++) {
            const c = s[i];
            if (c == " ") {
                n++;
            } else if (c == "\n") {
                b = i + 1;
                n = 0;
            } else {
                break;
            }
        }
        return s.substr(b, n);
    }

    findLastNonEmpty(s: string): number {
        let i = s.length - 1;
        let foundChars = false;

        for (; i >= 0; i--) {
            const c = s[i];
            if (c == "\n") {
                if (foundChars) {
                    i++;
                    break;
                }
            } else if (c != " ") {
                foundChars = true;
            }
        }
        return i;
    }

    copyString(s: string | undefined, event: UIEvent) {
        copyToClipboard(s ?? "");
        event?.preventDefault();
    }

    elementSelectAll(event: KeyboardEvent) {
        if (event.ctrlKey && event.key === "a") {
            const element = event.target as HTMLElement;
            if (!element) {
                return;
            }
            const range = document.createRange();
            range.selectNodeContents(element);

            // Luo Selection-objekti ja aseta siihen aiemmin luotu Range-objekti
            const selection = window.getSelection();
            if (!selection) {
                return;
            }
            selection.removeAllRanges();
            selection.addRange(range);
            event.preventDefault();
        }
    }

    copyCode() {
        let pre = "";
        let post = "";
        let extra = false;
        if (this.viewCode && this.precode) {
            // TODO: get if not present?
            pre = this.precode + "\n";
            extra = true;
        }

        if (this.viewCode && this.postcode) {
            // TODO: get if not present?
            post = this.postcode + "\n";
            extra = true;
        }

        const usercode = this.usercode;

        // TODO: begin and end texts as a parameter and then indent picked there
        let ind = "";
        if (extra) {
            ind = this.getSameIndent(this.usercode, 0);
            const c = languageTypes.getCommentMarkers(this.rtype);
            pre += ind + c[0] + "BYCODEBEGIN" + c[1] + "\n"; // TODO: ask comment string from language
            const i = this.findLastNonEmpty(usercode);
            if (i > 0) {
                ind = this.getSameIndent(this.usercode, i);
            }
            post = "\n" + ind + c[0] + "BYCODEEND" + c[1] + "\n" + post; // TODO: ask comment string from language
        }
        const s = pre + this.usercode + post;
        copyToClipboard(s);
    }

    checkByCodeRemove() {
        // TODO: begin and end texts as a parameter and then indent picked there
        if (this.nocode || !(this.file || this.program)) {
            return;
        }
        const BEGINCODE = "BYCODEBEGIN";
        const ENDCODE = "BYCODEEND";
        const BEGINCODE2 = "--- Write your code below this line. ---";
        const ENDCODE2 = "--- Write your code above this line. ---";
        let code = this.usercode;
        let i = code.indexOf(BEGINCODE);
        if (i < 0) {
            i = code.indexOf(BEGINCODE2);
        }
        if (i >= 0) {
            const endl = code.indexOf("\n", i);
            if (endl < 0) {
                return;
            } // NO user code
            code = code.substr(endl + 1);
        }
        i = code.indexOf(ENDCODE);
        if (i < 0) {
            i = code.indexOf(ENDCODE2);
        }
        if (i >= 0) {
            let endl = code.lastIndexOf("\n", i);
            if (endl > 0 && code[endl - 1] == "\r") {
                endl--;
            } // if there are linefeeds like cr lf
            if (endl >= 0) {
                code = code.substr(0, endl);
            }
        }
        if (code.length == this.usercode.length) {
            return;
        }
        this.usercode = code;
    }

    checkIndent() {
        if (!this.indent || !this.usercode) {
            return;
        }
        let spaces = "";
        for (let j1 = 0; j1 < this.indent; j1++) {
            spaces += " ";
        }
        let n = 0;
        const st = this.usercode.split("\n");
        for (let i = 0; i < st.length; ++i) {
            let s = st[i];
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
        return this.markup.replace ?? this.attrsall.replace;
    }

    maybeReplace(st: string[]): [string, string, string] {
        let r = "";
        const rp = ["", ""]; // alkuosa, loppuosa
        let step = 0;
        let nl = "";
        let nls = "";
        const needReplace = !!this.replace;
        const regexp = new RegExp(this.replace ?? "");
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
        const r = await to(
            $http<{msg: string; error: string} | string>({
                method: "POST",
                url: "/cs/",
                params: {
                    print: 1,
                    replace: "",
                },
                data: params,
            })
        );
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

    async exportMDAsImg() {
        if (!this.mdHtmlDiv) {
            return;
        }
        this.runError = false;
        if (!this.markup.targetCanvas) {
            this.runError = true;
            this.error = $localize`No target set`;
            return;
        }
        const rc = this.vctrl.getReviewCanvas(this.markup.targetCanvas);
        if (!rc) {
            this.runError = true;
            this.error = $localize`Couldn't find target ${this.markup.targetCanvas}`;
            return;
        }

        this.exportingMD = true;
        await timeout(); // Let loading icon appear

        const el = this.mdHtmlDiv.nativeElement;
        const canvas = await html2canvas(el, {
            ignoreElements: (e) => {
                const $e = $(e);

                // Allow head element
                if ($e.closest("head").length > 0) {
                    return false;
                }

                // Allow elements that contain the .csMDHTML class
                if ($e.hasClass("csMDHTML")) {
                    return false;
                }

                // Allow if element contains a .csMDHTML element
                if ($e.find(".csMDHTML").length > 0) {
                    return false;
                }

                // Allow if element is the mdContent
                if ($e.closest(".csMDHTML").length > 0) {
                    return false;
                }

                // Skip all other elements to speed up DOM copying
                return true;
            },
            onclone: (doc) => {
                // Add padding to the cloned element so that the image does not look cropped in reviewcanvas
                const style = document.createElement("style");
                style.innerHTML = `.csMDHTML { padding: 50px !important; }`;
                doc.head.appendChild(style);
            },
        });

        // Convert to blob
        const blob = await new Promise<Blob | null>((r) =>
            canvas.toBlob(r, "image/png")
        );
        if (!blob) {
            this.runError = true;
            this.error = $localize`Unable to automatically create image`;
            this.exportingMD = false;
            return;
        }
        const taskId = this.pluginMeta.getTaskId();
        const file = new File([blob], taskId ? taskId.name : "matheditor");
        rc.uploadFiles([file]);
        this.result = $localize`Image sent to task ${this.markup.targetCanvas}`;
        this.exportingMD = false;
    }

    async showMD() {
        this.runError = false;
        this.result = "";
        if (!this.usercode) {
            if (this.mdHtml) {
                this.mdHtml = "";
            }
            return;
        }
        const taskId = this.pluginMeta.getTaskId();
        if (!taskId?.docId) {
            this.runError = true;
            this.error = $localize`Plugin is missing task id`;
            return;
        }
        if (this.precode == undefined) {
            await this.getAllCode();
        }
        const text = this.precode + "\n" + this.usercode + "\n" + this.postcode;
        if (text === this.lastMD) {
            return;
        }
        this.lastMD = text;
        const r = await this.httpPost<IPluginInfoResponse>(
            `/preview/${taskId.docId}`,
            {
                text: text,
                settings: {
                    math_type: "mathjax",
                    ...(this.markup.previewExtraSettings ?? {}),
                },
            }
        );
        if (r.ok) {
            const data = r.result;
            const element: JQuery = $($.parseHTML(data.texts) as HTMLElement[]);
            // Remove par class as it's used to identify real paragraphs
            element.removeClass("par");
            // Remove editline as well as it's not valid for preview
            element.children(".editline").remove();
            // give class for possibility of styling
            element.addClass("mdPreviewDiv");

            data.texts = element.wrapAll("<div>").parent().html();
            const viewCtrl = vctrlInstance;
            if (!viewCtrl) {
                // TODO: remove this when ParCompiler is updated to Angular 2+
                throw new Error("ViewCtrl was undefined");
            }
            await ParCompiler.compileAndAppendTo(
                this.preview,
                data,
                viewCtrl.scope
            );
        } else {
            const data = r.result;
            this.runError = true;
            this.error = $localize`Failed to show preview: ${data.error.error}`;
        }
    }

    write(s: string) {
        this.result += s;
    }

    writeln(s: string) {
        this.write(s + "\n");
    }

    getCode() {
        if (this.program && !this.codeInitialized) {
            this.localcode = this.program;
            this.getCodeFromLocalCode();
        }
        this.codeInitialized = true;
        let text = this.usercode;
        if (this.precode || this.postcode) {
            text = this.precode + "\n" + text + "\n" + this.postcode;
        }
        return text;
    }

    closeFrame() {
        this.iframesettings = undefined;
        this.loadedIframe = undefined;
        this.lastJS = "";
    }

    async showJS() {
        if (
            !this.markup.runeverytime &&
            !this.usercode &&
            !this.userargs &&
            !this.userinput &&
            !this.firstTime
        ) {
            return;
        }
        this.firstTime = false;
        if (this.origLanguageType.includes("truthtable")) {
            const truthTable = (await import("./truthTable")).truthTable;
            this.result = truthTable(this.userargs);
            return;
        }
        const fullhtml = await this.getFullhtml();
        if (!this.iframesettings || fullhtml) {
            // create an iframe on first time
            let html = "";
            let scripts = "";
            if (this.origLanguageType.includes("/vis")) {
                html =
                    '<div id="myDiv" class="mydiv" width="800" height="400" ></div>';
                scripts =
                    "https://cdnjs.cloudflare.com/ajax/libs/vis/4.20.0/vis.min.js";
            }
            let fsrc = "/cs/gethtml/canvas.html";
            if (this.type === "wescheme") {
                fsrc = "/csstatic/WeScheme/openEditor.html";
            }
            let dw;
            let dh;
            if (this.glowscript) {
                fsrc = "/cs/gethtml/GlowScript.html";
                dh = 430;
                dw = 800;
            }
            if (this.isProcessing) {
                fsrc = "/cs/gethtml/processing.html";
            }
            const v = this.getVid(dw, dh);
            html = this.markup.html ?? html;
            html = encodeURI(html);
            const fh = await this.getfullhtmlext(this.getCode());
            if (!this.loadedIframe || !this.markup.useSameFrame) {
                let src = this.domSanitizer.bypassSecurityTrustResourceUrl(
                    fh
                        ? getIFrameDataUrl(fh)
                        : `${fsrc}?scripts=${
                              this.markup.scripts ?? scripts
                          }&html=${html}`
                );
                if (fullhtml?.startsWith("http")) {
                    src =
                        this.domSanitizer.bypassSecurityTrustResourceUrl(
                            fullhtml
                        );
                }
                this.iframesettings = {
                    id: v.vid,
                    width: v.width,
                    height: v.height,
                    src: src,
                };
            }
        }
        const text = this.usercode;
        if (
            !this.markup.runeverytime &&
            text === this.lastJS &&
            this.userargs === this.lastUserargs &&
            this.userinput === this.lastUserinput
        ) {
            return;
        }
        this.lastJS = text;
        this.lastUserargs = this.userargs;
        this.lastUserinput = this.userinput;

        if (!this.loadedIframe) {
            const self = this;
            const ld = await this.waitIframeLoad(function (
                event: MessageEvent
            ) {
                // setData may return dimensions for iframe
                if (event.data && self.iframesettings) {
                    const iframe = event.data.ret?.iframe;
                    if (!iframe) {
                        return;
                    }
                    const mu = self.markup;
                    if (iframe.height && (!mu.height || mu.height < 0)) {
                        self.iframesettings.height = iframe.height;
                    }
                    if (iframe.width && !mu.width) {
                        self.iframesettings.width = iframe.width;
                    }
                    // self.anyChanged(); //
                    self.cdr.detectChanges();
                }
            });
            if (!ld) {
                return;
            }
            this.loadedIframe = ld;
        }
        const load = this.loadedIframe;
        const f = load.iframe;
        const channel = load.channel;
        if (this.iframeClientHeight < 0) {
            this.iframeClientHeight = f.clientHeight;
        }
        let extra = {};
        if (this.type === "glowscript") {
            extra = {
                language: "GlowScript 2.1 JavaScript",
            };
        }

        /*
        const self = this;
        channel.port1.onmessage = function (event: MessageEvent) {
            // setData may return dimensions for iframe
            if (event.data && self.iframesettings) {
                const iframe = event.data.ret?.iframe;
                if (!iframe) {
                    return;
                }
                const mu = self.markup;
                if (iframe.height && (!mu.height || mu.height < 0)) {
                    self.iframesettings.height = iframe.height;
                }
                if (iframe.width && !mu.width) {
                    self.iframesettings.width = iframe.width;
                }
                // self.anyChanged(); //
                self.cdr.detectChanges();
            }
        };
         */

        channel.port1.postMessage({
            data: {
                code: this.getCode(),
                args: this.userargs,
                input: this.userinput,
                params: this.markup.jsparams,
                // TODO: Why would someone _not_ want console?
                //  There won't be anything visible in UI without it.
                // Answer: console means that consolelog goes to same
                // place than write.  Otherwise it goes to browser console
                console: !this.attrsall.markup.jsBrowserConsole, // true, // this.type.includes("/c"),
                ...extra,
            },
            msg: "setData",
        });
        // if (f.contentWindow.getConsoleHeight) {
        //     let ch = f.contentWindow.getConsoleHeight();
        //     if (ch < this.iframeClientHeight) {
        //         ch = this.iframeClientHeight;
        //     }
        //     this.iframesettings.height = ch;
        // }
    }

    async waitIframeLoad(
        messageHandler?: (e: MessageEvent) => void
    ): Promise<IFrameLoad | undefined> {
        this.iframedefer = new TimDefer<IFrameLoad>();
        this.iframemessageHandler = messageHandler;
        return this.iframedefer.promise;
    }

    get showRuntime() {
        return this.markup.showRuntime;
    }

    get codeover() {
        return this.markup.codeover;
    }

    get codeunder() {
        return this.markup.codeunder;
    }

    get inputstem() {
        return this.markup.inputstem;
    }

    get inputrows() {
        return this.markup.inputrows;
    }

    get spellcheck() {
        return this.markup.spellcheck;
    }

    async setData(data: unknown, save: boolean = false) {
        if (SetData.is(data)) {
            for (const name of ["usercode", "userargs", "userinput"] as const) {
                const v = data[name];
                if (v !== undefined) {
                    this[name] = v;
                }
            }
        }
        if (save) {
            await this.runCode();
        }
    }

    updateRunChanged(): boolean {
        if (this.editor?.parsonsEditor) {
            this.runChanged = false;
        } else {
            this.runChanged = this.byCode !== this.usercode;
        }
        this.updateCanReset();
        return this.runChanged;
    }

    /**
     * Creates a close button for the given div.
     * close button removes the parent div when clicked.
     * @param div - The HTMLDivElement to which the close button will be added.
     * @returns The created close button element.
     */
    createCloseButton(div: HTMLDivElement) {
        const closeBtn = document.createElement("button");
        closeBtn.innerHTML = "&times;";
        closeBtn.style.position = "absolute";
        closeBtn.style.top = "1px";
        closeBtn.style.right = "1px";
        closeBtn.style.background = "transparent";
        closeBtn.style.border = "none";
        closeBtn.style.fontSize = "18px";
        closeBtn.style.cursor = "pointer";
        closeBtn.setAttribute("aria-label", "Close");
        closeBtn.setAttribute("title", "Close");
        closeBtn.onclick = () => {
            div.remove();
        };
        div.appendChild(closeBtn);
        return closeBtn;
    }

    // @ViewChild("iframesContainer", {static: true})
    @ViewChild("csrunPreview", {static: true})
    private csrunPreviewDiv?: ElementRef<HTMLDivElement>;

    /**
     * Show iframes in the csrunPreviewDiv.
     * @param iframes - The IFramesType object containing iframe data.
     */
    showIframes(iframes: IFramesType) {
        if (!this.csrunPreviewDiv) {
            return;
        }
        let iframesContainer: HTMLDivElement | null =
            this.csrunPreviewDiv.nativeElement.querySelector(
                ".iframes-container"
            );

        const tid = this.getTaskId()?.name?.toString() ?? "iframe";

        if (!iframesContainer) {
            // if no container found, create a new one
            iframesContainer = document.createElement("div");
            iframesContainer.className = "iframes-container";
            iframesContainer.id = tid + "-iframes-container";
            this.csrunPreviewDiv.nativeElement.appendChild(iframesContainer);
        }
        if (iframes.style) {
            iframesContainer.setAttribute("style", iframes.style);
        }
        iframesContainer.innerHTML = "";
        iframesContainer.style.position = "relative";
        iframesContainer.style.overflow = "hidden";
        this.createCloseButton(iframesContainer);

        /* // Create an Angular close button for the iframes container
        const closeBtnRef =
            iframesContainer.createComponent(CloseButtonComponent);
        closeBtnRef.location.nativeElement.addEventListener("click", () =>
            iframesContainer?.remove()
        );
        */

        for (let i = 0; i < iframes.files?.length ?? 0; i++) {
            const iframe = iframes.files[i];
            const wset = iframe.width ?? iframes.defaults?.width;
            const w = wset ?? iframesContainer.offsetWidth;
            const hset = iframe.height ?? iframes.defaults?.height;
            const h = hset ?? 400;
            const id = iframe.id ?? "" + tid + "-iframe-" + i;
            const iframeElem = document.createElement("iframe");
            iframeElem.setAttribute("srcdoc", iframe.content); // htmlContent voi sisältää lainausmerkkejä
            iframe.content = ""; // to save memory, not needed anymore

            let sandbox = "allow-scripts";
            const sb = iframe.sandbox ?? iframes.defaults?.sandbox;
            if (sb !== undefined) {
                sandbox = "";
                for (const sbItem of sb.split(" ")) {
                    if (
                        [
                            "allow-scripts",
                            "allow-modals",
                            "allow-forms",
                        ].includes(sbItem)
                    ) {
                        sandbox += sbItem + " ";
                    }
                }
                sandbox = sandbox.trim();
            }
            iframeElem.setAttribute("sandbox", sandbox);
            iframesContainer.appendChild(iframeElem);

            const style = iframe.style ?? iframes.defaults?.style ?? "";
            if (style) {
                iframeElem.setAttribute("style", style);
            }
            iframeElem.setAttribute("class", "csRunIframe");
            iframeElem.setAttribute("id", id);
            iframeElem.setAttribute("title", iframe.filename ?? "iframe");

            const computedStyle = window.getComputedStyle(iframeElem);

            // Set width and height if they are not set or if they are asked to set
            if (computedStyle.width === "300px" || wset) {
                iframeElem.width = w.toString();
            }
            if (computedStyle.height === "150px" || hset) {
                iframeElem.height = h.toString();
            }
            // Ensure the iframe position is relative event user tries to style it
            iframeElem.style.position = "relative";
        }
    }

    @HostListener("focusout")
    autosave() {
        if (this.markup.autosave && this.edited) {
            this.save();
        }
    }
}

@Component({
    selector: "cs-runner",
    template: `
        <!--suppress TypeScriptUnresolvedVariable -->
        <div [ngClass]="{'csRunDiv': borders}" [class.cs-has-header]="header" class="type-{{rtype}} cs-flex"
             [ngStyle]="csRunDivStyle">
            <tim-markup-error class="csMarkupError" *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <h4 class="csHeader" *ngIf="header" [innerHTML]="header | purify"></h4>
            <div class="csAllSelector" *ngIf="isAll">
                <div>
                    {{ languageText }}
                    <select [(ngModel)]="selectedLanguage" required (ngModelChange)="languageChange()">
                        <option *ngFor="let o of progLanguages" [value]="o">{{ o }}</option>
                    </select>
                </div>
            </div>
            <p *ngIf="stem" class="stem" [innerHTML]="stem | purify"
               (keydown)="elementSelectAll($event)" tabindex="0"></p>
            <div class="csTaunoContent" *ngIf="isTauno">
                <p *ngIf="taunoOn" class="pluginHide"><a (click)="hideTauno()">{{ hideText }} Tauno</a></p>
                <iframe *ngIf="iframesettings"
                        [id]="iframesettings.id"
                        class="showTauno"
                        [src]="iframesettings.src"
                        (load)="onIframeLoad($event)"
                        [width]="iframesettings.width"
                        [height]="iframesettings.height"
                        sandbox="allow-scripts"></iframe>
                <p *ngIf="!taunoOn" class="pluginShow"><a (click)="showTauno()">{{ showText }} Tauno</a></p>
                <p *ngIf="taunoOn" class="pluginHide">
                    <a (click)="copyTauno()">{{ copyFromTaunoText }}</a> |
                    <a (click)="hideTauno()">{{ hideText }} Tauno</a></p>
                <p *ngIf="taunoOn" class="taunoOhje">
                    {{ taunoOhjeText }}</p>
            </div>
            <div class="csSimcirContent" *ngIf="isSimcir">
                <p *ngIf="simcirOn" class="pluginHide"><a (click)="hideSimcir()">{{ hideText }} SimCir</a></p>
                <div class="simcirContainer"><p></p></div>
                <p *ngIf="!simcirOn" class="pluginShow"><a (click)="showSimcir()">{{ showText }} SimCir</a></p>
                <p *ngIf="simcirOn && !noeditor" class="pluginHide">
                    <a (click)="copyFromSimcir()">copy from SimCir</a>
                    | <a (click)="copyToSimcir()">copy to SimCir</a> | <a (click)="hideSimcir()">hide SimCir</a>
                </p>
            </div>
            <div class="csUploadContent" *ngIf="upload && !this.formulaEditor">
                <file-select-manager class="small"
                                     [dragAndDrop]="dragAndDrop"
                                     [uploadUrl]="uploadUrl"
                                     [stem]="uploadstem"
                                     (file)="onFileLoad($event)"
                                     (upload)="onUploadResponse($event)"
                                     (uploadDone)="onUploadDone($event)">
                </file-select-manager>
                <div [hidden]="formulaEditor" class="form-inline small">
                    <span *ngFor="let item of uploadedFiles">
                        <cs-upload-result [src]="item.path" [type]="item.type"></cs-upload-result>
                    </span>
                </div>
            </div>
            <pre class="csViewCodeOver" *ngIf="viewCode && codeover">{{ code }}</pre>

            <div class="csRunCode">
                <div *ngIf="formulaEditor && editor">
                    <cs-formula-editor
                        #formulaEditorComponent
                        (okClose)="toggleFormulaEditor($event)"
                        (cancelClose)="toggleFormulaEditor($event)"
                        (toggle)="toggleFormulaEditor()"
                        [visible]="formulaEditorOpen"
                        [editor]="editor"
                        [currentSymbol]="currentSymbol"
                        [templateButtons]="templateButtons">
                        <file-select-manager class="small"
                                             [dragAndDrop]="dragAndDrop"
                                             [uploadUrl]="uploadUrl"
                                             [stem]="uploadstem"
                                             (file)="onFileLoad($event)"
                                             (upload)="onUploadResponse($event)"
                                             (uploadDone)="onUploadDone($event)">
                        </file-select-manager>
                    </cs-formula-editor>
                </div>
                <pre class="csRunPre" *ngIf="viewCode && !codeunder && !codeover">{{ precode }}</pre>
                <div class="csEditorAreaDiv" [hidden]="formulaEditor && formulaEditorOpen">
                    <cs-editor #mainEditor *ngIf="!noeditor || viewCode" class="csrunEditorDiv"
                               [base]="byCode"
                               [minRows]="rows"
                               [maxRows]="maxrows"
                               [wrap]="wrap"
                               [modes]="editorModes"
                               [editorIndex]="editorMode"
                               [parsonsOptions]="parsons"
                               (close)="onFileClose($event)"
                               (content)="onContentChange($event)"
                               [spellcheck]="spellcheck">
                    </cs-editor>
                    <div class="csRunChanged" *ngIf="runChanged && !hide.changed"></div>
                    <div class="csRunNotSaved" *ngIf="isUnSaved()"></div>
                </div>
                <pre class="csRunPost" *ngIf="viewCode && !codeunder && !codeover">{{ postcode }}</pre>
            </div>
            <div *ngIf="isSage" class="computeSage no-popup-menu"></div>
            <div class="csInputDiv" *ngIf="showInput && isInput">
                <p *ngIf="inputstem" class="stem">{{ inputstem }}</p>
                <div class="csRunCode">
            <textarea class="csRunArea csInputArea"
                      [rows]="inputrows"
                      [(ngModel)]="userinput"
                      [placeholder]="inputplaceholder">
            </textarea>
                </div>
            </div>
            <div class="csArgsDiv" *ngIf="showArgs &&!markup['noargs'] && isInput"><label>{{ argsstem }} </label>
                <span><input type="text"
                             class="csArgsArea"
                             [(ngModel)]="userargs"
                             [placeholder]="argsplaceholder"></span>
            </div>
            <cs-count-board class="csRunCode" *ngIf="count" [options]="count"></cs-count-board>
            <div #runSnippets class="csRunSnippets" [hidden]="formulaEditor && formulaEditorOpen"
                 *ngIf="templateButtonsCount && !noeditor">
                <button [class.math]="item.hasMath" class="btn btn-default"
                        *ngFor="let item of templateButtons | symbols"
                        (click)="addText(item)" [attr.title]="item.expl" [attr.aria-label]="item.expl"
                        [innerHTML]="item.text | purify"></button>
            </div>
            <cs-editor #externalEditor *ngIf="externalFiles && externalFiles.length" class="csrunEditorDiv"
                       [maxRows]="maxrows"
                       [disabled]="true">
            </cs-editor>
            <div class="csRunMenuArea" *ngIf="!forcedupload && !markup['norunmenu']"
                 [hidden]="formulaEditor && formulaEditorOpen">
                <p class="csRunMenu">
                    <button *ngIf="isRun && buttonText()"
                            [disabled]="isRunning || preventSave || (disableUnchanged && !isUnSaved() && isText)"
                            class="timButton btn-sm"
                            title="(Ctrl-S)"
                            (click)="runCode()"
                            [innerHTML]="buttonText()"></button>
                    &nbsp;
                    <span *ngIf="invalidMarker?.deadline" [tooltip]="invalidMarker?.deadline" placement="bottom"
                          class="glyphicon glyphicon-lock text-danger"></span>
                    <span *ngIf="invalidMarker?.modelAnswerLock" [tooltip]="invalidMarker?.modelAnswerLock" placement="bottom"
                          class="glyphicon glyphicon-lock text-danger"></span>
                    <ng-container *ngIf="invalidMarker">&nbsp;</ng-container>
                    <button *ngIf="mdSaveButton" [disabled]="!mdHtml" class="timButton btn-sm"
                            (click)="exportMDAsImg()">{{ mdSaveButton }}
                    </button>
                    <tim-loading *ngIf="mdSaveButton && exportingMD"></tim-loading>
                    &nbsp;
                    <button *ngIf="isExternalFetch"
                            [disabled]="isRunning"
                            class="timButton btn-sm"
                            (click)="fetchExternalFiles()"
                            [innerHTML]="externalFetchText()"></button>
                    <a href="#" *ngIf="undoButton && isUnSaved()" [title]="undoTitle"
                       (click)="tryResetChanges($event)"> &nbsp;{{ undoButton }}</a>
                    &nbsp;&nbsp;
                    <span *ngIf="savedText"
                          class="savedText"
                          [innerHTML]="savedText"></span>
                    &nbsp;&nbsp;
                    <button *ngIf="isTest"
                            [disabled]="isRunning"
                            (click)="runTest()"
                            class="timButton btn-sm"
                            [innerHTML]="testText"></button>
                    &nbsp;&nbsp;
                    <button *ngIf="isUnitTest"
                            class="timButton btn-sm"
                            [disabled]="isRunning"
                            (click)="runUnitTest()">UTest
                    </button>
                    <tim-loading *ngIf="isRunning"></tim-loading>
                    <span class="runningText" *ngIf="isRunning && runningText">{{ runningText }}</span>
                    &nbsp;&nbsp;
                    <span *ngIf="isDocument">
                <a href="#" [ngClass]="{'link-disable': isRunning}"
                   (click)="runDocument(); $event.preventDefault()">{{ docLink }}</a>&nbsp;&nbsp;
            </span>
                    <a href="#" *ngIf="!nocode && (file || program)"
                       (click)="showCode(); $event.preventDefault()">{{ showCodeLink }}</a>&nbsp;&nbsp;
                    <a href="#" *ngIf="canReset"
                       (click)="initCode(true); $event.preventDefault()">{{ resetText }} </a>
                    <a href="#" *ngIf="toggleEditor"
                       (click)="hideShowEditor(); $event.preventDefault()">{{ toggleEditorText[noeditor ? 0 : 1] }}</a>
                    <a href="#" *ngIf="!noeditor && editor && editor.nextModeText"
                       (click)="editor.showOtherEditor(); $event.preventDefault()">
                        {{ editor.nextModeText }}
                    </a>&nbsp;&nbsp;
                    <a href="#" *ngIf="copyLink"
                       (click)="copyCode(); $event.preventDefault()">{{ copyLink }}</a>
                    <span *ngIf="showRuntime"
                          class="inputSmall"
                          style="float: right;"
                          title="Run time in sec {{runtime}}">{{ oneruntime }}</span>
                    <span *ngIf="editor && wrap && wrap.n!=-1 && !hide.wrap && editor.mode < 2" class="inputSmall"
                          style="float: right;"
                          title="Put 0 to no wrap">
                <button class="timButton" title="Click to reformat text for given line length" (click)="editor.doWrap()"
                        style="font-size: x-small; height: 1.7em; padding: 1px; margin-top: -4px;">Wrap
                </button>
                &nbsp;
                <input type="checkbox" title="Check for automatic wrapping" [(ngModel)]="wrap.auto"
                       style="position: relative;top: 0.3em;" />
                &nbsp;
                <input type="text" title="Choose linelength for text.  0=no wrap" pattern="[0-9]*" [(ngModel)]="wrap.n"
                       size="2" />
            </span>
                    <span *ngIf="connectionErrorMessage" class="error" style="font-size: 12px"
                          [innerHTML]="connectionErrorMessage"></span>

                    <!--
                    <span *ngIf="wrap.n!=-1" class="inputSmall" style="float: right;">
                      <label title="Put 0 to no wrap">wrap: <input type="text"
                                                                  pattern="[0-9]*"
                                                                  [(ngModel)]="wrap.n"
                                                                  size="1"/></label>
                    </span>
                    -->
                </p>

            </div>
            <div *ngIf="isSage" class="outputSage no-popup-menu"></div>
            <pre class="csViewCodeUnder" *ngIf="viewCode && codeunder">{{ code }}</pre>
            <p class="unitTestGreen" *ngIf="runTestGreen">&nbsp;ok</p>
            <pre class="unitTestRed" *ngIf="runTestRed">{{ comtestError }}</pre>
            <div class="csRunErrorClass csRunError" *ngIf="runError">
                <p class="pull-right" *ngIf="!markup['noclose']">
                    <label class="normalLabel" title="Keep errors until next run">Keep <input type="checkbox"></label>
                    <tim-close-button (click)="closeError()"></tim-close-button>
                </p>
                <a class="copyErrorLink" *ngIf="markup.copyErrorLink"
                   (click)="copyString(error, $event)"
                   title="Copy text to clipboard"
                >{{ markup.copyErrorLink }}</a>
                <pre class="csRunError" (keydown)="elementSelectAll($event)" tabindex="0">{{ error }}</pre>
                <p class="pull-right" *ngIf="!markup['noclose']" style="margin-top: -1em">
                    <tim-close-button (click)="closeError()"></tim-close-button>
                </p>
            </div>
            <div class="csRunErrorClass csFetchError" *ngIf="fetchError">
                <p class="pull-right" *ngIf="!markup['noclose']">
                    <tim-close-button (click)="fetchError=undefined"></tim-close-button>
                </p>
                <a class="copyErrorLink" *ngIf="markup.copyErrorLink"
                   (click)="copyString(fetchError, $event)"
                   title="Copy text to clipboard"
                   aria-label="Copy text to clipboard"
                >{{ markup.copyErrorLink }}</a>
                <pre class="csRunError" (keydown)="elementSelectAll($event)" tabindex="0">{{ fetchError }}</pre>
                <p class="pull-right" *ngIf="!markup['noclose']" style="margin-top: -1em">
                    <tim-close-button (click)="fetchError=undefined"></tim-close-button>
                </p>
            </div>
            <div class="consoleDiv" [class.soft-hidden]="!result">
                <a class="copyConsoleLink" *ngIf="markup.copyConsoleLink"
                   (click)="copyString(result, $event)"
                   title="Copy console text to clipboard"
                   aria-label="Copy console text to clipboard"
                >{{ markup.copyConsoleLink }}</a>
                <pre id="resultConsole" class="console" (keydown)="elementSelectAll($event)"
                     tabindex="0" aria-live="assertive">{{ result }}</pre>
            </div>
            <div class="htmlresult" *ngIf="htmlresult"><span [innerHTML]="htmlresult | purify"
                                                             aria-live="polite"></span></div>
            <div #csrunPreview class="csrunPreview" [class.csrun-clicking]="formulaEditor"
                 (keydown)="elementSelectAll($event)"
                 tabindex="0" aria-live="polite"
                 (dblclick)="handleSelectFormulaFromPreview($event, preview)" #preview>
                <div *ngIf="iframesettings && !isTauno"
                     tim-draggable-fixed
                     caption="Preview"
                     detachable="true"
                     class="no-popup-menu">
                    <span class="csRunMenu" *ngIf="!markup['noclose']">
                        <tim-close-button
                            (click)="closeFrame()"
                            style="float: right">
                        </tim-close-button>
                    </span>
                    <iframe [id]="iframesettings.id"
                            class="jsCanvas"
                            [src]="iframesettings.src"
                            (load)="onIframeLoad($event)"
                            [width]="iframesettings.width"
                            [height]="iframesettings.height"
                            sandbox="allow-scripts allow-forms"
                            style="border:0">
                    </iframe>
                </div>
                <div class="csMDHTML" #mdHtmlDiv *ngIf="mdHtml" [innerHTML]="mdHtml | purify" aria-live="polite">
                </div>
            </div>
            <tim-graph-viz class="csGraphViz" *ngIf="isViz" [vizcmd]="fullCode" [jsparams]="jsparams"></tim-graph-viz>
            <tim-variables class="csVariables" *ngIf="isVars" [code]="fullCode"
                           [jsparams]="jsparams"
                           [height]="height"
            ></tim-variables> <!-- TODO: why direct markup.jsparam does not work -->
            <img *ngIf="imgURL" class="grconsole" [src]="imgURL" alt="" />
            <video class="csVideo" *ngIf="videoURL" [src]="videoURL" type="video/mp4" style="width: 100%;" controls=""
                   autoplay></video>
            <video class="csAudio" *ngIf="wavURL" [src]="wavURL" type="video/mp4" controls="" autoplay="true"
                   width="300"
                   height="40"></video>
            <div *ngIf="docURL" class="docurl">
                <p class="pull-right">
                    <tim-close-button (click)="closeDocument()"></tim-close-button>
                </p>
                <iframe width="800" height="600" [src]="docURL" target="csdocument" allowfullscreen></iframe>
            </div>
            <p class="footer" [innerHTML]="footer | purify"></p>
        </div>`,
    styleUrls: ["./csPlugin.scss"],
})
export class CsRunnerComponent extends CsController {
    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer, cdr);
    }
}
