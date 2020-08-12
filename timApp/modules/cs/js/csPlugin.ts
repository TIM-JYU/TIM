/* eslint-disable @typescript-eslint/no-explicit-any,@typescript-eslint/tslint/config,no-underscore-dangle */
import {
        Component,
        ViewChild,
        ChangeDetectorRef,
        ElementRef,
        Directive,
    } from "@angular/core";
import {HttpClient, HttpHeaders} from "@angular/common/http";
import {DomSanitizer, SafeResourceUrl} from "@angular/platform-browser";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import * as t from "io-ts";
import $ from "jquery";
import {ChangeType, FormModeOption, ISetAnswerResult, ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {IPluginInfoResponse, ParCompiler} from "tim/editor/parCompiler";
import {GenericPluginMarkup, Info, nullable, withDefault} from "tim/plugin/attributes";
import {getFormBehavior} from "tim/plugin/util";
import {$http, $sce, $timeout} from "tim/util/ngimport";
import {
    copyToClipboard,
    defaultErrorMessage,
    defaultTimeout,
    getClipboardHelper,
    to2,
    to,
    valueDefu,
    valueOr,
} from "tim/util/utils";
import {TimDefer} from "tim/util/timdefer";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {handleAnswerResponse} from "tim/document/interceptor";
import {CellInfo} from "./embedded_sagecell";
import {getIFrameDataUrl} from "./iframeutils";
import {Mode, EditorComponent, EditorFile} from "./editor/editor";
import {CountBoardComponent} from "./editor/countboard";
import {getInt} from "./util/util";
import {IFile, FileSelectManagerComponent, IFileSpecification} from "./util/file-select";
import {Set, OrderedSet} from "./util/set";

// js-parsons is unused; just declare a stub to make TS happy
declare class ParsonsWidget {
    static _graders: any;

    constructor(data: unknown);

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
    width: number;
    height: number;
}

/*
Sagea varten ks: https://github.com/sagemath/sagecell/blob/master/doc/embedding.rst#id3
*/

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

export const ConsolePWD = new CWPD();

const csJSTypes = ["js", "glowscript", "vpython", "html", "processing", "wescheme"];

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
        "smalltalk", "upload", "extcheck", "gitreg"];

    // For editor modes see: http://ace.c9.io/build/kitchen-sink.html ja sieltä http://ace.c9.io/build/demo/kitchen-sink/demo.js
    aceModes = ["pascal", "fortran", "css", "csharp", "scala", "java", "java", "c_cpp", "c_cpp", "sh", "python", "python", "python", "fsharp", "lisp",
        "javascript", "sql", "sql", "alloy", "text", "csharp", "run", "text", "javascript", "javascript", "python", "json",
        "xml", "matlab", "lua", "quorum", "swift", "text", "html", "javascript", "text", "r", "scheme", "text", "kotlin",
        "text", "text", "c_cpp", "text"];

    // What are known test types (be careful not to include partial word):
    testTypes = ["ccomtest", "jcomtest", "comtest", "scomtest"];
    testAceModes = ["c_cpp", "java", "csharp", "scala"];
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
            if (type.includes(ty)) {
                return ty;
            }
        }
        return def;
    }

    whatIsInAce(types: string[], type: string) {

        if (!type) {
            return undefined;
        }
        type = type.toLowerCase();
        for (let i = 0; i < types.length; i++) {
            if (type.includes(types[i])) {
                return this.aceModes[i];
            }
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
        if (type.match(/^all[^a-z0-9]?/)) {
            return true;
        }
        return false;

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
        return this.whatIsInAce(this.runTypes, type) ?? def as string;
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

export const languageTypes = new LanguageTypes();

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
interface ICountLimit extends t.TypeOf<typeof CountLimit> {}

const CountType = t.partial({
    preventSave: t.boolean,
    tooManyWord: t.string,
    tooFewWord: t.string,
    lines: CountLimit,
    words: CountLimit,
    chars: CountLimit,
});
interface ICountType extends t.TypeOf<typeof CountType> {}

const oneOrArray = <T extends t.Mixed>(type: T) => t.union([type, t.array(type)]);
const listify = <T>(e: T | T[]) => Array.isArray(e) ? e : [e];

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
type ICommonMarkup = t.TypeOf<typeof CommonMarkup>;

const EditorMarkupFields = t.intersection([ // no source attribute
    t.type({
        path: t.string,
        canClose: withDefault(t.boolean, false),
        canRename: withDefault(t.boolean, false),
    }),
    t.partial({
        mode: t.string,
        placeholder: t.string,
    }),
    CommonMarkup,
]);

const EditorMarkup = // with source attribute
    t.intersection([
        t.type({
            source: t.literal("editor"),
        }),
        EditorMarkupFields,
    ]);
type IEditorMarkup = t.TypeOf<typeof EditorMarkup>;

const UploadMarkupFields = t.intersection([// no source or paths attribute
    t.partial({
        maxSize: t.number,
        extensions: oneOrArray(t.string),
    }),
    CommonMarkup,
]);

const UploadMarkup = // with source and paths attribute
    t.intersection([
        t.type({
            paths: oneOrArray(t.string),
            source: t.literal("upload"),
        }),
        UploadMarkupFields,
    ]);
type IUploadMarkup = t.TypeOf<typeof UploadMarkup>;

const UploadByCodeMarkup =
    t.intersection([
        t.type({
            source: t.literal("uploadByCode"),
            show: withDefault(
                t.union([
                    t.boolean,
                    t.literal("loaded"), // show after load
                ]), false
            ),
        }),
        EditorMarkupFields,
        UploadMarkupFields,
    ]);
type IUploadByCodeMarkup = t.TypeOf<typeof UploadByCodeMarkup>;

const ExternalSourceMarkup =
    t.intersection([
        t.type({
            path: t.string,
            source: t.string,
        }),
        t.partial({
            maxSize: t.number,
            maxTotalSize: t.number,
        }),
    ]);
interface IExternalSourceMarkup extends t.TypeOf<typeof ExternalSourceMarkup> {}

const FileMarkup =
    t.union([
        t.type({source: withDefault(t.string, "editor")}),
        EditorMarkup,
        UploadMarkup,
        UploadByCodeMarkup,
        ExternalSourceMarkup,
    ]);
type IFileMarkup = IEditorMarkup | IUploadMarkup | IUploadByCodeMarkup | IExternalSourceMarkup;

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
export interface IFileSubmission extends t.TypeOf<typeof FileSubmission> {}

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
    librarySpecific: t.any,
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
    fields: t.dictionary(t.string,
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
    autoupdate: t.number,
    buttons: t.string,
    byCode: t.string,
    docurl: t.string,
    examples: t.array(Example),
    file: t.string,
    filename: t.string,
    fullhtml: t.string,
    git: GitMarkup,
    gitDefaults: GitDefaultsMarkup,
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
    wrap:  t.Integer,
    borders: withDefault(t.boolean, true),
    iframeopts: t.string,
    count: CountType,
    hide: t.any,
    savedText: t.string,
    rootPath: t.string,
    masterPath: t.string,
    files: oneOrArray(FileMarkup),
    moreFiles: oneOrArray(ExternalSourceMarkup),
    jsFiles: t.array(t.string),
    cssFiles: t.array(t.string),
    deleteFiles: t.array(t.string),
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
    dragAndDrop: withDefault(t.boolean, true),
    editorMode: withDefault(t.Integer, -1),
    editorModes: withDefault(t.union([t.string, t.Integer]), "01"),
    iframe: withDefault(t.boolean, false), // TODO this maybe gets deleted on server
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
    maxSize: withDefault(t.number, 50),
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
    validityCheckForceSave: withDefault(t.boolean, false),
    viewCode: withDefault(t.boolean, false),
    words: withDefault(t.boolean, false),
    allowMultipleFiles: withDefault(t.boolean, true),
    multipleUploadElements: withDefault(t.boolean, true),
    mayAddFiles: withDefault(t.boolean, false),
    /* eslint-enable quote-props */
});

const CsMarkup = t.intersection([CsMarkupOptional, CsMarkupDefaults, GenericPluginMarkup]);

const CsAll = t.intersection([
    t.partial({
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

export class CsBase extends AngularPluginBase<t.TypeOf<typeof CsMarkup>, t.TypeOf<typeof CsAll>, typeof CsAll> {
    usercode_: string = "";

    get usercode(): string {
        return this.usercode_;
    }
    set usercode(str: string) {
        this.usercode_ = str;
    }

    get byCode() {
        return commentTrim((this.attrsall.by ?? this.markup.byCode) ?? "");
    }

    get type() {
        return this.markup.type;
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

function createIframe(
    opts: {
        classname?: string,
        height?: number,
        id?: string,
        sandbox: string | undefined,
        src: string,
        width?: number,
        allowfullscreen?: boolean,
    }
) {
    const f = document.createElement("iframe");
    if (opts.classname) {
        f.className = opts.classname;
    }
    if (opts.id) {
        f.id = opts.id;
    }
    if (opts.width) {
        f.width = opts.width.toString();
    }
    if (opts.height) {
        f.height = opts.height.toString();
    }
    f.allowFullscreen = opts.allowfullscreen ?? false;
    f.src = opts.src;
    const noreadonly = f as unknown as {sandbox: string};
    if (opts.sandbox !== undefined) {
        noreadonly.sandbox = opts.sandbox;
    }
    return f;
}

interface IFrameLoad {
    iframe: HTMLIFrameElement & { contentWindow: WindowProxy };
    channel: MessageChannel;
}

interface IRunResponseWeb {
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
    language?: unknown, // determined by language
    "-replyImage"?: string,
    "-replyHTML"?: string,
    "-replyMD"?: string,
}

export interface IRunResponse {
    web: IRunResponseWeb,
    savedNew: number,
}

interface IFetchResponse {
    error?: string,
    files: IFileSubmission[],
}


interface IRunRequestInput extends Partial<IExtraMarkup> {
    usercode?: string;
    submittedFiles?: IFileSubmission[];
    userinput: string;
    isInput: boolean;
    userargs: string;
    uploadedFile?: string;
    uploadedType?: string;
    uploadedFiles?: {path: string, type: string}[];
    nosave: boolean;
    type: string,
    selectedLanguage?: string;
}

export interface IRunRequest {
    input: IRunRequestInput;
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
    htmlresult: string;
    iframeClientHeight: number;
    imgURL: string;
    indent!: number;
    initUserCode: boolean = false;
    isRunning: boolean = false;
    lastJS: string;
    lastMD: string;
    lastUserargs?: string;
    lastUserinput?: string;
    localcode?: string;
    muokattu: boolean;
    noeditor!: boolean;
    oneruntime?: string;
    out?: {write: () => void, writeln: () => void, canvas: Element};
    postcode?: string;
    precode?: string;
    preview!: JQuery<HTMLElement>;
    result?: string;
    runError?: string | boolean;
    runned: boolean = false;
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
    uploadedFiles = new Set((o: IUploadedFile) => this.uploadedFileName(o.path));
    uploadUrl?: string;
    userargs_: string = "";
    userinput_: string = "";
    viewCode!: boolean;
    wavURL: string = "";
    wrap!:  {n: number, auto: boolean};
    buttons: string[] = [];
    mdHtml?: string;

    iframesettings?: { src?: SafeResourceUrl; width: number; id: string; height: number };
    loadedIframe?: IFrameLoad;
    taunoFrame?: IFrameLoad;
    simcirElem?: HTMLElement;
    taunoCopy?: TimDefer<string>;
    iframedefer?: TimDefer<IFrameLoad>;
    iframemessageHandler?: (e: MessageEvent) => void;
    savedvals?: { args: string; input: string; code: string[] };
    preventSave: boolean = false;
    hide: {wrap?: boolean, changed?: boolean} = {};
    savedText: string = "";
    timeout: number = 0;
    editorModes: Mode[] = [];
    editor?: EditorComponent;
    externalEditor?: EditorComponent;
    hasExternalSources: boolean = false;
    fileSelect?: FileSelectManagerComponent;
    upload?: boolean;
    uploadByCodeFiles: {path: string, show: boolean | "loaded"}[] = [];
    @ViewChild(CountBoardComponent) countBoard?: CountBoardComponent;

    @ViewChild("externalEditor")
    set externalEditorViewSetter(new_value: EditorComponent | undefined) {
        this.externalEditor = new_value;
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
        this.editor = new_value;
        if (!this.editor) {
            return;
        }

        if (this.attrsall.submittedFiles || this.markup.files) {
            const files = new OrderedSet<EditorFile>((f) => f.path);
            const defaultMode = this.markup.mode ?? languageTypes.getAceModeType(this.type);
            if (this.markup.files) {
                const markupFiles = (listify(this.markup.files))
                        .filter((f) => f.source == "editor" || (f.source == "uploadByCode" && (f as IUploadByCodeMarkup).show === true)) as
                        (IEditorMarkup | IUploadByCodeMarkup)[];
                for (const f of markupFiles) {
                    let base: string | undefined;
                    if ("byCode" in f) {
                        this.initUserCode = true;
                        base = f.byCode;
                    }
                    const file = new EditorFile(f.path, base, f.mode ?? defaultMode, f.canClose, f.canRename);
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
                            if (((listify(this.markup.files)).find((f) => f.source == "uploadByCode" && (f as IUploadByCodeMarkup).path == file.path) as IUploadByCodeMarkup)?.show) {
                                include = true;
                            }
                        } else if(this.markup.uploadbycode && file.path == "") {
                            include = true;
                        }
                    }
                    if (include) {
                        const f = files.getByKey(file.path) ?? new EditorFile(file.path, "", defaultMode);
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
                this.savedvals.code = files.toArray().map((f) => f.content);
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
                this.savedvals.code = [this.usercode];
            }
        }
        this.editor.placeholder = this.placeholder;
        this.editor.mayAddFiles = this.markup.mayAddFiles;
        if (this.markup?.allowedPaths != "*") {
            this.editor.allowedPaths = this.editor.files.map((f) => f.path).concat(this.markup?.allowedPaths ?? []);
        }
    }

    @ViewChild(FileSelectManagerComponent)
    set fileSelectSetter(component: FileSelectManagerComponent | undefined) {
        this.fileSelect = component;
        if(!component || !this.upload) {
            return;
        }

        const files: IFileSpecification[] = [];
        if (this.markup.files) {
            const markupFiles = (listify(this.markup.files))
                    .filter((f) => f.source == "upload" || f.source == "uploadByCode") as
                    (IUploadMarkup | IUploadByCodeMarkup)[];
            for (const fs of markupFiles) {
                const paths = listify("path" in fs ? fs.path : fs.paths);
                let extensions: string[] | undefined;
                if (fs.extensions) {
                    extensions = listify(fs.extensions);
                } else {
                    const exts = paths.map((p) => "." + (p.split(".").slice(1)[0] ?? "")).filter((ext) => ext != ".");
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
                if ("show" in fs) {
                    this.uploadByCodeFiles.push(
                        ...paths.map((p) => ({path: p, show: fs.show}))
                    );
                }
            }
        } else if (this.markup.type.includes("upload") || this.markup.upload || this.markup.uploadbycode) {
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

        component.allowMultiple = this.markup.allowMultipleFiles;
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

    constructor(el: ElementRef<HTMLElement>, http: HttpClient, domSanitizer: DomSanitizer, public cdr: ChangeDetectorRef) {
        super(el, http, domSanitizer);

        this.errors = [];
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
        this.docLink = "Document";
        this.muokattu = false;
        this.editorIndex = 0;
    }

    onIframeLoad(e: Event) {
        const fr = e.target as HTMLIFrameElement & {contentWindow: WindowProxy};
        // onIframeLoad gets called twice on chrome, on the first time src is empty
        if (fr.src == "") { return; }

        const channel = new MessageChannel();
        if (this.iframemessageHandler) {
            channel.port1.onmessage = this.iframemessageHandler;
        }

        fr.contentWindow.postMessage({msg: "init"}, "*", [channel.port2]);
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
        return {saved: true, message: undefined};
    }

    formBehavior(): FormModeOption {
        return getFormBehavior(this.markup.form, FormModeOption.Undecided);
    }

    setAnswer(content: { [index: string]: unknown }): ISetAnswerResult {
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
            message = `Couldn't find related content ("usercode") from ${JSON.stringify(content)}`;
            this.error = message;
        }
        return {ok: ok, message: message};
    }

    isUnSaved() {
        return this.edited;
    }

    hasUnSavedInput(): boolean {
        if (this.savedvals == null) {
            return false;
        }
        if (this.editor) {
            const allFiles = this.editor.allFiles;
            if (allFiles.length != this.savedvals.code.length) {
                return true;
            }
            for (let i = 0; i < allFiles.length; ++i) {
                if (allFiles[i].content != this.savedvals.code[i]) {
                    return true;
                }
            }
        }
        return (this.savedvals.args !== this.userargs || this.savedvals.input !== this.userinput)
                && this.pluginMeta.getTaskId() !== undefined && !this.nosave;
    }

    /**
     * Checks whether usercode/args/input differ from previously saved values
     */
    textChanged(): void {
        this.runError = "";
        const nowUnsaved = this.hasUnSavedInput();
        if (!this.edited && nowUnsaved) {
            this.edited = true;
            this.updateListeners(ChangeType.Modified);
        } else if (this.edited && !nowUnsaved) {
            this.edited = false;
            this.updateListeners(ChangeType.Saved);
        }
    }

    updateListeners(state: ChangeType) {
        if (!this.vctrl) {
            return;
        }
        const taskId = this.pluginMeta.getTaskId();
        if (!taskId) {
            return;
        }
        this.vctrl.informChangeListeners(taskId, state, (this.markup.tag ? this.markup.tag : undefined));
    }

    tryResetChanges(): void {
        if (this.undoConfirmation && !window.confirm(this.undoConfirmation)) {
            return;
        }
        this.resetChanges();
    }

    resetChanges(): void {
        this.usercode = this.savedvals?.code[this.editor?.editorIndex ?? 0] ?? "";
        this.userargs = (this.savedvals ? this.savedvals.args : "");
        this.userinput = (this.savedvals ? this.savedvals.input : "");
        this.edited = false;
        this.updateListeners(ChangeType.Saved);
    }

    get english() {
        return this.markup.lang === "en";
    }

    get isInput() {
        return this.markup.type.includes("input") || this.markup.type.includes("args");
    }

    get isSimcir() {
        return this.markup.type.includes("simcir");
    }

    get isTauno() {
        return !!this.attrsall.isTauno;
    }

    get program() {
        return this.attrsall.program ?? this.markup.program;
    }

    get hideText() {
        return (this.english ? "Hide " : "Piilota ");
    }

    get showText() {
        return (this.english ? "Show " : "Näytä ");
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
        return this.type === "upload" && !this.markup.button ;
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
        return this.type === "upload" || this.markup.nocode;
    }

    get placeholder() {
        const tiny = this.type.includes("tiny");
        return valueDefu(this.markup.placeholder, (tiny ? "" : this.english ? "Write your code here" : "Kirjoita koodi tähän:"));
    }

    get inputplaceholder() {
        return valueOr(this.markup.inputplaceholder, (this.english ? "Write your input here" : "Kirjoita syöte tähän"));
    }

    get isText() {
        const rt = this.rtype;
        return rt === "text" || rt === "xml" || rt === "css";
    }

    get argsplaceholder() {
        return valueOr(this.markup.argsplaceholder, (this.isText ? (this.english ? "Write file name here" : "Kirjoita tiedoston nimi tähän") : (this.english ? "Write your program args here" : "Kirjoita ohjelman argumentit tähän")));
    }

    get argsstem() {
        return valueOr(this.markup.argsstem, (this.isText ? (this.english ? "File name:" : "Tiedoston nimi:") : (this.english ? "Args:" : "Args")));
    }

    get fullhtml() {
        const r = this.markup.fullhtml;
        if (!r && this.type.includes("html") || this.isProcessing) {
            return "REPLACEBYCODE";
        }
        return r;
    }

    getfullhtmlext(text: string) {
        const fh = this.fullhtml;
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
        if (typeof this.toggleEditor === "string" && this.toggleEditor.includes("|")) {
            return this.toggleEditor.split("|");
        } else {
            return this.english ? ["Edit", "Hide"] : ["Muokkaa", "Piilota"];
        }
    }

    get minRows() {
        return getInt(this.markup.rows) ?? 0;
    }

    get isAll() {
        return languageTypes.isAllType(this.type);
    }

    get glowscript() {
        return languageTypes.isInArray(this.rtype, ["glowscript", "vpython"]);
    }

    get isRun() {
        return ((languageTypes.getRunType(this.type, "") !== "" || this.isAll) && !this.markup.norun)
            || (this.type.includes("text") || this.isSimcir || this.markup.justSave)
            || this.markup.button; // or this.buttonText()?
    }

    buttonText() {
        const txt = super.buttonText();
        if (txt) {
            return txt;
        }
        if (this.markup.button === null || this.markup.buttonText === null) {
            return null;
        }
        if (this.type.includes("text") || this.isSimcir || this.markup.justSave) {
            return this.english ? "Save" : "Tallenna";
        }
        return this.english ? "Run" : "Aja";
    }

    get isExternalFetch(): boolean {
        return !!this.isRun && !!this.hasExternalSources;
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

        const r = await to2(this.http.post<IFetchResponse>(`/plugin${this.pluginMeta.getTaskIdUrl()}/fetchExternal`, {},
            {headers: new HttpHeaders({timeout: `${defaultTimeout}`})}
        ).toPromise());
        if (r.ok) {
            if (r.result.error) {
                this.fetchError = "Failed to fetch files: " + r.result.error;
                this.externalFiles = undefined;
            } else if(r.result.files.length == 0) {
                this.fetchError = "No files were available";
                this.externalFiles = undefined;
            } else {
                this.externalFiles = r.result.files;
                this.updateExternalEditor();
            }
        } else {
            this.fetchError = "Failed to fetch files: " + r.result.error;
            this.externalFiles = undefined;
        }
        this.isRunning = false;
        this.cdr.detectChanges();
    }

    get isTest() {
        return languageTypes.getTestType(this.type, this.selectedLanguage, "") !== "";
    }

    get isUnitTest() {
        return languageTypes.getUnitTestType(this.type, this.selectedLanguage, "") !== "";
    }

    get isDocument() {
        return this.type.includes("doc");
    }

    get showInput() {
        return this.type.includes("input");
    }

    get showArgs() {
        return this.type.includes("args");
    }

    get uploadstem() {
        return valueOr(this.markup.uploadstem, (this.english ? "Upload image/file" : "Lataa kuva/tiedosto"));
    }

    get file() {
        return this.markup.file;
    }

    get showCodeOn() {
        return valueDefu(this.markup.showCodeOn, (this.english ? "Show all code" : "Näytä koko koodi"));
    }

    get showCodeOff() {
        return valueOr(this.markup.showCodeOff, (this.english ? "Hide extra code" : "Piilota muu koodi"));
    }

    get resetText() {
        return valueDefu(this.markup.resetText, (this.english ? "Reset" : "Alusta"));
    }

    getTemplateButtons(): string[] {
        let b = this.markup.buttons;
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
            const langs = this.markup.languages;
            if (langs) {
                return langs.split(/[\n;, \/]/);
            } else {
                return languageTypes.runTypes.sort();
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

        this.upload = this.type === "upload" || this.markup.upload || this.markup.uploadbycode;
        if (!this.upload) {
            this.upload = listify(this.markup.files).some((f) => f?.source == "upload" || f?.source == "uploadByCode");
        }

        if (this.markup.files) {
            this.hasExternalSources = listify(this.markup.files).some((f) => !["upload", "uploadByCode", "editor"].includes(f.source));
        }

        this.vctrl = vctrlInstance!;
        this.hide = this.attrsall.markup.hide || {};
       //  if ( typeof this.markup.borders !== 'undefined' ) this.markup.borders = true;
        this.buttons = this.getTemplateButtons();
        const rt = this.rtype;
        const isText = this.isText;
        const isArgs = this.type.includes("args");
        if (this.attrsall.markup.docurl) {
            this.docURL = this.domSanitizer.bypassSecurityTrustResourceUrl(this.attrsall.markup.docurl);
            this.docLink = "Hide document";
        }

        const taskId = this.pluginMeta.getTaskId();
        if(this.upload && !this.markup.uploadbycode && taskId) {
            this.uploadUrl = `/pluginUpload/${taskId.docId}/${taskId.name}/`;
        }

        this.timeout = valueOr(this.attrsall.timeout, 0)*1000;
        this.userinput = valueOr(this.attrsall.userinput, (this.markup.userinput ?? "").toString());
        this.userargs = valueOr(this.attrsall.userargs, (this.markup.userargs ?? (isText && isArgs ? this.markup.filename ?? "" : "")).toString());
        this.selectedLanguage = this.attrsall.selectedLanguage ?? rt;
        this.noeditor = valueOr(this.markup.noeditor, this.isSimcir || (this.type === "upload"));

        const wn =  this.markup.wrap ?? (isText ? 70 : -1);
        this.wrap = { n:wn == -1 ? -1 : Math.abs(wn), auto: wn > 0 };

        this.viewCode = this.markup.viewCode;

        const editorText = [
            valueDefu(this.markup.normal, this.english ? "Normal" : "Tavallinen"),
            valueDefu(this.markup.highlight, "Highlight"),
            this.markup.parsons,
            this.markup.jsparsons,
        ];
        for (const c of this.markup.editorModes.toString()) {
            const mode = parseInt(c, 10);
            this.editorModes.push(new Mode(mode, editorText[mode]));
        }

        if (this.markup.editorMode != -1 && this.editorModes.findIndex((m) => m.id == this.markup.editorMode) == -1) {
            this.editorModes.push(new Mode(this.markup.editorMode, editorText[this.markup.editorMode]));
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
            this.uploadedFiles.push({path: this.attrsall.uploadedFile ?? "", type: this.attrsall.uploadedType ?? ""});
        }

        this.initSaved();
        this.vctrl.addTimComponent(this);
        // if (this.isText) {
        //     this.preventSave = true;
        // }
    }
    async ngAfterViewInit() {
        if (!this.usercode) {
            this.usercode = this.attrsall.usercode ?? this.byCode ?? "";
        }

        this.preview = this.element.find(".csrunPreview");
        const styleArgs = this.markup["style-args"];
        if (styleArgs) {
            const argsEdit = this.getRootElement().getElementsByClassName("csArgsArea");
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
    }

    uploadedFileName(url: string) {
        return url.split("/").slice(6).join("/");
    }

    initSaved() {
        this.savedvals = {
            code: this.editor?.files.map((f) => f.content ?? f.base) ?? [""],
            args: this.userargs,
            input: this.userinput,
        };
        this.edited = false;
        this.updateListeners(ChangeType.Saved);
    }

    isChanged(): boolean {
        return this.isUnSaved();
    }

    onContentChange(str: string) {
        super.usercode = str;
        this.checkByCodeRemove();
        if (!this.copyingFromTauno && str !== this.byCode) {
            this.muokattu = true;
        }
        this.copyingFromTauno = false;
        if (this.viewCode) {
            this.pushShowCodeNow();
        }
        this.countBoard?.count(str);
        if (this.isText) {
            this.savedText = "";
        }

        this.anyChanged();
    }

    anyChanged() {
        this.textChanged();
        if (this.runned && this.markup.autoupdate) {
            if (this.autoupdateHandle) {
                window.clearTimeout(this.autoupdateHandle);
            }
            this.autoupdateHandle = window.setTimeout(
                () => {
                    this.autoupdateHandle = undefined;
                    this.runCodeAuto();
                }, this.markup.autoupdate
            );
        }
    }

    onFileLoad(file: IFile) {
        if (this.uploadByCodeFiles.length != 0) {
            if (this.uploadByCodeFiles.find((f) => f.path == file.path)?.show) {
                this.editor?.setFileContent(file.path, file.content);
            }
        } else if (this.markup.uploadbycode) {
            this.usercode = file.content;
            if (this.markup.uploadautosave) { this.runCode(); }
        }
    }

    onUploadResponse(resp: unknown) {
        if (!resp) {return;}

        const response = resp as IUploadResponse;
        if (!this.markup.files) {
            this.uploadedFiles.clear();
        }
        this.uploadedFiles.push({path: response.file, type: response.type});
    }

    onUploadDone(success: boolean) {
        if (success && (this.markup.uploadautosave || (this.markup.type.includes("upload") && !this.markup.button) || !(this.isRun && this.buttonText()))) {
            this.doRunCode("upload", false);
        }
    }

    async processPluginMath() {
        if (!this.isMathCheck) {
            return;
        }

        await new Promise((resolve) => { setTimeout(resolve); });
        await ParCompiler.processMathJaxAsciiMath(this.element[0]);
    }

    runCodeIfCR(event: KeyboardEvent) {
        if (event.keyCode === 13) {
            this.runCode();
        }
    }

    async runCodeCommon(nosave: boolean, extraMarkUp?: IExtraMarkup) {
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
        this.docURL = undefined;
        this.docLink = "Document";
    }

    closeError() {
        this.runError = false;
    }


    hideShowEditor() {
        this.noeditor = !this.noeditor;
    }

    async doRunCode(runType: string, nosave: boolean, extraMarkUp?: IExtraMarkup) {
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
            this.usercode = await this.getCircuitData(this.simcir);
        } else if (this.taunoFrame && (!this.muokattu || !this.usercode)) {
            this.copyTauno();
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
        this.wavURL = "";
        this.runSuccess = false;
        if (!(languageTypes.isInArray(runType, csJSTypes) || this.markup.noConsoleClear)) {
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

        const editorFiles: IFileSubmission[] = this.editor?.allFiles.map((f) => ({source: "editor", ...f})) ?? [];
        const fileSelectFiles: IFileSubmission[] = this.fileSelect?.loadedFiles.toArray()
                .filter((f) =>
                    this.uploadByCodeFiles.find(
                        (f2) => f2.path == f.path
                    )?.show
                ).map((f) => ({source: "uploadByCode", ...f})) ?? [];
        const uploadedFiles: IFileSubmission[] = this.uploadedFiles.toArray().map((f) => ({source: "upload:" + f.path, path: this.uploadedFileName(f.path), type: f.type}));
        const externalFiles = this.externalFiles ?? [];

        let allFiles: IFileSubmission[] = editorFiles.concat(fileSelectFiles).concat(externalFiles);
        if (allFiles.length == 0 && !this.noeditor) {
            allFiles = [{source: "editor", path: "", content: this.usercode}];
        }

        let msg = "";
        for (const file of allFiles) {
            const m = validityCheck(file.content!.replace(/\r/g, ""));
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
                ...(this.isAll ? {selectedLanguage: this.selectedLanguage} : {}),
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
        const r = await this.postAnswer<IRunResponse>(params, new HttpHeaders({timeout: `${this.timeout + defaultTimeout}`}));
        if (r.ok) {
            this.isRunning = false;
            this.initSaved();
            const data = r.result;
            const tsruntime = ((performance.now() - t0run) / 1000).toFixed(3);
            const runtime = (data.web.runtime ?? "").trim();
            this.oneruntime = "" + tsruntime + " " + runtime.split(" ", 1)[0];
            this.runtime = "\nWhole: " + tsruntime + "\ncsPlugin: " + runtime;
            if (this.isText && data.savedNew) {
                // let savedText = "saved";
                // this.savedText = data.web.error ?? "saved";
                this.savedText = this.attrsall.markup.savedText ?? "saved";
                // this.preventSave = true;
                data.web.error = "";
            }
            if (data.web.pwd) {
                ConsolePWD.setPWD(data.web.pwd, this);
            }
            if (!noErrorClear) { this.error = data.web.error; }
            this.runSuccess = true;

            this.runError = this.error;

            const imgURL = data.web.image;
            // if ( !imgURL ) imgURL = data.web["-replyImage"];
            this.imgURL = data.web["-replyImage"] ?? "";
            this.htmlresult = (data.web["-replyHTML"] ?? "") + (data.web["-replyMD"] ?? "");
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
                this.docURL = this.domSanitizer.bypassSecurityTrustResourceUrl(docURL);
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
                    if (this.markup.isHtml) {
                        this.htmlresult = removeXML(err) + this.htmlresult;
                    } else if (!languageTypes.isInArray(runType, csJSTypes)) {
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
            this.connectionErrorMessage = this.error ?? this.markup.connectionErrorMessage ?? defaultErrorMessage;
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

    addText(s: string) {
        if (this.noeditor) {
            this.userargs += s + " ";
            return;
        }
        const text = s.replace(/\\n/g, "\n");
        this.editor?.insert?.(text);
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
            const initstr = this.markup.initSimcir;
            if (initstr) {
                const initdata = JSON.parse(initstr);
                data = {...data, ...initdata};
            }
        } catch (err) {
            this.error = err.message;
            this.runError = true;
        }

        // width and height are passed to svg viewBox attribute that needs numbers
        data.width = numOrDef(this.markup.width, 800);
        data.height = numOrDef(this.markup.height, 400);
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

    async showSimcir() {
        const v = this.getVid();
        this.simcirElem = this.element.find(".simcirContainer")[0];
        this.simcirElem.textContent = "";
        const div = document.createElement("div");
        div.id = v.vid;
        this.simcirElem.appendChild(div);
        this.simcir = $(this.simcirElem).children().first();
        await this.setCircuitData();
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
            }                      // table by it's items
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

    initCode() {
        this.muokattu = false;
        this.imgURL = "";
        this.runSuccess = false;
        this.runError = false;
        this.result = "";
        this.viewCode = this.markup.viewCode;
        if (this.editor) {
            this.editor?.reset();
        } else {
            this.usercode = this.byCode;
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
            autoeval: this.markup.autorun || firstTime,
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

    copyCode() {
        let pre = "";
        let post = "";
        let extra = false;
        if (this.viewCode && this.precode) { // TODO: get if not present?
            pre = this.precode + "\n";
            extra = true;
        }

        if (this.viewCode && this.postcode) { // TODO: get if not present?
            post = this.postcode + "\n";
            extra = true;
        }

        const usercode = this.usercode;

        // TODO: begin and end texts as a parameter and then indext picked there
        let ind = "";
        if (extra) {
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
        if (this.nocode || !(this.file || this.program)) {
            return;
        }
        const BEGINCODE = "BYCODEBEGIN";
        const ENDCODE = "BYCODEEND";
        let code = this.usercode;
        let i = code.indexOf(BEGINCODE);
        if (i >= 0) {
            const endl = code.indexOf("\n", i);
            if (endl < 0) {
                return;
            } // NO user code
            code = code.substr(endl + 1);
        }
        i = code.indexOf(ENDCODE);
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
        const r = await to($http<{ msg: string, error: string } | string>({
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
            if (this.mdHtml) {
                this.mdHtml = "";
            }
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
        const text = this.precode + "\n" + this.usercode + "\n" + this.postcode;
        if (text === this.lastMD) {
            return;
        }
        this.lastMD = text;
        const r = await this.httpPost<IPluginInfoResponse>(
            `/preview/${taskId.docId}`, {
                text: text,
            });
        if (r.ok) {
            const data = r.result;
            const element: JQuery = $($.parseHTML(data.texts) as HTMLElement[]);
            await ParCompiler.processAllMath(element);
            this.mdHtml = element.html();
        } else {
            const data = r.result;
            alert("Failed to show preview: " + data.error);
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
        if (!this.markup.runeverytime && !this.usercode && !this.userargs && !this.userinput) {
            return;
        }
        if (this.type.includes("truthtable")) {
            const truthTable = (await import("./truthTable")).truthTable;
            this.result = truthTable(this.userargs);
            return;
        }
        if (!this.iframesettings || this.fullhtml) { // create an iframe on first time
            let html = "";
            let scripts = "";
            if (this.type.includes("/vis")) {
                html = '<div id="myDiv" class="mydiv" width="800" height="400" ></div>';
                scripts = "https://cdnjs.cloudflare.com/ajax/libs/vis/4.20.0/vis.min.js";
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
            html = (this.markup.html ?? html);
            html = encodeURI(html);
            const fh = this.getfullhtmlext(this.getCode());
            this.iframesettings = {
                id: v.vid,
                width: v.width,
                height: v.height,
                src: this.domSanitizer.bypassSecurityTrustResourceUrl(fh ? getIFrameDataUrl(fh) : `${fsrc}?scripts=${this.markup.scripts ?? scripts}&html=${html}`),
            };
        }
        const text = this.usercode;
        if (!this.markup.runeverytime && text === this.lastJS && this.userargs === this.lastUserargs && this.userinput === this.lastUserinput) {
            return;
        }
        this.lastJS = text;
        this.lastUserargs = this.userargs;
        this.lastUserinput = this.userinput;

        if (!this.loadedIframe) {
            const ld = await this.waitIframeLoad();
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
        channel.port1.postMessage({
            data: {
                code: this.getCode(),
                args: this.userargs,
                input: this.userinput,
                console: this.type.includes("/c"),
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

    async waitIframeLoad(messageHandler?: (e: MessageEvent) => void): Promise<IFrameLoad | undefined> {
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

    async setData(data: any, save: boolean = false) {
        for (const key of Object.keys(data)) {
            if (key in this) {
                try {
                    if (key === "attrs") {
                        const atrs = data[key]; // TODO: make the most important to work
                        for (const akey of Object.keys(atrs)) {
                            // @ts-ignore
                            this.markup[akey] = atrs[akey];
                        }
                    } else if (key === "commands") {
                        // TODO: implement commands
                    } else {
                        // @ts-ignore
                        this[key] = data[key];
                    }
                }
                catch(err) {
                    console.log(err);
                }
            }
        }
        if (save) {
            await this.runCode();
        }
        return;
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

@Component({
    selector: "cs-runner",
    template: `
<div [ngClass]="{'csRunDiv': markup.borders}" class="type-{{rtype}}">
    <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
    <h4 *ngIf="header" [innerHTML]="header"></h4>
    <p *ngIf="stem" class="stem" [innerHTML]="stem"></p>
    <div *ngIf="isTauno">
        <p *ngIf="taunoOn" class="pluginHide"><a (click)="hideTauno()">{{hideText}} Tauno</a></p>
        <iframe *ngIf="iframesettings"
                id="iframesettings.id"
                class="showTauno"
                [src]="iframesettings.src"
                (load)="onIframeLoad($event)"
                [width]="iframesettings.width"
                [height]="iframesettings.height"
                sandbox="allow-scripts"></iframe>
        <p *ngIf="!taunoOn" class="pluginShow"><a (click)="showTauno()">{{showText}} Tauno</a></p>
        <p *ngIf="taunoOn" class="pluginHide">
            <a (click)="copyTauno()">{{copyFromTaunoText}}</a> |
            <a (click)="hideTauno()">{{hideText}} Tauno</a></p>
        <p *ngIf="taunoOn" class="taunoOhje">
            {{taunoOhjeText}}</p>
    </div>
    <div *ngIf="isSimcir">
        <p *ngIf="simcirOn" class="pluginHide"><a (click)="hideSimcir()">{{hideText}} SimCir</a></p>
        <div class="simcirContainer"><p></p></div>
        <p *ngIf="!simcirOn" class="pluginShow"><a (click)="showSimcir()">{{showText}} SimCir</a></p>
        <p *ngIf="simcirOn && !noeditor" class="pluginHide">
            <a (click)="copyFromSimcir()">copy from SimCir</a>
            | <a (click)="copyToSimcir()">copy to SimCir</a> | <a (click)="hideSimcir()">hide SimCir</a>
        </p>
    </div>
    <ng-container *ngIf="upload">
        <file-select-manager class="small"
                [dragAndDrop]="markup.dragAndDrop"
                [uploadUrl]="uploadUrl"
                [stem]="uploadstem"
                (file)="onFileLoad($event)"
                (upload)="onUploadResponse($event)"
                (uploadDone)="onUploadDone($event)">
        </file-select-manager>
        <div class="form-inline small">
            <span *ngFor="let item of uploadedFiles">
                <cs-upload-result [src]="item.path" [type]="item.type"></cs-upload-result>
            </span>
        </div>
    </ng-container>
    <div *ngIf="isAll" style="float: right;">{{languageText}}
        <select [(ngModel)]="selectedLanguage" required>
            <option *ngFor="let o of progLanguages" [value]="o">{{o}}</option>
        </select>
    </div>
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
                    [placeholder]="inputplaceholder">
            </textarea>
        </div>
    </div>
    <div class="csArgsDiv" *ngIf="showArgs && isInput"><label>{{argsstem}} </label>
        <span><input type="text"
                    class="csArgsArea"
                    [(ngModel)]="userargs"
                    [placeholder]="argsplaceholder"></span>
    </div>
    <cs-count-board *ngIf="markup.count" [options]="markup.count"></cs-count-board>
    <p class="csRunSnippets" *ngIf="buttons">
        <button *ngFor="let item of buttons" (click)="addText(item)">{{addTextHtml(item)}}</button>
        &nbsp;&nbsp;
    </p>
    <cs-editor #externalEditor *ngIf="externalFiles && externalFiles.length" class="csrunEditorDiv"
            [maxRows]="markup.maxrows"
            [disabled]="true">
    </cs-editor>
    <div class="csRunMenuArea" *ngIf="!forcedupload">
        <p class="csRunMenu">
            <button *ngIf="isRun && buttonText()"
                    [disabled]="isRunning || preventSave || (markup.disableUnchanged && !isUnSaved() && isText)"
                    class="timButton btn-sm"
                    title="(Ctrl-S)"
                    (click)="runCode()"
                    [innerHTML]="buttonText()"></button>
            &nbsp;
            <button *ngIf="isExternalFetch"
                    [disabled]="isRunning"
                    class="timButton btn-sm"
                    (click)="fetchExternalFiles()"
                    [innerHTML]="externalFetchText()"></button>
            <a href="#" *ngIf="undoButton && isUnSaved()" title="undoTitle"
                    (click)="tryResetChanges(); $event.preventDefault()"> &nbsp;{{undoButton}}</a>
            &nbsp;&nbsp;
            <span *ngIf="savedText"
                    class="savedText"
                    [innerHTML]="savedText"></span>
            &nbsp;&nbsp;
            <button *ngIf="isTest"
                    [disabled]="isRunning"
                    (click)="runTest()"
                    class="timButton btn-sm">Test</button>
            &nbsp;&nbsp;
            <button *ngIf="isUnitTest"
                    class="timButton btn-sm"
                    [disabled]="isRunning"
                    (click)="runUnitTest()">UTest
            </button>
            <tim-loading *ngIf="isRunning"></tim-loading>
            &nbsp;&nbsp;
            <span *ngIf="isDocument">
                <a href="#" [ngClass]="{'link-disable': isRunning}"
                        (click)="runDocument(); $event.preventDefault()">{{docLink}}</a>&nbsp;&nbsp;
            </span>
            <a href="#" *ngIf="!nocode && (file || program)"
                    (click)="showCode(); $event.preventDefault()">{{showCodeLink}}</a>&nbsp;&nbsp;
            <a href="#" *ngIf="editor && editor.modified"
                    (click)="initCode(); $event.preventDefault()">{{resetText}}</a>
            <a href="#" *ngIf="toggleEditor"
                    (click)="hideShowEditor(); $event.preventDefault()">{{toggleEditorText[noeditor ? 0 : 1]}}</a>
            <a href="#" *ngIf="!noeditor && editor && editor.nextModeText"
                    (click)="editor?.showOtherEditor(); $event.preventDefault()">
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
                &nbsp;
                <input type="checkbox" title="Check for automatic wrapping" [(ngModel)]="wrap.auto" style="position: relative;top: 0.3em;"/>
                &nbsp;
                <input type="text" title="Choose linelength for text.  0=no wrap" pattern="[0-9]*" [(ngModel)]="wrap.n" size="2"/>
            </span>
            <span *ngIf="connectionErrorMessage" class="error" style="font-size: 12px" [innerHTML]="connectionErrorMessage"></span>

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
    <pre *ngIf="viewCode && codeunder">{{code}}</pre>
    <p class="unitTestGreen" *ngIf="runTestGreen">&nbsp;ok</p>
    <pre class="unitTestRed" *ngIf="runTestRed">{{comtestError}}</pre>
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
    <pre class="console" *ngIf="result">{{result}}</pre>
    <div class="htmlresult" *ngIf="htmlresult"><span [innerHTML]="htmlresult | purify"></span></div>
    <div class="csrunPreview">
        <div *ngIf="iframesettings && !isTauno"
                tim-draggable-fixed
                caption="Preview"
                detachable="true"
                class="no-popup-menu">
            <span class="csRunMenu">
                <tim-close-button
                        (click)="closeFrame()"
                        style="float: right">
                </tim-close-button>
            </span>
            <iframe id="iframesettings.id"
                    class="jsCanvas"
                    [src]="iframesettings.src"
                    (load)="onIframeLoad($event)"
                    [width]="iframesettings.width"
                    [height]="iframesettings.height"
                    sandbox="allow-scripts allow-forms"
                    style="border:0">
            </iframe>
        </div>
        <div *ngIf="mdHtml" [innerHTML]="mdHtml | purify">
        </div>
    </div>
    <img *ngIf="imgURL" class="grconsole" [src]="imgURL" alt=""/>
    <video *ngIf="wavURL" [src]="wavURL" type="video/mp4" controls="" autoplay="true" width="300"
            height="40"></video>
    <div *ngIf="docURL" class="docurl">
        <p class="pull-right">
            <tim-close-button (click)="closeDocument()"></tim-close-button>
        </p>
        <iframe width="800" height="600" [src]="docURL" target="csdocument" allowfullscreen></iframe>
    </div>
    <p class="footer" [innerHTML]="markup.footer"></p>
</div>`,
})
export class CsRunnerComponent extends CsController {
    constructor(el: ElementRef<HTMLElement>, http: HttpClient, domSanitizer: DomSanitizer, cdr: ChangeDetectorRef) {
        super(el, http, domSanitizer, cdr);
    }
}
