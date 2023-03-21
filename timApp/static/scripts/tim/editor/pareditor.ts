import type {IScope} from "angular";
import angular from "angular";
import * as t from "io-ts";
import $ from "jquery";
import rangyinputs from "rangyinputs";
import {setCurrentEditor} from "tim/editor/editorScope";
import {markAsUsed, TimStorage, to, to2} from "tim/util/utils";
import {DialogController} from "tim/ui/dialogController";
import {showRestampDialog} from "tim/editor/showRestampDialog";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {
    fullscreenSupported,
    getFullscreenElement,
    toggleFullScreen,
} from "tim/util/fullscreen";
import {replaceTemplateValues} from "tim/ui/showTemplateReplaceDialog";
import type {IDocument, ILanguage, ITranslator} from "tim/item/IItem";
import {
    updateTranslationData,
    updateTranslatorLanguages,
} from "tim/document/languages";
import type {IExtraData, ITags} from "tim/document/editing/edittypes";
import type {IDocSettings, MeetingDateEntry} from "tim/document/IDocSettings";
import {getCitePar} from "tim/document/parhelpers";
import type {ViewCtrl} from "tim/document/viewctrl";
import {registerDialogComponentForModule} from "tim/ui/dialog";
import type {IMeetingMemoSettings} from "tim/util/globals";
import {documentglobals, genericglobals} from "tim/util/globals";
import {$compile, $http, $injector, $timeout, $upload} from "tim/util/ngimport";
import {AceParEditor} from "tim/editor/AceParEditor";
import type {SelectionRange} from "tim/editor/BaseParEditor";
import {EditorType} from "tim/editor/BaseParEditor";
import type {IPluginInfoResponse} from "tim/editor/parCompiler";
import {ParCompiler} from "tim/editor/parCompiler";
import {RestampDialogClose} from "tim/editor/restamp-dialog.component";
import {TextAreaParEditor} from "tim/editor/TextAreaParEditor";

markAsUsed(rangyinputs);

const TIM_TABLE_CELL = "timTableCell";

export interface ITag {
    name: keyof ITags;
    desc: string;
    title?: string;
}

export interface IChoice {
    desc: string;
    name: string;
    title?: string;
    opts: Array<{desc: string; value: string; title?: string}>;
}

export interface IEditorParams {
    initialText?: string;
    viewCtrl?: ViewCtrl;
    defaultSize: "sm" | "md" | "lg";
    extraData: IExtraData;
    options: {
        deleteMsg?: string;
        caption: string;
        localSaveTag: string;
        showDelete: boolean;
        showPlugins: boolean;
        showSettings: boolean;
        showImageUpload: boolean;
        touchDevice: boolean;
        tags: ITag[];
        choices?: IChoice[];
        cursorPosition?: number;
    };
    previewCb: (
        text: string,
        proofread: boolean
    ) => Promise<IPluginInfoResponse>;
    saveCb: (text: string, data: IExtraData) => Promise<{error?: string}>;
    deleteCb: () => Promise<{error?: string}>;
    unreadCb: () => Promise<void>;
}

interface IEditorMenuItem {
    title: string;
    func: (e: Event) => void;
    name: string;
}

interface IEditorMenu {
    title: string;
    items: IEditorMenuItem[];
}

type EditorEntry = IEditorMenu | IEditorMenuItem;

export type IEditorResult =
    | {type: "save"; text: string}
    | {type: "delete"}
    | {type: "markunread"}
    | {type: "cancel"};

const MenuItemFileObject = t.intersection([
    t.type({
        file: t.string,
    }),
    t.partial({
        expl: t.string,
        text: t.string,
    }),
]);

const MenuItemDataObject = t.intersection([
    t.type({
        data: t.string,
    }),
    t.partial({
        expl: t.string,
        text: t.string,
    }),
]);

const MenuItemObject = t.union([MenuItemDataObject, MenuItemFileObject]);

export interface IAttachmentData {
    issueNumber: string | number;
    attachmentLetter: string;
    uploadUrl: string; // Could be empty string or placeholder.
    upToDate: boolean; // Whether the stamp data hasn't changed after previous stamping.
}

export interface IStampingData {
    attachments: IAttachmentData[];
    customStampModel: string | undefined;
    stampFormat: string;
    meetingDate: string;
}

const MenuItemR = t.union([MenuItemObject, t.string]);

type MenuItem = t.TypeOf<typeof MenuItemR>;

const MenuItemEntriesR = t.union([MenuItemR, t.array(MenuItemR)]);
type MenuItemEntries = t.TypeOf<typeof MenuItemEntriesR>;

const EditorTabContent = t.record(t.string, MenuItemEntriesR);

const EditorTemplateFormatV1 = t.type({
    text: t.array(t.string),
    templates: t.array(t.array(MenuItemR)),
});

const EditorTemplateFormatV2 = t.type({
    templates: EditorTabContent,
});

function EditorItem<T extends t.Any>(x: T) {
    return t.intersection([
        t.type({
            text: t.string,
            items: x,
        }),
        t.partial({
            shortcut: t.string,
        }),
    ]);
}

const EditorTemplateFormatV3 = t.type({
    editor_tabs: t.array(EditorItem(t.array(EditorItem(MenuItemEntriesR)))),
});

interface IEditorTab {
    entries: EditorEntry[];
    extra?: string;
    name: string;

    show?(): boolean;
}

type MenuNameAndItems = Array<[string, MenuItemEntries]>;

export interface ISpellWordInfo {
    word: string;
    occurrence: number;
    blockIndex: number;
    suggestions: string[];
}

export class PareditorController extends DialogController<
    {params: IEditorParams},
    IEditorResult
> {
    static component = "pareditor";
    static $inject = ["$element", "$scope"] as const;
    private spellcheck = false;
    private deleting = false;
    private editor?: TextAreaParEditor | AceParEditor; // $onInit
    private file?: File & {progress?: number; error?: string};
    private isIE: boolean = false;
    private oldmeta?: HTMLMetaElement;
    private wrap!: {n: number; auto: boolean}; // $onInit
    private outofdate: boolean;
    private parCount: number;
    private proeditor!: boolean; // $onInit
    private saving: boolean = false;
    private scrollPos?: number;
    private storage!: {
        noteAccess: TimStorage<string>;
        acebehaviours: TimStorage<boolean>;
        acewrap: TimStorage<boolean>;
        autocomplete: TimStorage<boolean>;
        proeditor: TimStorage<boolean>;
        spellcheck: TimStorage<boolean>;
        editortab: TimStorage<string>;
        wrap: TimStorage<string>;
        oldMode: TimStorage<string>;
        diffSideBySide: TimStorage<boolean>;
        translator: TimStorage<string>;
    };
    private touchDevice: boolean;
    private autocomplete!: boolean; // $onInit
    private citeText!: string; // $onInit
    private docSettings?: IDocSettings;
    private memoMinutesSettings?: IMeetingMemoSettings;
    private uploadedFile?: string;
    private activeTab?: string;
    private lastTab?: string;
    private tabs: IEditorTab[];
    private trdiff?: {old: string; new: string};
    private activeAttachments?: IAttachmentData[]; // Attachments (with stamps) currently in the editor.
    private liiteMacroStringBegin = "%%liite("; // Attachment macro with stamping.
    private liiteMacroStringEnd = ")%%"; // TODO: May be confused with other macro endings.
    private perusliiteMacroStringBegin = "%%perusliite("; // Attachment macro without stamping.
    private perusliiteMacroStringEnd = ")%%";
    private lastKnownDialogHeight?: number;
    private sourceLanguages: ILanguage[] = [];
    private targetLanguages: ILanguage[] = [];
    private documentLanguages: ILanguage[] = [];
    private translators: ITranslator[] = [];
    private docTranslator: string = "";
    private translatorAvailable = true;
    private sideBySide: boolean = false;
    private hideDiff: boolean = true;
    private hidePreview: boolean = false;
    private hideOriginalPreview: boolean = false;
    private translationInProgress: boolean = false;
    private nothingSelected: boolean = false;
    private errorMessage = "";
    private availableTranslators: string[] = [];
    private originalDocument: boolean = true;

    private formulaEditorOpen: boolean = false;

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);

        if (navigator.userAgent.match(/Trident/i)) {
            this.isIE = true;
        }

        this.getEditorContainer().on("resize", () => this.adjustPreview());

        const backTicks = "```";

        const tables = {
            normal: `
Otsikko1 Otsikko2 Otsikko3 Otsikko4
-------- -------- -------- --------
1.rivi   x        x        x
2.rivi   x        x        x       `.trim(),

            example: `
Table:  Otsikko taulukolle

Otsikko    Vasen laita    Keskitetty    Oikea laita
---------- ------------- ------------ -------------
1. rivi      2                  3         4
2. rivi        1000      2000             30000`.trim(),

            noheaders: `
:  Otsikko taulukolle

---------- ------------- ------------ -------------
1. rivi      2                  3         4
2. rivi        1000      2000             30000
---------- ------------- ------------ -------------
`.trim(),

            multiline: `
Table:  Otsikko taulukolle voi\\
jakaantua usealle riville

-----------------------------------------------------
Ekan       Toisen         kolmas            neljäs\\
sarakkeen  sarakkeen     keskitettynä      oikeassa\\
otsikko    otsikko                           reunassa
---------- ------------- -------------- -------------
1. rivi     toki              3         4
voi olla    sisältökin
useita      voi\\
rivejä      olla
            monella\\
            rivillä

2. rivi        1000      2000             30000
-----------------------------------------------------
`.trim(),

            strokes: `
: Viivoilla tehty taulukko

+---------------+---------------+----------------------+
| Hedelmä       | Hinta         | Edut                 |
+===============+===============+======================+
| Banaani       |  1.34 €       | - valmis kääre       |
|               |               | - kirkas väri        |
+---------------+---------------+----------------------+
| Appelsiini    |  2.10 €       | - auttaa keripukkiin |
|               |               | - makea              |
+---------------+---------------+----------------------+
`.trim(),

            pipe: `
: Taulukko, jossa tolpat määräävät sarakkeiden paikat.

| Oikea | Vasen | Oletus  | Keskitetty |
|------:|:------|---------|:----------:|
|   12  |  12   |    12   |    12      |
|  123  |  123  |   123   |   123      |
|    1  |    1  |     1   |     1      |
`.trim(),

            timTable1x1: `
${backTicks} {plugin="timTable"}
table:
    countRow: 1
    countCol: 1
    rows:
      - row:
        - cell: ''
${backTicks}
`.trim(),

            timTable2x2: `
${backTicks} {plugin="timTable"}
# forcedEditMode: true
table:
    countRow: 2
    countCol: 2
    rows:
      - row:
        - cell: 'Otsikko 1'
        - cell: 'Otsikko 2'
        class: headers
        fontWeight: bold
      - row:
        - cell: 'Solu 1'
        - cell: 'Solu 2'
${backTicks}
`.trim(),
        };

        this.tabs = [
            {
                entries: [
                    {
                        title: "Undo",
                        name: "&#8630;",
                        func: () => this.editor!.undoClicked(),
                    },
                    {
                        title: "Redo",
                        name: "&#8631;",
                        func: () => this.editor!.redoClicked(),
                    },
                    {
                        title: "Move left",
                        name: "&#8592;",
                        func: () => this.editor!.leftClicked(),
                    },
                    {
                        title: "Move right",
                        name: "&#8594;",
                        func: () => this.editor!.rightClicked(),
                    },
                    {
                        title: "Move up",
                        name: "&#8593;",
                        func: () => this.editor!.upClicked(),
                    },
                    {
                        title: "Move down",
                        name: "&#8595;",
                        func: () => this.editor!.downClicked(),
                    },
                    {
                        title: "Move to the beginning of the line",
                        name: "Home",
                        func: () => this.editor!.homeClicked(),
                    },
                    {
                        title: "Move to the end of the line",
                        name: "End",
                        func: () => this.editor!.endClicked(),
                    },
                    {
                        title: "Move to the beginning of the file",
                        name: "Top",
                        func: () => this.editor!.topClicked(),
                    },
                    {
                        title: "Move to the end of the file",
                        name: "Bottom",
                        func: () => this.editor!.bottomClicked(),
                    },
                    {
                        title: "Toggle insert mode on or off",
                        name: "Ins",
                        func: () => this.editor!.insertClicked(),
                    },
                ],
                name: "Navigation",
            },
            {
                // To change keys, change also:  TextAreaParEditor.ts ja AceParEditor.ts and maybe keycodes.ts
                entries: [
                    {
                        title: "Indent selection/line",
                        func: () => this.editor!.indentClicked(),
                        name: "&#8649;",
                    },
                    {
                        title: "Outdent selection/line",
                        func: () => this.editor!.outdentClicked(),
                        name: "&#8647;",
                    },
                    {
                        title: "Bold (Ctrl-B)",
                        func: () => this.editor!.surroundClicked("**", "**"),
                        name: "<b>B</b>",
                    },
                    {
                        title: "Italic (Ctrl-I)",
                        func: () => this.editor!.italicSurroundClicked(),
                        name: "<i>I</i>",
                    },
                    {
                        title: "Underline",
                        func: () => this.editor!.surroundClicked("<u>", "</u>"),
                        name: "<u>U</u>",
                    },
                    {
                        title: "Strikethrough",
                        func: () => this.editor!.surroundClicked("<s>", "</s>"),
                        name: "<s>Z</s>",
                    },
                    {
                        title: "Color",
                        items: [
                            {
                                title: "Red text",
                                func: () =>
                                    this.editor!.styleClicked("Teksti", "red"),
                                name: "Red",
                            },
                            {
                                title: "Blue text",
                                func: () =>
                                    this.editor!.styleClicked("Teksti", "blue"),
                                name: "Blue",
                            },
                            {
                                title: "Green text",
                                func: () =>
                                    this.editor!.styleClicked("Teksti", "lime"),
                                name: "Green",
                            },
                            {
                                title: "Red background",
                                func: () =>
                                    this.editor!.styleClicked(
                                        "Teksti",
                                        "bgred"
                                    ),
                                name: "Red bg",
                            },
                            {
                                title: "Blue background",
                                func: () =>
                                    this.editor!.styleClicked(
                                        "Teksti",
                                        "bgblue"
                                    ),
                                name: "Blue bg",
                            },
                            {
                                title: "Green background",
                                func: () =>
                                    this.editor!.styleClicked(
                                        "Teksti",
                                        "bglime"
                                    ),
                                name: "Green bg",
                            },
                            {
                                title: "Yellow background",
                                func: () =>
                                    this.editor!.styleClicked(
                                        "Teksti",
                                        "bgyellow"
                                    ),
                                name: "Yellow bg",
                            },
                            {
                                title: "Button",
                                func: () =>
                                    this.editor!.styleClicked(
                                        "Teksti",
                                        "button"
                                    ),
                                name: "Button",
                            },
                            {
                                title: "Tim button",
                                func: () =>
                                    this.editor!.styleClicked(
                                        "Teksti",
                                        "timButton"
                                    ),
                                name: "Tim button",
                            },
                            {
                                title: "Answer text",
                                func: () =>
                                    this.editor!.styleClicked(
                                        "Teksti",
                                        "answer"
                                    ),
                                name: "Answer",
                            },
                        ],
                    },
                    {
                        title: "Code (Ctrl-O)",
                        func: () => this.editor!.surroundClicked("`", "`"),
                        name: "Code",
                    },
                    {
                        title: "Code block (Ctrl-Alt-O)",
                        func: () => this.editor!.codeBlockClicked(),
                        name: "Code block",
                    },
                    {
                        title: "Subscript",
                        func: () => this.editor!.surroundClicked("~", "~"),
                        name: "X_",
                    },
                    {
                        title: "Superscript",
                        func: () => this.editor!.surroundClicked("^", "^"),
                        name: "X^",
                    },
                    {
                        title: "Heading 1 (Ctrl-1)",
                        func: () => this.editor!.headerClicked("#"),
                        name: "H1",
                    },
                    {
                        title: "Heading 2 (Ctrl-2)",
                        func: () => this.editor!.headerClicked("##"),
                        name: "H2",
                    },
                    {
                        title: "Heading 3 (Ctrl-3)",
                        func: () => this.editor!.headerClicked("###"),
                        name: "H3",
                    },
                    {
                        title: "Heading 4 (Ctrl-4)",
                        func: () => this.editor!.headerClicked("####"),
                        name: "H4",
                    },
                    {
                        title: "Heading 5 (Ctrl-5)",
                        func: () => this.editor!.headerClicked("#####"),
                        name: "H5",
                    },
                    {
                        title: "No translation",
                        func: () =>
                            this.editor!.styleClicked("Teksti", "notranslate"),
                        name: "No translation",
                    },
                ],
                name: "Style",
            },
            {
                entries: [
                    {
                        title: "Add link",
                        func: () =>
                            this.editor!.linkClicked(
                                "Linkkiteksti",
                                "Linkkiosoite",
                                false
                            ),
                        name: "Link",
                    },
                    {
                        title: "Add image",
                        func: () =>
                            this.editor!.linkClicked(
                                "Kuvateksti",
                                "Kuvasoite",
                                true
                            ),
                        name: "Image",
                    },
                    {
                        title: "List item",
                        func: () => this.editor!.listClicked(),
                        name: "List",
                    },
                    {
                        title: "Slide",
                        items: [
                            {
                                name: "Slide break",
                                title: "Break text to start a new slide. In browser, slides scroll horizontally.",
                                func: () => this.editor!.ruleClicked(false),
                            },
                            {
                                name: "Subslide break",
                                title: "Break text to start a subslide within the same slide group. In browser, subslides scroll vertically.",
                                func: () => this.editor!.ruleClicked(true),
                            },
                            {
                                name: "Slide fragment",
                                title: "Content inside the fragment will be hidden and shown when next is clicked in slide view",
                                func: () =>
                                    this.editor!.surroundClicked("§§", "§§"),
                            },
                            {
                                name: "Fragment block",
                                title: "Content inside will show as a fragment and may contain inner slide fragments",
                                func: () =>
                                    this.editor!.surroundClicked("<§", "§>"),
                            },
                        ],
                    },
                    {
                        title: "Table",
                        items: Object.entries(tables).map(([k, v]) => ({
                            name: k,
                            title: "",
                            func: () => {
                                this.editor!.insertTemplate(v);
                            },
                        })),
                    },
                    {
                        title: "Breaks",
                        items: [
                            {
                                title: "Break text to start a new paragraph (Shift-Enter)",
                                func: () => this.editor!.paragraphClicked(),
                                name: "Paragraph break",
                            },
                            {
                                title: "Forces line to end at cursor position (Ctrl-Enter)",
                                func: () => this.editor!.endLineClicked(),
                                name: "End line",
                            },
                            {
                                title: "Creates a page break for web printing",
                                func: () => this.editor!.pageBreakClicked(),
                                name: "Page break",
                            },
                        ],
                    },
                    {
                        title: "Numbering",
                        items: [
                            {
                                name: "c_ - general counter",
                                title: "General counter",
                                func: () =>
                                    this.editor!.surroundClicked(
                                        '%%"',
                                        '"|c_("")%%'
                                    ),
                            },
                            {
                                name: "c_eq - equation counter",
                                title: "Equation counter for LaTeX",
                                func: () =>
                                    this.editor!.surroundClicked(
                                        '%%"',
                                        '"|c_eq%%'
                                    ),
                            },
                            {
                                name: "c_tag - tag counter for LaTeX",
                                title: "Equation counter for LaTeX for for one line",
                                func: () =>
                                    this.editor!.surroundClicked(
                                        '%%"',
                                        '"|c_tag%%\\\\'
                                    ),
                            },
                            {
                                name: "§\\ - autonamed tag-counter for LaTeX",
                                title: "Autonamed equation counter for LaTeX for end of line (tag)",
                                func: () =>
                                    this.editor!.surroundClicked("§\\", ""),
                            },
                            {
                                name: "§a - autonamed, label",
                                title: "To be used as an autonamed counter with label",
                                func: () =>
                                    this.editor!.surroundClicked("§a", ""),
                            },
                            {
                                name: "§n - autonamed, no label",
                                title: "To be used as an autonamed counter without label",
                                func: () =>
                                    this.editor!.surroundClicked("§n", ""),
                            },
                            {
                                name: "{§/§} - named counter",
                                title: "Named counter",
                                func: () =>
                                    this.editor!.surroundClicked("{§", "§}"),
                            },
                            {
                                name: "c_fig - figure counter",
                                title: "Figure counter",
                                func: () =>
                                    this.editor!.surroundClicked(
                                        '%%"',
                                        '"|c_fig%%'
                                    ),
                            },
                            {
                                name: "c_task - task counter",
                                title: "Task counter",
                                func: () =>
                                    this.editor!.surroundClicked(
                                        '%%"',
                                        '"|c_task%%'
                                    ),
                            },
                            {
                                name: "ref - ref to counter",
                                title: "Reference to counter",
                                func: () =>
                                    this.editor!.surroundClicked(
                                        '%%"',
                                        '"|ref%%'
                                    ),
                            },
                            {
                                name: "c_begin1/c_end - LaTeX environment, on formula",
                                title: "LaTeX begin equation* / end with tag-counter and label",
                                func: () =>
                                    this.editor!.surroundClicked(
                                        '%%"',
                                        '"| c_begin1("equation*")%%\nf\n%%""|c_end%%\n'
                                    ),
                            },
                            {
                                name: "c_begin/c_end - LaTeX environment",
                                title: "LaTeX begin align* / end with tag-counter and label",
                                func: () =>
                                    this.editor!.surroundClicked(
                                        '%%"',
                                        '"| c_begin("align*")%%\nf §\\\n%%""|c_end%%\n'
                                    ),
                            },
                            {
                                name: "c_auto - start autonaming",
                                title: "begin autonaming, give basename and counter type name",
                                func: () =>
                                    this.editor!.surroundClicked(
                                        '%%"',
                                        '"| c_auto("fig")%%\n'
                                    ),
                            },
                            {
                                name: "labels - list counternams used as hypelinks",
                                title: "List of counter names that can be hyperlink targets",
                                func: () =>
                                    this.editor!.surroundClicked(
                                        '%%["',
                                        ' "]| labels%%\n'
                                    ),
                            },
                        ],
                    },
                    {
                        title: "Creates a comment block or sets the line to a comment (Ctrl-Y)",
                        func: () => this.editor!.commentClicked(),
                        name: "Comment",
                    },
                ],
                name: "Insert",
            },
            {
                entries: [
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "@",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "#",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "`",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "$",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "€",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "%",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "&",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "{",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "}",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "[",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "]",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "/",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event),
                        name: "\\",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.insertTemplate("&shy;"),
                        name: "Soft hyphen",
                    },
                    {
                        title: "",
                        func: ($event) => this.editor!.charClicked($event, "⁞"),
                        name: "Cursor",
                    },
                ],
                name: "Characters",
            },
            {
                entries: [],
                name: "TeX",
            },
            {
                entries: [
                    {
                        title: "Auto number headings level",
                        func: () =>
                            this.editor!.insertTemplate(
                                "auto_number_headings: 1\n"
                            ),
                        name: "Heading numbering",
                    },
                    {
                        title: "Use document like a form",
                        func: () =>
                            this.editor!.insertTemplate("form_mode: true\n"),
                        name: "Form mode",
                    },
                    {
                        title: "Styles for this document",
                        func: () =>
                            this.editor!.insertTemplate(
                                "css: |!!\n.style {\n\n}\n!!\n"
                            ),
                        name: "CSS",
                    },
                    {
                        title: "Macro values for document",
                        func: () =>
                            this.editor!.insertTemplate(
                                "macros:\n  key: value\n"
                            ),
                        name: "Macros",
                    },
                    {
                        title: "Show task summary in the beginning of the doc",
                        func: () =>
                            this.editor!.insertTemplate(
                                "show_task_summary: true\n"
                            ),
                        name: "Task summary",
                    },
                    {
                        title: "Update document automatically at this interval (seconds)",
                        func: () =>
                            this.editor!.insertTemplate("live_updates: 5\n"),
                        name: "Live update",
                    },
                    {
                        title: "Calculate plugins lazy or not",
                        func: () =>
                            this.editor!.insertTemplate("lazy: false\n"),
                        name: "Lazy",
                    },
                    {
                        title: "Global values for the plugins",
                        func: () =>
                            this.editor!.insertTemplate(
                                "global_plugin_attrs:\n csPlugin:\n   stem: value\n all:\n   stem: value\n"
                            ),
                        name: "Global plugin",
                    },
                    {
                        title: "SVG images for math",
                        func: () =>
                            this.editor!.insertTemplate("math_type: svg\n"),
                        name: "Math SVG",
                    },
                    {
                        title: "Insert new editor templates",
                        func: () =>
                            this.editor!.insertTemplate(
                                `editor_templates:
    editor_tabs:
     - text: VISIBLE TEXT FOR NEW TAB
       items:
          - text: VISIBLE TEXT FOR NEW MENU ITEM
            items:
              - data: TEXT THAT COMES FROM SUBMENU
                text: VISIBLE TEXT FOR SUBMENU (optional)
                expl: EXPLANATION FOR ITEM (optional)
`
                            ),
                        name: "Editor templates",
                    },
                    {
                        title: "Autonumber settings, if macros already exits, remove macros-word",
                        func: () =>
                            this.editor!.insertTemplate(
                                `macros: 
  autocounters:
    all:
      reset: 2  
    task:
      reset: 2
      show: "Tehtävä {p}"
      ref: "tehtävä {p}"
    fig:
      reset: 2
      show: "Kuva {p}"
      ref: "kuva {p}"
      prexif: "fig:"
    eq:
      reset: 2
      ref: "kaava ({p})"
      long: "kaava ({p})"
`
                            ),
                        name: "Autonumber settings",
                    },
                ],
                name: "Settings",
                show: () => this.getOptions().showSettings,
                extra: `
<a href="https://tim.jyu.fi/view/tim/ohjeita/documentin-asetukset#settings_list"
   target="_blank"
   title="Help for settings">
   <i class="helpButton glyphicon glyphicon-question-sign"></i>
</a>`,
            },
            {
                name: "Plugins",
                entries: [],
                show: () => this.getOptions().showPlugins,
                extra: `
<a class="helpButton"
   title="Help for plugin attributes"
   href="https://tim.jyu.fi/view/tim/ohjeita/csPlugin"
   target="_blank">
    <i class="helpButton glyphicon glyphicon-question-sign"></i>
</a>
            `,
            },
            {
                name: "Upload",
                entries: [],
            },
        ];

        $(document).on(
            "webkitfullscreenchange fullscreenchange MSFullscreenChange",
            (event) => {
                const editor = element[0];
                if (!getFullscreenElement()) {
                    editor.removeAttribute("style");
                }
            }
        );

        this.outofdate = false;
        this.parCount = 0;
        this.touchDevice = false;
    }

    getEditor() {
        return this.editor;
    }

    getVisibleTabs() {
        // Upload tab is shown separately in the template because
        // it has special content that cannot be placed under "extra".

        return this.tabs.filter(
            (tab) => (!tab.show || tab.show()) && tab.name !== "Upload"
        );
    }

    getUploadTab() {
        return this.findTab("upload");
    }

    findTab(name: string) {
        return this.tabs.find(
            (tab) => tab.name.toLowerCase() === name.toLowerCase()
        );
    }

    getTabIndex(tab: IEditorTab) {
        return tab.name.toLowerCase();
    }

    $doCheck() {
        if (this.lastTab != this.activeTab) {
            this.lastTab = this.activeTab;
            this.wrapFn();
        }
    }

    wrapValue() {
        return this.wrap.n * (this.wrap.auto ? 1 : -1);
    }

    /**
     * Tracks the editing and Difference in original document views' positioning (see pareditor.html).
     */
    changePositioning() {
        if (this.originalDocument) {
            return;
        }
        const doc = document.getElementById("editorflex");
        const editdoc = document.getElementById("editorbox");
        if (doc != null && this.sideBySide) {
            doc.classList.remove("sidebyside");
            doc.classList.add("stacked");
            this.sideBySide = false;
            if (editdoc != null) {
                editdoc.classList.remove("forceHalfSize");
            }
        } else if (doc != null) {
            doc.classList.remove("stacked");
            doc.classList.add("sidebyside");
            this.sideBySide = true;
            if (editdoc != null) {
                editdoc.classList.add("forceHalfSize");
            }
        }

        this.refreshEditorSize();
    }

    $onInit() {
        super.$onInit();
        this.docSettings = documentglobals().docSettings;
        const saveTag = this.getSaveTag();
        this.storage = {
            acebehaviours: new TimStorage("acebehaviours" + saveTag, t.boolean),
            acewrap: new TimStorage("acewrap" + saveTag, t.boolean),
            autocomplete: new TimStorage("autocomplete" + saveTag, t.boolean),
            editortab: new TimStorage("editortab" + saveTag, t.string),
            noteAccess: new TimStorage("noteAccess", t.string), // No saveTag here.
            oldMode: new TimStorage("oldMode" + saveTag, t.string),
            proeditor: new TimStorage("proeditor" + saveTag, t.boolean),
            spellcheck: new TimStorage("spellcheck" + saveTag, t.boolean),
            wrap: new TimStorage("wrap" + saveTag, t.string),
            diffSideBySide: new TimStorage(
                "diffSideBySide" + saveTag,
                t.boolean
            ),
            translator: new TimStorage<string>(
                "translator" + saveTag,
                t.string
            ),
        };
        setCurrentEditor(this);

        this.originalDocument = this.isOriginalDocument();

        this.docTranslator = this.storage.translator.get() ?? "";

        this.spellcheck = this.storage.spellcheck.get() ?? false;
        this.autocomplete = this.storage.autocomplete.get() ?? false;
        this.proeditor =
            this.storage.proeditor.get() ??
            (saveTag === "par" || saveTag === TIM_TABLE_CELL);
        this.activeTab = this.storage.editortab.get() ?? "navigation";
        if (this.originalDocument && this.activeTab == "translator") {
            this.activeTab = "navigation";
        }
        this.sideBySide = this.storage.diffSideBySide.get() ?? true;
        this.changePositioning();
        this.lastTab = this.activeTab;
        this.citeText = this.getCiteText();
        const sn = this.storage.wrap.get();
        let n = parseInt(sn ?? "-90", 10);
        if (isNaN(n)) {
            n = -90;
        }
        this.wrap = {n: Math.abs(n), auto: n > 0};

        if (this.getOptions().touchDevice) {
            if (!this.oldmeta) {
                const meta = $("meta[name='viewport']");
                this.oldmeta = meta[0] as HTMLMetaElement;
                meta.remove();
                $("head").prepend(
                    '<meta name="viewport" content="width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=0">'
                );
            }
        }
        this.draggable.setResizeCallback((params) => {
            if (!params.state.down || params.h == null) {
                return;
            }
            this.lastKnownDialogHeight = params.h;
            this.refreshEditorSize();
        });
        const oldMode =
            this.storage.oldMode.get() ??
            (this.getOptions().touchDevice ? "text" : "ace");
        (async () => {
            await this.changeEditor(oldMode);
        })();
        this.scope.$watch(
            () => this.autocomplete,
            () => {
                this.isAce()?.setAutoCompletion(this.autocomplete);
            }
        );
        this.memoMinutesSettings = documentglobals().memoMinutesSettings;

        this.activeAttachments = this.updateAttachments(
            true,
            undefined,
            undefined
        );

        if (!this.originalDocument) {
            void this.initTranslatorData();
        }
    }

    /**
     * Fetches the translator data on initialization and adds it to the lists.
     */
    async initTranslatorData() {
        const result = await updateTranslationData(
            this.docTranslator,
            this.errorMessage,
            false
        );
        this.sourceLanguages = result.source;
        this.documentLanguages = result.document;
        this.targetLanguages = result.target;
        this.translators = result.translators;
        this.availableTranslators = result.availableTransls;
        this.errorMessage = result.error;

        if (this.errorMessage != "") {
            this.translatorAvailable = false;
        } else if (this.availableTranslators.length == 0) {
            this.errorMessage =
                "You do not have any machine translator API keys added to you account.";
            this.translatorAvailable = false;
        }
    }

    /**
     * Compiles the preview of the source block (the original document's block the current block is based on) in
     * translation documents.
     */
    async compileOriginalPreview() {
        const previewOriginalDiv = angular.element(".previeworiginalcontent");
        // This is called for the first time before this component is created, so this check is needed to avoid
        // needless running of this function.
        if (previewOriginalDiv.length == 0) {
            await this.editorChanged();
            return;
        }

        let text = "";
        if (this.trdiff != undefined) {
            text = this.trdiff.new;
        }
        const spellCheckInEffect = this.spellcheck && this.isFinnishDoc();
        const data = await this.resolve.params.previewCb(
            text,
            spellCheckInEffect
        );

        await ParCompiler.compileAndAppendTo(
            previewOriginalDiv,
            data,
            this.scope,
            this.resolve.params.viewCtrl
        );
    }

    private refreshEditorSize() {
        if (this.lastKnownDialogHeight == null) {
            return;
        }
        const cont = this.getEditorContainer()[0];
        const elemHeight = this.element[0].clientHeight;
        const clientBottom = cont.clientTop + cont.clientHeight;
        const remainingSpace =
            this.lastKnownDialogHeight -
            cont.clientTop -
            (elemHeight - clientBottom);
        const ace = this.isAce();
        if (ace) {
            let lh = ace.editor.renderer.lineHeight;
            if (lh <= 0) {
                lh = 15;
            } // TODO: get a better value here
            const lines = remainingSpace / lh;
            ace.editor.setOptions({
                maxLines: Math.max(lines, 10),
                minLines: Math.min(lines, 5),
            });
        } else if (this.editor?.type == EditorType.Textarea) {
            this.editor.editor.height(Math.max(remainingSpace, 100));
        }
    }

    $postLink() {
        this.initCustomTabs();
    }

    $onDestroy() {
        setCurrentEditor(undefined);
    }

    getCiteText(): string {
        const par = this.getExtraData().par;
        if (!par) {
            return "";
        }
        return getCitePar(this.getExtraData().docId, par);
    }

    selectAllText(evt: Event) {
        (evt.target as HTMLInputElement).select();
    }

    initCustomTabs() {
        const tabs: Record<
            string,
            Record<string, IEditorMenuItem[] | undefined> | undefined
        > = {};
        for (const [plugin, d] of Object.entries(documentglobals().reqs)) {
            const data = d;
            let currTabs: ReadonlyArray<[string, MenuNameAndItems]>;
            if (EditorTemplateFormatV3.is(data)) {
                // this needs type casts inside maps because otherwise the types are inferred as plain arrays and not tuples
                currTabs = Object.entries(data.editor_tabs).map(([k, v]) => [
                    v.text,
                    v.items // editor_tabs is an array, so k is just (unused) index here
                        .map((m) => [m.text, m.items]),
                ]);
            } else if (EditorTemplateFormatV2.is(data)) {
                currTabs = Object.entries({
                    plugins: Object.entries(data.templates),
                });
            } else if (EditorTemplateFormatV1.is(data)) {
                const menus: MenuNameAndItems = [];
                const menuNames = data.text || [plugin];
                for (let i = 0; i < menuNames.length; i++) {
                    menus.push([menuNames[i], data.templates[i]]);
                }
                currTabs = Object.entries({plugins: menus});
            } else {
                if (t.record(t.string, t.unknown).is(d)) {
                    if ("editor_tabs" in d || "templates" in d) {
                        console.warn("Unrecognized reqs template format: ", d);
                    }
                }
                continue;
            }
            for (const [tab, menus] of currTabs) {
                for (const [j, [menu, templs]] of menus.entries()) {
                    let templsArray: MenuItem[];
                    if (Array.isArray(templs)) {
                        templsArray = templs;
                    } else {
                        templsArray = [templs];
                    }

                    for (const template of templsArray) {
                        let templateObj;
                        if (typeof template === "string") {
                            templateObj = {text: template, data: template};
                        } else {
                            templateObj = template;
                        }
                        let text;
                        let clickfn;
                        if (MenuItemFileObject.is(templateObj)) {
                            text = templateObj.text ?? templateObj.file;
                            const f = templateObj.file;
                            clickfn = async () => {
                                await this.getTemplate(plugin, f, j.toString()); // TODO: Why is index needed for getTemplate...?
                            };
                        } else {
                            text = templateObj.text ?? templateObj.data;
                            const dt = templateObj.data;
                            clickfn = () => {
                                this.putTemplate(dt);
                            };
                        }
                        const existingTab = tabs[tab];
                        const item = {
                            name: text,
                            title: templateObj.expl ?? "",
                            func: clickfn,
                        };
                        if (!existingTab) {
                            tabs[tab] = {[menu]: [item]};
                        } else {
                            const existingMenu = existingTab[menu];
                            if (!existingMenu) {
                                existingTab[menu] = [item];
                            } else {
                                existingMenu.push(item);
                            }
                        }
                    }
                }
            }
        }
        for (const [tabName, menus] of Object.entries(tabs)) {
            let tab = this.findTab(tabName);
            if (!tab) {
                tab = {name: tabName, entries: []};
                this.tabs.push(tab);
            }
            for (const [k, v] of Object.entries(menus!)) {
                tab.entries.push({
                    title: k,
                    items: v!,
                });
            }
        }
    }

    getInitialText() {
        return this.resolve.params.initialText;
    }

    async setInitialText() {
        const initialText = this.getInitialText();
        if (initialText) {
            const pos = this.getOptions().cursorPosition;
            const editor = this.editor!;
            editor.setEditorText(initialText);
            await $timeout(10);
            if (pos !== undefined) {
                editor.setPosition([pos, pos]);
            } else {
                editor.bottomClicked();
            }
        }
    }

    adjustPreview() {
        window.setTimeout(() => {
            const editor = this.element;
            const previewContent = this.element.find(".previewcontent");
            const previewOriginalContent = this.element.find(
                ".previeworiginalcontent"
            );
            // Check that editor doesn't go out of bounds
            const editorOffset = editor.offset();
            if (!editorOffset) {
                return;
            }
            const newOffset = editorOffset;
            if (editorOffset.top < 0) {
                newOffset.top = 0;
            }
            if (editorOffset.left < 0) {
                newOffset.left = 0;
            }
            editor.offset(newOffset);
            if (this.scrollPos) {
                previewContent.scrollTop(this.scrollPos);
                previewOriginalContent.scrollTop(this.scrollPos);
            }
        }, 25);
    }

    createTextArea(text: string) {
        const textarea = $(`
<textarea rows="10"
      id="teksti"
      wrap="off">
</textarea>`);
        this.getEditorContainer().append(textarea);
        this.editor = new TextAreaParEditor(this.element.find("#teksti"), {
            wrapFn: () => this.wrapFn(),
            saveClicked: () => this.saveClicked(),
            getWrapValue: () => this.wrapValue(),
        });
        this.editor.setEditorText(text);
        textarea.on("input", () => this.editorChanged());
        textarea.on("paste", (e) => this.onPaste(e));
        textarea.on("drop", (e) => this.onDrop(e));
        textarea.on("dragover", (e) => this.allowDrop(e));
    }

    isFinnishDoc() {
        const item = this.resolve.params.viewCtrl?.item;
        if (!item) {
            return true; // Assume Finnish by default.
        }
        return item.lang_id == null || item.lang_id == "fi";
    }

    async editorChanged() {
        const editor = this.editor!;
        this.outofdate = true;
        const text = editor.getEditorText();
        this.scope.$applyAsync();
        await $timeout(500);
        if (text !== editor.getEditorText()) {
            return;
        }
        const previewDiv = angular.element(".previewcontent");
        this.scrollPos = previewDiv.scrollTop() ?? this.scrollPos;
        this.outofdate = true;
        const spellCheckInEffect = this.spellcheck && this.isFinnishDoc();
        const data = await this.resolve.params.previewCb(
            text,
            spellCheckInEffect
        );
        if (spellCheckInEffect) {
            $injector.loadNewModules([
                (await import("../document/editing/spell-error.component"))
                    .spellModule.name,
            ]);
        }
        await ParCompiler.compileAndAppendTo(
            previewDiv,
            data,
            this.scope,
            this.resolve.params.viewCtrl
        );
        if (data.trdiff) {
            const module = await import("angular-diff-match-patch");
            $injector.loadNewModules([module.default]);
        }
        this.trdiff = data.trdiff;
        this.outofdate = false;
        this.parCount = previewDiv.children().length;
        if (spellCheckInEffect) {
            previewDiv
                .find(".parContent[ng-non-bindable] tim-spell-error")
                .each((i, e) => {
                    $compile(e)(this.scope);
                });
        }
        if (this.trdiff != undefined) {
            await this.compileOriginalPreview();
        }
        this.getEditorContainer().resize();
        this.scope.$applyAsync();
    }

    wrapFn(func: (() => void) | null = null) {
        if (!this.touchDevice) {
            // For some reason, on Chrome, re-focusing the editor messes up scroll position
            // when clicking a tab and moving mouse while clicking, so
            // we save and restore it manually.
            this.focusEditor();
        }
        if (func != null) {
            func();
        }
        if (this.isIE) {
            this.editorChanged();
        }
    }

    changeMeta() {
        if (!this.oldmeta) {
            return;
        }
        $("meta[name='viewport']").remove();
        const meta = $(this.oldmeta);
        $("head").prepend(meta);
    }

    async deleteClicked() {
        if (!this.getOptions().showDelete) {
            this.cancelClicked(); // when empty and save clicked there is no par
            return;
        }
        if (this.deleting) {
            return;
        }
        if (
            !window.confirm(
                this.getOptions().deleteMsg || "Delete - are you sure?"
            )
        ) {
            return;
        }
        this.deleting = true;
        const result = await this.resolve.params.deleteCb();
        if (result.error) {
            await showMessageDialog(result.error);
            this.deleting = false;
            return;
        }
        this.close({type: "delete"});
    }

    showUnread() {
        const par = this.getExtraData().par;
        return par?.getReadline()?.classList.contains("read");
    }

    async unreadClicked() {
        await this.resolve.params.unreadCb();
        if (this.resolve.params.initialText === this.editor!.getEditorText()) {
            this.close({type: "markunread"});
        }
    }

    /**
     * Updates the list of target languages based on the selected translator.
     */
    async updatePareditorTranslatorLanguages() {
        const result = await updateTranslatorLanguages(this.docTranslator);
        if (result.ok) {
            this.sourceLanguages = [];
            this.targetLanguages = [];
            this.sourceLanguages.push(...result.result.source);
            this.targetLanguages.push(...result.result.target);
            this.translatorAvailable = true;
            this.errorMessage = "";
        } else {
            this.translatorAvailable = false;
            this.errorMessage = result.result;
        }
    }

    /**
     * Checks if the document is a translation document and if its languages are supported by the selected translator.
     * @returns Whether or not the document can be translated automatically
     */
    isTranslationSupported() {
        if (this.originalDocument) {
            return false;
        }
        const trs = documentglobals().translations;
        const orig = trs.find((tab) => tab.id === tab.src_docid);
        if (orig && this.resolve.params.viewCtrl?.item.lang_id) {
            const orig_lang = orig.lang_id;
            const tr_lang = this.resolve.params.viewCtrl.item.lang_id;
            return this.isTranslationPairSupported(orig_lang, tr_lang);
        }
        return false;
    }

    /**
     * Checks whether the language combination is supported by the selected translator.
     * @param orig The source document's language code
     * @param tr The current document's language code
     * @returns Whether or not the language combination is supported
     */
    isTranslationPairSupported(orig: string, tr: string) {
        for (const target of this.targetLanguages) {
            if (target.code.toUpperCase() == tr.toUpperCase()) {
                for (const source of this.sourceLanguages) {
                    if (source.code.toUpperCase() == orig.toUpperCase()) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Checks whether or not the document is the original document.
     */
    isOriginalDocument() {
        const tags = this.getExtraData().tags;
        return tags.marktranslated == undefined;
    }

    /**
     * Translates the whole block.
     */
    async translateParagraph() {
        const parId = this.getExtraData().par!.originalPar.id;
        const docId = this.resolve.params.viewCtrl!.item.id;
        this.translationInProgress = true;

        const lang = this.resolve.params.viewCtrl!.item.lang_id!;
        const r = await to(
            $http.post<IDocument>(
                `/translate/paragraph/${docId}/${parId}/${lang}/${this.docTranslator}`,
                {
                    originaltext: this.trdiff!.new,
                }
            )
        );
        if (r.ok) {
            const response = await to(
                $http.get<{text: string}>(`/getBlock/${docId}/${parId}`)
            );
            if (response.ok) {
                this.getEditor()!.setEditorText(response.result.data.text);
            } else {
                await showMessageDialog(response.result.data.error);
            }
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    /**
     * Handles sending either the selected editor text or the entire block to the translator.
     */
    async translateClicked() {
        const translatableText = this.getTranslatableText();
        const helper = this.getEditor()!.getEditorText();
        const editText = helper.substring(helper.indexOf("\n") + 1);

        const mayContinue = await this.checkMayTranslate(editText);

        if (mayContinue) {
            this.translationInProgress = true;
            if (this.nothingSelected) {
                await this.translateParagraph();
            } else {
                const lang = this.resolve.params.viewCtrl!.item.lang_id!;
                const r = await to(
                    $http.post<string>(
                        `/translate/${
                            this.resolve.params.viewCtrl!.item.id
                        }/${lang}/translate_block/${this.docTranslator}`,
                        {
                            originaltext: translatableText,
                        }
                    )
                );
                if (r.ok) {
                    const resultText = r.result.data;
                    if (
                        this.trdiff == undefined ||
                        translatableText != this.trdiff.new
                    ) {
                        this.editor!.replaceTranslation(resultText);
                    } else {
                    }
                } else {
                    await showMessageDialog(r.result.data.error);
                }
            }
            this.translationInProgress = false;
        }
    }

    /**
     * Toggle formula editor visibility
     */
    onFormulaEditorAddFormula() {
        this.formulaEditorOpen = !this.formulaEditorOpen;
        this.scope.$digest();
    }

    onFormulaEditorCloseOk() {
        this.formulaEditorOpen = !this.formulaEditorOpen;
    }

    onFormulaEditorCloseCancel() {
        this.formulaEditorOpen = !this.formulaEditorOpen;
    }

    /**
     * Checks whether translation may be done.
     * @param editText The editor's text, used for comparison to original block's text in trdiff
     * @returns True if translation may be done, false if not
     */
    async checkMayTranslate(editText: string) {
        if (this.nothingSelected && this.trdiff == undefined) {
            await showMessageDialog(
                "There is no original text to be translated. Please check the Difference in original document view."
            );
            return false;
        } else if (!this.resolve.params.viewCtrl?.item.lang_id) {
            await showMessageDialog(
                "This document does not have a language set. Please set a language and try again."
            );
            return false;
        } else if (
            this.trdiff != undefined &&
            this.trdiff.new != editText &&
            this.trdiff.old != editText &&
            this.nothingSelected
        ) {
            return window.confirm(
                "This will overwrite all previous changes to this block and cannot be undone!" +
                    " Do you want to continue?"
            );
        }
        return true;
    }

    /**
     * Gets the text to be sent to the translator.
     * @returns The selected text, the source block's text or nothing if there is neither
     */
    getTranslatableText() {
        const selection = this.getEditor()!.checkTranslationSelection();
        if (selection == "") {
            this.nothingSelected = true;
            if (this.trdiff == null) {
                return selection;
            } else {
                return this.trdiff.new;
            }
        }
        this.nothingSelected = false;
        return selection;
    }

    close(r: IEditorResult) {
        this.saveOptions();
        if (this.getOptions().touchDevice) {
            this.changeMeta();
        }
        super.close(r);
    }

    dismiss() {
        if (this.confirmDismiss()) {
            this.close({type: "cancel"});
        }
    }

    cancelClicked() {
        this.dismiss();
    }

    releaseClicked() {
        this.adjustPreview();
    }

    async saveClicked() {
        if (this.saving) {
            return;
        }
        this.saving = true;

        // Start checking attachments only if the document has a minutes date.
        const date = this.getCurrentMeetingDate();
        if (date) {
            const tempAttachments = this.activeAttachments;
            this.activeAttachments = this.updateAttachments(
                false,
                this.activeAttachments,
                undefined
            );
            if (
                this.activeAttachments &&
                this.docSettings &&
                this.memoMinutesSettings &&
                !this.allAttachmentsUpToDate()
            ) {
                let stampFormat = this.memoMinutesSettings.stampformat;
                if (stampFormat === undefined) {
                    stampFormat = "";
                }
                const customStampModel = this.docSettings.custom_stamp_model;
                const r = await to2(
                    showRestampDialog({
                        attachments: this.activeAttachments,
                        customStampModel: customStampModel,
                        meetingDate: date,
                        stampFormat: stampFormat,
                    })
                );
                if (r.ok) {
                    if (
                        r.result === RestampDialogClose.RestampedReturnToEditor
                    ) {
                        // If restamped and then returned, all are up-to-date.
                        this.saving = false;
                        this.activeAttachments.forEach(
                            (att) => (att.upToDate = true)
                        );
                        return;
                    }
                    if (
                        r.result ===
                            RestampDialogClose.NoRestampingReturnToEditor ||
                        r.result ===
                            RestampDialogClose.RestampingFailedReturnToEditor
                    ) {
                        this.saving = false;
                        // If returned to editor without restamping, return to state prior to saving.
                        this.activeAttachments = tempAttachments;
                        return;
                    }
                }
                // If Save and exit was chosen, falls through to normal saving process.
                // Dismiss (pressing x to close) is considered the same as Save and exit.
            }
        }
        const text = this.editor!.getEditorText();
        if (text.trim() === "") {
            await this.deleteClicked();
            this.saving = false;
            return;
        }
        const result = await this.resolve.params.saveCb(
            text,
            this.getExtraData()
        );
        if (result.error) {
            await showMessageDialog(result.error);
            this.saving = false;
            return;
        } else {
            this.close({type: "save", text});
        }
    }

    isAce(): AceParEditor | undefined {
        return this.editor?.type == EditorType.Ace ? this.editor : undefined;
    }

    onPaste(e: JQuery.TriggeredEvent) {
        const clipboardData = (e.originalEvent as ClipboardEvent).clipboardData;
        if (!clipboardData) {
            return;
        }
        const items = clipboardData.items;
        // find pasted image among pasted items
        let blobs = 0;
        for (const i of items) {
            // TODO: one could inspect if some item contains image name and then use that to name the image
            if (i.type.startsWith("image")) {
                const blob = i.getAsFile();
                if (blob !== null) {
                    this.onFileSelect(blob);
                    blobs++;
                }
            }
        }
        if (blobs > 0) {
            e.preventDefault();
        }
    }

    onDrop(e: JQuery.DropEvent) {
        e.preventDefault();
        const de = e.originalEvent as DragEvent;
        if (!de.dataTransfer) {
            return;
        }
        const files = de.dataTransfer.files;
        for (const f of files) {
            this.onFileSelect(f);
        }
    }

    allowDrop(e: JQuery.Event) {
        e.preventDefault();
    }

    /**
     * Get start and end indices of a macro nearest to the cursor.
     * For example (|= selected area, ^ = found indices, x = intervening macro):
     *
     * something %%liite("attachment_stuff")%% more stuff
     *           ^        |              |   ^
     * -> return indices
     *
     * something %%liite("attachment")%% something in between %%liite("another_attachment")%% even more something
     *           ^                     x      |                x                            ^
     * -> error, return undefined
     * @param str The string where the selection and possible macro are.
     * @param beginStr Macro beginning.
     * @param endStr Macro end.
     * @param selectionRange The index range "painted" by the user. Can start and end at same index.
     * @returns Macro start and end indices in SelectionRange, or undefined if selection is not inside the macro.
     */
    getMacroRange(
        str: string,
        beginStr: string,
        endStr: string,
        selectionRange: SelectionRange
    ): SelectionRange | undefined {
        const selectIndexA = selectionRange[0];
        const selectIndexB = selectionRange[1];
        const partA = str.substring(0, selectIndexA);
        const partB = str.substring(selectIndexB);
        const indexA = partA.lastIndexOf(beginStr);
        const indexB = partB.indexOf(endStr);
        const interveningIndexA = partA.substring(indexA).lastIndexOf(endStr);
        const interveningIndexB = partB.substring(0, indexB).indexOf(beginStr);

        // If there are any extra macro parts in between or either marcro part wasn't found at all,
        // return undefined.
        if (
            interveningIndexA !== -1 ||
            interveningIndexB !== -1 ||
            indexA === -1 ||
            indexB === -1
        ) {
            return undefined;
        }
        return [indexA, selectIndexB + indexB + endStr.length];
    }

    async onFilesSelect(files: File[], invalidFiles: File[]) {
        for (const file of files) {
            await this.onFileSelect(file);
        }
    }

    async onFileSelect(file: File) {
        const editor = this.editor!;
        await this.focusEditor();
        this.file = file;
        const editorText = editor.getEditorText();
        let autostamp = false;
        let attachmentParams;
        let macroParams;
        let stamped: IAttachmentData;

        const kokousDate = this.getCurrentMeetingDate();
        const selectionRange = editor.getPosition(); // Selected area in the editor
        // For attachments with stamps:
        const macroRange = this.getMacroRange(
            editorText,
            this.liiteMacroStringBegin,
            this.liiteMacroStringEnd,
            selectionRange
        );

        // If there's an attachment macro in the editor (i.e. macroRange is defined), assume need to stamp.
        // Also requires data from preamble to work correctly (dates and knro).
        // If there's no stampFormat set in preamble, uses hard-coded default format.
        if (
            macroRange &&
            this.docSettings &&
            this.memoMinutesSettings &&
            kokousDate
        ) {
            autostamp = true;
            try {
                // Macro begin and end not included.
                const macroText = editorText.substring(
                    macroRange[0] + this.liiteMacroStringBegin.length,
                    macroRange[1] - this.liiteMacroStringEnd.length
                );
                macroParams = this.getMacroParamsFromString(macroText);
            } catch {
                const errorMessage = "Parsing stamp parameters failed";
                this.file.error = errorMessage;
                throw new Error(errorMessage);
            }
            let stampFormat = this.memoMinutesSettings.stampformat;
            if (stampFormat === undefined) {
                stampFormat = "";
            }
            const customStampModel = this.docSettings.custom_stamp_model;
            attachmentParams = [
                kokousDate,
                stampFormat,
                ...macroParams,
                customStampModel,
                autostamp,
            ];
            stamped = this.macroParamsToAttachmentData(macroParams);
            stamped.upToDate = true;
        }
        if (file) {
            this.file.progress = 0;
            this.file.error = undefined;
            const upload = $upload.upload<{image: string; file: string}>({
                data: {
                    attachmentParams: JSON.stringify(attachmentParams),
                    doc_id: this.getExtraData().docId.toString(),
                    file,
                },
                method: "POST",
                url: "/upload/",
            });
            upload.progress((evt) => {
                if (this.file) {
                    this.file.progress = Math.min(
                        100,
                        Math.floor((100.0 * evt.loaded) / evt.total)
                    );
                }
            });

            const result = await to(upload);

            if (result.ok) {
                const response = result.result;
                $timeout(() => {
                    const ed = this.editor!;
                    // This check is needed for uploads in other (non-attachment) plugins.
                    // TODO: Could this be editor.contains(...)?
                    const isplugin = ed.editorStartsWith("``` {");
                    let start = "[File](";
                    let end = ")";
                    let savedir = "/files/";
                    if (
                        response.data.image ||
                        response.data.file.toString().includes(".svg")
                    ) {
                        start = "![Image](";
                    }
                    if (response.data.image) {
                        savedir = "/images/";
                        this.uploadedFile = savedir + response.data.image;
                    } else {
                        this.uploadedFile = savedir + response.data.file;
                    }
                    // For attachments without stamps (only look for this if stamped version wasn't found):
                    let macroRange2;
                    if (!macroRange) {
                        macroRange2 = this.getMacroRange(
                            editorText,
                            this.perusliiteMacroStringBegin,
                            this.perusliiteMacroStringEnd,
                            selectionRange
                        );
                    }
                    if (isplugin || macroRange || macroRange2) {
                        start = "";
                        end = "";
                        if (
                            // is showImages?  How to make this general and ask from plugin?
                            editorText.match(
                                /^```[^\n]*plugin="showImages"[^\n]*\n/
                            )
                        ) {
                            const [i1, s] = ed.getCurrentLinePosAndLine();
                            if (s.trim() === "") {
                                start = "  - name: ";
                                end = "\n";
                                ed.setPosition([i1, i1]);
                            }
                        }
                    }
                    ed.insertTemplate(`${start}${this.uploadedFile}${end}`);
                    // Separate from isPlugin so this is run only when there are attachments with stamps.
                    if (macroRange && kokousDate) {
                        stamped.uploadUrl = this.uploadedFile;
                        this.activeAttachments = this.updateAttachments(
                            false,
                            this.activeAttachments,
                            stamped
                        );
                    }
                });
            } else {
                const response = result.result;
                if (this.file) {
                    this.file.error = response.data.error;
                }
            }
        }
    }

    async putTemplate(data: string) {
        await this.focusEditor();
        data = await replaceTemplateValues(data);
        if (!data) {
            return;
        }
        this.editor!.insertTemplate(data);
    }

    async getTemplate(plugin: string, template: string, index: string) {
        const response = await to(
            $http.get<string>(`/${plugin}/template/${template}/${index}`)
        );
        if (!response.ok) {
            return;
        }
        let data = response.result.data;
        data = data.replace(/\\/g, "\\\\");
        data = await replaceTemplateValues(data);
        if (!data) {
            return;
        }
        this.editor!.insertTemplate(data);
        await this.focusEditor();
    }

    tabClicked() {
        this.wrapFn();
    }

    /**
     * Sets the active tab.
     * @param name The tab to activate.
     */
    setActiveTab(name: string) {
        this.activeTab = name;
    }

    fullscreenSupported() {
        return fullscreenSupported(this.element[0]);
    }

    goFullScreen() {
        const wentFullscreen = toggleFullScreen(this.element[0]);
        if (wentFullscreen) {
            this.element[0].setAttribute(
                "style",
                "width: 100%; height: 100%; position: absolute; top: 0px;" +
                    "padding: 2em 5px 5px 5px; background: rgb(224, 224, 224); -webkit-box-sizing: border-box;" +
                    "-moz-box-sizing: border-box; box-sizing: border-box;"
            );
        }
    }

    /**
     * Switches editor between Ace and textarea.
     */
    async changeEditor(initialMode?: string) {
        let text = "";
        const editorContainer = this.getEditorContainer();
        // const pasteInput = this.element.find(".pasteinput");
        // pasteInput.on("drop", (e) => this.onDrop(e));
        // pasteInput.on("dragover", (e) => this.allowDrop(e));
        const pasteInput = document.getElementById("pasteInput");
        if (pasteInput) {
            $(pasteInput).on("drop", (e) => this.onDrop(e));
            $(pasteInput).on("dragover", (e) => this.allowDrop(e));
        }
        editorContainer.addClass("editor-loading");
        let oldPosition;
        if (this.editor) {
            text = this.editor.getEditorText();
            oldPosition = this.editor.getPosition();
        }
        let oldeditor;
        const h = (await this.draggable.getSizeAsNum()).h;
        if (h != null) {
            this.lastKnownDialogHeight = h;
        }
        if (this.isAce() || initialMode === "text") {
            oldeditor = this.element.find("#ace_editor");
            oldeditor.remove();
            this.createTextArea(text);
            this.refreshEditorSize();
        } else {
            oldeditor = this.element.find("#teksti");
            oldeditor.remove();
            const ace = (await import("tim/editor/ace")).ace;
            const neweditorElem = $("<div>", {
                class: "editor",
                id: "ace_editor",
            });
            editorContainer.append(neweditorElem);
            const neweditor = ace.edit(neweditorElem[0]);

            this.editor = new AceParEditor(
                ace,
                neweditor,
                {
                    wrapFn: () => this.wrapFn(),
                    saveClicked: () => this.saveClicked(),
                    getWrapValue: () => this.wrapValue(),
                },
                this.getSaveTag() === "addAbove" ||
                this.getSaveTag() === "addBelow"
                    ? "ace/mode/text"
                    : "ace/mode/markdown"
            );
            this.editor.setAutoCompletion(this.autocomplete);
            this.editor.editor.renderer.$cursorLayer.setBlinking(
                !genericglobals().IS_TESTING
            );
            /* iPad does not open the keyboard if not manually focused to editable area
             var iOS = /(iPad|iPhone|iPod)/g.test($window.navigator.platform);
             if (!iOS) editor.focus();*/

            neweditor.getSession().on("change", () => {
                this.editorChanged();
            });
            this.editor.addContainerEventListener("paste", (e) =>
                this.onPaste(e)
            );
            this.editor.addContainerEventListener("drop", (e) =>
                this.onDrop(e)
            );
            this.editor.addContainerEventListener("dragover", (e) =>
                this.allowDrop(e)
            );
            neweditor.setBehavioursEnabled(
                this.storage.acebehaviours.get() ?? false
            );
            neweditor
                .getSession()
                .setUseWrapMode(this.storage.acewrap.get() ?? false);
            neweditor.setOptions({maxLines: 28});
            this.editor.setEditorText(text);
            this.refreshEditorSize();

            interface ILanguageTools {
                setCompleters(completers: unknown[]): void;
                snippetCompleter: unknown;
                textCompleter: unknown;
                keyWordCompleter: unknown;
            }
            const langTools = ace.require(
                "ace/ext/language_tools"
            ) as ILanguageTools;

            const r = await to(
                $http.get<{word_list: string}>("/settings/get/word_list", {
                    params: {_: Date.now()},
                })
            );
            const wordListStr = r.ok ? r.result.data.word_list : undefined;
            const userWordList = wordListStr ? wordListStr.split("\n") : [];
            const createCompleter = (wordList: string[], context: string) => ({
                getCompletions(
                    editor: unknown,
                    session: unknown,
                    pos: unknown,
                    prefix: unknown,
                    callback: (
                        x: null,
                        words: Array<{
                            caption: string;
                            meta: string;
                            value: string;
                        }>
                    ) => void
                ) {
                    callback(
                        null,
                        wordList.map((word) => ({
                            caption: word,
                            meta: context,
                            value: word,
                        }))
                    );
                },
            });
            langTools.setCompleters([
                langTools.snippetCompleter,
                langTools.textCompleter,
                langTools.keyWordCompleter,
                createCompleter(documentglobals().wordList, "document"),
                createCompleter(userWordList, "user"),
            ]);
        }
        if (this.editor?.addFormulaEditorOpenHandler) {
            this.editor.addFormulaEditorOpenHandler(() =>
                this.onFormulaEditorAddFormula()
            );
        }
        if (initialMode != null) {
            await this.setInitialText();
        } else if (this.editor && oldPosition) {
            this.editor.setPosition(oldPosition);
        }
        await this.focusEditor();
        this.getEditorContainer().removeClass("editor-loading");
        this.adjustPreview();
    }

    private getEditorContainer() {
        return this.element.find(".editorContainer");
    }

    scrollIntoView() {
        this.element[0].scrollIntoView();
    }

    getPreviewCaption() {
        let str;
        if (this.parCount === 0) {
            str = "Preview (empty)";
        } else if (this.parCount === 1) {
            str = "Preview (1 paragraph)";
        } else {
            str = `Preview (${this.parCount} paragraphs)`;
        }
        if (this.outofdate) {
            str += " ...";
        }
        return str;
    }

    getSourceDocumentLink() {
        const trs = documentglobals().translations;
        const orig = trs.find((tab) => tab.id === tab.src_docid);
        const par = this.getExtraData().par;
        if (orig && par) {
            return `/view/${orig.path}#${par.originalPar.attrs.rp}`;
        }
    }

    protected getTitle(): string {
        return this.resolve.params.options.caption;
    }

    protected confirmDismiss() {
        if (this.editor!.getEditorText() === this.getInitialText()) {
            return true;
        }
        return window.confirm("You have unsaved changes. Close editor anyway?");
    }

    private getOptions() {
        return this.resolve.params.options;
    }

    private getExtraData() {
        return this.resolve.params.extraData;
    }

    private getSaveTag() {
        return this.getOptions().localSaveTag;
    }

    private async focusEditor() {
        await $timeout();
        const s = $(window).scrollTop();
        this.editor!.focus();
        await $timeout();
        $(window).scrollTop(s ?? this.scrollPos ?? 0);
    }

    private saveOptions() {
        this.storage.spellcheck.set(this.spellcheck);
        this.storage.editortab.set(this.activeTab ?? "navigation");
        this.storage.autocomplete.set(this.autocomplete);
        const ace = this.isAce();
        this.storage.oldMode.set(ace ? "ace" : "text");
        this.storage.wrap.set("" + this.wrapValue());
        if (!this.originalDocument) {
            this.storage.diffSideBySide.set(!this.sideBySide);
        }
        this.storage.translator.set(this.docTranslator);
        const acc = this.getExtraData().access;
        if (acc != null) {
            this.storage.noteAccess.set(acc);
        }
        const tags = this.getExtraData().tags;
        const tagKeys = ["markread", "marktranslated"] as const;
        for (const key of tagKeys) {
            const v = tags[key];
            if (v != null) {
                window.localStorage.setItem(key, v.toString());
            }
        }
        this.storage.proeditor.set(this.proeditor);
        if (ace) {
            this.storage.acewrap.set(ace.editor.getSession().getUseWrapMode());
            this.storage.acebehaviours.set(ace.editor.getBehavioursEnabled()); // some of these are in editor and some in session?
        }
    }

    /**
     * Returns the current meeting date from document settings, if it exists.
     */
    private getCurrentMeetingDate(): string | undefined {
        if (this.memoMinutesSettings) {
            // Knro usage starts from 1 but dates starts from 0 but there is dummy item first
            const knro = this.memoMinutesSettings.knro;
            const dates = this.memoMinutesSettings.dates;
            if (dates != null && knro != null) {
                const entry = dates[knro] as MeetingDateEntry | undefined;
                return entry?.[0];
            }
        }
    }

    /**
     * Updates list of attachments and their data present in the open editor,
     * including whether they have changed since previous check.
     * @param firstCheck Check during initialization (i.e. up-to-date by default).
     * @param previousAttachments Previous check's data (if there's any).
     * @param stamped Attachment-to-stamp (if there's one), which is up-to-date by default.
     */
    private updateAttachments(
        firstCheck: boolean,
        previousAttachments: IAttachmentData[] | undefined,
        stamped: IAttachmentData | undefined
    ) {
        const attachments = [];
        if (!this.editor) {
            return undefined;
        }
        const editorText = this.editor.getEditorText();
        const pluginSplit = editorText.split("```");
        for (const part of pluginSplit) {
            // Do closer check only on paragraphs containing showPdf-plugins and stamped attachment macros
            // (because same plugin type can also contain stampless macros).
            if (
                part.length >
                    this.liiteMacroStringBegin.length +
                        this.liiteMacroStringEnd.length &&
                part.includes('plugin="showPdf"') &&
                part.includes(this.liiteMacroStringBegin)
            ) {
                const macroText = part.substring(
                    part.lastIndexOf(this.liiteMacroStringBegin) +
                        this.liiteMacroStringBegin.length,
                    part.lastIndexOf(this.liiteMacroStringEnd)
                );
                const macroParams = this.getMacroParamsFromString(macroText);
                const current = this.macroParamsToAttachmentData(macroParams);
                // On first editor load attachment is up-to-date.
                if (firstCheck) {
                    current.upToDate = true;
                } else {
                    // Others are up to date if they have a match from check.
                    // TODO: Does changing attachment order cause problems?
                    if (previousAttachments) {
                        for (const previous of previousAttachments) {
                            if (
                                previous.uploadUrl === current.uploadUrl &&
                                previous.attachmentLetter ===
                                    current.attachmentLetter &&
                                previous.issueNumber === current.issueNumber
                            ) {
                                current.upToDate = true;
                            }
                        }
                    } else {
                        // If there were no attachments before, all are assumed to be up-to-date.
                        current.upToDate = true;
                    }
                }
                // Stamped attachment is always up-to-date.
                if (stamped) {
                    if (
                        stamped.attachmentLetter === current.attachmentLetter &&
                        stamped.issueNumber === current.issueNumber &&
                        stamped.uploadUrl === current.uploadUrl
                    ) {
                        current.upToDate = true;
                    }
                }
                attachments.push(current);
            }
        }
        return attachments;
    }

    /**
     * Removes line breaks and parses macro string into a dictionary.
     * @param macroText String in JSON-compatible format.
     */
    private getMacroParamsFromString(macroText: string): unknown[] {
        // Normal line breaks cause exception with JSON.parse, and replacing them with ones parse understands
        // causes exceptions when line breaks are outside parameters, so just remove them before parsing.
        macroText = macroText.replace(/(\r\n|\n|\r)/gm, "");
        macroText = macroText.replace(/(\t)/gm, " ");
        return JSON.parse(`[${macroText}]`) as unknown[];
    }

    /**
     * Check whether all attachments in open editor are up-to-date.
     * @returns True if none have changed since previous check.
     */
    private allAttachmentsUpToDate() {
        if (this.activeAttachments) {
            return this.activeAttachments.every((att) => att.upToDate);
        }
        return true;
    }

    /**
     * Picks data relevant for restamping from an array and returns it as an object.
     * @param macroParams Array with attachment letter at index 1, issue number 2 and upload url 3.
     */
    private macroParamsToAttachmentData(
        macroParams: unknown[]
    ): IAttachmentData {
        if (MacroParams.is(macroParams)) {
            return {
                attachmentLetter: macroParams[1],
                issueNumber: macroParams[2],
                upToDate: false,
                uploadUrl: macroParams[3],
            };
        } else {
            const err = `Unexpected type of macroParams: ${JSON.stringify(
                macroParams
            )}`;
            void showMessageDialog(err);
            throw new Error(err);
        }
    }
}

const MacroParams = t.tuple([
    t.unknown,
    t.string,
    t.union([t.string, t.number]),
    t.string,
]);

export const parEditorModule = angular.module("timEditor", []);

parEditorModule.component("timEditorMenu", {
    bindings: {
        data: "<",
    },
    template: `
<div class="btn-group" uib-dropdown>
    <button type="button" class="editorButton" uib-dropdown-toggle>
        {{ $ctrl.data.title }} <span class="caret"></span>
    </button>
    <ul class="dropdown-menu"
        uib-dropdown-menu
        role="menu"
        aria-labelledby="single-button">
        <li ng-repeat="item in $ctrl.data.items" role="menuitem">
            <a title="{{ item.title }}"
               href="#"
               ng-click="item.func($event); $event.preventDefault()"
               ng-bind-html="item.name"></a>
        </li>
    </ul>
</div>
    `,
});

parEditorModule.component("timEditorEntry", {
    bindings: {
        data: "<",
    },
    template: `
<div ng-switch on="$ctrl.data.items != null">
    <button ng-switch-default
            type="button"
            class="editorButton"
            title="{{ $ctrl.data.title }}"
            ng-click="$ctrl.data.func($event); $event.preventDefault()"
            ng-bind-html="$ctrl.data.name">
    </button>
    <tim-editor-menu ng-switch-when="true" data="$ctrl.data"></tim-editor-menu>
</div>
    `,
});

registerDialogComponentForModule(parEditorModule, PareditorController, {
    templateUrl: "/static/templates/parEditor.html",
});
