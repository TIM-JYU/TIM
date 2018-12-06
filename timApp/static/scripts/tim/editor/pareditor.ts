import angular, {IPromise, IRootElementService, IScope} from "angular";
import $ from "jquery";
import rangyinputs from "rangyinputs";
import {setEditorScope} from "tim/editor/editorScope";
import {fixDefExport, markAsUsed} from "tim/util/utils";
import {timApp} from "../app";
import {IExtraData, ITags} from "../document/editing/edittypes";
import {getElementByParId, getParAttributes} from "../document/parhelpers";
import {IItem} from "../item/IItem";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../ui/dialog";
import {$http, $injector, $localStorage, $timeout, $upload, $window} from "../util/ngimport";
import {IAceEditor} from "./ace-types";
import {AceParEditor} from "./AceParEditor";
import {IPluginInfoResponse, ParCompiler} from "./parCompiler";
import {TextAreaParEditor} from "./TextAreaParEditor";
import DroppableEvent = JQueryUI.DroppableEvent;

markAsUsed(rangyinputs);

const TIM_TABLE_CELL = "timTableCell";

export interface IPreviewResult {
    html: string;
}

export interface ITag {
    name: keyof ITags;
    desc: string;
}

export interface IChoice {
    desc: string;
    name: string;
    opts: Array<{desc: string, value: string}>;
}

export interface IEditorParams {
    initialText?: string;
    defaultSize: "sm" | "md" | "lg";
    extraData: IExtraData;
    options: {
        deleteMsg?: string;
        caption: string,
        localSaveTag: string,
        showDelete: boolean,
        showPlugins: boolean,
        showSettings: boolean,
        showImageUpload: boolean,
        touchDevice: boolean,
        tags: ITag[],
        choices?: IChoice[],
        cursorPosition?: number,
    };
    previewCb: (text: string) => Promise<IPluginInfoResponse>;
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

export type IEditorResult = {type: "save", text: string} | {type: "delete"} | {type: "markunread"} | {type: "cancel"};

export function getCitePar(docId: number, par: string) {
    return `#- {rd="${docId}" rl="no" rp="${par}"}`;
}

interface IMenuItemObject {
    data: string;
    expl?: string;
    text?: string;
    file?: string;
}

type MenuItem = IMenuItemObject | string;

type MenuItemEntries = MenuItem | MenuItem[];

interface IEditorTabContent {
    [name: string]: MenuItemEntries;
}

interface IEditorTemplateFormatV1 {
    text: string[];
    templates: MenuItem[][];
}

interface IEditorTemplateFormatV2 {
    templates: IEditorTabContent;
}

interface EditorItem<T> {
    text: string;
    shortcut: string;
    items: T;
}

interface IEditorTemplateFormatV3 {
    editor_tabs: Array<EditorItem<Array<EditorItem<MenuItemEntries>>>>;
}

interface IEditorTab {
    entries: EditorEntry[];
    extra?: string;
    name: string;

    show?(): boolean;
}

function isV1Format(d: any): d is IEditorTemplateFormatV1 {
    return Array.isArray(d.text) && Array.isArray(d.templates);
}

function isV2Format(d: any): d is IEditorTemplateFormatV2 {
    return d.text == null && !Array.isArray(d.templates) && d.templates != null;
}

function isV3Format(d: any): d is IEditorTemplateFormatV3 {
    return d.editor_tabs != null;
}

type MenuNameAndItems = [string, MenuItemEntries][];

export class PareditorController extends DialogController<{params: IEditorParams}, IEditorResult, "pareditor"> {
    private static $inject = ["$scope", "$element"];
    private deleting = false;
    private editor?: TextAreaParEditor | AceParEditor; // $onInit
    private isACE : boolean = false;
    private file?: File & {progress?: number, error?: string};
    private isIE: boolean = false;
    private oldmeta?: HTMLMetaElement;
    private wrap!: {n: number}; // $onInit
    private outofdate: boolean;
    private parCount: number;
    private proeditor!: boolean; // $onInit
    private saving: boolean = false;
    private scrollPos?: number;
    private storage: Storage;
    private touchDevice: boolean;
    private autocomplete!: boolean; // $onInit
    private citeText!: string; // $onInit
    private docSettings?: {macros: {dates: string[], knro: number, stampformat: string}};
    private uploadedFile?: string;
    private activeTab?: string;
    private lastTab?: string;
    private tabs: IEditorTab[];
    private trdiff?: {old: string, new: string};

    constructor(protected scope: IScope, protected element: IRootElementService) {
        super(element, scope);
        this.storage = localStorage;

        if ((navigator.userAgent.match(/Trident/i))) {
            this.isIE = true;
        }

        this.element.find(".editorContainer").on("resize", () => this.adjustPreview());

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
    rows:
      - row:
        - cell: "solu"
${backTicks}
`.trim(),

            timTable2x2: `
${backTicks} {plugin="timTable"}
table:
    rows:
      - row:
        - cell: 'Ekan sarakkeen otsikko'
        - cell: 'Tokan sarakkeen otsikko'
        backgroundColor: lightgray
        fontWeight: bold
      - row:
        - cell: '1. solun sisältö'
        - cell: '2. solun sisältö'
${backTicks}
`.trim(),
        };

        this.tabs = [
            {
                entries: [
                    {title: "Undo", name: "&#8630;", func: () => this.editor!.undoClicked()},
                    {title: "Redo", name: "&#8631;", func: () => this.editor!.redoClicked()},
                    {title: "Move left", name: "&#8592;", func: () => this.editor!.leftClicked()},
                    {title: "Move right", name: "&#8594;", func: () => this.editor!.rightClicked()},
                    {title: "Move up", name: "&#8593;", func: () => this.editor!.upClicked()},
                    {title: "Move down", name: "&#8595;", func: () => this.editor!.downClicked()},
                    {title: "Move to the beginning of the line", name: "Home", func: () => this.editor!.homeClicked()},
                    {title: "Move to the end of the line", name: "End", func: () => this.editor!.endClicked()},
                    {title: "Move to the beginning of the file", name: "Top", func: () => this.editor!.topClicked()},
                    {title: "Move to the end of the file", name: "Bottom", func: () => this.editor!.bottomClicked()},
                    {title: "Toggle insert mode on or off", name: "Ins", func: () => this.editor!.insertClicked()},
                ],
                name: "Navigation",
            },
            { // To change keys, change also:  TextAreaParEditor.ts ja AceParEditor.ts and maybe keycodes.ts
                entries: [
                    {title: "Indent selection/line", func: () => this.editor!.indentClicked(), name: "&#8649;"},
                    {title: "Outdent selection/line", func: () => this.editor!.outdentClicked(), name: "&#8647;"},
                    {title: "Bold (Ctrl-B)", func: () => this.editor!.surroundClicked("**", "**"), name: "<b>B</b>"},
                    {title: "Italic (Ctrl-I)", func: () => this.editor!.italicSurroundClicked(), name: "<i>I</i>"},
                    {title: "Underline", func: () => this.editor!.surroundClicked("<u>", "</u>"), name: "<u>U</u>"},
                    {title: "Strikethrough", func: () => this.editor!.surroundClicked("<s>", "</s>"), name: "<s>Z</s>"},
                    {title: "Add any style", func: () => this.editor!.styleClicked("Teksti", "red"), name: "Style"},
                    {title: "Code (Ctrl-O)", func: () => this.editor!.surroundClicked("`", "`"), name: "Code"},
                    {title: "Code block (Ctrl-Alt-O)", func: () => this.editor!.codeBlockClicked(), name: "Code block"},
                    {title: "Subscript", func: () => this.editor!.surroundClicked("~", "~"), name: "X_"},
                    {title: "Superscript", func: () => this.editor!.surroundClicked("^", "^"), name: "X^"},
                    {title: "Heading 1 (Ctrl-1)", func: () => this.editor!.headerClicked("#"), name: "H1"},
                    {title: "Heading 2 (Ctrl-2)", func: () => this.editor!.headerClicked("##"), name: "H2"},
                    {title: "Heading 3 (Ctrl-3)", func: () => this.editor!.headerClicked("###"), name: "H3"},
                    {title: "Heading 4 (Ctrl-4)", func: () => this.editor!.headerClicked("####"), name: "H4"},
                    {title: "Heading 5 (Ctrl-5)", func: () => this.editor!.headerClicked("#####"), name: "H5"},
                ],
                name: "Style",
            },
            {
                entries: [
                    {
                        title: "Add link",
                        func: () => this.editor!.linkClicked("Linkin_teksti", "Linkin_osoite", false),
                        name: "Link",
                    },
                    {
                        title: "Add image",
                        func: () => this.editor!.linkClicked("Kuvan_teksti", "Kuvan_osoite", true),
                        name: "Image",
                    },
                    {title: "List item", func: () => this.editor!.listClicked(), name: "List"},
                    {
                        title: "Slide",
                        items: [
                            {
                                name: "Slide break",
                                title: "Break text to start a new slide",
                                func: () => this.editor!.ruleClicked(),
                            },
                            {
                                name: "Slide fragment",
                                title: "Content inside the fragment will be hidden and shown when next is clicked in slide view",
                                func: () => this.editor!.surroundClicked("§§", "§§"),
                            },
                            {
                                name: "Fragment block",
                                title: "Content inside will show as a fragment and may contain inner slide fragments",
                                func: () => this.editor!.surroundClicked("<§", "§>"),
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
                        title: "Creates a comment block or sets the line to a comment (Ctrl-Y)",
                        func: () => this.editor!.commentClicked(),
                        name: "Comment",
                    },
                    {
                        title: "Creates a page break for web printing",
                        func: () => this.editor!.pageBreakClicked(),
                        name: "Page break",
                    },
                ],
                name: "Insert",
            },
            {
                entries: [
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "@"},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "#"},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "`"},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "$"},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "€"},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "%"},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "&"},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "{"},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "}"},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "["},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "]"},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "/"},
                    {title: "", func: ($event) => this.editor!.charClicked($event), name: "\\"},
                    {title: "", func: ($event) => this.editor!.charClicked($event, "&#173;"), name: "Soft hyphen"},
                    {title: "", func: ($event) => this.editor!.charClicked($event, "⁞"), name: "Cursor"},
                ],
                name: "Characters",
            },
            {
                entries: [
                    {title: "", func: () => this.editor!.surroundClicked("$", "$"), name: "TeX equation"},
                    {title: "", func: () => this.editor!.surroundClicked("$$", "$$"), name: "TeX block"},
                    {title: "", func: () => this.editor!.indexClicked(), name: "X&#x2093;"},
                    {title: "", func: () => this.editor!.powerClicked(), name: "X&#x207f;"},
                    {title: "", func: () => this.editor!.squareClicked(), name: "&radic;"},
                ],
                name: "TeX",
            },
            {
                entries: [
                    {
                        title: "Auto number headings level",
                        func: () => this.editor!.insertTemplate("auto_number_headings: 1\n"),
                        name: "Heading numbering",
                    },
                    {
                        title: "Styles for this document",
                        func: () => this.editor!.insertTemplate("css: |!!\n.style {\n\n}\n!!\n"),
                        name: "CSS",
                    },
                    {
                        title: "Macro values fo document",
                        func: () => this.editor!.insertTemplate("macros:\n  key: value\n"),
                        name: "Macros",
                    },
                    {
                        title: "Show task summary in the begining of the doc",
                        func: () => this.editor!.insertTemplate("show_task_summary: true\n"),
                        name: "Task summary",
                    },
                    {
                        title: "Update documetn automatically in this interval (sec)",
                        func: () => this.editor!.insertTemplate("live_updates: 5\n"),
                        name: "Live update",
                    },
                    {
                        title: "Calculate plugins lazy or not",
                        func: () => this.editor!.insertTemplate("lazy: false\n"),
                        name: "Lazy",
                    },
                    {
                        title: "Global values for the plugins",
                        func: () => this.editor!.insertTemplate("global_plugin_attrs:\n csPlugin:\n   stem: value\n all:\n   stem: value\n"),
                        name: "Global plugin",
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

        $(document).on("webkitfullscreenchange mozfullscreenchange fullscreenchange MSFullscreenChange", (event) => {
            const editor = element[0];
            const doc: any = document;
            if (!doc.fullscreenElement &&    // alternative standard method
                !doc.mozFullScreenElement && !doc.webkitFullscreenElement && !doc.msFullscreenElement) {
                editor.removeAttribute("style");
            }
        });

        this.outofdate = false;
        this.parCount = 0;
        this.touchDevice = false;
    }

    getVisibleTabs() {
        // Upload tab is shown separately in the template because
        // it has special content that cannot be placed under "extra".
        return this.tabs.filter((t) => (!t.show || t.show()) && t.name !== "Upload");
    }

    getUploadTab() {
        return this.findTab("upload");
    }

    findTab(name: string) {
        return this.tabs.find((t) => t.name.toLowerCase() === name.toLowerCase());
    }

    getTabIndex(t: IEditorTab) {
        return t.name.toLowerCase();
    }

    $doCheck() {
        if (this.lastTab != this.activeTab) {
            this.lastTab = this.activeTab;
            this.wrapFn();
        }
    }

    $onInit() {
        super.$onInit();
        this.autocomplete = this.getLocalBool("autocomplete", false);
        const saveTag = this.getSaveTag();
        this.proeditor = this.getLocalBool("proeditor",
            saveTag === "par" || saveTag === TIM_TABLE_CELL);
        this.activeTab = this.getLocalValue("editortab") || "navigation";
        this.lastTab = this.activeTab;
        this.citeText = this.getCiteText();
        const sn = this.getLocalValue("wrap");
        let n = parseInt(sn || "-90");
        if (isNaN(n)) {
            n = -90;
        }

        this.wrap = {n: n};

        if (this.getOptions().touchDevice) {
            if (!this.oldmeta) {
                const $meta = $("meta[name='viewport']");
                this.oldmeta = $meta[0] as HTMLMetaElement;
                $meta.remove();
                $("head").prepend('<meta name="viewport" content="width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=0">');
            }
        }
        this.draggable.makeHeightAutomatic();
        const oldMode = window.localStorage.getItem("oldMode" + this.getOptions().localSaveTag) || (this.getOptions().touchDevice ? "text" : "ace");
        this.changeEditor(oldMode);
        this.scope.$watch(() => this.autocomplete, () => {
            if (this.isAce(this.editor)) {
                this.editor.setAutoCompletion(this.autocomplete);
            }
        });
        this.docSettings = $window.docSettings;
    }

    $postLink() {
        this.initCustomTabs();
    }

    $onDestroy() {
        setEditorScope(undefined);
    }

    getCiteText(): string {
        return getCitePar(this.getExtraData().docId, this.getExtraData().par);
    }

    selectAllText(evt: Event) {
        (evt.target as HTMLInputElement).select();
    }

    getLocalBool(name: string, def: boolean): boolean {
        const val = this.storage.getItem(name + this.getSaveTag());
        if (!val) {
            return def;
        }
        return val === "true";
    }

    setLocalValue(name: string, val: string) {
        window.localStorage.setItem(name + this.getSaveTag(), val);
    }

    initCustomTabs() {
        const tabs: {[tab: string]: {[menuName: string]: IEditorMenuItem[] | undefined} | undefined} = {};
        for (const [plugin, d] of Object.entries($window.reqs)) {
            const data = d as unknown;
            let currTabs: Array<[string, MenuNameAndItems]>;
            if (isV3Format(data)) {
                // this needs type casts inside maps because otherwise the types are inferred as plain arrays and not tuples
                currTabs = Object
                    .entries(data.editor_tabs)
                    .map(([k, v]) => [v.text, v.items // editor_tabs is an array, so k is just (unused) index here
                        .map((m) => [m.text, m.items] as [string, MenuItemEntries])] as [string, MenuNameAndItems]);
            } else if (isV2Format(data)) {
                currTabs = Object.entries({plugins: Object.entries(data.templates)});
            } else if (isV1Format(data)) {
                const menus: MenuNameAndItems = [];
                const menuNames = data.text || [plugin];
                for (let i = 0; i < menuNames.length; i++) {
                    menus.push([menuNames[i], data.templates[i]]);
                }
                currTabs = Object.entries({plugins: menus});
            } else {
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
                        const text = (templateObj.text || templateObj.file || templateObj.data);
                        const file = templateObj.file;
                        const tempdata = (templateObj.data || null);
                        let clickfn;
                        if (tempdata) {
                            clickfn = () => {
                                this.putTemplate(tempdata);
                            };
                        } else if (file) {
                            clickfn = async () => {
                                await this.getTemplate(plugin, file, j.toString()); // TODO: Why is index needed for getTemplate...?
                            };
                        } else {
                            continue;
                        }
                        const existingTab = tabs[tab];
                        const item = {name: text, title: templateObj.expl || "", func: clickfn};
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
            this.editorChanged();
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
            const $editor = this.element;
            const $previewContent = this.element.find(".previewcontent");
            // Check that editor doesn't go out of bounds
            const editorOffset = $editor.offset();
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
            $editor.offset(newOffset);
            if (this.scrollPos) {
                $previewContent.scrollTop(this.scrollPos);
            }
        }, 25);

    }

    createTextArea(text: string) {
        const $textarea = $(`
<textarea rows="10"
      id="teksti"
      wrap="off">
</textarea>`);
        this.element.find(".editorContainer").append($textarea);
        this.editor = new TextAreaParEditor(this.element.find("#teksti"), {
            wrapFn: () => this.wrapFn(),
            saveClicked: () => this.saveClicked(),
            getWrapValue: () => this.wrap.n,
        });
        this.editor.setEditorText(text);
        $textarea.on("input", () => this.editorChanged());
        $textarea.on("paste", (e) => this.onPaste(e));
        $textarea.on("drop", (e) => this.onDrop(e));
        $textarea.on("dragover", (e) => this.allowDrop(e));

    }

    editorChanged() {
        this.scope.$evalAsync(async () => {
            const editor = this.editor!;
            this.outofdate = true;
            const text = editor.getEditorText();
            await $timeout(500);
            if (text !== editor.getEditorText()) {
                return;
            }
            this.scrollPos = this.element.find(".previewcontent").scrollTop() || this.scrollPos;
            this.outofdate = true;
            const data = await this.resolve.params.previewCb(text);
            const compiled = await ParCompiler.compile(data, this.scope);
            if (data.trdiff) {
                const module = fixDefExport(await import("angular-diff-match-patch"));
                $injector.loadNewModules([module]);
            }
            this.trdiff = data.trdiff;
            const $previewDiv = angular.element(".previewcontent");
            $previewDiv.empty().append(compiled);
            this.outofdate = false;
            this.parCount = $previewDiv.children().length;
            this.element.find(".editorContainer").resize();
        });
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
        const $meta = $(this.oldmeta);
        $("head").prepend($meta);
    }

    async deleteClicked() {
        if (!this.getOptions().showDelete) {
            this.cancelClicked(); // when empty and save clicked there is no par
            return;
        }
        if (this.deleting) {
            return;
        }
        if (!window.confirm(this.getOptions().deleteMsg || "Delete - are you sure?")) {
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
        return this.getExtraData().par !== "NEW_PAR" && getElementByParId(this.getExtraData().par).find(".readline.read").length > 0;
    }

    async unreadClicked() {
        await this.resolve.params.unreadCb();
        if (this.resolve.params.initialText === this.editor!.getEditorText()) {
            this.close({type: "markunread"});
        }
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
        const text = this.editor!.getEditorText();
        if (text.trim() === "") {
            this.deleteClicked();
            this.saving = false;
            return;
        }
        const result = await this.resolve.params.saveCb(text, this.getExtraData());
        if (result.error) {
            await showMessageDialog(result.error);
            this.saving = false;
            return;
        } else {
            this.close({type: "save", text});
        }
    }

    aceEnabled(): boolean {
        return this.isAce(this.editor);
    }

    isAce(editor: AceParEditor | TextAreaParEditor | undefined): editor is AceParEditor {
        return editor !== undefined && (editor.editor as IAceEditor).renderer != null;
    }

    onPaste(e: any) {
        let event = e as any; // ClipboardEvent;
        let items = (event.clipboardData || event.originalEvent.clipboardData).items;
        // find pasted image among pasted items
        let blob = null;
        let blobs = 0;
        for (let i = 0; i < items.length; i++) {
            // TODO: one could inspect if some item contains image name and then use that to name the image
            if (items[i].type.indexOf("image") === 0) {
                blob = items[i].getAsFile();
                if (blob !== null) {
                    this.onFileSelect(blob)
                    blobs++;
                }
            }
        }
        if ( blobs > 0 ) e.preventDefault();
    }

    onDrop(e: any) {
        e.preventDefault();
        let event = e as DroppableEvent;
        let files = e.originalEvent.dataTransfer.files;
        if ( !files ) return;
        for (let i = 0; i < files.length; i++)
            this.onFileSelect(files[i]);
    }

    allowDrop(e:object) {
        let event = e as DragEvent;
        event.preventDefault();
    }

    loadFiles(items: any) {

    }


    onFileSelect(file: File) {
        const editor = this.editor!;
        this.focusEditor();
        this.file = file;
        const editorText = editor.getEditorText();
        let autostamp = false;
        let attachmentParams;
        let macroParams;

        // To identify attachment-macro. // TODO: jos editorissa monta liitettä, tekee väärin
        const macroStringBegin = "%%liite(";
        const macroStringEnd = ")%%";

        // If there's an attachment macro in editor, assume need to stamp.
        // Also requires data from preamble to work correctly (dates and knro).
        // If there's no stampFormat set in preamble, uses hard coded default format.
        if (editorText.length > 0 && editorText.lastIndexOf(macroStringBegin) > 0 && this.docSettings) {
            autostamp = true;
            try {
                const macroText = editorText.substring(
                    editorText.lastIndexOf(macroStringBegin) + macroStringBegin.length,
                    editorText.lastIndexOf(macroStringEnd));
                macroParams = JSON.parse(`[${macroText}]`);
            } catch {
                throw new Error("Parsing stamp parameters failed");
            }

            // Knro usage starts from 1 but dates starts from 0 but there is dummy item first
            const knro = this.docSettings.macros.knro;
            const dates = this.docSettings.macros.dates;
            // dates = ["ERROR", ...dates];
            const kokousDate = dates[knro][0];  // dates is 2-dim array

            // If stampFormat isn't set in preamble,
            let stampFormat = this.docSettings.macros.stampformat;
            if (stampFormat === undefined) {
                stampFormat = "";
            }
            attachmentParams = [kokousDate, stampFormat, ...macroParams, autostamp];
        }
        if (file) {
            this.file.progress = 0;
            this.file.error = undefined;
            const upload = $upload.upload<{image: string, file: string}>({
                data: {
                    attachmentParams: JSON.stringify(attachmentParams),
                    doc_id: this.getExtraData().docId.toString(),
                    file,
                },
                method: "POST",
                url: "/upload/",
            });

            // TODO: Better plugin check.
            // May fail when there are non-plugin-paragraphs before a plugin-paragraph in editor.
            upload.then((response) => {
                $timeout(() => {
                    const editor = this.editor!;
                    const isplugin = (editor.editorStartsWith("``` {"));
                    let start = "[File](";
                    let savedir = '/files/'
                    if (response.data.image  || response.data.file.toString().indexOf(".svg") >= 0 ) {
                        start = "![Image](";
                    }
                    if (response.data.image ) {
                        savedir = "/images/"
                        this.uploadedFile = savedir + response.data.image;
                    } else {
                        this.uploadedFile = savedir + response.data.file;
                    }
                    if (isplugin) {
                        editor.insertTemplate(this.uploadedFile);
                    } else {
                        editor.insertTemplate(start + this.uploadedFile + ")");
                    }
                });
            }, (response) => {
                if (response.status > 0 && this.file) {
                    this.file.error = response.data.error;
                }
            }, (evt) => {
                if (this.file) {
                    this.file.progress = Math.min(100, Math.floor(100.0 *
                        evt.loaded / evt.total));
                }
            });

            upload.finally(() => {
            });
        }
    }

    putTemplate(data: string) {
        this.focusEditor();
        this.editor!.insertTemplate(data);
    }

    async getTemplate(plugin: string, template: string, index: string) {
        const response = await $http.get<string>(`/${plugin}/template/${template}/${index}`);
        let data = response.data;
        data = data.replace(/\\/g, "\\\\");
        this.editor!.insertTemplate(data);
        this.focusEditor();
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

    /**
     * @returns {boolean} true if device supports fullscreen, otherwise false
     */
    fullscreenSupported() {
        const div: any = $(this.element).get(0);
        const requestMethod = div.requestFullScreen ||
            div.webkitRequestFullscreen ||
            div.webkitRequestFullScreen ||
            div.mozRequestFullScreen ||
            div.msRequestFullscreen;
        return (typeof (requestMethod) !== "undefined");
    }

    /**
     * Makes editor div fullscreen
     */
    goFullScreen() {
        const div: any = this.element[0];
        const doc: any = document;
        if (!doc.fullscreenElement &&    // alternative standard method
            !doc.mozFullScreenElement && !doc.webkitFullscreenElement && !doc.msFullscreenElement) {

            const requestMethod = div.requestFullScreen ||
                div.webkitRequestFullscreen ||
                div.webkitRequestFullScreen ||
                div.mozRequestFullScreen ||
                div.msRequestFullscreen;

            if (requestMethod) {
                requestMethod.apply(div);
                div.setAttribute("style", "width: 100%; height: 100%; position: absolute; top: 0px;" +
                    "padding: 2em 5px 5px 5px; background: rgb(224, 224, 224); -webkit-box-sizing: border-box;" +
                    "-moz-box-sizing: border-box; box-sizing: border-box;");
            }
        } else {
            if (doc.exitFullscreen) {
                doc.exitFullscreen();
            } else if (doc.msExitFullscreen) {
                doc.msExitFullscreen();
            } else if (doc.mozCancelFullScreen) {
                doc.mozCancelFullScreen();
            } else if (doc.webkitExitFullscreen) {
                doc.webkitExitFullscreen();
            }
        }
    }

    /**
     * Switches editor between Ace and textarea.
     */
    async changeEditor(initialMode?: string) {
        let text = "";
        const editorContainer = this.element.find(".editorContainer");
        // const pasteInput = this.element.find(".pasteinput");
        // pasteInput.on("drop", (e) => this.onDrop(e));
        // pasteInput.on("dragover", (e) => this.allowDrop(e));
        const pasteInput = document.getElementById("pasteInput");
        if ( pasteInput ) {
            pasteInput.ondrop = (e) => this.onDrop(e);
            pasteInput.ondragover = (e) => this.allowDrop(e);
        }
        editorContainer.addClass("editor-loading");
        let oldPosition;
        if (this.editor) {
            text = this.editor.getEditorText();
            oldPosition = this.editor.getPosition();
        }
        let oldeditor;
        if (this.isAce(this.editor) || initialMode === "text") {
            oldeditor = this.element.find("#ace_editor");
            oldeditor.remove();
            this.createTextArea(text);
            this.isACE = false;
        } else {
            this.isACE = true;
            oldeditor = this.element.find("#teksti");
            oldeditor.remove();
            const ace = (await import("tim/editor/ace")).ace;
            const neweditorElem = $("<div>", {
                class: "editor",
                id: "ace_editor",
            });
            editorContainer.append(neweditorElem);
            const neweditor = ace.edit(neweditorElem[0]);

            this.editor = new AceParEditor(ace, neweditor, {
                wrapFn: () => this.wrapFn(),
                saveClicked: () => this.saveClicked(),
                getWrapValue: () => this.wrap.n,
            }, (this.getSaveTag() === "addAbove" || this.getSaveTag() === "addBelow") ? "ace/mode/text" : "ace/mode/markdown");
            this.editor.setAutoCompletion(this.autocomplete);
            this.editor.editor.renderer.$cursorLayer.setBlinking(!$window.IS_TESTING);
            /*iPad does not open the keyboard if not manually focused to editable area
             var iOS = /(iPad|iPhone|iPod)/g.test($window.navigator.platform);
             if (!iOS) editor.focus();*/

            neweditor.getSession().on("change", () => {
                this.editorChanged();
            });
            neweditor.getSession().on("paste", (e) => {
                this.onPaste(e); // TODO: does newer fire
            });
            neweditor.getSession().on("drop", (e) => {
                this.onDrop(e); // TODO: does newer fire
            });
            neweditor.getSession().on("dragover", (e) => {
                this.allowDrop(e); // TODO: does newer fire
            });
            neweditor.onPaste = (e) => {
               // this.onPaste(e); // only works for text input.
                return;
            };
            neweditor.setBehavioursEnabled(this.getLocalBool("acebehaviours", false));
            neweditor.getSession().setUseWrapMode(this.getLocalBool("acewrap", false));
            neweditor.setOptions({maxLines: 28});
            this.editor.setEditorText(text);

            const langTools = ace.require("ace/ext/language_tools");

            const wordListStr = (await $http.get<{word_list: string}>("/settings/get/word_list", {params: {_: Date.now()}})).data.word_list;
            const userWordList = wordListStr ? wordListStr.split("\n") : [];
            const createCompleter = (wordList: string[], context: string) => ({
                getCompletions(editor: any, session: any, pos: any, prefix: any, callback: any) {
                    callback(null, wordList.map((word) => ({
                        caption: word,
                        meta: context,
                        value: word,
                    })));
                },
            });
            langTools.setCompleters([
                langTools.snippetCompleter,
                langTools.textCompleter,
                langTools.keyWordCompleter,
                createCompleter($window.wordList, "document"),
                createCompleter(userWordList, "user"),
            ]);
        }
        if (initialMode != null) {
            await this.setInitialText();
        } else if (this.editor && oldPosition) {
            this.editor.setPosition(oldPosition);
        }
        await this.focusEditor();
        this.element.find(".editorContainer").removeClass("editor-loading");
        setEditorScope(this.editor!);
        this.adjustPreview();
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
        const trs: IItem[] = $window.translations;
        const orig = trs.find((t) => t.id === t.src_docid);
        if (orig) {
            const parId = this.getExtraData().par;
            const par = getElementByParId(parId);
            const rp = getParAttributes(par).rp;
            return `/view/${orig.path}#${rp}`;
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

    private getLocalValue(val: string) {
        return this.storage.getItem(val + this.getSaveTag());
    }

    private async focusEditor() {
        await $timeout();
        const s = $(window).scrollTop();
        this.editor!.focus();
        await $timeout();
        $(window).scrollTop(s || this.scrollPos || 0);
    }

    private saveOptions() {
        this.setLocalValue("editortab", this.activeTab || "navigation");
        this.setLocalValue("autocomplete", this.autocomplete.toString());
        this.setLocalValue("oldMode", this.isAce(this.editor) ? "ace" : "text");
        this.setLocalValue("wrap", "" + this.wrap.n);
        if (this.getExtraData().access != null) {
            $localStorage.noteAccess = this.getExtraData().access;
        }
        for (const [k, v] of Object.entries(this.getExtraData().tags)) {
            if (v != null) {
                window.localStorage.setItem(k, v.toString());
            }
        }
        this.setLocalValue("proeditor", this.proeditor.toString());

        if (this.isAce(this.editor)) {
            this.setLocalValue("acewrap", this.editor.editor.getSession().getUseWrapMode().toString());
            this.setLocalValue("acebehaviours", this.editor.editor.getBehavioursEnabled().toString()); // some of these are in editor and some in session?
        }
    }
}

timApp.component("timEditorMenu", {
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

timApp.component("timEditorEntry", {
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

registerDialogComponent("pareditor",
    PareditorController,
    {templateUrl: "/static/templates/parEditor.html"});

export function openEditor(p: IEditorParams): IPromise<IEditorResult> {
    return showDialog<PareditorController>(
        "pareditor",
        {params: () => p},
        {saveKey: p.options.localSaveTag, absolute: true, size: p.defaultSize, forceMaximized: true}).result;
}

export function openEditorSimple(docId: number, text: string, caption: string, localSaveTag: string) {
    return openEditor({
        defaultSize: "lg",
        initialText: text,
        extraData: {docId, tags: {markread: false}, par: "nothing"}, options: {
            caption: caption,
            choices: undefined,
            localSaveTag: localSaveTag,
            showDelete: false,
            showImageUpload: true,
            showPlugins: false,
            showSettings: false,
            tags: [],
            touchDevice: false,
        }, previewCb: async (txt) => {
            const resp = await $http.post<IPluginInfoResponse>(`/preview/${docId}`, {text: txt, isComment: true});
            return resp.data;
        },
        saveCb: async (txt, data) => {
            return await {};
        },
        deleteCb: async () => {
            return await {};
        },
        unreadCb: async () => {
        },
    });
}

export function test() {
    const fns = [];
    let i = -1;
    for (const x of "hi") {
        i++;
        fns.push(() => console.log(i));
    }
    for (const f of fns) {
        f();
    }
}
