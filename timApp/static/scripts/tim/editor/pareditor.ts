import angular, {IPromise, IRootElementService, IScope} from "angular";
import $ from "jquery";
import rangyinputs from "rangyinputs";
import {setEditorScope} from "tim/editor/editorScope";
import sessionsettings from "tim/session";
import {markAsUsed, setSetting} from "tim/util/utils";
import {IExtraData} from "../document/editing/edittypes";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../ui/dialog";
import {$compile, $http, $localStorage, $log, $timeout, $upload, $window} from "../util/ngimport";
import {IAceEditor} from "./ace-types";
import {AceParEditor} from "./AceParEditor";
import {IPluginInfoResponse, ParCompiler} from "./parCompiler";
import {TextAreaParEditor} from "./TextAreaParEditor";
import {getElementByParId} from "../document/parhelpers";

markAsUsed(rangyinputs);

const MENU_BUTTON_CLASS = "menuButtons";
const MENU_BUTTON_CLASS_DOT = "." + MENU_BUTTON_CLASS;
const TIM_TABLE_CELL = "timTableCell";

export interface IPreviewResult {
    html: string;
}

export interface ITag {
    name: string;
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

export type IEditorResult = {type: "save", text: string} | {type: "delete"} | {type: "markunread"} | {type: "cancel"};

export function getCitePar(docId: number, par: string) {
    return `#- {rd="${docId}" rl="no" rp="${par}"}`;
}

export class PareditorController extends DialogController<{params: IEditorParams}, IEditorResult, "pareditor"> {
    private static $inject = ["$scope", "$element"];
    private deleting = false;
    private editor?: TextAreaParEditor | AceParEditor; // $onInit
    private file?: File & {progress?: number, error?: string};
    private isIE: boolean = false;
    private oldmeta?: HTMLMetaElement;
    private wrap!: {n: number}; // $onInit
    private outofdate: boolean;
    private parCount: number;
    private pluginButtonList: {[tabName: string]: JQuery[]};
    private proeditor!: boolean; // $onInit
    private saving: boolean = false;
    private scrollPos?: number;
    private tables: {
        normal: string,
        example: string,
        noheaders: string,
        multiline: string,
        strokes: string,
        pipe: string,
        timTable1x1: string,
        timTable2x2: string,
    };
    private storage: Storage;
    private touchDevice: boolean;
    private plugintab?: JQuery;
    private autocomplete!: boolean; // $onInit
    private citeText!: string; // $onInit
    private docSettings?: {macros: {dates: string[], knro: number, stampformat: string}};
    private uploadedFile?: string;

    private getOptions() {
        return this.resolve.params.options;
    }

    private getExtraData() {
        return this.resolve.params.extraData;
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

    private getSaveTag() {
        return this.getOptions().localSaveTag;
    }

    constructor(protected scope: IScope, protected element: IRootElementService) {
        super(element, scope);
        this.storage = localStorage;

        this.pluginButtonList = {};

        if ((navigator.userAgent.match(/Trident/i))) {
            this.isIE = true;
        }

        this.element.find(".editorContainer").on("resize", () => this.adjustPreview());

        const backTicks = "```";

        this.tables = {
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

    $onInit() {
        super.$onInit();
        this.autocomplete = this.getLocalBool("autocomplete", false);
        const saveTag = this.getSaveTag();
        this.proeditor = this.getLocalBool("proeditor",
            saveTag === "par" || saveTag === TIM_TABLE_CELL);
        this.citeText = this.getCiteText();
        const sn = this.storage.getItem("wrap" + this.getSaveTag());
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
        const oldMode = $window.localStorage.getItem("oldMode" + this.getOptions().localSaveTag) || (this.getOptions().touchDevice ? "text" : "ace");
        this.changeEditor(oldMode);
        this.scope.$watch(() => this.autocomplete, () => {
            if (this.isAce(this.editor)) {
                this.editor.setAutoCompletion(this.autocomplete);
            }
        });
        this.docSettings = $window.docSettings;
    }

    $postLink() {
        this.plugintab = this.element.find("#pluginButtons");
        this.getPluginsInOrder();

        if (sessionsettings.editortab) {
            const tab = sessionsettings.editortab.substring(0, sessionsettings.editortab.lastIndexOf("Buttons"));
            const tabelement = this.element.find("#" + tab);
            if (tabelement.length) {
                this.setActiveTab(tabelement, sessionsettings.editortab);
            }
        }
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
        let ret = def;
        if (!ret) {
            ret = false;
        }
        const val = this.storage.getItem(name + this.getSaveTag());
        if (!val) {
            return ret;
        }
        return val === "true";
    }

    setLocalValue(name: string, val: string) {
        $window.localStorage.setItem(name + this.getSaveTag(), val);
    }

    /*
Template format is either the old plugin syntax:

    {
        'text' : ['my1', 'my2'],      // list of tabs firts
        'templates' : [               // and then array of arrays of items
            [
                {'data': 'cat', 'expl': 'Add cat', 'text': 'Cat'},
                {'data': 'dog', 'expl': 'Add dog'},
            ],
            [
                {'data': 'fox', 'expl': 'Add fox', 'text': 'Fox'},
            ]
        ]
    }

or newer one that is more familiar to write in YAML:

    {
        'templates' :
          { 'my1':  // list of objects where is the name of tab as a key
            [
                {'data': 'cat', 'expl': 'Add cat', 'text': 'Cat'},
                {'data': 'dog', 'expl': 'Add dog'},
            ]
           ,
           'my2':  // if only one, does not need to be array
                {'data': 'fox', 'expl': 'Add fox', 'text': 'Fox'},
          }
    }

 */
    getPluginsInOrder() {
        for (const plugin in $window.reqs) {
            if ($window.reqs.hasOwnProperty(plugin)) {
                const data = $window.reqs[plugin];
                if (data.templates) {
                    const isobj = !(data.templates instanceof Array);
                    let tabs = data.text || [plugin];
                    if (isobj) { tabs = Object.keys(data.templates); }
                    const len = tabs.length;
                    for (let j = 0; j < len; j++) {
                        const tab = tabs[j];
                        let templs = null;
                        if (isobj) {
                            templs = data.templates[tab];
                        } else {
                            templs = data.templates[j];
                        }
                        if (!(templs instanceof Array)) { templs = [templs]; }
                        if (!this.pluginButtonList[tab]) {
                            this.pluginButtonList[tab] = [];
                        }
                        for (let k = 0; k < templs.length; k++) {
                            let template = templs[k];
                            if (!(template instanceof Object)) {
                                template = {text: template, data: template};
                            }
                            const text = (template.text || template.file || template.data);
                            const tempdata = (template.data || null);
                            let clickfn;
                            if (tempdata) {
                                clickfn = `$ctrl.putTemplate(${JSON.stringify(tempdata)})`;
                            } else {
                                clickfn = `$ctrl.getTemplate(${JSON.stringify(plugin)}, ${JSON.stringify(template.file)}, ${JSON.stringify(j)})`;
                            }
                            this.pluginButtonList[tab].push(this.createMenuButton(text, template.expl, clickfn));
                        }
                    }
                }
            }
        }

        if (!this.plugintab) {
            throw new Error("Plugin tab was not initialized");
        }

        for (const key in this.pluginButtonList) {
            if (this.pluginButtonList.hasOwnProperty(key)) {
                const clickfunction = `$ctrl.pluginClicked($event, ${JSON.stringify(key)})`;
                const button = $("<button>", {
                    "class": "editorButton",
                    "text": key,
                    "title": key,
                    "ng-click": clickfunction,
                });
                this.plugintab.append($compile(button)(this.scope));
            }
        }

        const help = $("<a>", {
            class: "helpButton",
            text: "[?]",
            title: "Help for plugin attributes",
            onclick: "window.open('https://tim.jyu.fi/view/tim/ohjeita/csPlugin', '_blank')",
        });
        this.plugintab.append($compile(help)(this.scope));
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

    private async focusEditor() {
        await $timeout();
        const s = $(window).scrollTop();
        this.editor!.focus();
        await $timeout();
        $(window).scrollTop(s || this.scrollPos || 0);
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

    private saveOptions() {
        this.setLocalValue("autocomplete", this.autocomplete.toString());
        this.setLocalValue("oldMode", this.isAce(this.editor) ? "ace" : "text");
        this.setLocalValue("wrap", "" + this.wrap.n);
        if (this.getExtraData().access != null) {
            $localStorage.noteAccess = this.getExtraData().access;
        }
        if (this.getExtraData().tags.markread != null) { // TODO: use a loop
            $window.localStorage.setItem("markread", this.getExtraData().tags.markread.toString());
        }
        this.setLocalValue("proeditor", this.proeditor.toString());

        if (this.isAce(this.editor)) {
            this.setLocalValue("acewrap", this.editor.editor.getSession().getUseWrapMode().toString());
            this.setLocalValue("acebehaviours", this.editor.editor.getBehavioursEnabled().toString()); // some of these are in editor and some in session?
        }
    }

    aceEnabled(): boolean {
        return this.isAce(this.editor);
    }

    isAce(editor: AceParEditor | TextAreaParEditor | undefined): editor is AceParEditor {
        return editor !== undefined && (editor.editor as IAceEditor).renderer != null;
    }

    onFileSelect(file: File) {
        const editor = this.editor!;
        this.focusEditor();
        this.file = file;
        const editorText = editor.getEditorText();
        let autostamp = false;
        let attachmentParams;
        let macroParams;

        // To identify attachment-macro.
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
            let dates = this.docSettings.macros.dates;
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
                    if (response.data.image) {
                        this.uploadedFile = "/images/" + response.data.image;
                        start = "![Image](";
                    } else {
                        this.uploadedFile = "/files/" + response.data.file;
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

    closeMenu(e: JQueryEventObject | null, force: boolean) {
        const container = $(MENU_BUTTON_CLASS_DOT);
        if (force || (e != null && !container.is(e.target) && container.has(e.target as Element).length === 0)) {
            container.remove();
            $(document).off("mouseup.closemenu");
        }
    }

    createMenuButton(text: string, title: string, clickfunction: string) {
        const $span = $("<span>", {class: "actionButtonRow"});
        const button_width = 130;
        $span.append($("<button>", {
            "class": "timButton editMenuButton",
            "text": text,
            "title": title,
            "ng-click": clickfunction + "; $ctrl.closeMenu($event, true); $ctrl.wrapFn()",
            // "width": button_width,
        }));
        return $span;
    }

    createMenu($event: Event, buttons: JQuery[]) {
        if (!$event.target) {
            return;
        }
        this.closeMenu(null, true);
        const $button = $($event.target);
        const coords = {left: $button.position().left, top: $button.position().top};
        let $actionDiv = $("<div>", {class: MENU_BUTTON_CLASS});

        for (let i = 0; i < buttons.length; i++) {
            $actionDiv.append(buttons[i]);
        }

        $actionDiv.append(this.createMenuButton("Close menu", "", ""));
        $actionDiv.offset(coords);
        $actionDiv.css("position", "absolute"); // IE needs this
        $actionDiv = $compile($actionDiv)(this.scope);
        $button.parent().prepend($actionDiv);
        $(document).on("mouseup.closemenu", (e: JQueryEventObject) => this.closeMenu(e, false));
    }

    tableClicked($event: Event) {
        const buttons = [];
        for (const key in this.tables) {
            if (this.tables.hasOwnProperty(key)) {
                const text = key.charAt(0).toUpperCase() + key.substring(1);
                const clickfn = `$ctrl.editor.insertTemplate($ctrl.tables[${JSON.stringify(key)}])`;
                buttons.push(this.createMenuButton(text, "", clickfn));
            }
        }
        this.createMenu($event, buttons);
    }

    slideClicked($event: Event) {
        const buttons = [];
        buttons.push(this.createMenuButton("Slide break", "Break text to start a new slide", "$ctrl.editor.ruleClicked()"));
        buttons.push(this.createMenuButton("Slide fragment", "Content inside the fragment will be hidden and shown when next is clicked in slide view", "$ctrl.editor.surroundClicked('§§', '§§')"));
        buttons.push(this.createMenuButton("Fragment block", "Content inside will show as a fragment and may contain inner slide fragments", "$ctrl.editor.surroundClicked('<§', '§>')"));
        this.createMenu($event, buttons);
    }

    pluginClicked($event: Event, key: string) {
        this.createMenu($event, this.pluginButtonList[key]);
    }

    putTemplate(data: string) {
        this.focusEditor();
        this.editor!.insertTemplate(data);
    }

    getTemplate(plugin: string, template: string, index: string) {
        $.ajax({
            type: "GET",
            url: "/" + plugin + "/template/" + template + "/" + index,
            dataType: "text",
            processData: false,
            success: (data) => {
                data = data.replace(/\\/g, "\\\\");
                this.editor!.insertTemplate(data);
            },
            error() {
                $log.error("Error getting template");
            },
        });
        this.focusEditor();
    }

    tabClicked($event: Event, area: string) {
        if (!$event.target) {
            return;
        }
        const active = $($event.target as HTMLElement).parent();
        setSetting("editortab", area);
        this.setActiveTab(active, area);
        this.wrapFn();
    }

    /**
     * Sets active tab
     * @param active tab <li> element
     * @param area area to make visible
     */
    setActiveTab(active: JQuery, area: string) {
        const naviArea = this.element.find("#" + area);
        const buttons = this.element.find(".extraButtonArea");
        const tabs = this.element.find(".tab");
        for (let i = 0; i < buttons.length; i++) {
            $(buttons[i]).attr("class", "extraButtonArea hidden");
        }
        for (let j = 0; j < tabs.length; j++) {
            $(tabs[j]).removeClass("active");
        }
        $(active).attr("class", "tab active");
        $(naviArea).attr("class", "extraButtonArea");
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
            str += " ..."
        }
        return str;
    }
}

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
