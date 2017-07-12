// TODO: save cursor position when changing editor

import angular, {IPromise, IController, IRootElementService, IScope} from "angular";
import $ from "jquery";
import rangyinputs from "rangyinputs";
import {timApp} from "tim/app";
import * as draggable from "tim/directives/draggable";
import {setEditorScope} from "tim/editorScope";
import {markAsUsed} from "tim/utils";
import {setsetting} from "tim/utils";
import Editor = AceAjax.Editor;
import VirtualRenderer = AceAjax.VirtualRenderer;
import Ace = AceAjax.Ace;
import * as acemodule from "tim/ace";
import {lazyLoadTS} from "../lazyLoad";
import {$compile, $http, $localStorage, $log, $timeout, $upload, $window} from "../ngimport";
import {ParCompiler} from "../services/parCompiler";
import {getActiveDocument} from "../controllers/view/document";
import {TextAreaParEditor} from "./TextAreaParEditor";
import {AceParEditor} from "./AceParEditor";

markAsUsed(draggable, rangyinputs);

const MENU_BUTTON_CLASS = "menuButtons";
const MENU_BUTTON_CLASS_DOT = "." + MENU_BUTTON_CLASS;

export class PareditorController implements IController {
    private static $inject = ["$scope", "$element"];
    private afterDelete: (params: {extraData: {}, saveData: {}}) => void;
    private afterCancel: (params: {extraData: {}}) => void;
    private afterSave: (params: {extraData: {}, saveData: {}}) => void;
    private data: {original_par: any};
    private dataLoaded: boolean;
    private deleteUrl: string;
    private deleting: boolean;
    private duplicates: any[];
    private editor: TextAreaParEditor | AceParEditor;
    private element: JQuery;
    private extraData: {
        attrs: {classes: string[]},
        docId: number,
        par: string,
        access: string,
        tags: {markread: boolean},
        isComment: boolean,
    };
    private file: any;
    private initialText: string;
    private initialTextUrl: string;
    private inputs: JQuery[];
    private isIE: boolean;
    private lstag: string;
    private minSizeSet: boolean;
    private newAttr: string;
    private newPars: string[];
    private oldmeta: HTMLMetaElement;
    private options: {
        localSaveTag: string,
        showDelete: boolean,
        destroyAfterSave: boolean,
        touchDevice: boolean,
        metaset: boolean,
    };
    private originalPar: any;
    private outofdate: boolean;
    private parCount: number;
    private pluginButtonList: {[tabName: string]: JQuery[]};
    private pluginRenameForm: any;
    private previewReleased: boolean;
    private previewUrl: string;
    private proeditor: boolean;
    private renameFormShowing: boolean;
    private saveUrl: string;
    private saving: boolean;
    private scrollPos: number;
    private settings: {editortab: string};
    private tables: {
        normal: string,
        example: string,
        noheaders: string,
        multiline: string,
        strokes: string,
        pipe: string,
    };
    private timer: IPromise<void>;
    private unreadUrl: string;
    private uploadedFile: string;
    private scope: IScope;
    private storage: Storage;
    private touchDevice: boolean;
    private tag: string;
    private plugintab: JQuery;

    constructor(scope: IScope, element: IRootElementService) {
        this.scope = scope;
        this.tag = this.options.localSaveTag || "";
        this.storage = localStorage;
        this.lstag = this.tag;

        this.proeditor = this.getLocalBool("proeditor", this.tag === "par");

        this.settings = $window.sessionsettings;
        this.pluginButtonList = {};

        if ((navigator.userAgent.match(/Trident/i))) {
            this.isIE = true;
        }

        this.dataLoaded = false; // allow load in first time what ever editor

        $(".editorContainer").on("resize", this.adjustPreview);

        /* Add citation info to help tab */
        document.getElementById("helpCite").setAttribute("value", '#- {rd="' + this.extraData.docId + '" rl="no" rp="' + this.extraData.par + '"}');

        this.element = element;

        this.tables = {normal: null, example: null, noheaders: null, multiline: null, pipe: null, strokes: null};

        this.tables.normal = "Otsikko1 Otsikko2 Otsikko3 Otsikko4\n" +
            "-------- -------- -------- --------\n" +
            "1.rivi   x        x        x       \n" +
            "2.rivi   x        x        x       ";

        this.tables.example = "Table:  Otsikko taulukolle\n\n" +
            "Otsikko    Vasen laita    Keskitetty    Oikea laita\n" +
            "---------- ------------- ------------ -------------\n" +
            "1. rivi      2                  3         4\n" +
            "2. rivi        1000      2000             30000";

        this.tables.noheaders = ":  Otsikko taulukolle\n\n" +
            "---------- ------------- ------------ -------------\n" +
            "1. rivi      2                  3         4\n" +
            "2. rivi        1000      2000             30000\n" +
            "---------- ------------- ------------ -------------\n";

        this.tables.multiline = "Table:  Otsikko taulukolle voi\n" +
            "jakaantua usealle riville\n\n" +
            "-----------------------------------------------------\n" +
            "Ekan       Toisen\         kolmas\            neljäs\\\n" +
            "sarkkeen   sarakkeen\     keskitettynä      oikeassa\\\n" +
            "otsikko    otsikko                           reunassa\n" +
            "---------- ------------- -------------- -------------\n" +
            "1. rivi     toki\              3         4\n" +
            "voi olla    sisältökin\n" +
            "useita        voi\\\n" +
            "rivejä      olla \n" +
            "            monella\\\n" +
            "            rivillä\n" +
            "            \n" +
            "2. rivi        1000      2000             30000\n" +
            "-----------------------------------------------------\n";
        this.tables.strokes = ": Viivoilla tehty taulukko\n\n" +
            "+---------------+---------------+----------------------+\n" +
            "| Hedelmä       | Hinta         | Edut                 |\n" +
            "+===============+===============+======================+\n" +
            "| Banaani       |  1.34 €       | - valmis kääre       |\n" +
            "|               |               | - kirkas väri        |\n" +
            "+---------------+---------------+----------------------+\n" +
            "| Appelsiini    |  2.10 €       | - auttaa keripukkiin |\n" +
            "|               |               | - makea              |\n" +
            "+---------------+---------------+----------------------+\n";

        this.tables.pipe = ": Taulukko, jossa tolpat määräävat sarkkeiden paikat.\n\n" +
            "|Oikea  | Vasen | Oletus | Keskitetty |\n" +
            "|------:|:-----|---------|:------:|\n" +
            "|   12  |  12  |    12   |    12  |\n" +
            "|  123  |  123 |   123   |   123  |\n" +
            "|    1  |    1 |     1   |     1  |\n";

        $(document).on("webkitfullscreenchange mozfullscreenchange fullscreenchange MSFullscreenChange", (event) => {
            const editor = $(element).find("#pareditor").get(0);
            const doc: any = document;
            if (!doc.fullscreenElement &&    // alternative standard method
                !doc.mozFullScreenElement && !doc.webkitFullscreenElement && !doc.msFullscreenElement) {
                editor.removeAttribute("style");
            }
        });

        this.timer = null;
        this.outofdate = false;
        this.parCount = 0;
        this.touchDevice = false;

        const oldMode = $window.localStorage.getItem("oldMode" + this.options.localSaveTag) || (this.options.touchDevice ? "text" : "ace");
        this.changeEditor(oldMode);

        if (this.options.touchDevice) {
            if (!this.options.metaset) {
                const $meta = $("meta[name='viewport']");
                this.oldmeta = $meta[0] as HTMLMetaElement;
                $meta.remove();
                $("head").prepend('<meta name="viewport" content="width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=0">');
            }
            this.options.metaset = true;
        }

        const viewport: any = {};
        viewport.top = $(window).scrollTop();
        viewport.bottom = viewport.top + $(window).height();
        const bounds: any = {};
        bounds.top = element.offset().top;
        bounds.bottom = bounds.top + element.outerHeight();
        if (bounds.bottom > viewport.bottom || bounds.top < viewport.top) {
            $("html, body").scrollTop(element.offset().top);
        }
    }

    $postLink() {
        this.plugintab = $("#pluginButtons");
        this.getPluginsInOrder();

        if (this.settings.editortab) {
            const tab = this.settings.editortab.substring(0, this.settings.editortab.lastIndexOf("Buttons"));
            const tabelement = $("#" + tab);
            if (tabelement.length) {
                this.setActiveTab(tabelement, this.settings.editortab);
            }
        }
    }

    $onDestroy() {
        setEditorScope(null);
    }

    getLocalBool(name, def) {
        let ret = def;
        if (!ret) {
            ret = false;
        }
        const val = this.storage.getItem(name + this.lstag);
        if (!val) {
            return ret;
        }
        return val === "true";
    }

    setLocalValue(name, val) {
        $window.localStorage.setItem(name + this.lstag, val);
    }

    setEditorMinSize() {
        const editor = $("pareditor");
        this.previewReleased = false;

        const editorOffsetStr = this.storage.getItem("editorReleasedOffset" + this.tag);
        if (editorOffsetStr) {
            const editorOffset = JSON.parse(editorOffsetStr);
            editor.css("left", editorOffset.left - editor.offset().left);
        }

        if (this.storage.getItem("previewIsReleased" + this.tag) === "true") {
            this.releaseClicked();
        }
        this.minSizeSet = true;
    }

    deleteAttribute(key) {
        delete this.extraData.attrs[key];
    }

    deleteClass(classIndex) {
        this.extraData.attrs.classes.splice(classIndex, 1);
    }

    addClass() {
        this.extraData.attrs.classes.push("");
    }

    addAttribute() {
        if (this.newAttr === "classes") {
            this.extraData.attrs[this.newAttr] = [];
        } else {
            this.extraData.attrs[this.newAttr] = "";
        }
        this.newAttr = "";
    }

    getPluginsInOrder() {
        for (const plugin in $window.reqs) {
            if ($window.reqs.hasOwnProperty(plugin)) {
                const data = $window.reqs[plugin];
                if (data.templates) {
                    const tabs = data.text || [plugin];
                    for (let j = 0; j < tabs.length; j++) {
                        const tab = tabs[j];
                        if (!this.pluginButtonList[tab]) {
                            this.pluginButtonList[tab] = [];
                        }
                        for (let k = 0; k < data.templates[j].length; k++) {
                            const template = data.templates[j][k];
                            const text = (template.text || template.file);
                            const clickfn = `$ctrl.getTemplate('${plugin}','${template.file}', '${j}'); $ctrl.wrapFn()`;
                            this.pluginButtonList[tab].push(this.createMenuButton(text, template.expl, clickfn));
                        }
                    }
                }
            }
        }

        for (const key in this.pluginButtonList) {
            if (this.pluginButtonList.hasOwnProperty(key)) {
                const clickfunction = "$ctrl.pluginClicked($event, '" + key + "')";
                const button = $("<button>", {
                    "class": "editorButton",
                    "text": key,
                    "title": key,
                    "ng-click": clickfunction,
                });
                this.plugintab.append($compile(button)(this.scope));
            }
        }
    }

    async setInitialText() {
        if (this.dataLoaded || !this.initialTextUrl) {
            return;
        }
        this.editor.setEditorText("Loading text...");
        this.dataLoaded = true; // prevent data load in future
        const response = await $http.get<{text: string, extraData: any}>(this.initialTextUrl, {
            params: this.extraData,
        });
        const data = response.data;
        this.editor.setEditorText(data.text);
        this.initialText = data.text;
        angular.extend(this.extraData, data.extraData);
        this.editorChanged();
    }

    adjustPreview() {
        window.setTimeout(() => {
            const $editor = $(".editorArea");
            const $previewContent = $(".previewcontent");
            const previewDiv = $("#previewDiv");
            // If preview is released make sure that preview doesn't go out of bounds
            if (this.previewReleased) {
                const previewOffset = previewDiv.offset();
                if (previewOffset.top < 0 /*|| previewOffset.top > $window.innerHeight */) {
                    previewDiv.offset({top: 0, left: previewDiv.offset().left});
                }
                if (previewOffset.left < 0 || previewOffset.left > $window.innerWidth) {
                    previewDiv.offset({top: previewDiv.offset().top, left: 0});
                }
            }
            // Check that editor doesn't go out of bounds
            const editorOffset = $editor.offset();
            if (editorOffset.top < 0) {
                $editor.offset({top: 0, left: $editor.offset().left});
            }
            if (editorOffset.left < 0) {
                $editor.offset({top: $editor.offset().top, left: 0});
            }
            $previewContent.scrollTop(this.scrollPos);
        }, 25);

    }

    createTextArea(text: string) {
        if (!this.minSizeSet) {
            this.setEditorMinSize();
        }
        const $textarea = $(`
<textarea rows="10"
      id="teksti"
      wrap="off">
</textarea>`);
        $(".editorContainer").append($textarea);
        this.editor = new TextAreaParEditor($("#teksti"), () => this.wrapFn(), () => this.saveClicked());
        this.editor.setEditorText(text);
    }

    editorReady() {
        this.editor.focus();
        this.editor.bottomClicked();
    }

    editorChanged() {
        this.scope.$evalAsync(() => {
            this.outofdate = true;
            if (this.timer) {
                $timeout.cancel(this.timer);
            }

            this.timer = $timeout(() => {
                const text = this.editor.getEditorText();
                this.scrollPos = $(".previewcontent").scrollTop();
                $http.post(this.previewUrl, angular.extend({
                    text,
                }, this.extraData)).then(async (response) => {
                    const data = response.data;
                    const compiled = await ParCompiler.compile(data, this.scope);
                    const $previewDiv = angular.element(".previewcontent");
                    $previewDiv.empty().append(compiled);
                    this.outofdate = false;
                    this.parCount = $previewDiv.children().length;
                    $(".editorContainer").resize();
                }, (response) => {
                    $window.alert("Failed to show preview: " + response.data.error);
                });
                this.outofdate = true;
            }, 500);
        });
    }

    wrapFn(func = null) {
        if (!this.touchDevice) {
            // For some reason, on Chrome, re-focusing the editor messes up scroll position
            // when clicking a tab and moving mouse while clicking, so
            // we save and restore it manually.
            const s = $(window).scrollTop();
            this.editor.focus();
            $(window).scrollTop(s);
        }
        if (func !== null) {
            func();
        }
        if (this.isIE) {
            this.editorChanged();
        }
    }

    changeMeta() {
        $("meta[name='viewport']").remove();
        const $meta = $(this.oldmeta);
        $("head").prepend($meta);
    }

    deleteClicked() {
        if (!this.options.showDelete) {
            this.cancelClicked(); // when empty and save clicked there is no par
            return;
        }
        if (this.deleting) {
            return;
        }
        if (!$window.confirm("Delete - are you sure?")) {
            return;
        }
        this.deleting = true;

        $http.post(this.deleteUrl, this.extraData).then((response) => {
            const data = response.data;
            this.afterDelete({
                extraData: this.extraData,
                saveData: data,
            });
            if (this.options.destroyAfterSave) {
                this.element.remove();
            }
            this.deleting = false;
        }, (response) => {
            $window.alert("Failed to delete: " + response.data.error);
            this.deleting = false;
        });
        if (this.options.touchDevice) {
            this.changeMeta();
        }
    }

    showUnread() {
        return this.extraData.par !== "NEW_PAR" && this.element.parents(".par").find(".readline.read").length > 0;
    }

    unreadClicked() {
        if (this.options.touchDevice) {
            this.changeMeta();
        }
        $http.put(this.unreadUrl + "/" + this.extraData.par, {}).then((response) => {
            this.element.parents(".par").find(".readline").removeClass("read read-modified");
            if (this.initialText === this.editor.getEditorText()) {
                this.element.remove();
                this.afterCancel({
                    extraData: this.extraData,
                });
            }
            getActiveDocument().refreshSectionReadMarks();
        }, (response) => {
            $log.error("Failed to mark paragraph as unread");
        });
    }

    cancelClicked() {
        if (this.options.touchDevice) {
            this.changeMeta();
        }
        this.element.remove();
        this.afterCancel({
            extraData: this.extraData,
        });
    }

    releaseClicked() {
        const div = $("#previewDiv");
        const content = $(".previewcontent");
        const editor = $(".editorArea");
        this.previewReleased = !(this.previewReleased);
        const tag = this.options.localSaveTag || "";
        const storage = $window.localStorage;

        if (div.css("position") === "absolute") {
            // If preview has been clicked back in, save the preview position before making it static again
            if (this.minSizeSet) {
                this.savePreviewData(true);
            }
            div.css("position", "static");
            div.find(".draghandle").css("visibility", "hidden");
            content.css("max-width", "");
            div.css("display", "default");
            editor.css("overflow", "hidden");
            content.css("max-height", "40vh");
            content.css("overflow-x", "");
            content.css("width", "");
            div.css("padding", 0);
            document.getElementById("releaseButton").innerHTML = "&#8594;";
        } else {
            let top = div.offset().top;
            let left = div.offset().left;
            // If preview has just been released or it was released last time editor was open
            if (this.minSizeSet || storage.getItem("previewIsReleased" + tag) === "true") {
                if (storage.getItem("previewReleasedOffset" + tag)) {
                    const savedOffset = (JSON.parse(storage.getItem("previewReleasedOffset" + tag)));
                    left = editor.offset().left + savedOffset.left;
                    top = editor.offset().top + savedOffset.top;
                } else {
                    if ($(window).width() < editor.width() + div.width()) {
                        top += 5;
                        left += 5;
                    } else {
                        top = editor.offset().top;
                        left = editor.offset().left + editor.width() + 3;
                    }
                }
            }
            div.css("position", "absolute");
            editor.css("overflow", "visible");
            div.find(".draghandle").css("visibility", "visible");
            div.css("display", "table");
            div.css("width", "100%");
            div.css("padding", 5);
            const height = window.innerHeight - 90;
            content.css("max-height", height);
            content.css("max-width", window.innerWidth - 90);
            content.css("overflow-x", "auto");
            document.getElementById("releaseButton").innerHTML = "&#8592;";
            div.offset({left, top});
        }
        this.adjustPreview();
    }

    savePreviewData(savePreviewPosition) {
        const tag = this.options.localSaveTag || "";
        const storage = $window.localStorage;
        const editorOffset = $(".editorArea").offset();
        storage.setItem("editorReleasedOffset" + tag, JSON.stringify(editorOffset));
        if (savePreviewPosition) {
            // Calculate distance from editor's top and left
            const previewOffset = $("#previewDiv").offset();
            const left = previewOffset.left - editorOffset.left;
            const top = previewOffset.top - editorOffset.top;
            storage.setItem("previewReleasedOffset" + tag, JSON.stringify({left, top}));
        }
        storage.setItem("previewIsReleased" + tag, this.previewReleased.toString());
    }

    /**
     * Called when user wants to cancel changes after entering duplicate task-ids
     */
    cancelPluginRenameClicked() {
        // Cancels recent changes to paragraph/document
        $http.post("/cancelChanges/", angular.extend({
            newPars: this.newPars,
            originalPar: this.originalPar,
            docId: this.extraData.docId,
            parId: this.extraData.par,
        }, this.extraData)).then((response) => {
            // Remove the form and return to editor
            this.element.find("#pluginRenameForm").get(0).remove();
            this.renameFormShowing = false;
            this.saving = false;
            this.deleting = false;
        }, (response) => {
            $window.alert("Failed to cancel save: " + response.data.error);
        });
    }

    /**
     * Function that handles different cases of user input in plugin rename form
     * after user has saved multiple plugins with the same taskname
     * @param inputs - The input fields in plugin rename form
     * @param duplicates - The duplicate tasks, contains duplicate taskIds and relevant parIds
     * @param renameDuplicates - Whether user wants to rename task names or not
     */
    renameTaskNamesClicked(inputs, duplicates, renameDuplicates = false) {
        // If user wants to ignore duplicates proceed like normal after saving
        if (!renameDuplicates) {
            this.renameFormShowing = false;
            if (this.options.destroyAfterSave) {
                this.afterSave({
                    extraData: this.extraData,
                    saveData: this.data,
                });
                this.element.remove();
                return;
            }
        }
        const duplicateData = [];
        let duplicate;

        // if duplicates are to be renamed automatically (user pressed "rename automatically")
        if (typeof inputs === "undefined") {
            if (renameDuplicates) {
                if (duplicates.length > 0) {
                    for (let i = 0; i < duplicates.length; i++) {
                        duplicate = [];
                        duplicate.push(duplicates[i][0]);
                        duplicate.push("");
                        duplicate.push(duplicates[i][1]);
                        duplicateData.push(duplicate);
                    }
                }
            }
        } else {
            // use given names from the input fields
            for (let j = 0; j < duplicates.length; j++) {
                duplicate = [];
                duplicate.push(duplicates[j][0]);
                duplicate.push((inputs[j][0] as HTMLInputElement).value);
                duplicate.push(duplicates[j][1]);
                duplicateData.push(duplicate);
            }
        }
        // Save the new task names for duplicates
        $http.post<{duplicates: string[]}>("/postNewTaskNames/", angular.extend({
            duplicates: duplicateData,
            renameDuplicates,
        }, this.extraData)).then((response) => {
            const data = response.data;
            // If no new duplicates were founds
            if (data.duplicates.length <= 0) {
                this.renameFormShowing = false;
                this.afterSave({
                    extraData: this.extraData,
                    saveData: this.data,
                });
                if (this.options.destroyAfterSave) {
                    this.element.remove();
                }
            }
            // If there still are duplicates remake the form
            if (data.duplicates.length > 0) {
                this.element.find("#pluginRenameForm").get(0).remove();
                this.createPluginRenameForm(data);
            }
            if (angular.isDefined(this.extraData.access)) {
                $localStorage.noteAccess = this.extraData.access;
            }
        }, (response) => {
            $window.alert("Failed to save: " + response.data.error);
        });
        if (this.options.touchDevice) {
            this.changeMeta();
        }
    }

    /**
     * Function that creates a form for renaming plugins with duplicate tasknames
     * @param data - The data received after saving editor text
     */
    createPluginRenameForm(data) {
        // Hides other texteditor elements when form is created
        this.renameFormShowing = true;
        this.duplicates = data.duplicates;
        // Get the editor div
        const $editorTop = $(".editorArea");
        // Create a new div
        let $actionDiv = $("<div>", {class: "pluginRenameForm", id: "pluginRenameForm"});
        $actionDiv.css("position", "relative");
        // Add warning and info texts
        $actionDiv.append($("<strong>", {
            text: "Warning!",
        }));
        $actionDiv.append($("<p>", {
            text: "There are multiple objects with the same task name in this document.",
        }));
        $actionDiv.append($("<p>", {
            text: "Plugins with duplicate task names might not work properly.",
        }));
        $actionDiv.append($("<p>", {
            text: 'Rename the duplicates by writing new names in the field(s) below and click "Save",',
        }));
        $actionDiv.append($("<p>", {
            text: 'choose "Rename automatically" or "Ignore" to proceed without renaming.',
        }));
        $actionDiv.append($("<strong>", {
            text: "Rename duplicates:",
        }));

        // Create the rename form
        const $form = $("<form>");
        this.inputs = [];
        let input;
        let span;

        // Add inputs and texts for each duplicate
        for (let i = 0; i < data.duplicates.length; i++) {
            // Make a span element
            span = $("<span>");
            span.css("display", "block");
            // Add a warning if the plugin has answers related to it
            const $warningSpan = $("<span>", {
                class: "pluginRenameExclamation",
                text: "!",
                title: "There are answers related to this task. Those answers might be lost upon renaming this task.",
            });
            if (data.duplicates[i][2] !== "hasAnswers") {
                $warningSpan.css("visibility", "hidden");
            }
            span.append($warningSpan);
            // Add the duplicate name
            span.append($("<label>", {
                class: "pluginRenameObject",
                text: data.duplicates[i][0],
                for: "newName" + i,
            }));
            // Add input field for a new name to the duplicate
            input = $("<input>", {
                class: "pluginRenameObject",
                type: "text",
                id: data.duplicates[i][1],
            });
            // Add the span to the form
            this.inputs.push(input);
            span.append(input);
            $form.append(span);
        }
        // Make a new div for buttons
        const $buttonDiv = $("<div>");
        // A button for saving with input field values or automatically if no values given
        $buttonDiv.append($("<button>", {
            "class": "timButton, pluginRenameObject",
            "text": "Save",
            "title": "Rename task names with given names (Ctrl + S)",
            "ng-click": "$ctrl.renameTaskNamesClicked(inputs, duplicates, true)",
        }));
        // Button for renaming duplicates automatically
        $buttonDiv.append($("<button>", {
            "class": "timButton, pluginRenameObject",
            "text": "Rename Automatically",
            "title": "Rename duplicate task names automatically",
            "ng-click": "$ctrl.renameTaskNamesClicked(undefined, duplicates, true)",
        }));
        // Button for ignoring duplicates
        $buttonDiv.append($("<button>", {
            "class": "timButton, pluginRenameObject",
            "text": "Ignore",
            "title": "Proceed without renaming",
            "ng-click": "$ctrl.renameTaskNamesClicked(undefined, undefined, false)",
        }));
        // Button that allows user to return to edit and cancel save
        $buttonDiv.append($("<button>", {
            "class": "timButton, pluginRenameObject",
            "text": "Cancel",
            "title": "Return to editor",
            "ng-click": "$ctrl.cancelPluginRenameClicked()",
        }));
        // Add the new divs to editor container
        $actionDiv.append($form);
        $actionDiv.append($buttonDiv);
        $actionDiv = $compile($actionDiv)(this.scope);
        $editorTop.append($actionDiv);
        // Focus the first input element
        this.inputs[0].focus();
        this.pluginRenameForm = $actionDiv;
        // Add hotkey for quick saving (Ctrl + S)
        this.pluginRenameForm.keydown((e) => {
            if (e.ctrlKey) {
                if (e.keyCode === 83) {
                    this.renameTaskNamesClicked(this.inputs, this.duplicates, true);
                    e.preventDefault();
                }
            }
        });
    }

    saveClicked() {
        if (this.saving) {
            return;
        }
        this.saving = true;
        if (this.renameFormShowing) {
            this.renameTaskNamesClicked(this.inputs, this.duplicates, true);
        }
        if (this.previewReleased) {
            this.savePreviewData(true);
        } else {
            this.savePreviewData(false);
        }
        const text = this.editor.getEditorText();
        if (text.trim() === "") {
            this.deleteClicked();
            this.saving = false;
            return;
        }
        $http.post<{
            duplicates: string[],
            original_par: string,
            new_par_ids: string[],
        }>(this.saveUrl, angular.extend({
            text,
        }, this.extraData)).then((response) => {
            const data = response.data;
            if (data.duplicates.length > 0) {
                this.data = data;
                this.createPluginRenameForm(data);
                if (data.original_par !== null) {
                    this.originalPar = data.original_par;
                }
                if (data.new_par_ids !== null) {
                    this.newPars = data.new_par_ids;
                }
            }
            if (data.duplicates.length <= 0) {
                if (this.options.destroyAfterSave) {
                    this.element.remove();
                }
                this.afterSave({
                    extraData: this.extraData,
                    saveData: data,
                });
            }
            if (angular.isDefined(this.extraData.access)) {
                $localStorage.noteAccess = this.extraData.access;
            }
            if (angular.isDefined(this.extraData.tags.markread)) { // TODO: tee silmukassa
                $window.localStorage.setItem("markread", this.extraData.tags.markread.toString());
            }
            $window.localStorage.setItem("proeditor" + this.lstag, this.proeditor.toString());

            if (this.isAce(this.editor)) {
                this.setLocalValue("acewrap", this.editor.editor.getSession().getUseWrapMode());
                this.setLocalValue("acebehaviours", this.editor.editor.getBehavioursEnabled()); // some of these are in editor and some in session?
            }
            this.saving = false;

        }, (response) => {
            $window.alert("Failed to save: " + response.data.error);
            this.saving = false;
        });
        if (this.options.touchDevice) {
            this.changeMeta();
        }
    }

    isAce(editor: any): editor is AceParEditor {
        return editor && typeof editor.snippetManager !== "undefined";
    }

    saveOldMode(oldMode) {
        $window.localStorage.setItem("oldMode" + this.options.localSaveTag, oldMode);
    }

    onFileSelect(file) {
        this.uploadedFile = "";
        this.editor.focus();
        this.file = file;

        if (file) {
            this.file.progress = 0;
            this.file.error = null;
            file.upload = $upload.upload({
                data: {
                    file,
                },
                method: "POST",
                url: "/upload/",
            });

            file.upload.then((response) => {
                $timeout(() => {
                    if (response.data.image) {
                        this.uploadedFile = "/images/" + response.data.image;
                        if (this.editor.editorStartsWith("``` {")) {
                            this.editor.insertTemplate(this.uploadedFile);
                        }
                        else {
                            this.editor.insertTemplate("![Image](" + this.uploadedFile + ")");
                        }
                    } else {
                        this.uploadedFile = "/files/" + response.data.file;
                        this.editor.insertTemplate("[File](" + this.uploadedFile + ")");
                    }
                });
            }, (response) => {
                if (response.status > 0) {
                    this.file.error = response.data.error;
                }
            }, (evt) => {
                this.file.progress = Math.min(100, Math.floor(100.0 *
                    evt.loaded / evt.total));
            });

            file.upload.finally(() => {
            });
        }
    }

    closeMenu(e: Event, force: boolean) {
        const container = $(MENU_BUTTON_CLASS_DOT);
        if (force || (!container.is(e.target) && container.has(e.target as Element).length === 0)) {
            container.remove();
            $(document).off("mouseup.closemenu");
        }
    }

    createMenuButton(text, title, clickfunction) {
        const $span = $("<span>", {class: "actionButtonRow"});
        const button_width = 130;
        $span.append($("<button>", {
            "class": "timButton",
            "text": text,
            "title": title,
            "ng-click": clickfunction,
            "width": button_width,
        }));
        return $span;
    }

    createMenu($event, buttons) {
        this.closeMenu(null, true);
        const $button = $($event.target);
        const coords = {left: $button.position().left, top: $button.position().top};
        let $actionDiv = $("<div>", {class: MENU_BUTTON_CLASS});

        for (let i = 0; i < buttons.length; i++) {
            $actionDiv.append(buttons[i]);
        }

        $actionDiv.append(this.createMenuButton("Close menu", "", "$ctrl.closeMenu(null, true); wrapFn()"));
        $actionDiv.offset(coords);
        $actionDiv.css("position", "absolute"); // IE needs this
        $actionDiv = $compile($actionDiv)(this.scope);
        $button.parent().prepend($actionDiv);
        $(document).on("mouseup.closemenu", this.closeMenu);
    }

    tableClicked($event) {
        const buttons = [];
        for (const key in this.tables) {
            if (this.tables.hasOwnProperty(key)) {
                const text = key.charAt(0).toUpperCase() + key.substring(1);
                const clickfn = "$ctrl.insertTemplate(tables['" + key + "']); $ctrl.closeMenu(); $ctrl.wrapFn()";
                buttons.push(this.createMenuButton(text, "", clickfn));
            }
        }
        this.createMenu($event, buttons);
    }

    slideClicked($event) {
        const buttons = [];
        buttons.push(this.createMenuButton("Slide break", "Break text to start a new slide", "$ctrl.ruleClicked(); $ctrl.wrapFn()"));
        buttons.push(this.createMenuButton("Slide fragment", "Content inside the fragment will be hidden and shown when next is clicked in slide view", "$ctrl.editor.surroundClicked('§§', '§§')"));
        buttons.push(this.createMenuButton("Fragment block", "Content inside will show as a fragment and may contain inner slide fragments", "$ctrl.editor.surroundClicked('<§', '§>')"));
        this.createMenu($event, buttons);
    }

    pluginClicked($event, key) {
        this.createMenu($event, this.pluginButtonList[key]);
    }

    getTemplate(plugin, template, index) {
        $.ajax({
            type: "GET",
            url: "/" + plugin + "/template/" + template + "/" + index,
            dataType: "text",
            processData: false,
            success: (data) => {
                data = data.replace(/\\/g, "\\\\");
                this.editor.insertTemplate(data);
            },
            error() {
                $log.error("Error getting template");
            },
        });
        if (!this.touchDevice) {
            this.editor.focus();
        }
    }

    tabClicked($event, area) {
        const active = $($event.target).parent();
        setsetting("editortab", area);
        this.setActiveTab(active, area);
        this.wrapFn();
    }

    /**
     * Sets active tab
     * @param active tab <li> element
     * @param area area to make visible
     */
    setActiveTab(active, area) {
        const naviArea = $("#" + area);
        const buttons = $(".extraButtonArea");
        const tabs = $(".tab");
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
        const div: any = $(this.element).find("#pareditor").get(0);
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
    async changeEditor(newMode) {
        let text = "";
        if (this.editor) {
            text = this.editor.getEditorText();
        }
        let oldeditor = null;
        if (this.isAce(this.editor) || newMode === "text") {
            oldeditor = $("#ace_editor");
            this.saveOldMode("text");
            this.createTextArea(text);
        } else {
            oldeditor = $("#teksti");
            const ace = (await lazyLoadTS<typeof acemodule>("tim/ace", __moduleName)).ace;
            this.saveOldMode("ace");
            const neweditorElem = $("<div>", {
                class: "editor",
                id: "ace_editor",
            });
            $(".editorContainer").append(neweditorElem);
            const neweditor = ace.edit("ace_editor");

            this.editor = new AceParEditor(ace, neweditor, () => this.wrapFn(), () => this.saveClicked());
            if (!this.minSizeSet) {
                this.setEditorMinSize();
            }
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
        }
        try {
            await this.setInitialText();
        } catch (response) {
            if (response.status === 404) {
                if (this.extraData.isComment) {
                    $window.alert("This comment has been deleted.");
                } else {
                    $window.alert("This paragraph has been deleted.");
                }
            } else {
                $window.alert("Error occurred: " + response.data.error);
            }
            $timeout(() => {
                this.element.remove();
            }, 1000);
            return;
        }
        this.editorReady();
        setEditorScope(this.editor);
        if (oldeditor) {
            oldeditor.remove();
        }
        this.adjustPreview();
        if (!this.proeditor && this.lstag === "note") {
            const editor = $("pareditor");
            editor.css("max-width", "40em");
        }
    }
}

timApp.component("pareditor", {
    templateUrl: "/static/templates/parEditor.html",
    bindings: {
        saveUrl: "@",
        deleteUrl: "@",
        previewUrl: "@",
        unreadUrl: "@",
        extraData: "=",
        afterSave: "&",
        afterCancel: "&",
        afterDelete: "&",
        options: "=",
        initialTextUrl: "@",
    },
    controller: PareditorController,
});
