import angular, {IController, IFormController, IRootElementService, IScope} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {$compile, $http, $log, $timeout, $upload, $window} from "../ngimport";
import {IItem} from "../IItem";
import {markAsUsed} from "../utils";
import * as copyFolder from "./copyFolder";
import {Duplicate} from "../edittypes";

markAsUsed(copyFolder);

interface IChangelogEntry {

}

export interface ITranslation {
    id: number;
    old_langid: string;
    lang_id: string;
    old_title: string;
    title: string;
}

export interface IAlias {
    name: string;
    location: string;
    path: string;
    publicChanged: boolean;
    public: boolean;
}

export class PermCtrl implements IController {
    private static $inject = ["$scope", "$element"];

    private wikiRoot: string;
    private newTitle: string;
    private newFolderName: string;
    private hasMoreChangelog: boolean;
    private translations: (IItem & {old_title: string})[];
    private newTranslation: {language: string, title: string};
    private accessTypes: {}[];
    private item: IItem;
    private newName: string;
    private oldFolderName: string;
    private oldName: string;
    private fulltext: string;
    private changelogLoading: boolean;
    private newAlias: {location: string};
    private copyParams: {copy: number};
    private citeParams: {cite: number};
    private aliases: IItem[];
    private old_title: string;
    private fileUploadError: string | null;
    private tracWikiText: string;
    private renameFormShowing: boolean;
    private inputs: JQuery[];
    private saving: boolean;
    private readUpdating: boolean;
    private file: any;
    private newAliasForm: IFormController;
    private duplicates: Duplicate[];
    private data: {};
    private notifySettings: {};
    private sc: IScope;
    private element: IRootElementService;
    private objName: string;

    constructor(scope: IScope, element: IRootElementService) {
        this.sc = scope;
        this.element = element;
    }

    $onInit() {
        this.wikiRoot = "https://trac.cc.jyu.fi/projects/ohj2/wiki/"; // Todo: replace something remembers users last choice
        this.accessTypes = $window.accessTypes;
        this.item = $window.item;
        this.objName = $window.objName;
        this.hasMoreChangelog = true;
        this.newFolderName = this.item.location;
        this.newTitle = this.item.title;
        this.newAlias = {location: this.newFolderName};
        this.copyParams = {copy: this.item.id};
        this.citeParams = {cite: this.item.id};
        this.translations = [];
        this.newTranslation = {language: "", title: ""};

        if (this.item.isFolder) {
            this.newName = this.item.name;
            this.newFolderName = this.item.location;
            this.oldName = this.newName;
            this.oldFolderName = this.newFolderName;
        } else {
            this.getNotifySettings();
            this.item.fulltext = this.item.fulltext.trim();
            this.fulltext = this.item.fulltext;
            if (this.item.rights.manage) {
                this.aliases = this.getAliases();
            }
            if (this.item.rights.manage) {
                this.translations = this.getTranslations();
            }
        }
    }

    showMoreChangelog() {
        const newLength = this.item.versions.length + 100;
        this.changelogLoading = true;
        $http.get<{versions: IChangelogEntry[]}>("/changelog/" + this.item.id + "/" + (newLength)).then((response) => {
            this.item.versions = response.data.versions;
            this.hasMoreChangelog = this.item.versions.length === newLength;
        }, (response) => {
            $log.error("Failed to get more changelog.");
        }).finally(() => {
            this.changelogLoading = false;
        });
    }

    getAliases() {
        $http.get<IItem[]>("/alias/" + this.item.id, {}).then((response) => {
            const data = response.data;
            if (this.aliases.length > 0 &&
                data.length > 0 &&
                data[0].path !== this.aliases[0].path) {
                // The first name has changed, reload to update the links
                $window.location.replace("/manage/" + data[0].path);
            } else {
                this.aliases = data;
            }
            // mark the form pristine; otherwise it will complain about required field unnecessarily
            this.newAliasForm.$setPristine();
            this.newAlias = {location: this.item.location};
        }, (response) => {
            $window.alert("Error loading aliases: " + response.data.error);
        });

        return [];
    }

    getTranslations() {
        if (this.item.isFolder) {
            return [];
        }

        $http.get<Array<{lang_id: string, title: string}>>("/translations/" + this.item.id, {}).then((response) => {
            const data = response.data;
            this.translations = [];

            for (let i = 0; i < data.length; i++) {
                const tr = data[i];
                const trnew = JSON.parse(JSON.stringify(tr));
                trnew.old_langid = tr.lang_id;
                trnew.old_title = tr.title;
                this.translations.push(trnew);
            }

            this.old_title = this.item.title;

        }, (response) => {
            $window.alert("Error loading translations: " + response.data.error);
        });

        return [];
    }

    trChanged(tr: ITranslation) {
        return tr.title !== tr.old_title || tr.lang_id !== tr.old_langid;
    }

    updateTranslation(tr: ITranslation) {
        $http.post("/translation/" + tr.id, {
            new_langid: tr.lang_id,
            new_title: tr.title,
            old_title: tr.old_title,
        }).then((response) => {
            const data = response.data;
            this.getTranslations();
            if (tr.id === this.item.id) {
                this.syncTitle(tr.title);
            }
        }, (response) => {
            $window.alert(response.data.error);
        });
    }

    syncTitle(title: string) {
        this.item.title = title;
        this.newTitle = title;
        $window.document.title = title + " - Manage - TIM";
        for (let i = 0; i < this.translations.length; ++i) {
            if (this.translations[i].id === this.item.id) {
                this.translations[i].title = title;
                this.translations[i].old_title = title;
            }
        }
    }

    changeTitle() {
        $http.put("/changeTitle/" + this.item.id, {
            new_title: this.newTitle,
        }).then((response) => {
            this.syncTitle(this.newTitle);
        }, (response) => {
            $window.alert(response.data.error);
        });
    }

    renameFolder(newName: string) {
        $http.put("/rename/" + this.item.id, {
            new_name: this.oldFolderName + "/" + newName,
        }).then((response) => {
            const data = response.data;
            window.location.assign("/manage/" + this.oldFolderName + "/" + newName);
        }, (response) => {
            $window.alert(response.data.error);
        });
    }

    moveFolder(newLocation: string) {
        $http.put("/rename/" + this.item.id, {
            new_name: newLocation + "/" + this.oldName,
        }).then((response) => {
            const data = response.data;
            // This is needed to update the breadcrumbs
            location.reload();
        }, (response) => {
            $window.alert(response.data.error);
        });
    }

    combine(folder: string, name: string) {
        return (folder + "/" + name).replace(/(^\/+)|(\/+$)/, "");
    }

    aliasPublicClicked(alias: IAlias) {
        alias.public = !alias.public;
        alias.publicChanged = !alias.publicChanged;
    }

    aliasChanged(alias: IAlias) {
        return alias.publicChanged || alias.path !== this.combine(alias.location, alias.name);
    }

    addAlias(newAlias: IAlias) {
        $http.put("/alias/" + this.item.id + "/" + $window.encodeURIComponent(this.combine(newAlias.location || "", newAlias.name)), {
            public: Boolean(newAlias.public),
        }).then((response) => {
            const data = response.data;
            this.getAliases();
        }, (response) => {
            $window.alert(response.data.error);
        });
    }

    removeAlias(alias: IAlias) {
        $http.delete("/alias/" + $window.encodeURIComponent(alias.path), {}).then((response) => {
            const data = response.data;
            this.getAliases();
        }, (response) => {
            $window.alert(response.data.error);
        });
    }

    updateAlias(alias: IAlias, first: boolean) {
        const new_alias = this.combine(alias.location, alias.name);
        $http.post("/alias/" + $window.encodeURIComponent(alias.path), {
            public: Boolean(alias.public),
            new_name: new_alias,
        }).then((response) => {
            const data = response.data;

            if (!first || new_alias === alias.path) {
                this.getAliases();
            } else {
                location.replace("/manage/" + new_alias);
            }

        }, (response) => {
            $window.alert(response.data.error);
        });
    }

    deleteDocument(docId: number) {
        if ($window.confirm("Are you sure you want to delete this document?")) {
            $http.delete("/documents/" + docId)
                .then((response) => {
                    const data = response.data;
                    location.replace("/view/");
                }, (response) => {
                    $window.alert(response.data.error);
                });
        }
    }

    deleteFolder(folderId: number) {
        if ($window.confirm("Are you sure you want to delete this folder?")) {
            $http.delete("/folders/" + folderId)
                .then((response) => {
                    const data = response.data;
                    location.replace("/view/");
                }, (response) => {
                    $window.alert(response.data.error);
                });
        }
    }

    updateDocument(file: any) {
        this.file = file;
        this.fileUploadError = null;
        if (file) {
            this.file.progress = 0;
            file.upload = $upload.upload({
                url: "/update/" + this.item.id,
                data: {
                    file,
                    original: this.item.fulltext,
                    version: this.item.versions[0],
                },
                method: "POST",
            });

            file.upload.then((response: any) => {
                $timeout(() => {
                    this.file.result = response.data;
                    this.item.versions = response.data.versions;
                    this.item.fulltext = response.data.fulltext;
                    this.fulltext = response.data.fulltext;
                });
            }, (response: any) => {
                if (response.status > 0) {
                    this.fileUploadError = "Error: " + response.data.error;
                }
            }, (evt: any) => {
                this.file.progress = Math.min(100, Math.floor(100.0 *
                    evt.loaded / evt.total));
            });

            file.upload.finally(() => {
            });
        }
    }

    convertDocument(doc: string) {
        let text = this.tracWikiText;
        const wikiSource = this.wikiRoot.replace("wiki", "browser");
        //var text = this.fulltext;
        text = text.replace(/\[\[PageOutline\]\].*\n/g, "");  // remove contents
        text = text.replace(/ !([a-zA-Z])/g, " $1");                   // remove cat !IsBig
        text = text.replace(/\r\n/g, "\n");                   // change windows nl's
        text = text.replace(/{{{(.*?)}}}/g, "`$1`");         // change verbatim
        // text = text.replace(/{{{\n(.*?)\n}}}/, "```\n$1\n```"); // change code blocks
        text = text.replace(/\n{{{#!comment\n((.|\s|\S)*?)\n}}}\n/g, "\n#- {.comment}\n$1\n#-\n"); // comments
        text = text.replace(/\n{{{\n/g, "\n#-\n```\n"); // change code blocks
        text = text.replace(/\n}}}\n/g, "\n```\n"); // change code blocks
        text = text.replace(/\n====\s+(.*?)\s+====.*\n/g, "\n#-\n#### $1\n"); // headings
        text = text.replace(/\n===\s+(.*?)\s+===.*\n/g, "\n#-\n### $1\n");
        text = text.replace(/\n==\s+(.*?)\s+==.*\n/g, "\n#-\n## $1\n");
        text = text.replace(/\n=\s+(.*?)\s+=.*\n/g, "\n#-\n# $1\n");
        // text = text.replace(/^ \d+. ', r'1.');
        const lines = text.split("\n");
        for (let i = 0; i < lines.length; i++) {
            let line = lines[i];
            if (true || line.lastIndexOf("    ", 0) !== 0) {
                line = line.replace(/\[((https?|mms):\/\/[^\s\[\]]+)\s+([^\[\]]+)\]/g, "[$3]($1)"); // ordinary links
                line = line.replace(/\[((https?|mms):\/\/[^\s\[\]]+)\s+(\[.*?\])\]/g, "[$3]($1)");   // links like [url [text]]
                line = line.replace(/\[wiki:([^\s\[\]]+)\]/g, "[$1](" + (this.wikiRoot || "") + "$1)"); // [wiki:local] -> [local](URL)
                line = line.replace(/\[wiki:([^\s\[\]]+)\s+([^\[\]]+)\]/g, "[$2](" + (this.wikiRoot || "") + "$1)"); // [wiki:local text] -> [text](URL)
                line = line.replace(/\[source:([^\s\[\]]+)\s+([^\[\]]+)\]/g, "[$2](" + (wikiSource || "") + "$1)");
                line = line.replace(/\!(([A-Z][a-z0-9]+){2,})/, "$1");
                line = line.replace(/\'\'\'(.*?)\'\'\'/, "**$1**");  // bold
                line = line.replace(/\'\'(.*?)\'\'/, "*$1*");   // italics
            }
            lines[i] = line;
        }
        text = lines.join("\n");

        /*

         text = re.sub(r'(?m)^====\s+(.*?)\s+====.*$', r'#-\n#### \1', text)
         text = re.sub(r'(?m)^===\s+(.*?)\s+===.*$', r'#-\n### \1', text)
         text = re.sub(r'(?m)^==\s+(.*?)\s+==.*$', r'#-\n## \1', text)
         text = re.sub(r'(?m)^=\s+(.*?)\s+=.*$', r'#-\n# \1', text)
         #text = re.sub(r'^       * ', r'****', text)
         #text = re.sub(r'^     * ', r'***', text)
         #text = re.sub(r'^   * ', r'**', text)
         #text = re.sub(r'^ * ', r'*', text)
         text = re.sub(r'^ \d+. ', r'1.', text)

         a = []
         for line in text.split('\n'):
         if True or not line.startswith('    '):
         line = re.sub(r'\[(https?://[^\s\[\]]+)\s([^\[\]]+)\]', r'[\2](\1)', line)
         line = re.sub(r'\[(wiki:[^\s\[\]]+)\s([^\[\]]+)\]', r'[\2](/\1/)', line)
         line = re.sub(r'\!(([A-Z][a-z0-9]+){2,})', r'\1', line)
         line = re.sub(r'\'\'\'(.*?)\'\'\'', r'*\1*', line)
         line = re.sub(r'\'\'(.*?)\'\'', r'_\1_', line)
         line = re.sub(r'\(/wiki:', r'(https://trac.cc.jyu.fi/projects/ohj2/wiki/', line)
         a.append(line)
         text = '\n'.join(a)
         */
        //this.tracWikiText = text;
        this.fulltext = text;
    }

    cancelPluginRenameClicked() {
        $("#pluginRenameForm").remove();
        this.renameFormShowing = false;
    }

    renameTaskNamesClicked(duplicates: Duplicate[], renameDuplicates: boolean) {
        // A list of duplicate task names and possible new names
        if (typeof renameDuplicates === "undefined" || renameDuplicates === false) {
            $("#pluginRenameForm").remove();
            this.renameFormShowing = false;
            //$element.find("#pluginRenameForm").get(0).remove();
            return;
        }
        const duplicateData: {}[] = [];
        let duplicate;
        // use given names from the input fields
        for (let i = 0; i < duplicates.length; i++) {
            duplicate = [];
            duplicate.push(duplicates[i][0]);
            duplicate.push($("#" + duplicates[i][1]).prop("value"));
            duplicate.push(duplicates[i][1]);
            duplicateData.push(duplicate);
        }
        $http.post<{fulltext: string, versions: IChangelogEntry[], duplicates: Duplicate[]}>("/postNewTaskNames/", angular.extend({
            duplicates: duplicateData,
            renameDuplicates,
            manageView: true,
            docId: this.item.id,
        })).then((response) => {
            const data = response.data;
            this.renameFormShowing = false;
            this.fulltext = data.fulltext;
            this.item.fulltext = this.fulltext;
            this.item.versions = data.versions;
            $("#pluginRenameForm").remove();
            if (data.duplicates.length > 0) {
                this.createPluginRenameForm(data);
            }
        }, (response) => {
            $window.alert("Failed to save: " + response.data.error);
        });
    }

    createPluginRenameForm(data: {duplicates: Duplicate[]}) {
        this.renameFormShowing = true;
        this.duplicates = data.duplicates;
        this.data = data;

        // TODO make this an Angular component!
        const $editorTop = $("#documentEditorDiv");
        //var coords = {left: $editorTop.position().left, top: $editorTop.position().top};
        let $actionDiv = $("<div>", {class: "pluginRenameForm", id: "pluginRenameForm"});
        $actionDiv.css("position", "relative");
        const $innerTextDiv = $("<div>", {class: "pluginRenameInner"});
        $innerTextDiv.append($("<strong>", {
            text: "Warning!",
        }));
        $innerTextDiv.append($("<p>", {
            text: "There are multiple objects with the same task name in this document.",
        }));
        $innerTextDiv.append($("<p>", {
            text: "Plugins with duplicate task names might not work properly.",
        }));
        $innerTextDiv.append($("<p>", {
            text: 'Rename the duplicates by writing new names in the field(s) below and click "Save",',
        }));
        $innerTextDiv.append($("<p>", {
            text: 'choose "Rename automatically" or "Ignore" to proceed without renaming.',
        }));
        $innerTextDiv.append($("<strong>", {
            text: "Rename duplicates:",
        }));
        $actionDiv.append($innerTextDiv);
        const $form = $("<form>", {class: "pluginRenameInner"});
        this.inputs = [];
        let input;
        let span;
        for (let i = 0; i < data.duplicates.length; i++) {
            span = $("<span>");
            span.css("display", "block");
            const $warningSpan = $("<span>", {
                class: "pluginRenameExclamation",
                text: "!",
                title: "There are answers related to this task. Those answers might be lost upon renaming this task.",
            });
            if (data.duplicates[i][2] !== "hasAnswers") {
                $warningSpan.css("visibility", "hidden");
            }
            span.append($warningSpan);
            span.append($("<label>", {
                class: "pluginRenameObject",
                text: data.duplicates[i][0],
                for: "newName" + i,
            }));
            input = $("<input>", {
                class: "pluginRenameObject",
                type: "text",
                id: data.duplicates[i][1],
            });
            this.inputs.push(input);
            span.append(input);
            $form.append(span);
        }
        const $buttonDiv = $("<div>", {class: "pluginRenameInner"});
        $buttonDiv.append($("<button>", {
            "class": "timButton, pluginRenameObject",
            "text": "Save",
            "title": "Rename task names with given names",
            "ng-click": "renameTaskNamesClicked(duplicates, true)",
        }));
        $buttonDiv.append($("<button>", {
            "class": "timButton, pluginRenameObject",
            "text": "Rename Automatically",
            "title": "Rename duplicate task names automatically",
            "ng-click": "renameTaskNamesClicked(duplicates, true)",
        }));
        $buttonDiv.append($("<button>", {
            "class": "timButton, pluginRenameObject",
            "text": "Ignore",
            "title": "Proceed without renaming",
            "ng-click": "renameTaskNamesClicked(undefined, false)",
        }));
        $buttonDiv.append($("<button>", {
            "class": "timButton, pluginRenameObject",
            "text": "Cancel",
            "title": "Return to editing document",
            "ng-click": "cancelPluginRenameClicked()",
        }));
        $actionDiv.append($form);
        $actionDiv.append($buttonDiv);
        $actionDiv = $compile($actionDiv)(this.sc);
        $editorTop.parent().prepend($actionDiv);
    }

    saveDocument() {
        this.saving = true;
        $http.post<{duplicates: Duplicate[], fulltext: string, versions: IChangelogEntry[]}>("/update/" + this.item.id,
            {
                fulltext: this.fulltext,
                original: this.item.fulltext,
                version: this.item.versions[0],
            }).then((response) => {
            const data = response.data;
            if (data.duplicates.length > 0) {
                this.createPluginRenameForm(data);
            }
            this.fulltext = data.fulltext;
            this.item.fulltext = this.fulltext;
            this.item.versions = data.versions;
            this.saving = false;
        }, (response) => {
            const data = response.data;
            this.saving = false;
            if ("is_warning" in data && data.is_warning) {
                if ($window.confirm(data.error + "\n\nDo you still wish to save the document?")) {
                    this.saveDocumentWithWarnings();
                }
            } else {
                $window.alert(data.error);
            }
        });
    }

    saveDocumentWithWarnings() {
        this.saving = true;
        $http.post<{fulltext: string, versions: IChangelogEntry[]}>("/update/" + this.item.id,
            {
                fulltext: this.fulltext,
                original: this.item.fulltext,
                ignore_warnings: true,
                version: this.item.versions[0],
            }).then((response) => {
            const data = response.data;
            this.fulltext = data.fulltext;
            this.item.fulltext = this.fulltext;
            this.item.versions = data.versions;
        }, (response) => {
            $window.alert(response.data.error);
        }).finally(() => {
            this.saving = false;
        });
    }

    markAllAsRead() {
        this.readUpdating = true;
        $http.put("/read/" + this.item.id, {})
            .then((response) => {
                const data = response.data;
            }, (response) => {
                $window.alert("Could not mark the document as read.");
            }).finally(() => {
            this.readUpdating = false;
        });
    }

    createTranslation() {
        $http.post<{path: string}>("/translate/" + this.item.id + "/" + this.newTranslation.language, {
            doc_title: this.newTranslation.title,
        }).then((response) => {
            const data = response.data;
            location.href = "/view/" + data.path;
        }, (response) => {
            $window.alert(response.data.error);
        });
    }

    getNotifySettings() {
        $http.get("/notify/" + this.item.id)
            .then((response) => {
                const data = response.data;
                this.notifySettings = data;
            }, (response) => {
                $window.alert("Could not get notification settings. Error message is: " + response.data.error);
            }).finally(() => {
        });
    }

    notifyChanged() {
        $http.post("/notify/" + this.item.id, this.notifySettings).then((response) => {
            const data = response.data;
        }, (response) => {
            $window.alert("Could not change notification settings. Error message is: " + response.data.error);
        });
    }
}

timApp.component("timManage", {
    controller: PermCtrl,
    templateUrl: "/static/templates/manage.html",
    transclude: true,
});
