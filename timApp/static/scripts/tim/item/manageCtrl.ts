import {IController, IFormController, IHttpResponse} from "angular";
import {timApp} from "tim/app";
import {
    isManageResponse,
    showRenameDialog,
} from "tim/document/editing/showRenameDialog";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import * as snv from "tim/ui/shortNameValidator";
import * as tem from "tim/ui/formErrorMessage";
import {IChangelogEntry} from "tim/document/editing/IChangelogEntry";
import {
    IManageResponse,
    updateLanguages,
    listTranslators,
    listLanguages,
    availableTranslators,
    isOptionAvailable,
    updateTranslationData,
} from "../document/editing/edittypes";
import {IGroup} from "../user/IUser";
import {Users} from "../user/userService";
import {manageglobals} from "../util/globals";
import {$http} from "../util/ngimport";
import {capitalizeFirstLetter, clone, markAsUsed, to, to2} from "../util/utils";
import {
    IDocument,
    IFolder,
    IFullDocument,
    IItem,
    IEditableTranslation,
    ILanguages,
    redirectToItem,
    getItemTypeName,
    ITranslators,
} from "./IItem";

markAsUsed(snv, tem);

export interface IAlias {
    name: string;
    location: string;
    path: string;
    publicChanged: boolean;
    public: boolean;
}

export class PermCtrl implements IController {
    private wikiRoot: string;
    private newTitle: string;
    private newFolderName: string;
    private hasMoreChangelog?: boolean;
    private translations: Array<IEditableTranslation> = [];
    private sourceLanguages: Array<ILanguages> = [];
    private targetLanguages: Array<ILanguages> = [];
    private documentLanguages: Array<ILanguages> = [];
    private translators: Array<ITranslators> = [];
    private newTranslation: {
        language: string;
        title: string;
        translator: string;
    };
    private mayTranslate = false;
    private notManual = false;
    private translatorAvailable = true;
    private translationInProgress: boolean = false;
    private deleteButtonText = "";
    private errorMessage = "";
    private availableTranslators: string[] = [];
    private accessTypes: Array<unknown>; // TODO proper type
    private orgs: IGroup[];
    private item: IFullDocument | IFolder;
    private newName?: string;
    private oldFolderName?: string;
    private oldName?: string;
    private fulltext?: string;
    private changelogLoading: boolean = false;
    private newAlias: {location: string};
    private copyParams: {copy: number};
    private citeParams: {cite: number};
    private aliases: IItem[] = [];
    private oldTitle?: string;
    private fileUploadError: string | undefined;
    private tracWikiText: string = "";
    private saving: boolean = false;
    private readUpdating: boolean = false;
    private file?: File;
    private newAliasForm!: IFormController; // initialized in the template
    private objName: string;
    private progress?: number;
    private result?: boolean;

    constructor() {
        this.newTranslation = {
            language: "",
            title: "",
            translator: "Manual", // Default
        };
        this.accessTypes = manageglobals().accessTypes;
        this.orgs = manageglobals().orgs;
        this.item = manageglobals().curr_item;
        this.objName = capitalizeFirstLetter(getItemTypeName(this.item));
        this.newFolderName = this.item.location;
        this.newAlias = {location: this.newFolderName};

        // TODO: replace with something that remembers user's last choice
        this.wikiRoot = "https://trac.cc.jyu.fi/projects/ohj2/wiki/";

        this.hasMoreChangelog = true;
        this.newTitle = this.item.title;
        this.copyParams = {copy: this.item.id};
        this.citeParams = {cite: this.item.id};
        this.translations = [];
    }

    async $onInit() {
        this.translationInProgress = false;
        if (this.item.isFolder) {
            this.newName = this.item.name;
            this.newFolderName = this.item.location;
            this.oldName = this.newName;
            this.oldFolderName = this.newFolderName;
        } else {
            this.updateFullText(this.item.fulltext);
            if (this.item.rights.manage) {
                await this.getAliases();
                await this.getTranslations();
            }
        }
        this.setDeleteText();

        await updateTranslationData(
            this.sourceLanguages,
            this.documentLanguages,
            this.targetLanguages,
            this.newTranslation.translator,
            this.translators,
            this.availableTranslators,
            this.errorMessage,
            this.translatorAvailable,
            true
        );
    }

    async showMoreChangelog() {
        const d = this.itemAsDocument();
        const newLength = d.versions.length + 100;
        this.changelogLoading = true;
        const r = await to(
            $http.get<{versions: IChangelogEntry[]}>(
                "/changelog/" + this.item.id + "/" + newLength
            )
        );
        this.changelogLoading = false;
        if (r.ok) {
            d.versions = r.result.data.versions;
            this.hasMoreChangelog = d.versions.length === newLength;
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async getAliases() {
        const r = await to($http.get<IItem[]>("/alias/" + this.item.id, {}));
        if (r.ok) {
            const data = r.result.data;
            if (
                this.aliases &&
                this.aliases.length > 0 &&
                data.length > 0 &&
                data[0].path !== this.aliases[0].path
            ) {
                // The first name has changed, reload to update the links
                window.location.replace("/manage/" + data[0].path);
            } else {
                this.aliases = data;
            }
            // mark the form pristine; otherwise it will complain about required field unnecessarily
            this.newAliasForm.$setPristine();
            this.newAlias = {location: this.item.location};
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async getTranslations() {
        if (this.item.isFolder) {
            return [];
        }

        const r = await to(
            $http.get<Array<IEditableTranslation>>(
                "/translations/" + this.item.id,
                {}
            )
        );
        if (r.ok) {
            const data = r.result.data;
            this.translations = [];

            for (const tr of data) {
                const trnew = clone(tr);
                trnew.old_langid = tr.lang_id;
                trnew.old_title = tr.title;
                this.translations.push(trnew);
            }

            this.oldTitle = this.item.title;
        } else {
            await showMessageDialog(
                `Error loading translations: ${r.result.data.error}`
            );
        }

        return [];
    }

    trChanged(tr: IEditableTranslation) {
        return tr.title !== tr.old_title || tr.lang_id !== tr.old_langid;
    }

    async updateTranslation(tr: IEditableTranslation) {
        const r = await to(
            $http.post("/translation/" + tr.id, {
                new_langid: tr.lang_id,
                new_title: tr.title,
                old_title: tr.old_title,
            })
        );
        if (r.ok) {
            await this.getTranslations();
            if (tr.id === this.item.id) {
                this.syncTitle(tr.title);
            }
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    syncTitle(title: string) {
        this.item.title = title;
        this.newTitle = title;
        document.title = title + " - Manage - TIM";
        for (const t of this.translations) {
            if (t.id === this.item.id) {
                t.title = title;
                t.old_title = title;
            }
        }
    }

    async changeTitle() {
        if (!this.newTitle) {
            await showMessageDialog("Title not provided.");
            return;
        }
        const r = await to(
            $http.put("/changeTitle/" + this.item.id, {
                new_title: this.newTitle,
            })
        );
        if (r.ok) {
            this.syncTitle(this.newTitle);
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async renameFolder(newName: string) {
        const r = await to(
            $http.put<{new_name: string}>("/rename/" + this.item.id, {
                new_name: this.oldFolderName + "/" + newName,
            })
        );
        if (r.ok) {
            window.location.assign("/manage/" + r.result.data.new_name);
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async moveFolder(newLocation: string) {
        const r = await to(
            $http.put<{new_name: string}>("/rename/" + this.item.id, {
                new_name: newLocation + "/" + this.oldName,
            })
        );
        if (r.ok) {
            window.location.assign("/manage/" + r.result.data.new_name);
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    combine(folder: string, name: string) {
        return (folder + "/" + name).replace(/(^\/+)|(\/+$)/, "");
    }

    aliasPublicClicked(alias: IAlias) {
        alias.public = !alias.public;
        alias.publicChanged = !alias.publicChanged;
    }

    aliasChanged(alias: IAlias) {
        return (
            alias.publicChanged ||
            alias.path !== this.combine(alias.location, alias.name)
        );
    }

    async addAlias(newAlias: IAlias) {
        const path = encodeURIComponent(
            this.combine(newAlias.location || "", newAlias.name)
        );
        const r = await to(
            $http.put(`/alias/${this.item.id}/${path}`, {
                public: newAlias.public,
            })
        );
        if (r.ok) {
            await this.getAliases();
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async removeAlias(alias: IAlias) {
        const r = await to(
            $http.delete("/alias/" + encodeURIComponent(alias.path), {})
        );
        if (r.ok) {
            await this.getAliases();
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async updateAlias(alias: IAlias, first: boolean) {
        const newAlias = this.combine(alias.location, alias.name);
        const r = await to(
            $http.post("/alias/" + encodeURIComponent(alias.path), {
                new_name: newAlias,
                public: alias.public,
            })
        );
        if (r.ok) {
            if (!first || newAlias === alias.path) {
                await this.getAliases();
            } else {
                location.replace("/manage/" + newAlias);
            }
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    /**
     * Sets the right text for the document deletion button.
     */
    setDeleteText() {
        const lang_name = this.findCurrentTrDocLang(this.item.id);
        if (lang_name == "") {
            this.deleteButtonText = "Delete document";
        } else {
            this.deleteButtonText = "Delete translation: " + lang_name;
        }
    }

    /*
    TODO: Handling the error code should be done better (it should never appear on the browser's console) but at
    least with Angular's catchError it cannot be done with ISaferHttpResponse because it doesn't support pipes.
     */
    async deleteDocument(id: number) {
        const trDocLang = this.findCurrentTrDocLang(id);
        const deletingCurrentDoc = this.item.id == id;
        if (
            window.confirm(
                `Are you sure you want to delete this ${
                    !this.item.isFolder && trDocLang == ""
                        ? "document"
                        : "translation: " + trDocLang
                }?`
            )
        ) {
            const r = await to($http.delete("/documents/" + id));
            if (r.ok) {
                if (!this.item.isFolder && deletingCurrentDoc) {
                    const originalDoc = this.item.path.substring(
                        0,
                        this.item.path.lastIndexOf("/")
                    );
                    location.replace(`/manage/${originalDoc}`);
                } else {
                    // TODO: Turn this from what's basically a page refresh to just updating the list of translations
                    location.replace(`/manage/${this.item.path}`);
                }
            } else {
                await showMessageDialog(r.result.data.error);
            }
        }
    }

    async deleteFolder() {
        if (
            window.confirm(
                `Are you sure you want to delete the folder ${this.item.path}?`
            )
        ) {
            const r = await to($http.delete(`/folders/${this.item.id}`));
            if (r.ok) {
                location.replace(`/view/${this.item.location}`);
            } else {
                await showMessageDialog(r.result.data.error);
            }
        }
    }

    async convertDocument(doc: string) {
        if (!this.wikiRoot) {
            await showMessageDialog("Wiki root missing.");
            return;
        }
        let text = this.tracWikiText;
        const wikiSource = this.wikiRoot.replace("wiki", "browser");
        // var text = this.fulltext;
        text = text.replace(/\[\[PageOutline\]\].*\n/g, ""); // remove contents
        text = text.replace(/ !([a-zA-Z])/g, " $1"); // remove cat !IsBig
        text = text.replace(/\r\n/g, "\n"); // change windows nl's
        text = text.replace(/{{{(.*?)}}}/g, "`$1`"); // change verbatim
        // text = text.replace(/{{{\n(.*?)\n}}}/, "```\n$1\n```"); // change code blocks
        text = text.replace(
            /\n{{{#!comment\n((.|\s|\S)*?)\n}}}\n/g,
            "\n#- {.comment}\n$1\n#-\n"
        ); // comments
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
                line = line.replace(
                    /\[((https?|mms):\/\/[^\s\[\]]+)\s+([^\[\]]+)\]/g,
                    "[$3]($1)"
                ); // ordinary links
                line = line.replace(
                    /\[((https?|mms):\/\/[^\s\[\]]+)\s+(\[.*?\])\]/g,
                    "[$3]($1)"
                ); // links like [url [text]]
                line = line.replace(
                    /\[wiki:([^\s\[\]]+)\]/g,
                    "[$1](" + (this.wikiRoot || "") + "$1)"
                ); // [wiki:local] -> [local](URL)
                line = line.replace(
                    /\[wiki:([^\s\[\]]+)\s+([^\[\]]+)\]/g,
                    "[$2](" + (this.wikiRoot || "") + "$1)"
                ); // [wiki:local text] -> [text](URL)
                line = line.replace(
                    /\[source:([^\s\[\]]+)\s+([^\[\]]+)\]/g,
                    "[$2](" + (wikiSource || "") + "$1)"
                );
                line = line.replace(/\!(([A-Z][a-z0-9]+){2,})/, "$1");
                line = line.replace(/\'\'\'(.*?)\'\'\'/, "**$1**"); // bold
                line = line.replace(/\'\'(.*?)\'\'/, "*$1*"); // italics
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
        // this.tracWikiText = text;
        this.fulltext = text;
    }

    hasTextChanged() {
        const d = this.itemAsDocument();
        return d.fulltext !== this.fulltext;
    }

    updateFullText(txt: string) {
        this.fulltext = txt.trim();
        const d = this.itemAsDocument();
        d.fulltext = this.fulltext;
    }

    itemAsDocument() {
        if (this.item.isFolder) {
            const message =
                "Current item is unexpectedly folder instead of document";
            void showMessageDialog(message);
            throw new Error(message);
        }
        return this.item;
    }

    async saveDocument() {
        const d = this.itemAsDocument();
        if (this.saving || !this.hasTextChanged()) {
            return;
        }
        this.saving = true;
        const r = await to<
            IHttpResponse<IManageResponse>,
            {data: {error: string; is_warning?: boolean}}
        >(
            $http.post<IManageResponse>(`/update/${this.item.id}`, {
                fulltext: this.fulltext,
                original: d.fulltext,
                version: d.versions[0],
            })
        );
        if (r.ok) {
            let data = r.result.data;
            if (data.duplicates.length > 0) {
                const renameResult = await to2(
                    showRenameDialog({
                        duplicates: data.duplicates,
                    })
                );
                if (renameResult.ok && isManageResponse(renameResult.result)) {
                    data = renameResult.result;
                }
            }
            this.updateFullText(data.fulltext);
            d.versions = data.versions;
            this.saving = false;
        } else {
            const data = r.result.data;
            this.saving = false;
            if (data.is_warning) {
                if (
                    window.confirm(
                        data.error +
                            "\n\nDo you still wish to save the document?"
                    )
                ) {
                    await this.saveDocumentWithWarnings();
                }
            } else {
                await showMessageDialog(data.error);
            }
        }
    }

    async saveDocumentWithWarnings() {
        const d = this.itemAsDocument();
        this.saving = true;
        const r = await to(
            $http.post<IManageResponse>("/update/" + this.item.id, {
                fulltext: this.fulltext,
                ignore_warnings: true,
                original: d.fulltext,
                version: d.versions[0],
            })
        );
        this.saving = false;
        if (r.ok) {
            const data = r.result.data;
            this.updateFullText(data.fulltext);
            d.versions = data.versions;
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async markAllAsRead() {
        this.readUpdating = true;
        const r = await to($http.put("/read/" + this.item.id, {}));
        this.readUpdating = false;
        if (r.ok) {
            // nothing to do
        } else {
            await showMessageDialog("Could not mark the document as read.");
        }
    }

    /**
     * Updates the list of available target languages when translator is changed.
     * TODO: Handling the error code should be done better (it should never appear on the browser's console) but at
     * least with Angular's catchError it cannot be done with ISaferHttpResponse because it doesn't support pipes.
     */
    async updateTranslatorLanguages() {
        let sources = await to(
            $http.post<ILanguages[]>("/translations/target-languages", {
                translator: this.newTranslation.translator,
            })
        );
        if (sources.ok) {
            this.targetLanguages = [];
            listLanguages(sources.result.data, this.targetLanguages);
            this.translatorAvailable = true;
            this.errorMessage = "";
        } else {
            this.translatorAvailable = false;
            this.errorMessage = sources.result.data.error;
            return;
        }
        sources = await to(
            $http.post<ILanguages[]>("/translations/source-languages", {
                translator: this.newTranslation.translator,
            })
        );
        if (sources.ok) {
            this.sourceLanguages = [];
            listLanguages(sources.result.data, this.sourceLanguages);
            this.translatorAvailable = true;
            this.errorMessage = "";
        } else {
            this.translatorAvailable = false;
            this.errorMessage = sources.result.data.error;
            return;
        }
        this.notManualCheck();
        this.checkTranslatability();
    }

    async createTranslation() {
        if (this.newTranslation.translator != "Manual") {
            this.translationInProgress = true;
        }

        const r = await to(
            $http.post<IDocument>(
                `/translate/${this.item.id}/${this.newTranslation.language}/${this.newTranslation.translator}`,
                {
                    doc_title: this.newTranslation.title,
                }
            )
        );
        if (r.ok) {
            const data = r.result.data;
            if (this.newTranslation.translator != "Manual") {
                await $http.post(`/markTranslated/${data.id}`, {
                    doc_id: data.id,
                });
            }
            redirectToItem(data);
        } else {
            this.translationInProgress = false;
            await showMessageDialog(r.result.data.error);
        }
    }

    /**
     * Finds the language of the translation document the given ID refers to.
     * @param id the id of the translation document
     * @returns The current translation document's language or nothing if not found or is source document.
     */
    findCurrentTrDocLang(id: number) {
        for (const translation of this.translations) {
            if (
                translation.id == id &&
                translation.id != translation.src_docid
            ) {
                return translation.lang_id;
            }
        }
        return "";
    }

    /**
     * Finds the language of the source document in the translations list.
     * @returns the source document's language or an empty string if for some reason was not found
     */
    findSourceDocLang() {
        for (const translation of this.translations) {
            if (translation.id == translation.src_docid) {
                return translation.lang_id;
            }
        }
        return "";
    }

    /**
     * Checks whether the translator chosen for the document is Manual (in which case a translator language is not
     * shown) or not
     */
    notManualCheck() {
        if (this.newTranslation.translator == "Manual") {
            this.notManual = false;
            this.translatorAvailable = true;
            return;
        }
        this.notManual = true;
    }

    /**
     * Checks if the document can be translated automatically.
     * TODO: Change the value of target language dropdown to ILanguage object rather than just the code and
     * handle finding languages with .find or .includes or something?
     * @returns Whether or not the document can be translated automatically with the selected translator
     */
    checkTranslatability() {
        let targetFound = false;
        if (this.newTranslation.translator == "Manual") {
            this.mayTranslate = false;
        } else {
            const src_lang = this.findSourceDocLang();
            for (const target of this.targetLanguages) {
                if (target.code == this.newTranslation.language) {
                    targetFound = true;
                    break;
                }
            }
            if (targetFound) {
                for (const source of this.sourceLanguages) {
                    if (source.code == src_lang) {
                        this.mayTranslate = true;
                        return;
                    }
                }
            }
        }
        this.mayTranslate = false;
    }

    loggedIn() {
        return Users.isLoggedIn();
    }
}

timApp.component("timManage", {
    controller: PermCtrl,
    templateUrl: "/static/templates/manage.html",
});
