import type {OnInit} from "@angular/core";
import {Component, ViewChild, ChangeDetectionStrategy} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {NgModel} from "@angular/forms";
import moment from "moment";
import {
    isManageResponse,
    showRenameDialog,
} from "tim/document/editing/showRenameDialog";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import type {IChangelogEntry} from "tim/document/editing/IChangelogEntry";
import {
    updateTranslationData,
    updateTranslatorLanguages,
} from "tim/document/languages";
import type {IManageResponse} from "tim/document/editing/edittypes";
import type {IGroup} from "tim/user/IUser";
import {Users} from "tim/user/userService";
import {manageglobals} from "tim/util/globals";
import {capitalizeFirstLetter, clone, to2, toPromise} from "tim/util/utils";
import type {
    IDocument,
    IFolder,
    IFullDocument,
    IEditableTranslation,
    ILanguage,
    ITranslator,
} from "tim/item/IItem";
import {redirectToItem, getItemTypeName} from "tim/item/IItem";
import type {IAccessType} from "tim/item/access-role.service";
import type {ReadonlyMoment} from "tim/util/readonlymoment";
import {KEY_ENTER, KEY_S} from "tim/util/keycodes";

export interface IAlias {
    name: string;
    location: string;
    path: string;
    publicChanged?: boolean;
    public?: boolean;
}

/**
 * The manage view of a document or a folder.
 */
@Component({
    selector: "tim-manage",
    templateUrl: "./manage.component.html",
    // The layout relies on the whitespace between the inline elements (e.g. buttons).
    preserveWhitespaces: true,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class ManageComponent implements OnInit {
    wikiRoot: string;
    newTitle: string;
    newFolderName: string;
    hasMoreChangelog?: boolean;
    translations: IEditableTranslation[] = [];
    sourceLanguages: ILanguage[] = [];
    targetLanguages: ILanguage[] = [];
    documentLanguages: ILanguage[] = [];
    translators: ITranslator[] = [];
    newTranslation: {
        language: string;
        title: string;
        translator: string;
    };
    mayTranslate = false;
    notManual = false;
    translatorAvailable = true;
    translationInProgress: boolean = false;
    deleteButtonText = "";
    errorMessage = "";
    whyCannotTranslate = "";
    availableTranslators: string[] = [];
    accessTypes: IAccessType[];
    orgs: IGroup[];
    item: IFullDocument | IFolder;
    newName?: string;
    oldFolderName?: string;
    oldName?: string;
    fulltext?: string;
    errors?: string;
    changelogLoading: boolean = false;
    newAlias: Partial<IAlias> & {location: string};
    copyParams: {copy: number};
    aliases: IAlias[] = [];
    oldTitle?: string;
    tracWikiText: string = "";
    showTracWiki = false;
    saving: boolean = false;
    readUpdating: boolean = false;
    objName: string;

    @ViewChild("newAliasLoc") private newAliasLoc?: NgModel;
    @ViewChild("newAliasName") private newAliasName?: NgModel;

    constructor(private http: HttpClient) {
        this.newTranslation = {
            language: "",
            title: "",
            translator: "Manual", // Default
        };
        this.accessTypes = manageglobals().accessTypes as IAccessType[];
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
        this.translations = [];
    }

    async ngOnInit() {
        if (this.item.isFolder) {
            this.newName = this.item.name;
            this.newFolderName = this.item.location;
            this.oldName = this.newName;
            this.oldFolderName = this.newFolderName;
        } else {
            this.errors = this.item.errors;
            this.updateFullText(this.item.fulltext);
            if (this.item.rights.manage) {
                await this.getAliases();
                await this.getTranslations();
            }
        }
        this.setDeleteText();

        const result = await updateTranslationData(
            this.newTranslation.translator,
            this.errorMessage,
            true
        );
        this.sourceLanguages = result.source;
        this.documentLanguages = result.document;
        this.targetLanguages = result.target;
        this.translators = result.translators;
        this.availableTranslators = result.availableTransls;
        this.errorMessage = result.error;

        if (this.errorMessage != "") {
            this.translatorAvailable = false;
        }
        this.checkCannotTranslate();
    }

    /**
     * The version history of the document.
     */
    get versions(): IChangelogEntry[] {
        return this.item.isFolder ? [] : this.item.versions;
    }

    /**
     * The id of the source document if the document is a translation.
     */
    get srcDocId(): number | undefined {
        return this.item.isFolder ? undefined : this.item.src_docid;
    }

    formatTime(time: ReadonlyMoment | string) {
        return moment(time as moment.MomentInput).format("DD.MM.YYYY HH:mm:ss");
    }

    /**
     * The names of the owners (maintainers) of the translation.
     */
    ownerNames(tr: IEditableTranslation) {
        return tr.owners.map((o) => o.name).join(", ");
    }

    /**
     * Makes the string URL friendly.
     */
    escapePath(str: string | undefined) {
        return encodeURIComponent(str ?? "").replace(/%2F/g, "/");
    }

    /**
     * Checks for reasons why the document cannot be translated.
     */
    checkCannotTranslate() {
        const base = "The following information is missing: ";
        let reasons = base;
        if (this.findSourceDocLang() == "") {
            reasons = reasons + "Source language";
        }
        if (this.newTranslation.title == "") {
            if (!(reasons === base)) {
                reasons = reasons + ", ";
            }
            reasons = reasons + "Title";
        }
        if (this.newTranslation.language == "") {
            if (!(reasons === base)) {
                reasons = reasons + ", ";
            }
            reasons = reasons + "Target language";
        }
        if (this.notManual && !this.mayTranslate && this.translatorAvailable) {
            if (reasons === base) {
                reasons =
                    "The chosen machine translator does not support the chosen language combination.";
            }
        }
        if (!(reasons === base)) {
            this.whyCannotTranslate = reasons;
        } else {
            this.whyCannotTranslate = "";
        }
    }

    async showMoreChangelog() {
        const d = this.itemAsDocument();
        const newLength = d.versions.length + 100;
        this.changelogLoading = true;
        const r = await toPromise(
            this.http.get<{versions: IChangelogEntry[]}>(
                "/changelog/" + this.item.id + "/" + newLength
            )
        );
        this.changelogLoading = false;
        if (r.ok) {
            d.versions = r.result.versions;
            this.hasMoreChangelog = d.versions.length === newLength;
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    async showCompleteChangelog() {
        const d = this.itemAsDocument();
        this.changelogLoading = true;
        const r = await toPromise(
            this.http.get<{versions: IChangelogEntry[]}>(
                "/changelog/" + this.item.id + "/" + "complete"
            )
        );
        if (r.ok) {
            this.changelogLoading = false;
            d.versions = r.result.versions;
            this.hasMoreChangelog = false;
        } else {
            this.changelogLoading = false;
            await showMessageDialog(r.result.error.error);
        }
    }

    async getAliases() {
        const r = await toPromise(
            this.http.get<IAlias[]>("/alias/" + this.item.id)
        );
        if (r.ok) {
            const data = r.result;
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
            this.newAlias = {location: this.item.location};
            // mark the fields pristine; otherwise they will complain about required field unnecessarily
            this.newAliasLoc?.control.markAsPristine();
            this.newAliasName?.control.markAsPristine();
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    async getTranslations() {
        if (this.item.isFolder) {
            return [];
        }

        const r = await toPromise(
            this.http.get<IEditableTranslation[]>(
                "/translations/" + this.item.id
            )
        );
        if (r.ok) {
            const data = r.result;
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
                `Error loading translations: ${r.result.error.error}`
            );
        }

        return [];
    }

    trChanged(tr: IEditableTranslation) {
        return tr.title !== tr.old_title || tr.lang_id !== tr.old_langid;
    }

    async updateTranslation(tr: IEditableTranslation) {
        const old_lang = tr.old_langid;
        const r = await toPromise(
            this.http.post("/translation/" + tr.id, {
                new_langid: tr.lang_id,
                new_title: tr.title,
                old_title: tr.old_title,
            })
        );
        if (r.ok) {
            await this.getTranslations();
            if (tr.id === this.item.id) {
                // syncTitle does not update the current document path,
                // force page reload manually with the new translation path
                const sep = tr.old_langid == "" ? "/" : "";
                const idx = tr.path.lastIndexOf(tr.old_langid);
                const new_path = tr.path
                    .substring(0, idx)
                    .concat(sep)
                    .concat(tr.lang_id);
                window.location.replace("/manage/" + new_path);
            }
        } else {
            tr.lang_id = old_lang;
            await showMessageDialog(r.result.error.error);
        }
        this.checkTranslatability();
        this.checkCannotTranslate();
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
        if (this.newTitle === this.item.title) {
            return;
        }
        const r = await toPromise(
            this.http.put("/changeTitle/" + this.item.id, {
                new_title: this.newTitle,
            })
        );
        if (r.ok) {
            this.syncTitle(this.newTitle);
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    async renameFolder(newName: string | undefined) {
        const r = await toPromise(
            this.http.put<{new_name: string}>("/rename/" + this.item.id, {
                new_name: this.oldFolderName + "/" + newName,
            })
        );
        if (r.ok) {
            window.location.assign("/manage/" + r.result.new_name);
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    async moveFolder(newLocation: string) {
        const r = await toPromise(
            this.http.put<{new_name: string}>("/rename/" + this.item.id, {
                new_name: newLocation + "/" + this.oldName,
            })
        );
        if (r.ok) {
            window.location.assign("/manage/" + r.result.new_name);
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    combine(folder: string, name: string) {
        return (folder + "/" + name).replace(/(^\/+)|(\/+$)/, "");
    }

    aliasPublicClicked(alias: Partial<IAlias>) {
        alias.public = !alias.public;
        alias.publicChanged = !alias.publicChanged;
    }

    aliasChanged(alias: IAlias) {
        return (
            !!alias.publicChanged ||
            alias.path !== this.combine(alias.location, alias.name)
        );
    }

    async addAlias(newAlias: Partial<IAlias>) {
        const path = encodeURIComponent(
            this.combine(newAlias.location ?? "", newAlias.name ?? "")
        );
        const r = await toPromise(
            this.http.put(`/alias/${this.item.id}/${path}`, {
                public: newAlias.public,
            })
        );
        if (r.ok) {
            await this.getAliases();
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    async removeAlias(alias: IAlias) {
        const r = await toPromise(
            this.http.delete("/alias/" + encodeURIComponent(alias.path))
        );
        if (r.ok) {
            await this.getAliases();
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    async updateAlias(alias: IAlias, first: boolean) {
        const newAlias = this.combine(alias.location, alias.name);
        const r = await toPromise(
            this.http.post("/alias/" + encodeURIComponent(alias.path), {
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
            await showMessageDialog(r.result.error.error);
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

    /**
     * Removes the chosen document.
     * @param id the id of the document to be deleted
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
                }? Deletion cannot be undone!`
            )
        ) {
            const r = await toPromise(this.http.delete("/documents/" + id));
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
                await showMessageDialog(r.result.error.error);
            }
        }
    }

    async deleteFolder() {
        if (
            window.confirm(
                `Are you sure you want to delete the folder ${this.item.path}?`
            )
        ) {
            const r = await toPromise(
                this.http.delete(`/folders/${this.item.id}`)
            );
            if (r.ok) {
                location.replace(`/view/${this.item.location}`);
            } else {
                await showMessageDialog(r.result.error.error);
            }
        }
    }

    async convertDocument() {
        if (!this.wikiRoot) {
            await showMessageDialog("Wiki root missing.");
            return;
        }
        let text = this.tracWikiText;
        const wikiSource = this.wikiRoot.replace("wiki", "browser");
        text = text.replace(/\[\[PageOutline\]\].*\n/g, ""); // remove contents
        text = text.replace(/ !([a-zA-Z])/g, " $1"); // remove cat !IsBig
        text = text.replace(/\r\n/g, "\n"); // change windows nl's
        text = text.replace(/{{{(.*?)}}}/g, "`$1`"); // change verbatim
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
        const lines = text.split("\n");
        for (let i = 0; i < lines.length; i++) {
            let line = lines[i];
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
                "[$1](" + this.wikiRoot + "$1)"
            ); // [wiki:local] -> [local](URL)
            line = line.replace(
                /\[wiki:([^\s\[\]]+)\s+([^\[\]]+)\]/g,
                "[$2](" + this.wikiRoot + "$1)"
            ); // [wiki:local text] -> [text](URL)
            line = line.replace(
                /\[source:([^\s\[\]]+)\s+([^\[\]]+)\]/g,
                "[$2](" + wikiSource + "$1)"
            );
            line = line.replace(/\!(([A-Z][a-z0-9]+){2,})/, "$1");
            line = line.replace(/\'\'\'(.*?)\'\'\'/, "**$1**"); // bold
            line = line.replace(/\'\'(.*?)\'\'/, "*$1*"); // italics
            lines[i] = line;
        }
        text = lines.join("\n");
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

    /**
     * Saves the document when Ctrl+S or Ctrl+Enter is pressed in the editor.
     */
    onEditorKeydown(event: KeyboardEvent) {
        if (
            event.ctrlKey &&
            (event.which === KEY_ENTER || event.which === KEY_S)
        ) {
            void this.saveDocument();
            event.preventDefault();
        }
    }

    async saveDocument() {
        const d = this.itemAsDocument();
        if (this.saving || !this.hasTextChanged()) {
            return;
        }
        this.errors = undefined;
        this.saving = true;
        const r = await toPromise<
            IManageResponse,
            {error: {error: string; is_warning?: boolean}}
        >(
            this.http.post<IManageResponse>(`/update/${this.item.id}`, {
                fulltext: this.fulltext,
                original: d.fulltext,
                version: d.versions[0],
            })
        );
        if (r.ok) {
            let data = r.result;
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
            const data = r.result.error;
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
        const r = await toPromise(
            this.http.post<IManageResponse>("/update/" + this.item.id, {
                fulltext: this.fulltext,
                ignore_warnings: true,
                original: d.fulltext,
                version: d.versions[0],
            })
        );
        this.saving = false;
        if (r.ok) {
            const data = r.result;
            this.updateFullText(data.fulltext);
            d.versions = data.versions;
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    async markAllAsRead() {
        this.readUpdating = true;
        const r = await toPromise(this.http.put("/read/" + this.item.id, {}));
        this.readUpdating = false;
        if (r.ok) {
            // nothing to do
        } else {
            await showMessageDialog("Could not mark the document as read.");
        }
    }

    /**
     * Updates the list of available languages when translator is changed.
     */
    async updateManageTranslatorLanguages() {
        const result = await updateTranslatorLanguages(
            this.newTranslation.translator
        );
        if (result.ok) {
            this.targetLanguages = [];
            this.sourceLanguages = [];
            this.targetLanguages.push(...result.result.target);
            this.sourceLanguages.push(...result.result.source);
            this.translatorAvailable = true;
            this.errorMessage = "";
        } else {
            this.translatorAvailable = false;
            this.errorMessage = result.result;
        }

        this.notManualCheck();
        this.checkTranslatability();
    }

    async createTranslation() {
        if (this.newTranslation.translator != "Manual") {
            this.translationInProgress = true;
        }

        const r = await toPromise(
            this.http.post<IDocument>(
                `/translate/${this.item.id}/${this.newTranslation.language}/${this.newTranslation.translator}`,
                {
                    doc_title: this.newTranslation.title,
                }
            )
        );
        if (r.ok) {
            const data = r.result;
            if (this.newTranslation.translator != "Manual") {
                await toPromise(
                    this.http.post(`/markTranslated/${data.id}`, {
                        doc_id: data.id,
                    })
                );
            }
            redirectToItem(data);
        } else {
            this.translationInProgress = false;
            await showMessageDialog(r.result.error.error);
        }
    }

    /**
     * Finds the language of the translation document the given ID refers to.
     * @param id the id of the translation document
     * @returns The current translation document's language or nothing if not found or is source document.
     */
    findCurrentTrDocLang(id: number) {
        return (
            this.translations.find((tr) => tr.id == id && tr.id != tr.src_docid)
                ?.lang_id ?? ""
        );
    }

    /**
     * Finds the language of the source document in the translations list.
     * @returns the source document's language or an empty string if for some reason was not found
     */
    findSourceDocLang() {
        return (
            this.translations.find((tr) => tr.id == tr.src_docid)?.lang_id ?? ""
        );
    }

    /**
     * Checks whether the translator chosen for the document is Manual (in which case a translator language is not
     * shown) or not.
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
                        this.checkCannotTranslate();
                        return;
                    }
                }
            }
        }
        this.mayTranslate = false;
        this.checkCannotTranslate();
    }

    loggedIn() {
        return Users.isLoggedIn();
    }

    /**
     * Returns the path to a document specified by doc_id
     * @param doc_id
     */
    getTranslationPath(doc_id: number) {
        return (
            this.translations.find((tr) => tr.id == doc_id)?.path ??
            this.item.path
        );
    }
}
