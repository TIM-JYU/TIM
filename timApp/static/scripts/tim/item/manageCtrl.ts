import {IController, IFormController, IHttpResponse} from "angular";
import {timApp} from "tim/app";
import {IChangelogEntry, IManageResponse} from "../document/editing/edittypes";
import {isManageResponse, showRenameDialog} from "../document/editing/pluginRenameForm";
import * as copyFolder from "../folder/copyFolder";
import {showMessageDialog} from "../ui/dialog";
import {IGroup} from "../user/IUser";
import {Users} from "../user/userService";
import {manageglobals} from "../util/globals";
import {$http} from "../util/ngimport";
import {clone, markAsUsed, to} from "../util/utils";
import {IDocument, IFolder, IFullDocument, IItem, ITranslation, redirectToItem} from "./IItem";

markAsUsed(copyFolder);

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
    private translations: Array<IItem & {old_title: string}> = [];
    private newTranslation: {language: string, title: string};
    private accessTypes: Array<{}>;
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
    private notifySettings: {} = {};
    private objName: string;
    private progress?: number;
    private result?: boolean;

    constructor() {
        this.newTranslation = {language: "", title: ""};
        this.accessTypes = manageglobals().accessTypes;
        this.orgs = manageglobals().orgs;
        this.item = manageglobals().curr_item;
        this.objName = this.item.isFolder ? "Folder" : "Document";
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
        this.getNotifySettings();
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
    }

    async showMoreChangelog() {
        const d = this.itemAsDocument();
        const newLength = d.versions.length + 100;
        this.changelogLoading = true;
        const r = await to($http.get<{versions: IChangelogEntry[]}>("/changelog/" + this.item.id + "/" + (newLength)));
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
            if (this.aliases && this.aliases.length > 0 &&
                data.length > 0 &&
                data[0].path !== this.aliases[0].path) {
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

        const r = await to($http.get<Array<IItem & {old_title: string, old_langid: string, lang_id: string}>>("/translations/" + this.item.id, {}));
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
            await showMessageDialog(`Error loading translations: ${r.result.data.error}`);
        }

        return [];
    }

    trChanged(tr: ITranslation) {
        return tr.title !== tr.old_title || tr.lang_id !== tr.old_langid;
    }

    async updateTranslation(tr: ITranslation) {
        const r = await to($http.post("/translation/" + tr.id, {
            new_langid: tr.lang_id,
            new_title: tr.title,
            old_title: tr.old_title,
        }));
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
            showMessageDialog("Title not provided.");
            return;
        }
        const r = await to($http.put("/changeTitle/" + this.item.id, {
            new_title: this.newTitle,
        }));
        if (r.ok) {
            this.syncTitle(this.newTitle);
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async renameFolder(newName: string) {
        const r = await to($http.put("/rename/" + this.item.id, {
            new_name: this.oldFolderName + "/" + newName,
        }));
        if (r.ok) {
            window.location.assign("/manage/" + this.oldFolderName + "/" + newName);
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async moveFolder(newLocation: string) {
        const r = await to($http.put("/rename/" + this.item.id, {
            new_name: newLocation + "/" + this.oldName,
        }));
        if (r.ok) {
            // This is needed to update the breadcrumbs
            location.reload();
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
        return alias.publicChanged || alias.path !== this.combine(alias.location, alias.name);
    }

    async addAlias(newAlias: IAlias) {
        const path = encodeURIComponent(this.combine(newAlias.location || "", newAlias.name));
        const r = await to($http.put(`/alias/${this.item.id}/${path}`, {
            public: newAlias.public,
        }));
        if (r.ok) {
            await this.getAliases();
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async removeAlias(alias: IAlias) {
        const r = await to($http.delete("/alias/" + encodeURIComponent(alias.path), {}));
        if (r.ok) {
            await this.getAliases();
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async updateAlias(alias: IAlias, first: boolean) {
        const newAlias = this.combine(alias.location, alias.name);
        const r = await to($http.post("/alias/" + encodeURIComponent(alias.path), {
            new_name: newAlias,
            public: alias.public,
        }));
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

    async deleteDocument() {
        if (window.confirm("Are you sure you want to delete this document?")) {
            const r = await to($http.delete("/documents/" + this.item.id));
            if (r.ok) {
                location.replace(`/view/${this.item.location}`);
            } else {
                await showMessageDialog(r.result.data.error);
            }
        }
    }

    async deleteFolder() {
        if (window.confirm(`Are you sure you want to delete the folder ${this.item.path}?`)) {
            const r = await to($http.delete(`/folders/${this.item.id}`));
            if (r.ok) {
                location.replace(`/view/${this.item.location}`);
            } else {
                await showMessageDialog(r.result.data.error);
            }
        }
    }

    convertDocument(doc: string) {
        if (!this.wikiRoot) {
            showMessageDialog("Wiki root missing.");
            return;
        }
        let text = this.tracWikiText;
        const wikiSource = this.wikiRoot.replace("wiki", "browser");
        // var text = this.fulltext;
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
            const message = "Current item is unexpectedly folder instead of document";
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
        const r = await to<IHttpResponse<IManageResponse>,
            {data: {error: string, is_warning?: boolean}}>($http.post<IManageResponse>(`/update/${this.item.id}`,
            {
                fulltext: this.fulltext,
                original: d.fulltext,
                version: d.versions[0],
            }));
        if (r.ok) {
            let data = r.result.data;
            if (data.duplicates.length > 0) {
                const renameResult = await to(showRenameDialog({
                    duplicates: data.duplicates,
                }));
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
                if (window.confirm(data.error + "\n\nDo you still wish to save the document?")) {
                    this.saveDocumentWithWarnings();
                }
            } else {
                await showMessageDialog(data.error);
            }
        }
    }

    async saveDocumentWithWarnings() {
        const d = this.itemAsDocument();
        this.saving = true;
        const r = await to($http.post<IManageResponse>("/update/" + this.item.id,
            {
                fulltext: this.fulltext,
                ignore_warnings: true,
                original: d.fulltext,
                version: d.versions[0],
            }));
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

    async createTranslation() {
        const r = await to($http.post<IDocument>(`/translate/${this.item.id}/${this.newTranslation.language}`, {
            doc_title: this.newTranslation.title,
        }));
        if (r.ok) {
            const data = r.result.data;
            redirectToItem(data);
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async getNotifySettings() {
        if (!this.loggedIn()) {
            return;
        }
        const r = await to($http.get<{}>("/notify/" + this.item.id));
        if (r.ok) {
            this.notifySettings = r.result.data;
        } else {
            await showMessageDialog(`Could not get notification settings. Error message is: ${r.result.data.error}`);
        }
    }

    async notifyChanged() {
        const r = await to($http.post("/notify/" + this.item.id, this.notifySettings));
        if (r.ok) {
            // nothing to do
        } else {
            await showMessageDialog(`Could not change notification settings. Error message is: ${r.result.data.error}`);
        }
    }

    loggedIn() {
        return Users.isLoggedIn();
    }
}

timApp.component("timManage", {
    controller: PermCtrl,
    templateUrl: "/static/templates/manage.html",
});
