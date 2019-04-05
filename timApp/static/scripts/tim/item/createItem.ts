import {IController} from "angular";
import {timApp} from "tim/app";
import * as formErrorMessage from "tim/ui/formErrorMessage";
import * as shortNameValidator from "tim/ui/shortNameValidator";
import {Binding, getURLParameter, markAsUsed} from "tim/util/utils";
import {$http} from "../util/ngimport";
import {slugify} from "../util/slugify";
import {to} from "../util/utils";
import {ITaggedItem, TagType} from "./IItem";

markAsUsed(formErrorMessage, shortNameValidator);

class CreateItemController implements IController {
    private fullPath?: Binding<string, "@?">;
    private automaticShortName: boolean = false;
    private itemLocation?: Binding<string, "@?">;
    private itemTitle?: Binding<string, "@?">;
    private itemName?: Binding<string, "@">;
    private alerts: Array<{}>;
    private itemType!: Binding<string, "@">;
    private params?: Binding<{template?: string, copy?: number}, "=?">;
    private force?: Binding<boolean, "<?">;
    private creating: boolean = false;
    private template?: Binding<string, "@?">;
    private tagsWithExpirations: boolean = false;

    constructor() {
        this.alerts = [];
    }

    async $onInit() {
        this.automaticShortName = !this.force;

        if (this.fullPath) {
            const str = this.fullPath;
            this.itemLocation = str.substring(0, str.lastIndexOf("/"));
            this.itemTitle = getURLParameter("title") || str.substring(str.lastIndexOf("/") + 1, str.length);
        }
        if (this.itemTitle) {
            this.itemName = slugify(this.itemTitle);
        }
        if (this.template) {
            this.params = this.params || {};
            this.params.template = this.template;
        }

        await this.checkExpiredTags();
    }

    /**
     * Checks whether the document to copy has regular tags (special tags aren't copied) with expiration dates.
     * @returns {Promise<void>}
     */
    private async checkExpiredTags() {
        if (this.params && this.params.copy) {
            const r = await to($http.get<ITaggedItem>(`/tags/getDoc/${this.params.copy}`));
            if (r.ok) {
                const tags = r.result.data.tags;
                for (const tag of tags) {
                    if (tag.expires && tag.type === TagType.Regular) {
                        this.tagsWithExpirations = true;
                        return;
                    }
                }
            }
        }
        this.tagsWithExpirations = false;
    }

    async createItem() {
        this.creating = true;
        const r = await to($http.post<{path: string}>("/createItem", {
            item_path: this.itemLocation + "/" + this.itemName,
            item_type: this.itemType,
            item_title: this.itemTitle,
            ...this.params,
        }));

        if (!r.ok) {
            this.alerts = [];
            this.alerts.push({msg: r.result.data.error, type: "danger"});
            this.creating = false;
            return;
        }

        window.location.href = "/view/" + r.result.data.path;
    }

    closeAlert(index: number) {
        this.alerts.splice(index, 1);
    }

    titleChanged() {
        if (!this.automaticShortName) {
            return;
        }
        if (this.itemTitle != null) {
            this.itemName = slugify(this.itemTitle);
        }
    }

    nameChanged() {
        this.automaticShortName = (this.itemName || []).length === 0;
    }
}

timApp.component("createItem", {
    bindings: {
        itemType: "@", // folder or document
        itemTitle: "@?",
        itemName: "@?",
        itemLocation: "@?",
        fullPath: "@?",
        params: "=?", // any additional parameters to be sent to server
        force: "<?",
        template: "@?",
    },
    controller: CreateItemController,
    templateUrl: "/static/templates/createItem.html",
});
