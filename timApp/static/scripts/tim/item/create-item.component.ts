import * as formErrorMessage from "tim/ui/formErrorMessage";
import {AlertSeverity} from "tim/ui/formErrorMessage";
import * as shortNameValidator from "tim/ui/shortNameValidator";
import {getURLParameter, markAsUsed} from "tim/util/utils";
import {Component, Input, OnInit} from "@angular/core";
import {$http} from "../util/ngimport";
import {slugify} from "../util/slugify";
import {to} from "../util/utils";
import {ITaggedItem, TagType} from "./IItem";

markAsUsed(formErrorMessage, shortNameValidator);

@Component({
    selector: "create-item",
    template: `
        <form name="itemForm" #f="ngForm">
            <tim-alert severity="warning" *ngIf="tagsWithExpirations">
                The source document has tags with expiration dates which may need to be updated manually.
            </tim-alert>
            <div class="form-group" timErrorState>
                <label>
                    Title:
                    <input class="form-control" required [(ngModel)]="itemTitle" name="itemTitle"
                           type="text" (input)="titleChanged()">
                </label>
                <tim-error-message></tim-error-message>
            </div>
            <div class="form-group" timErrorState>
                <label>
                    Short name:
                    <input [disabled]="force" class="form-control" timShortName required
                           [(ngModel)]="itemName" type="text" name="itemName"
                           placeholder="Type title first"
                           (keyup)="nameChanged()">
                </label>
                <div class="checkbox">
                    <label>
                        <input [disabled]="force" type="checkbox" [(ngModel)]="automaticShortName"
                               name="automaticShortName"> Set automatically
                        based on title
                    </label>
                </div>
                <tim-error-message></tim-error-message>
            </div>
            <div class="form-group" timErrorState>
                <label>
                    Location: <input size="50" [disabled]="force" class="form-control" type="text"
                                     timLocation [(ngModel)]="itemLocation" name="itemLocation">
                </label>
                <tim-error-message></tim-error-message>
            </div>
            <tim-alert *ngFor="let alert of alerts" [severity]="alert.type">
                {{ alert.msg }}
            </tim-alert>
            <button class="timButton" [disabled]="f.invalid || creating" (click)="createItem()" type="button">
                Create {{ itemType }}
            </button>
            <span *ngIf="creating">Creating...</span>
        </form>
    `,
})
export class CreateItemComponent implements OnInit {
    @Input() private fullPath?: string;
    automaticShortName = false;
    @Input() itemLocation?: string;
    @Input() itemTitle?: string;
    @Input() itemName?: string;
    alerts: Array<{type: AlertSeverity; msg: string}> = [];
    @Input() itemType!: string;
    @Input() private params?: {template?: string; copy?: number};
    @Input() force = false;
    creating = false;
    @Input() private template?: string;
    tagsWithExpirations = false;

    ngOnInit() {
        this.automaticShortName = !this.force;

        if (this.fullPath) {
            const str = this.fullPath;
            this.itemLocation = str.substring(0, str.lastIndexOf("/"));
            this.itemTitle =
                getURLParameter("title") ??
                str.substring(str.lastIndexOf("/") + 1, str.length);
        }
        if (this.itemTitle) {
            this.itemName = slugify(this.itemTitle);
        }
        if (this.template) {
            this.params = this.params ?? {};
            this.params.template = this.template;
        }

        void this.checkExpiredTags();
    }

    /**
     * Checks whether the document to copy has regular tags (special tags aren't copied) with expiration dates.
     */
    private async checkExpiredTags() {
        if (this.params?.copy) {
            const r = await to(
                $http.get<ITaggedItem>(`/tags/getDoc/${this.params.copy}`)
            );
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
        const r = await to(
            $http.post<{path: string}>("/createItem", {
                item_path: this.itemLocation + "/" + this.itemName,
                item_type: this.itemType,
                item_title: this.itemTitle,
                ...this.params,
            })
        );

        if (!r.ok) {
            this.alerts = [];
            this.alerts.push({msg: r.result.data.error, type: "danger"});
            this.creating = false;
            return;
        }

        window.location.href = "/view/" + r.result.data.path;
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
        this.automaticShortName = (this.itemName ?? []).length === 0;
    }
}
