import type {AlertSeverity} from "tim/ui/formErrorMessage";
import {getURLParameter, toPromise} from "tim/util/utils";
import type {OnInit} from "@angular/core";
import {Component, Input, ViewChild} from "@angular/core";
import {slugify} from "tim/util/slugify";
import type {ITaggedItem} from "tim/item/IItem";
import {TagType} from "tim/item/IItem";
import {HttpClient} from "@angular/common/http";
import {NgModel} from "@angular/forms";

@Component({
    selector: "create-item",
    template: `
        <form name="itemForm" #form #f="ngForm">
            <tim-alert severity="warning" *ngIf="tagsWithExpirations">
                The source document has tags with expiration dates which may need to be updated manually.
            </tim-alert>
            <div *ngIf="params?.copy">
                <p>Only document contents will be copied. No history or rights will be preserved.</p>
                <p>If you need to have the same document to appear in multiple folders, define multiple short names instead.</p>
            </div>
            <div class="form-group title" timErrorState>
                <label>
                    {{itemType | titlecase}} title:
                    <input class="form-control" required [(ngModel)]="itemTitle" name="itemTitle"
                           type="text" (input)="titleChanged()">
                </label>
                <tim-error-message></tim-error-message>
            </div>
            <div class="form-group shortname" timErrorState>
                <label>
                    Short name:
                    <input [disabled]="force" class="form-control" timShortName required
                           [(ngModel)]="itemName"
                           (ngModelChange)="checkCopyValidity()"
                           type="text" name="itemName"
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
            <div class="form-group location" timErrorState>
                <label>
                    {{itemType | titlecase}} location: <input size="50" [disabled]="force" class="form-control" type="text"
                                     timLocation [(ngModel)]="itemLocation" name="itemLocation"
                                     (ngModelChange)="checkCopyValidity()">
                </label>
                <tim-error-message></tim-error-message>
            </div>
            <tim-alert *ngFor="let alert of alerts" [severity]="alert.type">
                {{ alert.msg }}
            </tim-alert>
            <button class="timButton" *ngIf="showButton" [disabled]="f.invalid || creating || !canCopy" (click)="createItem()" type="button">
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
    @Input() params?: {
        template?: string;
        copy?: number;
        source?: string;
    };
    @Input() force = false;
    creating = false;
    @Input() private template?: string;
    @ViewChild("f", {static: true}) form!: NgModel;
    tagsWithExpirations = false;
    @Input() sourceLocation?: string;
    @Input() showButton = true;

    canCopy: boolean = true;
    private originalLocation?: string;
    private originalName?: string;

    constructor(private http: HttpClient) {}

    ngOnInit() {
        this.automaticShortName = !this.force;

        if (this.fullPath) {
            const str = this.fullPath;
            this.itemLocation = str.substring(0, str.lastIndexOf("/"));
            this.itemName = str.substring(str.lastIndexOf("/") + 1);
            this.itemTitle = getURLParameter("title") ?? this.itemName;
        } else if (this.itemTitle) {
            this.itemName = slugify(this.itemTitle);
        }
        if (this.template) {
            this.params = this.params ?? {};
            this.params.template = this.template;
        }

        void this.prefillCopyData();
        void this.checkExpiredTags();
    }

    private async prefillCopyData() {
        const copyDocId = this.params?.copy;
        if (!copyDocId) {
            return;
        }
        const r = await toPromise(
            this.http.get<{
                title: string;
                location: string;
                short_name: string;
            }>(`/itemInfo/${copyDocId}`)
        );
        if (!r.ok) {
            return;
        }

        this.itemLocation = r.result.location;
        this.itemTitle = r.result.title;
        this.itemName = r.result.short_name;
        this.originalName = r.result.short_name;
        this.originalLocation = r.result.location;
        this.canCopy = false;
    }

    /**
     * Checks whether the document to copy has regular tags (special tags aren't copied) with expiration dates.
     */
    private async checkExpiredTags() {
        if (this.params?.copy) {
            const r = await toPromise(
                this.http.get<ITaggedItem>(`/tags/getDoc/${this.params.copy}`)
            );
            if (r.ok) {
                const tags = r.result.tags;
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
        const r = await toPromise(
            this.http.post<{path: string}>("/createItem", {
                item_path: this.itemLocation + "/" + this.itemName,
                item_type: this.itemType,
                item_title: this.itemTitle,
                ...this.params,
            })
        );

        if (!r.ok) {
            this.alerts = [];
            this.alerts.push({msg: r.result.error.error, type: "danger"});
            this.creating = false;
            return;
        }

        window.location.href = "/view/" + r.result.path;
    }

    titleChanged() {
        if (!this.automaticShortName) {
            return;
        }
        if (this.itemTitle != null) {
            this.itemName = slugify(this.itemTitle);
            this.checkCopyValidity();
        }
    }

    nameChanged() {
        this.automaticShortName = (this.itemName ?? []).length === 0;
    }

    checkCopyValidity() {
        if (
            this.itemName == this.originalName &&
            this.itemLocation == this.originalLocation
        ) {
            this.canCopy = false;
            this.alerts.push({
                type: "danger",
                msg: "You cannot copy an item to the same location with the same name. Change either the short name or the location.",
            });
        } else {
            this.alerts = [];
            this.canCopy = true;
        }
    }
}
