/**
 * A component for viewing and changing document or folder relevance value.
 *
 * Relevance value denotes document or folder search priority. Higher value means the document or
 * documents in the folder will be higher in the results list. Low relevance documents will be excluded from search,
 * the relevance threshold value can be changed by user.
 */

import {Component, Input} from "@angular/core";
import {toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import type {IRelevance, IEditableTranslation} from "tim/item/IItem";
import {getItem, IItem} from "tim/item/IItem";
import {showMessageDialog} from "tim/ui/showMessageDialog";

export const relevanceSuggestions = [
    {value: -100, name: "-100 = Buried"},
    {value: 0, name: "0 = Ignored"},
    {value: 1, name: "1 = Not important"},
    {value: 10, name: "10 = Normal"},
    {value: 50, name: "50 = Important"},
    {value: 100, name: "100 = Very important"},
];

export interface IRelevanceResponse {
    default: boolean; // Item and its parents don't have set relevance.
    inherited: boolean; // Relevance was inherited from a parent.
    relevance: IRelevance;
}

@Component({
    selector: "tim-relevance-edit",
    template: `
        <p>View and set relevance value denoting item priority in search results</p>
        <div class="form-group form-inline">
            <input class="form-control" [(ngModel)]="relevance"
                   type="number"
                   title="Enter a new relevance value"
                   placeholder="Enter relevance value"
                   [typeaheadMinLength]="0"
                   [typeahead]="suggestions">
        </div>
        <div class="checkbox">
            <input type="checkbox" [(ngModel)]="updateTranslations" id="cbUpdateTranslationRelevance" style="margin-left: 0">
            <label for="cbUpdateTranslationRelevance" i18n>Update translations' relevance values</label>
        </div>
        <div *ngIf="isDefault" class="alert alert-info">
            <span class="glyphicon glyphicon-exclamation-sign"></span>
            Default relevance value. This may be affected by changes in parent directories.
        </div>
        <div *ngIf="isInherited" class="alert alert-info">
            <span class="glyphicon glyphicon-exclamation-sign"></span>
            Relevance value was inherited from a parent directory.
        </div>
        <div *ngIf="errorMessage" class="alert alert-info">
            <span class="glyphicon glyphicon-exclamation-sign"></span> {{errorMessage}}
        </div>
        <div>
            <button class="timButton" (click)="saveClicked()" title="Save new relevance value">Save</button>
            <button class="timButton" (click)="resetClicked()" [disabled]="isDefault"
                    title="Return default relevance value" style="margin-left: 0.3em">Reset
            </button>
        </div>
    `,
})
export class RelevanceEditComponent {
    @Input() item!: IItem;
    relevance: number | undefined;
    isDefault: boolean = false;
    isInherited: boolean = false;
    errorMessage: string | undefined;
    suggestions = relevanceSuggestions;
    updateTranslations: boolean = false;
    http: HttpClient;

    constructor(http: HttpClient) {
        this.http = http;
    }

    ngOnInit() {
        void this.getRelevance();
    }

    /**
     * Fetch the current item's relevance value and whether it was default or inherited from parent.
     */
    private async getRelevance() {
        this.errorMessage = undefined;
        const r = await toPromise(
            this.http.get<IRelevanceResponse>(
                `/items/relevance/get/` + this.item.id
            )
        );
        if (r.ok) {
            // console.log(r.result.data);
            this.isDefault = r.result.default;
            this.isInherited = r.result.inherited;
            this.relevance = r.result.relevance.relevance;
        } else {
            this.errorMessage = r.result.error.error;
        }
    }

    /**
     * Set new relevance value for the item.
     * @param itemID id number for the document/item
     * @param newValue Input value.
     */
    private async setRelevance(itemID: number, newValue: number) {
        this.errorMessage = undefined;
        const r = await toPromise(
            this.http.post(`/items/relevance/set/` + itemID, {
                value: newValue,
            })
        );
        if (r.ok) {
            this.isDefault = false;
            this.isInherited = false;
        } else {
            this.errorMessage = r.result.error.error;
        }
    }

    /**
     * Return default (or inherited) relevance value.
     * @param itemID id number for the document/item
     */
    private async resetRelevance(itemID: number) {
        this.errorMessage = undefined;
        const r = await toPromise(
            this.http.get<IRelevance>(`/items/relevance/reset/` + itemID)
        );
        if (!r.ok) {
            this.errorMessage = r.result.error.error;
        }
        void this.getRelevance();

        if (this.updateTranslations && this.relevance) {
            await this.updateTranslationRelevances(this.relevance);
        }
    }

    async resetClicked() {
        await this.resetRelevance(this.item.id);
    }

    async saveClicked() {
        if (this.relevance != null) {
            await this.setRelevance(this.item.id, this.relevance);
            if (this.updateTranslations) {
                await this.updateTranslationRelevances(this.relevance);
            }
        } else {
            this.errorMessage =
                "Incorrect relevance value: input a whole number!";
        }
    }

    /**
     * Update the relevance values for translations based on the current document
     * @param sourceRelevance relevance value for the source document
     * @private
     */
    private async updateTranslationRelevances(sourceRelevance: number) {
        const translations: IEditableTranslation[] = await this.getTranslations(
            this.item.id
        );
        translations.map((tr) => this.setRelevance(tr.id, sourceRelevance));
    }

    private async getTranslations(itemID: number) {
        const item = await getItem(itemID);
        if (item?.isFolder) {
            return [];
        }

        const r = await toPromise(
            this.http.get<IEditableTranslation[]>("/translations/" + itemID, {})
        );
        if (r.ok) {
            return r.result;
        } else {
            await showMessageDialog(
                `Error loading translations: ` + r.result.error.error
            );
        }
        return [];
    }
}
