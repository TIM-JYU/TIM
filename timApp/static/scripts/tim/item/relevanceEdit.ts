/**
 * A component for viewing and changing document or folder relevance value.
 *
 * Relevance value denotes document or folder search priority. Higher value means the document or
 * documents in the folder will be higher in the results list. Low relevance documents will be excluded from search,
 * the relevance threshold value can be changed by user.
 */

import {IController} from "angular";
import {Binding, to} from "tim/util/utils";
import {timApp} from "../app";
import {$http} from "../util/ngimport";
import {IItem, IRelevance} from "./IItem";

export const relevanceSuggestions = [
            {value: -100, name:  "-100 = Buried"},
            {value: 0, name:    "0 = Ignored"},
            {value: 1, name:    "1 = Not important"},
            {value: 10, name:   "10 = Normal"},
            {value: 50, name:   "50 = Important"},
            {value: 100, name:  "100 = Very important"}];

export interface IRelevanceResponse {
    default: boolean; // Item and its parents don't have set relevance.
    inherited: boolean; // Relevance was inherited from a parent.
    relevance: IRelevance;
}

class RelevanceCtrl implements IController {
    static $inject = ["$element", "$scope"];
    private item!: Binding<IItem, "<">;
    private relevance: number|undefined;
    private isDefault: boolean = false;
    private isInherited: boolean = false;
    private errorMessage: string|undefined;
    private suggestions = relevanceSuggestions;

    $onInit() {
        void this.getRelevance();
    }

    /**
     * Fetch the current item's relevance value and whether it was default or inherited from parent.
     */
    private async getRelevance() {
        this.errorMessage = undefined;
        const r = await to($http.get<IRelevanceResponse>(`/items/relevance/get/${this.item.id}`));
        if (r.ok) {
            // console.log(r.result.data);
            this.isDefault = r.result.data.default;
            this.isInherited = r.result.data.inherited;
            this.relevance = r.result.data.relevance.relevance;
        } else {
            this.errorMessage = r.result.data.error;
        }
    }

    /**
     * Set new relevance value for the item.
     * @param newValue Input value.
     */
    private async setRelevance(newValue: number) {
        this.errorMessage = undefined;
        const r = await to($http.post(
            `/items/relevance/set/${this.item.id}`,
            {value: newValue}));
        if (r.ok) {
            this.isDefault = false;
            this.isInherited = false;
        } else {
            this.errorMessage = r.result.data.error;
        }
    }

    /**
     * Return default (or inherited) relevance value.
     */
    private async resetRelevance() {
        this.errorMessage = undefined;
        const r = await to($http.get<IRelevance>(`/items/relevance/reset/${this.item.id}`));
        if (!r.ok) {
            this.errorMessage = r.result.data.error;
        }
        void this.getRelevance();
    }

    private async resetClicked() {
        await this.resetRelevance();
    }

    private async saveClicked() {
        if (this.relevance != null) {
            await this.setRelevance(this.relevance);
        } else {
            this.errorMessage = "Incorrect relevance value: input a whole number!";
        }
    }
}

timApp.component("relevanceEdit", {
    bindings: {
        focusField: "<",
        item: "<",
    },
    controller: RelevanceCtrl,
    template: `
        <p>View and set relevance value denoting item priority in search results</p>
        <div class="input-group">
            <input class="form-control" ng-model="$ctrl.relevance" type="number"
                title="Enter a new relevance value" placeholder="Enter relevance value" typeahead-min-length="0"
                uib-typeahead="s.value as s.name for s in $ctrl.suggestions | orderBy:'-value'">
        </div>
        <p></p>
        <div ng-if="$ctrl.isDefault" class="alert alert-info">
             <span class="glyphicon glyphicon-exclamation-sign"></span>
             Default relevance value. This may be affected by changes in parent directories.
        </div>
        <div ng-if="$ctrl.isInherited" class="alert alert-info">
             <span class="glyphicon glyphicon-exclamation-sign"></span>
             Relevance value was inherited from a parent directory.
        </div>
        <div ng-if="$ctrl.errorMessage" class="alert alert-info">
            <span class="glyphicon glyphicon-exclamation-sign"></span> {{$ctrl.errorMessage}}
        </div>
        <div>
            <button class="timButton" ng-click="$ctrl.saveClicked()" title="Save new relevance value">Save</button>
            <button class="timButton" ng-click="$ctrl.resetClicked()" ng-disabled="$ctrl.isDefault"
                title="Return default relevance value">Reset</button>
        </div>
    `,
});
