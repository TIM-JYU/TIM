/**
 * A component for viewing and changing document or folder relevance value.
 *
 * Relevance value denotes document or folder search priority. Higher value means the document or
 * documents in the folder will be higher in the results list. Item can be excluded from all search results by
 * setting relevance as zero.
 */

import {IController} from "angular";
import {to} from "tim/util/utils";
import {timApp} from "../app";
import {$http, $window} from "../util/ngimport";
import {IItem, IRelevance} from "./IItem";

export const DEFAULT_RELEVANCE_VALUE: number = 10;

class RelevanceCtrl implements IController {
    private static $inject = ["$element", "$scope"];
    private item: IItem = $window.item;
    private relevance: number|undefined;
    private isDefault: boolean = false;
    private errorMessage: string|undefined;

    async $onInit() {
        this.getRelevance();
    }

    private async getRelevance() {
        const r = await to($http.get<IRelevance>(`/items/relevance/get/${this.item.id}`));
        if (r.ok) {
            if (!r.result.data) {
                this.isDefault = true;
                this.relevance = DEFAULT_RELEVANCE_VALUE;
            } else {
                this.isDefault = false;
                this.relevance = r.result.data.relevance;
            }
        }
    }

    private async setRelevance(newValue: number) {
        const r = await to($http.post(
            `/items/relevance/set/${this.item.id}`,
            {"value": newValue}));
        if (r.ok) {
            this.isDefault = false;
        }
    }

    private async resetRelevance() {
        const r = await to($http.get<IRelevance>(`/items/relevance/get/${this.item.id}`));
        if (r.ok) {
            this.isDefault = true;
            this.relevance = DEFAULT_RELEVANCE_VALUE;
        } else {
            // TODO: error
        }
    }

    private async resetClicked() {
        void this.resetRelevance();
    }

    private async saveClicked() {
        // Check zero too because otherwise it's handled like null.
        if (this.relevance || this.relevance === 0) {
            void this.setRelevance(this.relevance);
        } else {
            // TODO: error
        }
    }
}

timApp.component("relevanceEdit", {
    bindings: {
    },
    controller: RelevanceCtrl,
    template: `
        <div class="input-group">
            <input class="form-control" ng-model="$ctrl.relevance" name="relevanceField"
                ng-keypress="$ctrl.keyPressed($event)" type="number"
                title="Enter a new relevance value; relevance of 0 or less will be completely excluded from search"
                placeholder="Enter a new relevance value" id="relevanceField">
            <span class="" ng-if="$ctrl.isDefault">* Default value: parent folder relevance may override this</span>
        </div>
        <p></p>
        <div>
            <button class="timButton" ng-click="$ctrl.saveClicked()" title="Save new relevance value">Save</button>
            <button class="timButton" ng-click="$ctrl.resetClicked()" ng-disabled="$ctrl.isDefault"
                title="Return default relevance value">Reset</button>
        </div>
    `,
});
