/**
 * A component for viewing and changing document or folder relevance value.
 *
 * Relevance value denotes document or folder search priority. Higher value means the document or
 * documents in the folder will be higher in the results list. Low relevance documents will be excluded from search,
 * the relevance threshold value can be changed by user.
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
    private isInherited: boolean = false;
    private errorMessage: string|undefined;

    async $onInit() {
        void this.getRelevance();
    }

    /**
     * Fetch the current item relevance value.
     */
    private async getRelevance() {
        this.errorMessage = undefined;
        const r = await to($http.get<IRelevance>(`/items/relevance/get/${this.item.id}`));
        if (r.ok) {
            if (!r.result.data) {

                const r2 = await to($http.get<IRelevance>(`/items/parentrelevance/get/${this.item.id}`));
                if (r2.ok) {
                    if (!r2.result.data) {
                        this.isDefault = true;
                        this.relevance = DEFAULT_RELEVANCE_VALUE;
                    } else {
                        this.isInherited = true;
                        this.relevance = r2.result.data.relevance;
                    }
                } else {
                    this.errorMessage = r2.result.data.error;
                }
            } else {
                this.isDefault = false;
                this.isInherited = false;
                this.relevance = r.result.data.relevance;
            }
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
        void this.resetRelevance();
    }

    private async saveClicked() {
        if (this.relevance != null) {
            void this.setRelevance(this.relevance);
        } else {
            this.errorMessage = "Incorrect relevance value: input a whole number!";
        }
    }
}

timApp.component("relevanceEdit", {
    bindings: {
    },
    controller: RelevanceCtrl,
    template: `
        <div class="input-group">
            <input class="form-control" ng-model="$ctrl.relevance" ng-keypress="$ctrl.keyPressed($event)" type="text"
                title="Enter a new relevance value; as default relevances less than 10 will be excluded from search"
                placeholder="Enter relevance value">
        </div>
        <p></p>
        <div ng-if="$ctrl.isDefault" class="alert alert-warning">
             <span class="glyphicon glyphicon-exclamation-sign"></span>
             Default relevance value. Note: this may be affected by changes in parent directories.
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
