/**
 * Controller and HTML template for tag dialog.
 */

import {IFormController, IRootElementService, IScope} from "angular";
import {Moment} from "moment";
import * as focusMe from "tim/directives/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {IParResponse} from "../edittypes";
import {IItem} from "../IItem";
import {$http} from "../ngimport";
import {markAsUsed, to} from "../utils";

markAsUsed(focusMe);

/*
 * Tag database attributes:
 * block_id = tagged document's id,
 * expires = tag expiration date and
 * tag = tag name string.
 */
export interface ITag {
    block_id: number;
    expires: Moment;
    tag: string;
}

/*
 * Tag editing dialog's controller.
 */
export class ShowTagController extends DialogController<{ params: IItem }, {}, "timEditTags"> {
    private static $inject = ["$element", "$scope"];
    private document: IItem;
    private tagName: string;
    private tagsList: ITag[]; // List of tags the document has.
    private expires: Moment;
    private actionSuccessful: boolean = false;
    private error: boolean = false;
    private errorMessage: string;
    private successMessage: string;
    private f: IFormController;
    private focusName: boolean;
    private allTags: string[]; // List of all unique tags.
    private allUnusedTags: string[]; // List of existing tags not used in the doc.

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    /*
     * Show tag list when dialog loads and focus on tag-field.
     */
    async $onInit() {
        super.$onInit();
        await this.updateTags();
        this.focusName = true;
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Edit document tags";
    }

    /*
     * Calls tag adding function when Enter is pressed.
     * @param event Keyboard event.
     */
    async chatEnterPressed(event: KeyboardEvent) {
        if (event.which === 13) {
            await this.addTagClicked();
        }
    }

    /*
     * Gets all unique tags in the database, always including special tags.
     */
    private async getUniqueTags() {
        const [err, response] = await to($http.get<string[]>(`/tags/getAllTags`, {}));
        if (err) {
            this.error = true;
            this.errorMessage = err.data.error;
            if (this.actionSuccessful) {
                this.actionSuccessful = false;
            }
            return;
        }
        if (response) {
            if (this.error) {
                this.error = false;
            }

            this.allTags = response.data;
            return;
        }
    }

    /*
     * Updates the list of existing tags for the document.
     */
    private async updateTags() {
        const docPath = this.resolve.params.path;
        const [err, response] = await to($http.get<ITag[]>(`/tags/getTags/${docPath}`, {}));
        if (err) {
            this.error = true;
            this.errorMessage = err.data.error;
            if (this.actionSuccessful) {
                this.actionSuccessful = false;
            }
            return;
        }
        if (response) {
            if (this.error) {
                this.error = false;
            }
            this.tagsList = response.data;

            // Get all globally used tags and remove document tags from them for tag suggestion list.
            await this.getUniqueTags();
            let usedTags: string[];
            this.allUnusedTags = [];
            usedTags = [];
            this.tagsList.forEach((tag) => usedTags.push(tag.tag));
            // Array group difference operation a1 \ a2.
            // For example: a1 = ['a','b','c'] and a2 = ['a','b','d'] returns ['c'].
            this.allUnusedTags = this.allTags.filter((tag) => usedTags.indexOf(tag) < 0);
            return;
        }
    }

    /*
     * Deletes the selected tag.
     */
    private async removeTag(t: ITag) {
        const docPath = this.resolve.params.path;
        const data = {tagObject: t};
        const [err, response] = await to($http.post<IParResponse>(`/tags/remove/${docPath}`, data));
        if (err) {
            this.error = true;
            this.errorMessage = err.data.error;
            // Either shows error message or success message;
            // the other one will be hidden.
            if (this.actionSuccessful) {
                this.actionSuccessful = false;
            }
            // Tag-list needs to be refreshed each time it has been changed.
            await this.updateTags();
            return;
        }
        if (response) {
            this.actionSuccessful = true;
            this.successMessage = "Tag '" + t.tag + "' was removed.";
            if (this.error) {
                this.error = false;
            }
            await this.updateTags();
            return;
        }
    }

    /*
     * Sends post method with tag name and optional expiration date, and
     * saves either success or error message to be used in the dialog.
     */
    private async addTagClicked() {
        if (this.f.$invalid) {
            return;
        }
        const docPath = this.resolve.params.path;
        const data = {tag: this.tagName, expires: this.expires};
        const [err, response] = await to($http.post<IParResponse>(`/tags/add/${docPath}`, data));
        if (err) {
            this.error = true;
            this.errorMessage = err.data.error;
            if (this.actionSuccessful) {
                this.actionSuccessful = false;
            }
            return;
        }
        if (response) {
            this.actionSuccessful = true;
            this.successMessage = "Tag '" + this.tagName + "' successfully added.";
            if (this.error) {
                this.error = false;
            }
            await this.updateTags();
            this.tagName = "";
            this.f.$setPristine();
            this.focusName = true;
            return;
        }
    }
}

registerDialogComponent("timEditTags",
    ShowTagController,
    {
        template:
            `<tim-dialog class="overflow-visible">
    <dialog-header>
    </dialog-header>
    <dialog-body>
        <h4>Document tags</h4>
        <p ng-if="$ctrl.tagsList.length === 0">No tags</p>
        <ul>
            <div class="tags-list" ng-repeat="x in $ctrl.tagsList">
                <li>
                    {{x.tag}} <span ng-if="x.expires">(Expires: {{x.expires | timdate}})</span>
                    <a>
                        <span class="glyphicon glyphicon-remove" title="Remove tag"
                              ng-click="$ctrl.removeTag(x)"></span>
                    </a>
                </li>
            </div>
        </ul>
        <h4>Add new tag</h4>
        <form name="$ctrl.f" class="form-horizontal">
            <div class="form-group" tim-error-state
                 ng-class="{'has-error': !$ctrl.f.nameField.$pristine && $ctrl.f.nameField.$error.required}">
                <label for="name" class="col-sm-4 control-label">Tag name:</label>
                <div class="col-sm-8">
                    <input required focus-me="$ctrl.focusName" tim-short-name ng-model="$ctrl.tagName" name="tagField"
                           type="text" title="Write tag name" ng-keypress="$ctrl.chatEnterPressed($event)"
                           class="form-control" id="name" placeholder="Tag name" autocomplete="off"
                           uib-typeahead="tag as tag for tag in $ctrl.allUnusedTags | filter:$viewValue"
                           typeahead-min-length="0">
                </div>
                <tim-error-message></tim-error-message>
            </div>
            <div class="form-group"
                 ng-class="{'has-error': !$ctrl.f.nameField.$pristine && $ctrl.f.nameField.$error.required}">
                <label for="name" class="col-sm-4 control-label">Expiration date:</label>
                <div class="col-sm-8">
                    <div class="input-group date" datetimepicker ng-model="$ctrl.expires"
                         data-options="datePickerOptionsFrom">
                        <input type="text" class="form-control"
                               placeholder="Leave blank for indefinite period of validity"/>
                        <span class="input-group-addon">
                        <span class="glyphicon glyphicon-calendar"></span>
                </span>
                    </div>
                </div>
            </div>
        </form>
        <button class="btn timButton" data-ng-disabled="$ctrl.f.$invalid" ng-click="$ctrl.addTagClicked()">Add</button>
        <button class="btn timButton" ng-click="$ctrl.dismiss()"><span>Close</span></button>
        <div ng-show="$ctrl.actionSuccessful" class="alert alert-success">
            <span class="glyphicon glyphicon-ok"></span> {{$ctrl.successMessage}}
        </div>
        <div ng-show="$ctrl.error" class="alert alert-warning">
            <span class="glyphicon glyphicon-exclamation-sign"></span> {{$ctrl.errorMessage}}
        </div>
    </dialog-body>
    <dialog-footer>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showTagDialog(d: IItem) {
    return await showDialog<ShowTagController>("timEditTags", {params: () => d}).result;
}
