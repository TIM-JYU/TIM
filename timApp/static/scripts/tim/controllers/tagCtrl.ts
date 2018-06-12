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
 * Tag database attributes.
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
    private tagsList: ITag[];
    private expires: Moment;
    private actionSuccessful: boolean = false;
    private error: boolean = false;
    private errorMessage: string;
    private successMessage: string;
    private f: IFormController;
    private focusName: boolean;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    /*
     * Show tag list when dialog loads and focus on tag-field.
     */
    $onInit() {
        super.$onInit();
        this.updateTags();
        this.focusName = true;
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Edit document tags";
    }

    /*
     * Adds tag when Enter is pressed.
     */
    public chatEnterPressed(event: KeyboardEvent) {
        if (event.which === 13) {
            this.addTagClicked();
        }
    }

    /*
     * Updates the list of existing tags for the document.
     */
    private async updateTags() {
        const docTitle = this.resolve.params.name;
        const [err, response] = await to($http.get<ITag[]>(`/tags/getTags/${docTitle}`, {}));
        if (response) {
            if (this.error) {
                this.error = false;
            }
            this.tagsList = response.data;
            return;
        }
    }

    /*
     * Deletes the selected tag.
     */
    private async removeTag(t: ITag) {
        const docTitle = this.resolve.params.name;
        const data = {tagObject: t};
        const [err, response] = await to($http.post<IParResponse>(`/tags/remove/${docTitle}`, data));
        if (err) {
            this.error = true;
            this.errorMessage = err.data.error;
            // Either shows error message or success message;
            // the other one will be hidden.
            if (this.actionSuccessful) {
                this.actionSuccessful = false;
            }
            // Tag-list needs to be refreshed each time it has been changed.
            this.updateTags();
            return;
        }
        if (response) {
            this.actionSuccessful = true;
            this.successMessage = "Tag '" + t.tag + "' was removed.";
            if (this.error) {
                this.error = false;
            }
            this.updateTags();
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
        const docTitle = this.resolve.params.name;
        const data = {tag: this.tagName, expires: this.expires};
        const [err, response] = await to($http.post<IParResponse>(`/tags/add/${docTitle}`, data));
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
            this.updateTags();
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
                           class="form-control" id="name" placeholder="Tag name" autocomplete="off">
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
