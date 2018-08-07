/**
 * Controller and HTML template for tag dialog.
 */

import {IFormController, IRootElementService, IScope} from "angular";
import {Moment} from "moment";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {IItem, ITag, tagIsExpired, TagType} from "./IItem";
import {ADMIN_GROUPNAME, TEACHERS_GROUPNAME} from "../user/IUser";
import {Users} from "../user/userService";
import {$http} from "../util/ngimport";
import {markAsUsed, to} from "../util/utils";

markAsUsed(focusMe);

const tagParsingSeparator = ",";

/*
 * Tag editing dialog's controller.
 */
export class ShowTagController extends DialogController<{ params: IItem }, {}, "timEditTags"> {
    private static $inject = ["$element", "$scope"];
    private tagName: string = "";
    private tagsList: ITag[] = []; // List of tags the document has.
    private expires?: Moment;
    private errorMessage?: string;
    private selected: ITag | undefined; // Target of editing, if any.
    private successMessage?: string;
    private f!: IFormController; // initialized in the template
    private focusName: boolean = true;
    private allTags?: ITag[]; // List of all unique tags.
    private allUnusedTags?: ITag[]; // List of existing tag names not used in the doc.
    private datePickerOptions: EonasdanBootstrapDatetimepicker.SetOptions;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
        this.datePickerOptions = {
            format: "D.M.YYYY HH:mm:ss",
            defaultDate: "",
            showTodayButton: true,
        };
    }

    /*
     * Show tag list when dialog loads and focus on tag-field.
     */
    async $onInit() {
        super.$onInit();
        await this.updateTags();
        this.focusName = true;
        await this.draggable.makeHeightAutomatic();
    }

    /**
     * Dialog title.
     */
    public getTitle() {
        return "Edit document tags";
    }

    /**
     * Calls tag adding function when Enter is pressed.
     * @param event Keyboard event.
     */
    private async keyPressed(event: KeyboardEvent) {
        if (event.which === 13) {
            await this.addTagClicked();
        }
    }

    /**
     * Gets all unique tags in the database as an ITag list.
     */
    private async getUniqueTags(): Promise<ITag[] | undefined> {
        const [err, response] = await to($http.get<ITag[]>(`/tags/getAllTags`));
        if (err) {
            this.errorMessage = err.data.error;
            this.successMessage = undefined;
            return;
        } else {
            this.errorMessage = undefined;
        }
        if (response) {
            this.allTags = response.data;
            return this.allTags;
        }
    }

    /**
     * Updates the list of existing tags for the document as a string list.
     */
    private async updateTags() {
        const docPath = this.resolve.params.path;
        const [err, response] = await to($http.get<ITag[]>(`/tags/getTags/${docPath}`));

        if (err) {
            this.errorMessage = err.data.error;
            this.successMessage = undefined;
        } else {
            this.errorMessage = undefined;
        }
        if (response) {
            this.tagsList = response.data;
            // Get all globally used tags and remove document's tags from them for tag suggestion list.
            const allTags = await this.getUniqueTags();
            const tagsListStrings = [];
            for (const tag of this.tagsList) {
                tagsListStrings.push(tag.name);
            }
            if (!allTags) {
                return;
            }
            this.allUnusedTags = arrayDifference(allTags, Object.assign([], tagsListStrings));
            return;
        }
    }

    /**
     * Replaces old tag with new tag of the same tag type.
     * @returns {Promise<void>}
     */
    private async editSelectedTag() {
        if (this.selected) {
            if (this.selected.type !== TagType.Regular) {
                if (!userBelongsToTeachersOrIsAdmin) {
                    this.errorMessage = `Editing this tag is only allowed for admins or ${TEACHERS_GROUPNAME} group!`;
                    this.successMessage = undefined;
                    return;
                }
            }
            let newName = this.tagName;
            if (this.selected.type === TagType.CourseCode) {
                newName = newName.trim().toUpperCase();
            }
            const docPath = this.resolve.params.path;
            const newTag = {
                block_id: this.resolve.params.id,
                expires: this.expires,
                name: newName,
                type: this.selected.type,
            };
            const data = {oldTag: this.selected, newTag: newTag};
            const [err, response] = await to($http.post(`/tags/edit/${docPath}`, data));

            if (err) {
                this.errorMessage = err.data.error;
                this.successMessage = undefined;
            } else {
                this.errorMessage = undefined;
            }
            if (response) {
                this.successMessage = `'${this.selected.name}' was edited.`;
                this.selected = undefined;
                await this.updateTags();
                return;
            }
        } else {
            this.successMessage = undefined;
            this.errorMessage = "Click a tag to edit";
        }
    }

    /**
     * Removes the selected tag from the database. Checks admin/teachers rights if
     * tag to remove is not of regular type.
     * @param {ITag} t Tag to delete.
     * @returns {Promise<void>}
     */
    private async removeTag(t: ITag) {
        if (t.type !== TagType.Regular) {
            if (!userBelongsToTeachersOrIsAdmin) {
                this.errorMessage = `Editing this tag is only allowed for admins or ${TEACHERS_GROUPNAME} group!`;
                this.successMessage = undefined;
                return;
            }
        }

        // To avoid complications with editing a non-existent tag.
        if (t === this.selected) {
            this.selected = undefined;
        }

        const docPath = this.resolve.params.path;
        const data = {tagObject: t};
        const [err, response] = await to($http.post(`/tags/remove/${docPath}`, data));

        if (err) {
            this.errorMessage = err.data.error;
            this.successMessage = undefined;
        } else {
            this.errorMessage = undefined;
        }
        if (response) {
            this.successMessage = `'${t.name}' was removed.`;
            await this.updateTags();
            return;
        }
    }

    /**
     * Sends post method with tag data including tag names and expiration dates from the dialog form and
     * saves either success or error message to be used in the dialog.
     * Note that all tags saved with this will be of regular type.
     */
    private async addTagClicked() {
        if (this.f.$invalid) {
            return;
        }
        const docPath = this.resolve.params.path;
        let tagObjects: ITag[];
        tagObjects = [];
        this.tagName.split(tagParsingSeparator).forEach((tag) => {
            if (tag) {
                tagObjects.push(
                    {
                        block_id: this.resolve.params.id,
                        expires: this.expires,
                        name: tag.trim(),
                        type: TagType.Regular,
                    });

            }
        });
        const data = {tags: tagObjects};
        const [err, response] = await to($http.post(`/tags/add/${docPath}`, data));

        if (err) {
            this.errorMessage = err.data.error;
            this.successMessage = undefined;
        } else {
            this.errorMessage = undefined;
        }
        if (response) {
            this.successMessage = `'${this.tagName}' successfully added.`;
            await this.updateTags();
            this.tagName = "";
            this.f.$setPristine();
            this.focusName = true;
            return;
        }
    }

    /**
     * Set tag css style classes depending on the tag type. Currently normal tags are light blue-green
     * special tags green, selected tag red with borders and expired tags fainter colored..
     * @param {ITag} tag The tag.
     * @returns {string} String containing style classes.
     */
    private tagStyleClass(tag: ITag) {
        let opacity = "";
        let highlight = "";
        let color = "btn-success";
        if (tagIsExpired(tag)) {
            opacity = "less-opacity";
        }
        if (tag.type === TagType.Regular) {
            color = "btn-primary";
        }
        if (tag === this.selected) {
            color = "btn-danger";
            highlight = "selected-tag";
        }
        return color + " " + opacity + " " + highlight;
    }

    /**
     * Select a tag or unselect a selected one.
     * @param {ITag} tag Tag to toggle.
     */
    private selectTag(tag: ITag) {
        if (this.selected === tag) {
            this.selected = undefined;
        } else {
            this.selected = tag;
            this.tagName = this.selected.name;
            this.expires = this.selected.expires;
        }
    }
}

/*
 * Array set difference operation a1 \ a2, i.e. filtering any a2 items from a1.
 * For example: a1 = ['a','b','c'] and a2 = ['a','b','d'] returns ['c'].
 *
 * Idea adapted from: https://stackoverflow.com/questions/38498258/typescript-difference-between-two-arrays.
 * @param a1 Array to filter.
 * @param a2 Array containing items to filter from a1.
 * @return Array a1 without any a2 items.
 */
function arrayDifference(a1: any[], a2: any[]) {
    return a1.filter((item) => a2.indexOf(item) < 0);
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
        <p ng-if="$ctrl.tagsList.length > 0">The following tags were found for the document. Select a tag to edit
         or input new tags.</p>
        <p ng-if="$ctrl.tagsList.length === 0">No tags were found for the document.</p>
        <div class="tags-list">
            <span ng-repeat="x in $ctrl.tagsList">
                <span class="btn-xs cursor-pointer" ng-click="$ctrl.selectTag(x)" title="Toggle selected tag"
                ng-class="$ctrl.tagStyleClass(x)">{{x.name}}</span>
                <i ng-if="x.expires" class="glyphicon glyphicon-time"
                uib-tooltip="Expires: {{x.expires | timdate}}"></i>
                <a><span class="glyphicon glyphicon-remove" title="Remove tag" ng-click="$ctrl.removeTag(x)"></span></a>
                <span style="opacity: 0">,</span>
            </span>
        </div>
        <h4 ng-if="!$ctrl.selected">Add new tags</h4>
        <h4 ng-if="$ctrl.selected">Edit '{{$ctrl.selected.name}}'</h4>
        <p>Tag the document by adding words that briefly describe and classify it.</p>
        <form name="$ctrl.f" class="form-horizontal">
            <div class="form-group" tim-error-state title="Write tag names separated by commas">
                <label for="tag-field" class="col-sm-4 control-label">Tag name:</label>
                <div class="col-sm-8">
                    <input required focus-me="$ctrl.focusName" ng-model="$ctrl.tagName" name="tag-field"
                           type="text"
                           ng-keypress="$ctrl.keyPressed($event)" autocomplete="off"
                           class="form-control" id="tag-field" placeholder="Tag names separated by commas"
            uib-typeahead="tag as tag for tag in $ctrl.allUnusedTags | filter:$viewValue | limitTo:15 | orderBy:'name'"
                           typeahead-min-length="1">
                </div>
                <tim-error-message></tim-error-message>
            </div>
            <div class="form-group" title="Add optional expiration date to specify how long the tag is valid">
                <label for="expiration-selector" class="col-sm-4 control-label">Expiration date:</label>
                <div class="col-sm-8">
                    <div class="input-group date" datetimepicker ng-model="$ctrl.expires"
                         data-options="$ctrl.datePickerOptions">
                        <input type="text" class="form-control" id="expiration-selector" name="expiration-selector"
                               placeholder="Leave blank for indefinite period of validity"/>
                        <span class="input-group-addon">
                            <span class="glyphicon glyphicon-calendar"></span>
                        </span>
                    </div>
                </div>
            </div>
        </form>
        <button ng-if="!$ctrl.selected" class="timButton" data-ng-disabled="$ctrl.f.$invalid"
        ng-click="$ctrl.addTagClicked()">Save new tags</button>
        <button ng-if="$ctrl.selected" class="timButton" data-ng-disabled="$ctrl.f.$invalid"
        ng-click="$ctrl.editSelectedTag()">Save changes</button>
        <button class="timButton" data-ng-disabled="!$ctrl.selected" title="Return to adding new tags"
        ng-click="$ctrl.selected = null">Unselect</button>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
        <div ng-show="$ctrl.successMessage" class="alert alert-success">
            <span class="glyphicon glyphicon-ok"></span> {{$ctrl.successMessage}}
        </div>
        <div ng-show="$ctrl.errorMessage" class="alert alert-warning">
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

/**
 * Checks whether user belongs to teachers or admins group.
 * @returns {boolean}
 */
function userBelongsToTeachersOrIsAdmin() {
    if (Users.belongsToGroup(ADMIN_GROUPNAME)) {
        return true;
    }
    if (Users.belongsToGroup(TEACHERS_GROUPNAME)) {
        return true;
    }
    return false;
}
