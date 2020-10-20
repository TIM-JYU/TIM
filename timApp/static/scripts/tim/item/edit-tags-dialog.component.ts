import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule, ViewChild} from "@angular/core";
import moment from "moment";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {FormsModule, NgForm} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {TypeaheadModule} from "ngx-bootstrap/typeahead";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {TEACHERS_GROUPNAME} from "../user/IUser";
import {userBelongsToGroupOrIsAdmin} from "../user/userService";
import {to2} from "../util/utils";
import {DatetimePickerModule} from "../ui/datetime-picker/datetime-picker.component";
import {IItem, ITag, tagStyleClass, TagType} from "./IItem";

const tagParsingSeparator = ",";

/*
 * Tag editing dialog.
 */
@Component({
    selector: "tim-edit-tags-dialog",
    template: `
        <tim-dialog-frame class="overflow-visible">
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <h4>Document tags</h4>
                <p *ngIf="tagsList.length > 0">The following tags were found for the document. Select a tag to edit
                    or input new tags.</p>
                <p *ngIf="tagsList.length === 0">No tags were found for the document.</p>
                <div class="tags-list">
            <span *ngFor="let x of tagsList">
                <span class="btn-xs cursor-pointer" (click)="selectTag(x)" title="Toggle selected tag"
                      [ngClass]="tagClass(x)">{{x.name}}</span>
                <i *ngIf="x.expires" class="glyphicon glyphicon-time"
                   tooltip="Expires: {{x.expires | timdate}}"></i>
                <a><span class="glyphicon glyphicon-remove" title="Remove tag" (click)="removeTag(x)"></span></a>
                <span style="opacity: 0">,</span>
            </span>
                </div>
                <h4 *ngIf="!selected">Add new tags</h4>
                <h4 *ngIf="selected">Edit '{{selected.name}}'</h4>
                <p>Tag the document by adding words that briefly describe and classify it.</p>
                <form #f="ngForm" class="form-horizontal">
                    <div class="form-group" timErrorState title="Write tag names separated by commas">
                        <label for="tag-field" class="col-sm-4 control-label">Tag name:</label>
                        <div class="col-sm-8">
                            <input required
                                   focusMe
                                   [(ngModel)]="tagName"
                                   name="tag-field"
                                   type="text"
                                   (keydown.enter)="enterPressed()"
                                   autocomplete="off"
                                   class="form-control"
                                   id="tag-field"
                                   placeholder="Tag names separated by commas"
                                   [typeahead]="allUnusedTags"
                                   [typeaheadMinLength]="1">
                        </div>
                        <tim-error-message></tim-error-message>
                    </div>
                    <div class="form-group" title="Add optional expiration date to specify how long the tag is valid">
                        <label for="expiration-selector" class="col-sm-4 control-label">Expiration date:</label>
                        <div class="col-sm-8">
                            <tim-datetime-picker [(time)]="expires"
                                                 placeholder="Leave blank for indefinite period of validity">
                            </tim-datetime-picker>
                        </div>
                    </div>
                </form>
                <button *ngIf="!selected" class="timButton" [disabled]="f.invalid"
                        (click)="addTagClicked()">Save new tags
                </button>
                <button *ngIf="selected" class="timButton" [disabled]="f.invalid"
                        (click)="editSelectedTag()">Save changes
                </button>
                <button class="timButton" [disabled]="!selected" title="Return to adding new tags"
                        (click)="selected = undefined">Unselect
                </button>
                <button class="timButton" (click)="dismiss()">Close</button>
                <div *ngIf="successMessage" class="alert alert-success">
                    <span class="glyphicon glyphicon-ok"></span> {{successMessage}}
                </div>
                <div *ngIf="errorMessage" class="alert alert-warning">
                    <span class="glyphicon glyphicon-exclamation-sign"></span> {{errorMessage}}
                </div>
            </ng-container>
            <ng-container footer>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class EditTagsDialogComponent extends AngularDialogComponent<
    IItem,
    void
> {
    protected dialogName = "EditTags";
    tagName: string = "";
    tagsList: ITag[] = []; // List of tags the document has.
    expires?: Date;
    errorMessage?: string;
    selected: ITag | undefined; // Target of editing, if any.
    successMessage?: string;
    @ViewChild("f")
    f!: NgForm;
    focusName: boolean = true;
    private allTags?: ITag[]; // List of all unique tags.
    allUnusedTags: string[] = []; // List of existing tag names not used in the doc.

    constructor(private http: HttpClient) {
        super();
    }

    /*
     * Show tag list when dialog loads and focus on tag-field.
     */
    ngOnInit() {
        (async () => {
            await this.updateTags();
            this.focusName = true;
        })();
    }

    getTitle() {
        return "Edit document tags";
    }

    async enterPressed() {
        await this.addTagClicked();
    }

    /**
     * Gets all unique tags in the database as an ITag list.
     */
    private async getUniqueTags(): Promise<ITag[] | undefined> {
        const r = await to2(
            this.http.get<ITag[]>(`/tags/getAllTags`).toPromise()
        );
        if (!r.ok) {
            this.errorMessage = r.result.error.error;
            this.successMessage = undefined;
            return;
        } else {
            this.errorMessage = undefined;
            this.allTags = r.result;
            return this.allTags;
        }
    }

    /**
     * Updates the list of existing tags for the document as a string list.
     */
    private async updateTags() {
        const docPath = this.data.path;
        const r = await to2(
            this.http.get<ITag[]>(`/tags/getTags/${docPath}`).toPromise()
        );

        if (!r.ok) {
            this.errorMessage = r.result.error.error;
            this.successMessage = undefined;
        } else {
            this.errorMessage = undefined;
            this.tagsList = r.result;
            // Get all globally used tags and remove document's tags from them for tag suggestion list.
            const allTags = await this.getUniqueTags();
            const tagsListStrings = [];
            for (const tag of this.tagsList) {
                tagsListStrings.push(tag.name);
            }
            if (!allTags) {
                return;
            }
            const unused = new Set(allTags.map((t) => t.name));
            tagsListStrings.forEach((used) => unused.delete(used));
            this.allUnusedTags = [...unused];
        }
    }

    /**
     * Replaces old tag with new tag of the same tag type.
     */
    async editSelectedTag() {
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
            const docPath = this.data.path;
            const newTag = {
                block_id: this.data.id,
                expires: this.expires,
                name: newName,
                type: this.selected.type,
            };
            const data = {oldTag: this.selected, newTag: newTag};
            const r = await to2(
                this.http.post(`/tags/edit/${docPath}`, data).toPromise()
            );

            if (!r.ok) {
                this.errorMessage = r.result.error.error;
                this.successMessage = undefined;
            } else {
                this.errorMessage = undefined;
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
     */
    async removeTag(t: ITag) {
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

        const docPath = this.data.path;
        const data = {tagObject: t};
        const r = await to2(
            this.http.post(`/tags/remove/${docPath}`, data).toPromise()
        );

        if (!r.ok) {
            this.errorMessage = r.result.error.error;
            this.successMessage = undefined;
        } else {
            this.errorMessage = undefined;
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
    async addTagClicked() {
        if (this.f.invalid) {
            return;
        }
        const docPath = this.data.path;
        const tagObjects: ITag[] = [];
        this.tagName.split(tagParsingSeparator).forEach((tag) => {
            if (tag) {
                tagObjects.push({
                    block_id: this.data.id,
                    expires: this.expires ? moment(this.expires) : undefined,
                    name: tag.trim(),
                    type: TagType.Regular,
                });
            }
        });
        const data = {tags: tagObjects};
        const r = await to2(
            this.http.post(`/tags/add/${docPath}`, data).toPromise()
        );

        if (!r.ok) {
            this.errorMessage = r.result.error.error;
            this.successMessage = undefined;
        } else {
            this.errorMessage = undefined;
            this.successMessage = `'${this.tagName}' successfully added.`;
            await this.updateTags();
            this.tagName = "";
            this.f.resetForm();
            this.focusName = true;
            return;
        }
    }

    /**
     * Select a tag or unselect a selected one.
     * @param {ITag} tag Tag to toggle.
     */
    selectTag(tag: ITag) {
        if (this.selected === tag) {
            this.selected = undefined;
        } else {
            this.selected = tag;
            this.tagName = this.selected.name;
            this.expires = this.selected.expires?.toDate();
        }
    }

    /**
     * Pick tag style classes.
     * @param {ITag} tag The tag.
     * @returns {string} Classes as string separated by spaces.
     */
    tagClass(tag: ITag) {
        let isSelected = false;
        if (this.selected === tag) {
            isSelected = true;
        }
        return tagStyleClass(tag, isSelected);
    }
}

@NgModule({
    declarations: [EditTagsDialogComponent],
    imports: [
        BrowserModule,
        FormsModule,
        HttpClientModule,
        TimUtilityModule,
        DialogModule,
        TypeaheadModule.forRoot(),
        TooltipModule.forRoot(),
        DatetimePickerModule,
    ],
})
export class EditTagsDialogModule {}

/**
 * Checks whether user belongs to teachers or admins group.
 * @returns {boolean}
 */
function userBelongsToTeachersOrIsAdmin() {
    return userBelongsToGroupOrIsAdmin(TEACHERS_GROUPNAME);
}
