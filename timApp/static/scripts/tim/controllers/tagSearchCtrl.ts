/**
 * Controller and HTML template for tag search dialog.
 */

import {IFormController, IRootElementService, IScope} from "angular";
import {Moment} from "moment";
import * as focusMe from "tim/directives/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {IItem, ITag} from "../IItem";
import {markAsUsed} from "../utils";
import * as tagLabel from "../components/tagLabel";

markAsUsed(tagLabel);
markAsUsed(focusMe);

const tagParsingSeparator = " ";

/*
 * Tag search dialog's controller.
 */
export class ShowTagSearchController extends DialogController<{ params: IItem }, {}, "timSearchTags"> {
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
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Tag search";
    }

}

registerDialogComponent("timSearchTags",
    ShowTagSearchController,
    {
        template:
            `<tim-dialog class="overflow-visible">
    <dialog-header>
    </dialog-header>
    <dialog-body>
        <h4>Input a tag name to search documents</h4>
        <tagged-document-list enable-search="true" tag-filter="" exact-match="false"
                              list-doc-tags="true"></tagged-document-list>
        <button class="btn timButton" ng-click="$ctrl.dismiss()"><span>Close</span></button>
    </dialog-body>
    <dialog-footer>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showTagSearchDialog(d: IItem) {
    return await showDialog<ShowTagSearchController>("timSearchTags", {params: () => d}).result;
}
