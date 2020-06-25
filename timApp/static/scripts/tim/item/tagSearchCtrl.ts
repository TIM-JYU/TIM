/**
 * Controller and HTML template for tag search dialog.
 */

import {IScope} from "angular";
import {ngStorage} from "ngstorage";
import * as focusMe from "tim/ui/focusMe";
import {DialogController} from "tim/ui/dialogController";
import {registerDialogComponent, showDialog} from "../ui/dialog";
import {$localStorage} from "../util/ngimport";
import {markAsUsed} from "../util/utils";

markAsUsed(focusMe);

/*
 * Tag search dialog's controller.
 */
export class TagSearchController extends DialogController<void, void> {
    static component = "timSearchTags";
    static $inject = ["$element", "$scope"] as const;
    private enableSearch = true;
    private header = "";
    private advancedOptions = false;
    private caseSensitive = false;
    private listDocTags = true;
    private exactMatch = false;
    private storage: ngStorage.StorageService & {searchOptionStorage: null | boolean[]};

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);
        this.storage = $localStorage.$default({
            searchOptionStorage: null,
        });
    }

    /*
     * Show tag list when dialog loads and focus on tag-field.
     */
    $onInit() {
        super.$onInit();
        if (this.enableSearch) {
            this.header = "Input a tag name to search documents";
            if (this.storage.searchOptionStorage && this.storage.searchOptionStorage.length === 3) {
                [this.caseSensitive, this.exactMatch, this.listDocTags] = this.storage.searchOptionStorage;
        }
        }
    }

    $onDestroy() {
        this.storage.searchOptionStorage = [this.caseSensitive, this.exactMatch, this.listDocTags];
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Tag search";
    }

}

registerDialogComponent(TagSearchController,
    {
        template:
            `<tim-dialog>
    <dialog-header>
    </dialog-header>
    <dialog-body>
        <h4>{{$ctrl.header}}</h4>
        <tagged-document-list enable-search="$ctrl.enableSearch" tag-filter=""
        exact-match="$ctrl.exactMatch" list-doc-tags="$ctrl.listDocTags"
        case-sensitive="$ctrl.caseSensitive"></tagged-document-list>
    <div>
        <label class="font-weight-normal"><input type="checkbox"
        ng-model="$ctrl.advancedOptions"> Advanced options</label>
    </div>
    <div ng-if="$ctrl.advancedOptions">
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.exactMatch"> Search whole words</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.caseSensitive"> Case sensitive</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.listDocTags"
        title="List all document tags in search results"> List all tags</label>
    </div>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showTagSearchDialog() {
    return await showDialog(TagSearchController, {}).result;
}
