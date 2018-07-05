/**
 * Controller and HTML template for tag search dialog.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {markAsUsed} from "../util/utils";
import {IItem} from "./IItem";
import {ngStorage} from "ngstorage";
import {$localStorage} from "../util/ngimport";

markAsUsed(focusMe);

/*
 * Tag search dialog's controller.
 */
export class ShowTagSearchController extends DialogController<{ params: IItem }, {}, "timSearchTags"> {
    private static $inject = ["$element", "$scope"];
    private enableSearch = true;
    private header = "";
    private advancedOptions = false;
    private caseSensitive = false;
    private listDocTags = true;
    private exactMatch = false;
    private storage: ngStorage.StorageService & {searchOptionStorage: null | boolean[]};

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
        this.storage = $localStorage.$default({
            searchOptionStorage: null,
        });
    }

    /*
     * Show tag list when dialog loads and focus on tag-field.
     */
    async $onInit() {
        super.$onInit();
        if (this.enableSearch) {
            this.header = "Input a tag name to search documents";
            if (this.storage.searchOptionStorage && this.storage.searchOptionStorage.length === 3) {
                this.caseSensitive = this.storage.searchOptionStorage[0];
                this.exactMatch = this.storage.searchOptionStorage[1];
                this.listDocTags = this.storage.searchOptionStorage[2];
        }
        }
    }

    $onDestroy() {
        this.storage.searchOptionStorage = [];
        this.storage.searchOptionStorage.push(this.caseSensitive);
        this.storage.searchOptionStorage.push(this.exactMatch);
        this.storage.searchOptionStorage.push(this.listDocTags);
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
            `<tim-dialog>
    <dialog-header>
    </dialog-header>
    <dialog-body>
        <h4>{{$ctrl.header}}</h4>
        <tagged-document-list enable-search="$ctrl.enableSearch" tag-filter=""
        exact-match="$ctrl.exactMatch" list-doc-tags="$ctrl.listDocTags"
        case-sensitive="$ctrl.caseSensitive"></tagged-document-list>
    <div>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.advancedOptions"
        class="ng-pristine ng-untouched ng-valid ng-not-empty"> Advanced options</label>
    </div>
    <div ng-if="$ctrl.advancedOptions">
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.exactMatch"
        class="ng-pristine ng-untouched ng-valid ng-not-empty"> Search exact words</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.caseSensitive"
        class="ng-pristine ng-untouched ng-valid ng-not-empty"> Case sensitive</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.listDocTags"
        class="ng-pristine ng-untouched ng-valid ng-not-empty"
        title="List all document tags in search results"> List all tags</label>
    </div>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showTagSearchDialog(d: IItem) {
    return await showDialog<ShowTagSearchController>("timSearchTags", {params: () => d}).result;
}
