/**
 * Controller and HTML template for tag search dialog.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {markAsUsed} from "../util/utils";
import {ISearchResult} from "./searchBox";

markAsUsed(focusMe);

export interface ISearchResultParams {
    results: ISearchResult[];
    searchWord: string;
    errorMessage: string;
}

/*
 * Tag search dialog's controller.
 */
export class ShowSearchResultController extends DialogController<{ params: ISearchResultParams }, {}, "timSearchResults"> {
    private static $inject = ["$element", "$scope"];
    private results: ISearchResult[] = [];
    private filteredResults: ISearchResult[] = [];
    private searchWord: string = "";

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    /*
     * Show tag list when dialog loads and focus on tag-field.
     */
    async $onInit() {
        this.results = this.resolve.params.results;
        this.filterResults();
        this.searchWord = this.resolve.params.searchWord;
        super.$onInit();
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Search results";
    }

    /**
     * Filters duplicate paragraphs (those with more than one match) from the results.
     */
    private filterResults() {
        const docs: string[] = [];
        for (const {item, index} of this.results.map((item, index) => ({ item, index }))) {
            if (item && (index === 0 || item.par.id !== this.results[index - 1].par.id)) {
                this.filteredResults.push(item);
                docs.push(item.doc.path);
            }
        }

        this.docCount = removeDuplicates(docs).length;
    }

    /**
     * Forms a preview of the paragraph around indices the match was found.
     * @param {ISearchResult} r
     * @returns {string}
     */
    private previewPar(r: ISearchResult) {
        const text = r.par.md;
        let start = r.match_start_index - 30;
        let end = r.match_end_index + 30;
        let prefix = "...";
        let postfix = "...";
        if (start < 0) {
            start = 0;
            prefix = "";
        }

        if (end > text.length) {
            end = text.length;
            postfix = "";
        }
        return prefix + text.substring(start, end).trim() + postfix;
    }
}

/**
 * Returns copy of a string array where duplicates have been removed.
 * Adapted from: https://codehandbook.org/how-to-remove-duplicates-from-javascript-array/
 * @param {string[]} array
 * @returns {string[]}
 */
function removeDuplicates(array: string[]) {
    const set: string[] = [];
    for (const item of array) {
        if (set.indexOf(item) === -1) {
            set.push(item);
        }
    }
    return set;
}

registerDialogComponent("timSearchResults",
    ShowSearchResultController,
    {
        template:
            `<tim-dialog class="search-result-dialog">
    <dialog-header>
    </dialog-header>
    <dialog-body>
    <div ng-show="$ctrl.errorMessage" class="alert alert-warning">
        <span class="glyphicon glyphicon-exclamation-sign"></span> {{$ctrl.errorMessage}}
    </div>
    <div ng-if="!$ctrl.beginning && $ctrl.filteredResults.length <= 0 && !$ctrl.errorMessage">
        <h5>Your search <i>{{$ctrl.searchWord}}</i> did not match any documents</h5>
    </div>
    <div ng-if="$ctrl.filteredResults.length > 0">
        <h5>Your search <i>{{$ctrl.searchWord}}</i> was found {{$ctrl.results.length}}
        <ng-pluralize count="$ctrl.filteredResults.length"
                 when="{'1': 'time',
                     'other': 'times'}"></ng-pluralize> in {{$ctrl.docCount}}
        <ng-pluralize count="$ctrl.filteredResults.length"
                 when="{'1': 'document',
                     'other': 'documents'}">
        </ng-pluralize></h5>
        <ul>
            <li ng-repeat="r in $ctrl.filteredResults">
                <a href="/view/{{r.doc.path}}#{{r.par.id}}" title="Open {{r.doc.title}}">{{r.doc.title}}</a>
                <i>({{r.doc.path}})</i>
                <p>{{$ctrl.previewPar(r)}}</p>
            </li>
        </ul>
    </div>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showSearchResultDialog(r: ISearchResultParams) {
    return await showDialog<ShowSearchResultController>("timSearchResults", {params: () => r}).result;
}
