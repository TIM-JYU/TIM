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
    private searchWord: string = "";

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    /*
     * Show tag list when dialog loads and focus on tag-field.
     */
    async $onInit() {
        this.results = this.resolve.params.results;
        this.searchWord = this.resolve.params.searchWord;
        super.$onInit();
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Displaying search results";
    }
}

registerDialogComponent("timSearchResults",
    ShowSearchResultController,
    {
        template:
            `<tim-dialog style="z-index:3;">
    <dialog-header>
    </dialog-header>
    <dialog-body>
    <div ng-show="$ctrl.errorMessage" class="alert alert-warning">
        <span class="glyphicon glyphicon-exclamation-sign"></span> {{$ctrl.errorMessage}}
    </div>
    <div ng-if="!$ctrl.beginning && $ctrl.results.length <= 0 && !$ctrl.errorMessage">
        <h5>Your search <i>{{$ctrl.searchWord}}</i> did not match any documents</h5>
    </div>
    <div ng-if="$ctrl.results.length > 0">
        <h5>Your search <i>{{$ctrl.searchWord}}</i> was found in {{$ctrl.results.length}} instances</h5>
        <ul>
            <li ng-repeat="r in $ctrl.results">
                <a href="/view/{{r.doc.path}}" title="Open {{r.doc.title}}">{{r.doc.path}}</a>
                 paragraph
                <a href="/view/{{r.doc.path}}#{{r.par.id}}" title="Open paragraph">{{r.par.id}}</a>
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
