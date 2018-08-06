/**
 * Controller and HTML template for search results dialog.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {ITag, TagType} from "../item/IItem";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {markAsUsed} from "../util/utils";
import {IDocSearchResult, ITagSearchResult, SearchBoxCtrl} from "./searchBox";

markAsUsed(focusMe);

export interface ISearchResultDisplay {
    result: IDocSearchResult;
    closed: boolean; // Whether this is shown collapsed or not.
    tags: ITag[];
    num_tag_results: number; // Same tag may contain the search word more than once.
}

export class ShowSearchResultController extends DialogController<{ ctrl: SearchBoxCtrl }, {}, "timSearchResults"> {
    private static $inject = ["$element", "$scope"];
    private results: IDocSearchResult[] = [];
    private searchWord: string = "";
    private displayResults: ISearchResultDisplay[] = [];
    private tagResults: ITagSearchResult[] = [];
    private titleResults: IDocSearchResult[] = [];
    private folder: string = "";
    private totalResults: number = 0;
    private limitedDisplay: boolean = false; // If there's large number of results, don't show previews.
    private limitedDisplayThreshold: number = 100000; // More than this could cause memory overload in browser.
    private errorMessage: string | undefined = undefined;
    private orderByOption = "1";
    private allClosed = true;
    private collapsables = false; // True if there are any collapsable results.
    private searchComponent: null | SearchBoxCtrl = null;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    async $onInit() {
        this.updateAttributes(this.resolve.ctrl);
        if (this.searchComponent) {
            this.searchComponent.registerResultsDialog(this);
        }
        super.$onInit();
    }

    $onDestroy() {
        // Unregister the dialog when closing.
        if (this.searchComponent) {
            this.searchComponent.registerResultsDialog(null);
        }
    }

    /**
     * Get all data from the search controller.
     * @param {SearchBoxCtrl} ctrl Controller which calls the search routes.
     */
    public updateAttributes(ctrl: SearchBoxCtrl) {
        this.collapsables = false;
        this.limitedDisplay = false;
        this.searchComponent = ctrl;
        this.results = ctrl.results;
        this.tagResults = ctrl.tagResults;
        this.titleResults = ctrl.titleResults;
        this.totalResults = ctrl.titleMatchCount + ctrl.tagMatchCount + ctrl.wordMatchCount;
        this.folder = ctrl.folder;
        this.errorMessage = ctrl.resultErrorMessage;
        this.searchWord = ctrl.query;
        // If result count is over the treshold, skip paragraph grouping and previews.
        // Without some limit massive results can crash browser.
        if (this.totalResults > this.limitedDisplayThreshold) {
            this.limitedDisplay = true;
        }
        if (!this.limitedDisplay && (ctrl.tagMatchCount > 0 || ctrl.wordMatchCount > 0)) {
            this.collapsables = true;
        }
        this.filterResults();
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Search results";
    }

    /**
     * Merge different types of results as display results with open/close state variable.
     */
    private filterResults() {
        this.displayResults = [];
        for (const r of this.results) {
            const newDisplayResult: ISearchResultDisplay = {
                closed: true,
                num_tag_results: 0,
                result: r,
                tags: [],
                };
            // Add tags to existing word search result objects.
            for (const t of this.tagResults) {
                if (t.doc.id === r.doc.id) {
                    newDisplayResult.tags = t.matching_tags;
                    newDisplayResult.num_tag_results = t.num_results;
                }
            }
            // Add titlemathces to existing word search result objects.
            for (const t of this.titleResults) {
                if (t.doc.id === r.doc.id) {
                    newDisplayResult.result.title_results = t.title_results;
                    newDisplayResult.result.num_title_results = t.num_title_results;
                }
            }
            this.displayResults.push(newDisplayResult);
        }

        for (const t of this.titleResults) {
            try {
                let found = false;
                for (const r of this.displayResults) {
                    if (t.doc.id === r.result.doc.id) {
                        found = true;
                    }
                }
                // Add documents found only with the title to results list.
                if (!found) {
                    const newDocResult = {
                        closed: true,
                        num_tag_results: 0,
                        result: {
                            doc: t.doc,
                            incomplete: false,
                            num_par_results: 0,
                            num_title_results: t.num_title_results,
                            par_results: [],
                            title_results: t.title_results,
                        },
                        tags: [],
                    };
                    this.displayResults.push(newDocResult);
                }
            } catch (e) {
                this.errorMessage = e.getMessage().toString();
            }
        }

        for (const t of this.tagResults) {
            try {
                let found = false;
                for (const r of this.displayResults) {
                    if (t.doc.path === r.result.doc.path) {
                        // r.tags = t.matching_tags;
                        // r.num_tag_results = t.num_results;
                        found = true;
                    }
                }
                // Add documents found only with the tag to results list.
                if (!found) {
                    const newDocResult = {
                        closed: true,
                        num_tag_results: t.num_results,
                        result: {
                            doc: t.doc,
                            incomplete: false,
                            num_par_results: 0,
                            num_title_results: 0,
                            par_results: [],
                            title_results: [],
                        },
                        tags: t.matching_tags,
                    };
                    this.displayResults.push(newDocResult);
                }
            } catch (e) {
                this.errorMessage = e.getMessage().toString();
            }
        }
    }

    /**
     * Changes tag css style depending on whether it's regular or special tag.
     * @param {ITag} tag
     * @returns {string}
     */
    private tagStyle(tag: ITag) {
        let style = "";
        if (tag.type === TagType.Regular) {
            style += "btn-primary";
        } else {
            style += "btn-success";
        }
        return style;
    }

    /**
     * Set all document content views to either collapsed or closed state.
     */
    private toggleCollapseAll() {
        this.allClosed = !this.allClosed;
        for (const r of this.displayResults) {
            r.closed = this.allClosed;
        }
    }

    /**
     * Pick how to order the results. Only information passed as parameters or the item in ng-repeat
     * can be used in the inner functions due to AngularJS' limitations to orderBy functions.
     *
     * @param {number} orderByOption A number corresponding to different order rules.
     * @returns {any} Search result order for AngularJS elements.
     */
    private resultOrder(orderByOption: string) {
        if (orderByOption.toString() === "2") {
            return function(r: ISearchResultDisplay) {
                return r.result.doc.title;
            };
        }
        if (orderByOption.toString() === "3") {
            return function(r: ISearchResultDisplay) {
                let matches =  - (r.result.num_par_results + r.num_tag_results + r.result.num_title_results);
                // Show "x or more matches" before "x matches".
                if (r.result.incomplete) {
                    matches -= 1;
                }
                return matches;
            };
        } else {
            return function(r: ISearchResultDisplay) {
                return r.result.doc.path;
            };
        }
    }
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
    <div ng-if="$ctrl.docResults.length <= 0 && !$ctrl.errorMessage">
        <h5>Your search <i>{{$ctrl.searchWord}}</i> did not match any documents in <i>{{$ctrl.folder}}</i></h5>
    </div>
    <div ng-if="$ctrl.displayResults.length > 0">
        <h5>Your search <i>{{$ctrl.searchWord}}</i> was found {{$ctrl.totalResults}} <ng-pluralize
        count="$ctrl.totalResults" when="{'1': 'time', 'other': 'times'}"></ng-pluralize>
            from {{$ctrl.displayResults.length}} documents in <i ng-if="$ctrl.folder">{{$ctrl.folder}}</i>
            <i ng-if="!$ctrl.folder">root</i>
            <a ng-if="$ctrl.collapsables && !$ctrl.limitedDisplay" title="Toggle results collapse"
                ng-click="$ctrl.toggleCollapseAll()">
                <i class="glyphicon" ng-class="$ctrl.allClosed ? 'glyphicon-plus-sign' : 'glyphicon-minus-sign'"></i>
            </a>
        </h5>
        <ul class="list-unstyled">
            <li ng-repeat="r in $ctrl.displayResults | orderBy:$ctrl.resultOrder($ctrl.orderByOption)">
                <a class="cursor-pointer" ng-click="r.closed = !r.closed"
             ng-if="$ctrl.collapsables && !$ctrl.limitedDisplay && (r.result.num_par_results + r.num_tag_results) > 0">
                    <i class="glyphicon" ng-class="r.closed ? 'glyphicon-plus' : 'glyphicon-minus'"
                    title="Toggle preview" style="width:1.2em;"></i></a>
                <span title="Note: hidden elements can affect the result count">
                <a href="/view/{{r.result.doc.path}}" title="Open {{r.result.doc.title}}"> {{r.result.doc.title}}</a>
                <i>{{r.result.doc.path}}</i>
                 ({{r.result.num_par_results + r.result.num_title_results + r.num_tag_results}} <span
                ng-if="r.result.incomplete">or more matches)</span>
                <ng-pluralize ng-if="!r.result.incomplete"
                count="r.result.num_par_results + r.result.num_title_results + r.num_tag_results"
                when="{'1': 'match)', 'other': 'matches)'}"></ng-pluralize>
                </span>
                <ul ng-if="!r.closed">
                    <li ng-repeat="p in r.result.par_results" ng-if="p.preview">
                        <a href="/view/{{r.result.doc.path}}#{{p.par_id}}" title="Open paragraph">{{p.preview}}</a>
                    </li>
                    <span ng-repeat="tag in r.tags" ng-if="!r.closed">
                        <span class="btn-xs" ng-class="$ctrl.tagStyle(tag)">{{tag.name}}</span>
                    </span>
                </ul>
            </li>
        </ul>
    </div>
    </dialog-body>
    <dialog-footer>
        <div class="float-left" id="order-selector-box">
            <select id="order-selector" ng-model="$ctrl.orderByOption"
                title="Select the result sorting order" name="order-selector">
                <option selected value="1">Sort by path</option>
                <option value="2">Sort by title</option>
                <option value="3">Sort by matches</option>
            </select>
        </div>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showSearchResultDialog(r: SearchBoxCtrl) {
    return await showDialog<ShowSearchResultController>("timSearchResults", {ctrl: () => r}).result;
}
