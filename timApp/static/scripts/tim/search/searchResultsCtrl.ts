/**
 * Controller and HTML template for search results dialog.
 */

import {IScope} from "angular";
import {DialogController} from "tim/ui/dialogController";
import {to} from "tim/util/utils";
import {ITag, TagType} from "../item/IItem";
import {registerDialogComponent, showDialog} from "../ui/dialog";
import {IDocSearchResult, ITagSearchResult, SearchBoxComponent} from "./search-box.component";

export interface ISearchResultDisplay {
    result: IDocSearchResult;
    closed: boolean; // Whether this is shown collapsed or not.
    tags: ITag[];
    num_tag_results: number; // Same tag may contain the search word more than once.
}

export class SearchResultController extends DialogController<{ ctrl: SearchBoxComponent }, void> {
    static component = "timSearchResults";
    static $inject = ["$element", "$scope"] as const;
    private results: IDocSearchResult[] = [];
    private searchWord: string = "";
    private displayResults: ISearchResultDisplay[] = [];
    private tagResults: ITagSearchResult[] = [];
    private titleResults: IDocSearchResult[] = [];
    private pathResults: IDocSearchResult[] = [];
    private folder: string = "";
    private totalResults: number = 0;
    private limitedDisplay: boolean = false; // If there's large number of results, don't show previews.
    private limitedDisplayThreshold: number = 100000; // More than this could cause memory overload in browser.
    private errorMessage: string | undefined;
    private allClosed = true;
    private collapsables = false; // True if there are any collapsable results.
    private searchComponent: undefined | SearchBoxComponent;
    private sortingOptions = [
            {value: "1", name: "Sort by relevance"},
            {value: "2", name: "Sort by title"},
            {value: "3", name: "Sort by matches"},
            {value: "4", name: "Sort by path"}];

    private orderByOption = this.sortingOptions[0];

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);
    }

    $onInit() {
        super.$onInit();
        this.updateAttributes(this.resolve.ctrl);
        if (this.searchComponent) {
            this.searchComponent.registerResultsDialog(this);
        }
    }

    $onDestroy() {
        // Unregister the dialog when closing.
        if (this.searchComponent) {
            this.searchComponent.registerResultsDialog(undefined);
        }
    }

    /**
     * Get all data from the search controller.
     * @param {SearchBoxComponent} ctrl Controller which calls the search routes.
     */
    public updateAttributes(ctrl: SearchBoxComponent) {
        this.collapsables = false;
        this.limitedDisplay = false;
        this.searchComponent = ctrl;
        this.results = ctrl.results;
        this.tagResults = ctrl.tagResults;
        this.titleResults = ctrl.titleResults;
        this.pathResults = ctrl.pathResults;
        this.totalResults = ctrl.titleMatchCount + ctrl.tagMatchCount + ctrl.wordMatchCount + ctrl.pathMatchCount;
        this.folder = ctrl.folder!;
        this.errorMessage = ctrl.resultErrorMessage;
        this.searchWord = ctrl.query;
        // If result count is over the threshold, skip paragraph grouping and previews.
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
        // Combine title-path results.
        const titleAndPathResults: IDocSearchResult[] = [];
        for (const p of this.pathResults) {
            let pathResultsFound = false;
            for (const t of this.titleResults) {
                // If path and title result are from same doc, combine.
                if (t.doc.id === p.doc.id) {
                    const temp = t;
                    temp.num_title_results = t.num_title_results + p.num_title_results;
                    titleAndPathResults.push(temp);
                    pathResultsFound = true;
                    break;
                }
            }
            // If path result didn't overlap with any title result, add.
            if (!pathResultsFound) {
                titleAndPathResults.push(p);
            }
        }
        // Add missing title results to combined title-path results.
        for (const t of this.titleResults) {
            let titleResultsFound = false;
            for (const tp of titleAndPathResults) {
                // If title result is already in combined list, go to next one.
                if (t.doc.id === tp.doc.id) {
                    titleResultsFound = true;
                    break;
                }
            }
            // If title result didn't overlap with any combined title and path result, add.
            if (!titleResultsFound) {
                titleAndPathResults.push(t);
            }
        }

        // Combine title and tag results with existing content results.
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
            // Add titlematches to existing word search result objects.
            for (const t of titleAndPathResults) {
                if (t.doc.id === r.doc.id) {
                    newDisplayResult.result.title_results.push(...t.title_results);
                    newDisplayResult.result.num_title_results = t.num_title_results;
                }
            }
            this.displayResults.push(newDisplayResult);
        }

        for (const t of titleAndPathResults) {
            let found = false;
            for (const r of this.displayResults) {
                if (t.doc.id === r.result.doc.id) {
                    found = true;
                }
            }
            // Add documents found only with the title and/or path to results list.
            if (!found) {
                const newDocResult = {
                    closed: true,
                    num_path_results: 0,
                    num_tag_results: 0,
                    path_results: [],
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
        }
        for (const t of this.tagResults) {
            let found = false;
            for (const r of this.displayResults) {
                if (t.doc.path === r.result.doc.path) {
                    // Add tag results to existing content-title-path results.
                    r.tags = t.matching_tags;
                    r.num_tag_results = t.num_results;
                    found = true;
                }
            }
            // Add documents found only with the tag to results list.
            if (!found) {
                const newDocResult = {
                    closed: true,
                    num_path_results: 0,
                    num_tag_results: t.num_results,
                    path_results: [],
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
        }
    }

    /**
     * Changes tag css class depending on whether it's regular or special tag.
     * @param {ITag} tag The tag to display.
     * @returns {string} The class as a string.
     */
    private tagClass(tag: ITag) {
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
     * Pick how to order the results.
     *
     * @param {number} orderByOption A number corresponding to different order rules.
     * @returns {any} Search result order for AngularJS elements.
     */
    private resultOrder(orderByOption: {value: string, name: string}) {
        if (orderByOption.value === "2") {
            return (r: ISearchResultDisplay) =>  r.result.doc.title;
        }
        if (orderByOption.value === "3") {
            return (r: ISearchResultDisplay) =>  {
                // Negative values to get ascending order.
                let matches =  - (r.result.num_par_results + r.num_tag_results + r.result.num_title_results);
                // Show "x or more matches" before "x matches".
                if (r.result.incomplete) {
                    matches -= 1;
                }
                return matches;
            };
        }
        if (orderByOption.value === "1") {
            return (r: ISearchResultDisplay) =>  {
                let matches =  - (r.result.num_par_results + r.num_tag_results + r.result.num_title_results);
                // Show "x or more matches" before "x matches".
                if (r.result.incomplete) {
                    matches -= 1;
                }
                const relevance = r.result.doc.relevance;
                let relevanceValue = 10; // TODO: Set a global constant for default relevance.
                if (relevance) {
                    relevanceValue = relevance.relevance;
                }
                return matches - relevanceValue;
            };
        } else {
            return (r: ISearchResultDisplay) =>  r.result.doc.path;
        }
    }
}

registerDialogComponent(SearchResultController,
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
                    title="Toggle preview" style="width:1.3em;"></i></a>
                <span title="Note: hidden elements can affect the result count">
                <a href="/view/{{r.result.doc.path}}"
                title="Open {{r.result.doc.title}}"> {{r.result.doc.title}}</a> {{r.result.doc.path}}
                <i> ({{r.result.num_par_results + r.result.num_title_results + r.num_tag_results}} <span
                ng-if="r.result.incomplete">or more matches)</span>
                <ng-pluralize ng-if="!r.result.incomplete"
                count="r.result.num_par_results + r.result.num_title_results + r.num_tag_results"
                when="{'1': 'match)', 'other': 'matches)'}"></ng-pluralize></i>
                </span>
                <ul ng-if="!r.closed">
                    <li ng-repeat="p in r.result.par_results" ng-if="p.preview">
                        <a href="/view/{{r.result.doc.path}}#{{p.par_id}}" title="Open paragraph">{{p.preview}}</a>
                    </li>
                    <span ng-repeat="tag in r.tags" ng-if="!r.closed">
                        <span class="btn-xs" ng-class="$ctrl.tagClass(tag)">{{tag.name}}</span>
                    </span>
                </ul>
            </li>
        </ul>
    </div>
    </dialog-body>
    <dialog-footer>
        <div class="pull-left" id="order-selector-box">
            <select id="order-selector" ng-model="$ctrl.orderByOption"
            ng-options="s as s.name for s in $ctrl.sortingOptions track by s.value"
                title="Select the result sorting order" name="order-selector">
            </select>
        </div>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export function showSearchResultDialog(r: SearchBoxComponent) {
    void to(showDialog(
        SearchResultController,
        {ctrl: () => r},
        {showMinimizeButton: false}).result);
}
