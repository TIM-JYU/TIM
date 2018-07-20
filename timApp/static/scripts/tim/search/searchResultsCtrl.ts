/**
 * Controller and HTML template for search results dialog.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {markAsUsed} from "../util/utils";
import {ISearchResult, ITagSearchResult} from "./searchBox";
import {IItem, ITag, TagType} from "../item/IItem";
import {IExtraData} from "../document/editing/edittypes";

markAsUsed(focusMe);

export interface ISearchResultParams {
    results: ISearchResult[];
    searchWord: string;
    errorMessage: string;
    tagResults: ITagSearchResult[];
    wordMatchCount: number;
    tagMatchCount: number;
    titleMatchCount: number;
    folder: string;
}

export interface ISearchResultParamsDoc {
    doc: IItem;
    in_title: boolean;
    pars: ISearchResultParamsPar[];
    closed: boolean; // Whether this is shown collapsed or not.
    tags: ITag[];
}

export interface ISearchResultParamsPar {
    par: IExtraData;
    preview: string; // Short snippet from the paragraph.
    match_start_index: number;
    match_end_index: number; // Regex searches may make match very long, so this isn't always reliable.
}

export class ShowSearchResultController extends DialogController<{ params: ISearchResultParams }, {}, "timSearchResults"> {
    private static $inject = ["$element", "$scope"];
    private results: ISearchResult[] = [];
    private filteredResults: ISearchResult[] = [];
    private searchWord: string = "";
    private docResults: ISearchResultParamsDoc[] = [];
    private tagResults: ITagSearchResult[] = [];
    private folder: string = "";
    private totalResults: number = 0;
    private limitedDisplay: boolean = false; // If there's large number of results, optimize shown results.
    private limitedDisplayTreshold: number = 1500;
    private errorMessage: string = "";
    private orderByOption = "1";

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    async $onInit() {
        this.results = this.resolve.params.results;
        this.tagResults = this.resolve.params.tagResults;
        this.totalResults = this.resolve.params.titleMatchCount +
            this.resolve.params.tagMatchCount +
            this.resolve.params.wordMatchCount;
        this.folder = this.resolve.params.folder;
        this.errorMessage = this.resolve.params.errorMessage;
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
     * Filters duplicate paragraph and title matches (i.e. those with more than one match) from the results
     * and groups paragraphs and tags under documents.
     */
    private filterResults() {
        // If result count is over the treshold, skip paragraph grouping and previews.
        if (this.totalResults > this.limitedDisplayTreshold) {
            this.limitedDisplay = true;
        }
        for (const {item, index} of this.results.map((item, index) => ({item, index}))) {
            // Remove matches from the same title.
            try {
                if (item && item.in_title) {
                     if (!this.results[index - 1]) {
                        this.filteredResults.push(item);
                     } else {
                         if (this.results[index - 1].doc.path === item.doc.path) {
                             //
                         } else {
                             this.filteredResults.push(item);
                         }
                     }
                }
                // Remove matches from the same paragraph.
                if (item && !item.in_title) {
                    if (!this.results[index - 1] || !this.results[index - 1].par) {
                        this.filteredResults.push(item);
                    } else {
                        if (index === 0 || item.par.id !== this.results[index - 1].par.id) {
                            this.filteredResults.push(item);
                        }
                    }
                }
            } catch (e) {
                this.errorMessage = e.getMessage().toString();
            }
        }
        // Group paragraphs under documents.
        for (const r of this.filteredResults) {
            try {
                const docIndex = this.docIndexInResults(r.doc, this.docResults);
                if (!this.limitedDisplay) {
                    if (r.par) {
                        const newParResult = {
                            match_end_index: r.match_end_index,
                            match_start_index: r.match_start_index,
                            par: r.par,
                            preview: this.previewPar(r, 80),
                        };
                        if (docIndex >= 0) {
                            this.docResults[docIndex].pars.push(newParResult);
                        } else {
                            const newDocResult = {
                                closed: true,
                                doc: r.doc,
                                in_title: r.in_title,
                                pars: [newParResult],
                                tags: [],
                            };
                            this.docResults.push(newDocResult);
                        }
                    } else {
                        const newDocResult = {
                            closed: true,
                            doc: r.doc,
                            in_title: r.in_title,
                            pars: [],
                            tags: [],
                        };
                        this.docResults.push(newDocResult);
                    }
                } else {
                    // If there's lots of results, don't add pars.
                    if (docIndex < 0) {
                        const newDocResult = {
                            closed: true,
                            doc: r.doc,
                            in_title: r.in_title,
                            pars: [],
                            tags: [],
                        };
                        this.docResults.push(newDocResult);
                    }
                }
            } catch (e) {
                this.errorMessage = e.getMessage().toString();
            }
        }
        // Tag results use different interface and need to be handled separately.
        for (const tagResult of this.tagResults) {
            try {
                let found = false;
                for (const docResult of this.docResults) {
                    // Add tags to corresponding document's tags-list.
                    if (tagResult.doc.path === docResult.doc.path) {
                        docResult.tags = tagResult.matching_tags;
                        found = true;
                    }
                }
                // Add documents found only with the tag to results list.
                if (!found) {
                    const newDocResult = {
                        closed: true,
                        doc: tagResult.doc,
                        in_title: false,
                        pars: [],
                        tags: tagResult.matching_tags,
                    };
                    this.docResults.push(newDocResult);
                }
            } catch (e) {
                this.errorMessage = e.getMessage().toString();
            }
        }
    }

    /**
     * Forms a preview of the paragraph around indices the match was found.
     * @param {ISearchResult} r
     * @param {number} snippetLength Maximum number of chars around the search word.
     * @returns {string}
     */
    private previewPar(r: ISearchResult, snippetLength: number) {
        if (r.in_title) {
            return "";
        }
        // TODO: Use correct interface for paragraphs.
        const text = r.par.md;
        let start = r.match_start_index - snippetLength / 2;

        // Regex searches may have a very long span between begin and end indices,
        // so search word length is used to avoid that.
        let end = r.match_start_index + this.searchWord.length + snippetLength / 2;
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

    /**
     * Gets index of document in document results.
     * If documents isn't yet in the list return -1.
     * @param {IItem} doc The document.
     * @param {ISearchResultParamsDoc} docs Search results as a document list.
     * @returns {any} The index of first instance of doc in docs or -1.
     */
    private docIndexInResults(doc: IItem, docs: ISearchResultParamsDoc[]) {
        for (const {item, index} of docs.map((item, index) => ({ item, index }))) {
            if (item.doc.id === doc.id) {
                return index;
            }
        }
        return -1;
    }

    /**
     * Changes tag css style depending on whether search is enabled and
     * if it's regular or special tag.
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
     * Pick how to order the results. All information needs to be passed as parameters and processed
     * in inner functions due to AngularJS' limitations in orderBy functions.
     *
     * @param {number} orderByOption A number corresponding to different order rules.
     * @returns {any} Search result order for AngularJS elements.
     */
    private resultOrder(orderByOption: string) {
        if (orderByOption.toString() === "2") {
            return function(r: ISearchResultParamsDoc) {
                return r.doc.title;
            };
        }
        if (orderByOption.toString() === "3") {
            return function(r: ISearchResultParamsDoc) {
                let titleMatch = 0;
                if (r.in_title) {
                    titleMatch = 1;
                }
                return - (r.pars.length + r.tags.length + titleMatch);
            };
        } else {
            return function(r: ISearchResultParamsDoc) {
                return r.doc.path;
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
    <div ng-show="$ctrl.resolve.params.errorMessage" class="alert alert-warning">
        <span class="glyphicon glyphicon-exclamation-sign"></span> {{$ctrl.errorMessage}}
    </div>
    <div ng-if="$ctrl.docResults.length <= 0 && !$ctrl.errorMessage">
        <h5>Your search <i>{{$ctrl.searchWord}}</i> did not match any documents in <i>{{$ctrl.folder}}</i></h5>
    </div>
    <div ng-if="$ctrl.docResults.length > 0">
        <h5>Your search <i>{{$ctrl.searchWord}}</i> was found {{$ctrl.totalResults}} <ng-pluralize
        count="$ctrl.totalResults" when="{'1': 'time', 'other': 'times'}"></ng-pluralize>
            in <i ng-if="$ctrl.folder">{{$ctrl.folder}}</i><i ng-if="!$ctrl.folder">root</i>
        </h5>
        <ul class="list-unstyled">
            <li ng-repeat="r in $ctrl.docResults | orderBy:$ctrl.resultOrder($ctrl.orderByOption)">
                <a class="cursor-pointer" ng-click="r.closed = !r.closed"
                ng-if="r.pars.length > 0 || r.tags.length > 0">
                    <i class="glyphicon" ng-class="r.closed ? 'glyphicon-plus' : 'glyphicon-minus'"
                    title="Toggle preview"></i></a>
                <a href="/view/{{r.doc.path}}" title="Open {{r.doc.title}}">{{r.doc.title}}</a>
                <i>({{r.doc.path}})</i>
                <ul>
                    <li ng-repeat="p in r.pars" ng-if="!r.closed && !$ctrl.limitedDisplay">
                        <a href="/view/{{r.doc.path}}#{{p.par.id}}" title="Open paragraph">{{p.preview}}</a>
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
                <option value="3">Sort by relevance</option>
            </select>
        </div>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showSearchResultDialog(r: ISearchResultParams) {
    return await showDialog<ShowSearchResultController>("timSearchResults", {params: () => r}).result;
}
