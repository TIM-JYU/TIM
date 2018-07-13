/**
 * Controller and HTML template for search results dialog.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {markAsUsed} from "../util/utils";
import {ISearchResult} from "./searchBox";
import {IItem, ITag, ITaggedItem, TagType} from "../item/IItem";
import {IExtraData} from "../document/editing/edittypes";

markAsUsed(focusMe);

export interface ISearchResultParams {
    results: ISearchResult[];
    searchWord: string;
    errorMessage: string;
    searchDocNames: boolean;
    tagResults: ITaggedItem[];
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

/*
 * Tag search dialog's controller.
 */
export class ShowSearchResultController extends DialogController<{ params: ISearchResultParams }, {}, "timSearchResults"> {
    private static $inject = ["$element", "$scope"];
    private results: ISearchResult[] = [];
    private filteredResults: ISearchResult[] = [];
    private searchWord: string = "";
    private docResults: ISearchResultParamsDoc[] = [];
    private searchDocNames: boolean = false;
    private tagResults: ITaggedItem[] = [];

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    async $onInit() {
        this.searchDocNames = this.resolve.params.searchDocNames;
        this.results = this.resolve.params.results;
        this.tagResults = this.resolve.params.tagResults;
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
     * Filters duplicate paragraphs (those with more than one match) from the results and groups
     * paragraphs under documents.
     */
    private filterResults() {
        // Remove matches in same paragraphs.
        for (const {item, index} of this.results.map((item, index) => ({item, index}))) {
            if (item && item.in_title) {
                this.filteredResults.push(item);
            }
            if (item && !item.in_title) {
                if (!this.results[index - 1] || !this.results[index - 1].par) {
                    this.filteredResults.push(item);
                } else {
                    if (index === 0 || item.par.id !== this.results[index - 1].par.id) {
                        this.filteredResults.push(item);
                    }
                }
            }
        }
        // Group paragraphs under documents.
        for (const r of this.filteredResults) {
            const docIndex = this.docIndexInResults(r.doc, this.docResults);
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
        }
        for (const tagResult of this.tagResults) {
            let found = false;
            for (const docResult of this.docResults) {
                if (tagResult.path === docResult.doc.path) {
                    docResult.tags = tagResult.tags;
                    found = true;
                }
            }
            if (!found) {
                const newDocResult = {
                    closed: true,
                    doc: tagResult,
                    in_title: false,
                    pars: [],
                    tags: tagResult.tags,
                };
                this.docResults.push(newDocResult);
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
     * @param {IItem} doc
     * @param {ISearchResultParamsDoc} docs
     * @returns {any}
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
        <h5>Your search <i>{{$ctrl.searchWord}}</i> did not match any documents</h5>
    </div>
    <div ng-if="$ctrl.docResults.length > 0">
        <h5>Your search <i>{{$ctrl.searchWord}}</i> was found {{$ctrl.results.length + $ctrl.tagResults.length}}
            <ng-pluralize count="$ctrl.results.length" when="{'1': 'time', 'other': 'times'}"></ng-pluralize>
            in {{$ctrl.docResults.length}} <ng-pluralize count="$ctrl.docResults.length"
            when="{'1': 'document', 'other': 'documents'}"></ng-pluralize>
        </h5>
        <ul class="list-unstyled">
            <li ng-repeat="r in $ctrl.docResults">
                <a class="cursor-pointer" ng-click="r.closed = !r.closed"
                ng-if="r.pars.length > 0 || r.tags.length > 0">
                    <i class="glyphicon" ng-class="r.closed ? 'glyphicon-plus' : 'glyphicon-minus'"
                    title="Toggle preview"></i></a>
                <a href="/view/{{r.doc.path}}" title="Open {{r.doc.title}}">{{r.doc.title}}</a>
                <i>({{r.doc.path}})</i>
                <ul>
                    <li ng-repeat="p in r.pars" ng-if="!r.closed">
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
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showSearchResultDialog(r: ISearchResultParams) {
    return await showDialog<ShowSearchResultController>("timSearchResults", {params: () => r}).result;
}
