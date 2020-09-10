/**
 * A search box component.
 */

import {ngStorage} from "ngstorage";
import {Component, Input, OnDestroy, OnInit} from "@angular/core";
import {DocumentOrFolder, IItem, ITag, ITaggedItem} from "../item/IItem";
import {relevanceSuggestions} from "../item/relevanceEdit";
import {someglobals} from "../util/globals";
import {$http, $localStorage} from "../util/ngimport";
import {to} from "../util/utils";
import {
    SearchResultController,
    showSearchResultDialog,
} from "./searchResultsCtrl";

/**
 * All data title/word search route returns.
 */
export interface ISearchResultsInfo {
    content_results: IDocSearchResult[];
    title_results: IDocSearchResult[];
    incomplete_search_reason: string; // Tells the reason why when the search is incomplete.
    word_result_count: number;
    title_result_count: number;
    errors: ISearchError[];
}

/**
 * Search error info for tag, word or title searches.
 */
export interface ISearchError {
    error: string;
    doc_path: string;
    par_id: string;
    tag_name: string;
}

/**
 * All data tag search route returns.
 */
export interface ITagSearchResultsInfo {
    results: ITagSearchResult[];
    incomplete_search_reason: string;
    tag_result_count: number;
    errors: ISearchError[];
}

/**
 * One document's search results.
 */
export interface IDocSearchResult {
    doc: IItem;
    par_results: IParSearchResult[];
    title_results: ITitleSearchResult[];
    num_par_results: number;
    num_title_results: number;
    incomplete: boolean;
}

/**
 * A paragraph's search results.
 */
export interface IParSearchResult {
    par_id: string;
    preview: string;
    results: IWordSearchResult[];
    num_results: number;
}

/**
 * Title search results including list of word matches in the title.
 */
export interface ITitleSearchResult {
    results: IWordSearchResult[];
    num_results: number;
}

/**
 * One match info with matched word and its location in par or title.
 */
export interface IWordSearchResult {
    match_word: string;
    match_start: number;
    match_end: number;
}

/**
 * One document's tag search results.
 */
export interface ITagSearchResult {
    doc: ITaggedItem;
    // Number of matches in the document's tags (not matching_tags length, because same tag may contain match
    // more than once.)
    num_results: number;
    matching_tags: ITag[]; // List of tags that matched the query.
}

@Component({
    selector: "tim-search-box",
    template: `
        <div class="input-group">
            <input [(ngModel)]="query" name="searchField" (keydown.enter)="search()"
                   type="text" focusMe
                   title="Search documents with a keyword"
                   placeholder="Input a search word"
                   class="form-control" autocomplete="on">
            <span class="input-group-addon btn" (click)="search()">
                <tim-loading *ngIf="loading"></tim-loading>
                <span *ngIf="!loading" class="glyphicon glyphicon-search"></span>
        </span>
            <span class="input-group-addon btn" (click)="advancedSearch = !advancedSearch"
                  title="Toggle advanced search">
                <span class="glyphicon glyphicon-menu-hamburger"></span>
        </span>
        </div>
        <tim-alert *ngIf="errorMessage" severity="danger">
            {{errorMessage}}
        </tim-alert>
        <div *ngIf="advancedSearch" title="Advanced search options">
            <h5>Advanced search options</h5>
            <div class="form-horizontal">
                <div class="form-group" title="Write folder path to search from">
                    <label for="folder-selector" class="col-sm-4 control-label font-weight-normal"
                           style="text-align:left;">Search folder:</label>
                    <div class="col-sm-8">
                        <input [(ngModel)]="folder" name="folder-selector"
                               type="text" class="form-control" id="folder-selector"
                               placeholder="Input a folder to search"
                               [typeahead]="folderSuggestions"
                               [typeaheadMinLength]="1">
                    </div>
                </div>
                <div class="form-group">
                    <label for="max-doc-results-selector" class="col-sm-4 control-label font-weight-normal"
                           style="text-align:left;">Max results / doc:</label>
                    <div class="col-sm-3" title="Input maximum number of results to get from a single document">
                        <input [(ngModel)]="maxDocResults" name="max-doc-results-selector"
                               type="number" class="form-control" id="max-doc-results-selector">
                    </div>
                    <label for="min-relevance-selector" class="col-sm-2 control-label font-weight-normal"
                           style="text-align:left;">Relevance:</label>
                    <div class="col-sm-3" title="Input minimum relevance value to include in results">
                        <ng-template #customItemTemplate let-model="item">
                            {{ model.name }}
                        </ng-template>
                        <input [(ngModel)]="relevanceThreshold" name="min-relevance-selector" type="number"
                               class="form-control" id="min-relevance-selector"
                               [typeaheadMinLength]="0"
                               [typeaheadItemTemplate]="customItemTemplate"
                               typeaheadOptionField="value"
                               [typeaheadLatinize]="false"
                               [typeahead]="suggestions">
                    </div>
                </div>

                <div class="flex rw space-between">
                    <div class="flex cl">
                        <label class="font-weight-normal"
                               title="Distinguish between upper and lower case letters">
                            <input type="checkbox" [(ngModel)]="caseSensitive"> Case sensitive</label>
                        <label class="font-weight-normal" title="Allow regular expressions">
                            <input type="checkbox" [(ngModel)]="regex"> Regex</label>
                        <label class="font-weight-normal" title="Leave plugin and setting contents out of the results">
                            <input type="checkbox" [(ngModel)]="ignorePlugins"> Ignore plugins and settings</label>
                        <label class="font-weight-normal" title="Search only whole words with one or more character">
                            <input type="checkbox" [(ngModel)]="searchWholeWords"> Search whole words</label>
                        <label class="font-weight-normal dropdown-item" title="Search from documents you own">
                            <input type="checkbox" [(ngModel)]="searchOwned"> Search owned documents</label>
                        <label class="font-weight-normal" title="Show result of each search in new window">
                            <input type="checkbox" [(ngModel)]="createNewWindow"> Open new window for each
                            search</label>
                    </div>
                    <div class="flex cl">
                        <p>Search scope:</p>
                        <label class="font-weight-normal" title="Search document content">
                            <input type="checkbox" [(ngModel)]="searchContent"> Contents</label>
                        <label class="font-weight-normal" title="Search document titles">
                            <input type="checkbox" [(ngModel)]="searchTitles"> Titles</label>
                        <label class="font-weight-normal" title="Search document tags">
                            <input type="checkbox" [(ngModel)]="searchTags"> Tags</label>
                        <label class="font-weight-normal" title="Search document paths">
                            <input type="checkbox" [(ngModel)]="searchPaths"> Paths</label>
                    </div>
                </div>
            </div>
        </div>
    `,
})
export class SearchBoxComponent implements OnInit, OnDestroy {
    // Results and variables search results dialog needs to know:
    public results: IDocSearchResult[] = [];
    public resultErrorMessage: string | undefined; // Message displayed only in results dialog.
    public tagMatchCount = 0;
    public wordMatchCount = 0;
    public titleMatchCount = 0;
    public pathMatchCount = 0;
    public tagResults: ITagSearchResult[] = [];
    public titleResults: IDocSearchResult[] = [];
    public pathResults: IDocSearchResult[] = [];
    public incompleteSearchReason: string | undefined;
    public query: string = "";
    @Input() folder?: string;

    // Route settings:
    regex = false; // Regular expressions.
    caseSensitive = false; // Take upper/lower case in account.
    advancedSearch = false; // Toggle advanced options panel.
    createNewWindow = false; // Open new dialog for each search.
    ignorePlugins = false; // Leave plugins out of the results.
    searchTitles = true; // Doc title search.
    searchTags = true; // Tag search.
    searchContent = true; // Content search.
    searchWholeWords = true; // Whole word search.
    searchOwned = false; // Limit search to docs owned by the user.
    searchPaths = false; // Search document paths.
    private ignoreRelevance = false; // Don't limit results by relevance.
    relevanceThreshold = 1; // Exclude documents with < X relevance.
    suggestions = relevanceSuggestions;

    // Other attributes:
    errorMessage: string | undefined; // Message displayed only in search panel.
    loading = false; // Display loading icon.
    private item?: DocumentOrFolder = someglobals().curr_item;
    private storage: ngStorage.StorageService & {
        maxDocResultsStorage: null | string;
        relevanceThresholdStorage: null | string;
        searchWordStorage: null | string;
        optionsStorage: null | boolean[];
    };
    folderSuggestions: string[] = []; // A list of folder path suggestions.
    private resultsDialog: SearchResultController | undefined; // The most recent search result dialog.
    maxDocResults = 1000;
    private timeWarningLimit = 20; // Gives a warning about long search time if over this.
    private timeout = 120; // Default timeout for search.

    /**
     * SearchBox constructor.
     */
    constructor() {
        this.storage = $localStorage.$default({
            maxDocResultsStorage: null,
            optionsStorage: null,
            optionsValueStorage: null,
            relevanceThresholdStorage: null,
            searchWordStorage: null,
        });
    }

    ngOnInit() {
        this.loadLocalStorage();
        this.defaultFolder();
        void this.loadFolderSuggestions();
    }

    ngOnDestroy() {
        this.updateLocalStorage();
    }

    /**
     * Word search on target folder.
     */
    async search() {
        if (this.loading) {
            return;
        }

        this.resetAttributes();
        this.loading = true;
        if (
            !this.searchTitles &&
            !this.searchTags &&
            !this.searchContent &&
            !this.searchPaths
        ) {
            this.errorMessage = `All search scope options are unchecked.`;
            this.loading = false;
            return;
        }

        const start = new Date().getTime();

        // Each search type has separate route.
        if (this.searchTags) {
            await this.tagSearch();
        }
        if (this.searchPaths) {
            await this.pathSearch();
        }
        if (this.searchContent || this.searchTitles) {
            await this.wordSearch();
        }
        this.loading = false;
        if (
            this.results.length === 0 &&
            this.tagResults.length === 0 &&
            this.titleResults.length === 0 &&
            this.pathResults.length === 0 &&
            !this.errorMessage
        ) {
            this.errorMessage = `Your search '${this.query}' did not match any documents.`;
            return;
        }
        if (this.errorMessage) {
            return;
        }
        // After successful search save search options to local storage.
        this.updateLocalStorage();

        // Give warnings if search was incomplete or took a long time.
        if ((new Date().getTime() - start) / 1000 > this.timeWarningLimit) {
            this.resultErrorMessage =
                "The search took a long time. " +
                "Use more specific search settings for a faster search.";
        }
        if (this.incompleteSearchReason) {
            this.resultErrorMessage =
                `Incomplete search: ${this.incompleteSearchReason}.` +
                ` For better results choose more specific search settings.`;
        }

        // Show results in result dialog.
        if (this.createNewWindow) {
            showSearchResultDialog(this);
        } else {
            if (!this.resultsDialog) {
                showSearchResultDialog(this);
            } else {
                this.resultsDialog.updateAttributes(this);
            }
        }
    }

    /**
     * Sets a search result controller.
     * @param {SearchResultController} resultsDialog The currently active search result window.
     */
    registerResultsDialog(resultsDialog: SearchResultController | undefined) {
        this.resultsDialog = resultsDialog;
    }

    /**
     * Saves options and search word to local storage.
     */
    private updateLocalStorage() {
        if (this.query.trim().length > 0) {
            this.storage.searchWordStorage = this.query;
        }
        if (this.relevanceThreshold != null) {
            this.storage.relevanceThresholdStorage = this.relevanceThreshold.toString();
        }
        if (this.maxDocResults != null) {
            this.storage.maxDocResultsStorage = this.maxDocResults.toString();
        }
        this.storage.optionsStorage = [];
        this.storage.optionsStorage = [
            this.advancedSearch,
            this.caseSensitive,
            this.createNewWindow,
            this.ignorePlugins,
            this.regex,
            this.searchTitles,
            this.searchWholeWords,
            this.searchTags,
            this.searchOwned,
            this.searchContent,
            this.searchPaths,
        ];
    }

    /**
     * Fetches options and search word from local storage, if existent.
     */
    private loadLocalStorage() {
        if (this.storage.searchWordStorage) {
            this.query = this.storage.searchWordStorage;
        }
        if (this.storage.relevanceThresholdStorage != null) {
            this.relevanceThreshold = +this.storage.relevanceThresholdStorage;
        }
        if (this.storage.maxDocResultsStorage != null) {
            this.maxDocResults = +this.storage.maxDocResultsStorage;
        }
        if (
            this.storage.optionsStorage &&
            this.storage.optionsStorage.length > 10
        ) {
            [
                this.advancedSearch,
                this.caseSensitive,
                this.createNewWindow,
                this.ignorePlugins,
                this.regex,
                this.searchTitles,
                this.searchWholeWords,
                this.searchTags,
                this.searchOwned,
                this.searchContent,
                this.searchPaths,
            ] = this.storage.optionsStorage;
        }
    }

    /**
     * If the component doesn't get a default folder as parameter, decides it here.
     *
     * Rules:
     *
     * root -> root
     * users/username/somesubfolders -> users/username
     * kurssit/faculty/course/somesubfolders -> kurssit/faculty/course
     * kurssit/faculty/course -> kurssit/faculty/course
     * somefolder/somesubfolders -> somefolder
     */
    private defaultFolder() {
        if (this.folder !== undefined) {
            return;
        }
        if (!this.item) {
            this.folder = "";
            return;
        }
        if (this.item.isFolder) {
            this.folder = this.item.path;
        } else {
            this.folder = this.item.location;
        }
        if (!this.folder) {
            this.folder = "";
        }
        const path = this.folder.split("/");
        if (path[0] === "users" && path.length >= 2) {
            this.folder = `${path[0]}/${path[1]}`;
            return;
        }
        if (path[0] === "kurssit" && path.length >= 3) {
            this.folder = `${path[0]}/${path[1]}/${path[2]}`;
            return;
        }
        if (path[0] === "kurssit" && path.length >= 2) {
            return;
        }
        if (path.length > 1) {
            this.folder = `${path[0]}`;
            return;
        }
    }

    private getCommonSearchOptions() {
        return {
            caseSensitive: this.caseSensitive,
            folder: this.folder,
            ignoreRelevance: this.ignoreRelevance,
            query: this.query,
            regex: this.regex,
            relevanceThreshold: this.relevanceThreshold,
            searchOwned: this.searchOwned,
            searchWholeWords: this.searchWholeWords,
            timeout: this.timeout,
        };
    }

    /**
     * Document path search.
     */
    private async pathSearch() {
        const r = await to(
            $http<ISearchResultsInfo>({
                method: "GET",
                params: {...this.getCommonSearchOptions()},
                url: "/search/paths",
            })
        );
        if (!r.ok) {
            this.errorMessage = r.result.data.error;
            this.pathResults = [];
            return;
        }
        // Path results use title interface.
        this.pathResults.push(...r.result.data.title_results);
        if (r.result.data.incomplete_search_reason) {
            this.incompleteSearchReason =
                r.result.data.incomplete_search_reason;
        }
        this.pathMatchCount = r.result.data.title_result_count;
    }

    /**
     * Document title search (unused).
     */
    private async titleSearch() {
        const r = await to(
            $http<ISearchResultsInfo>({
                method: "GET",
                params: {...this.getCommonSearchOptions()},
                url: "/search/titles",
            })
        );
        if (!r.ok) {
            this.errorMessage = r.result.data.error;
            this.titleResults = [];
            return;
        }
        this.titleResults = r.result.data.title_results;
        if (r.result.data.incomplete_search_reason) {
            this.incompleteSearchReason =
                r.result.data.incomplete_search_reason;
        }
        this.titleMatchCount = r.result.data.title_result_count;
    }

    /**
     * Document paragraph word and title search.
     */
    private async wordSearch() {
        const r = await to(
            $http<ISearchResultsInfo>({
                method: "GET",
                params: {
                    ignorePlugins: this.ignorePlugins,
                    maxDocResults: this.maxDocResults,
                    searchContent: this.searchContent,
                    searchTitles: this.searchTitles,
                    searchAttrs: true,
                    ...this.getCommonSearchOptions(),
                },
                url: "/search",
            })
        );
        if (!r.ok) {
            this.errorMessage = r.result.data.error;
            this.results = [];
            this.titleResults = [];
            return;
        }
        const response = r.result;
        this.results = response.data.content_results;
        this.titleResults = response.data.title_results;
        if (response.data.incomplete_search_reason) {
            this.incompleteSearchReason =
                response.data.incomplete_search_reason;
        }
        this.wordMatchCount = response.data.word_result_count;
        this.titleMatchCount = response.data.title_result_count;
    }

    /**
     * Search document tags.
     */
    private async tagSearch() {
        const r = await to(
            $http<ITagSearchResultsInfo>({
                method: "GET",
                params: {...this.getCommonSearchOptions()},
                url: "/search/tags",
            })
        );
        if (r.ok) {
            const response = r.result;
            this.tagResults = response.data.results;
            this.tagMatchCount = response.data.tag_result_count;
            if (response.data.incomplete_search_reason) {
                this.incompleteSearchReason =
                    response.data.incomplete_search_reason;
            }
        } else {
            this.errorMessage = r.result.data.error;
            this.tagResults = [];
            return;
        }
    }

    /**
     * Makes a list of folder paths starting from the current default search folder.
     */
    private async loadFolderSuggestions() {
        const response = await to(
            $http<string[]>({
                method: "GET",
                params: {
                    folder: this.folder,
                },
                url: "/search/getFolders",
            })
        );
        if (!response.ok) {
            return;
        }
        this.folderSuggestions = response.result.data;
    }

    /**
     * Reset all search specific attributes to avoid them carrying over to following searches.
     */
    private resetAttributes() {
        this.tagMatchCount = 0;
        this.wordMatchCount = 0;
        this.titleMatchCount = 0;
        this.pathMatchCount = 0;
        this.incompleteSearchReason = undefined;
        this.tagResults = [];
        this.titleResults = [];
        this.pathResults = [];
        this.results = [];
        this.errorMessage = undefined;
        this.resultErrorMessage = undefined;
    }
}
