/**
 * A search box component.
 */

import type {OnDestroy, OnInit} from "@angular/core";
import {Component, Input} from "@angular/core";
import {showSearchResultDialog} from "tim/search/showSearchResultDialog";
import * as t from "io-ts";
import type {DocumentOrFolder, IItem, ITag, ITaggedItem} from "tim/item/IItem";
import {relevanceSuggestions} from "tim/item/relevance-edit.component";
import {someglobals} from "tim/util/globals";
import {$http} from "tim/util/ngimport";
import {TimStorage, to} from "tim/util/utils";
import type {SearchResultsDialogComponent} from "tim/search/search-results-dialog.component";

/**
 * All data title/word search route returns.
 */
export interface ISearchResultsInfo {
    content_results: IDocSearchResult[];
    title_results: IDocSearchResult[];
    tags_results: ITagSearchResult[];
    paths_results: IDocSearchResult[];
    incomplete_search_reason: string; // Tells the reason why when the search is incomplete.
    word_result_count: number;
    title_result_count: number;
    tags_result_count: number;
    paths_result_count: number;
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
    num_path_results: number;
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
                   class="form-control" autocomplete="on" i18n-title i18n-placeholder>
            <span class="input-group-addon btn" (click)="search()">
                <tim-loading *ngIf="loading"></tim-loading>
                <span *ngIf="!loading" class="glyphicon glyphicon-search"></span>
            </span>
            <span class="input-group-addon btn" (click)="advancedSearch = !advancedSearch"
                  title="Toggle advanced search" i18n-title>
                <span class="glyphicon glyphicon-menu-hamburger"></span>
            </span>
        </div>
        <div class="small-note">
            <span i18n>Press <span class="text-italic">Ctrl-F / &#8984;-F</span> to search for text on the current page.</span>
        </div>
        <tim-alert *ngIf="errorMessage" severity="danger">
            {{errorMessage}}
        </tim-alert>
        <div *ngIf="advancedSearch" title="Advanced search options" i18n-title>
            <h5 i18n>Advanced search options</h5>
            <div class="form-horizontal">
                <div class="form-group" title="Write folder path to search from" i18n-title>
                    <label for="folder-selector" class="col-sm-4 control-label font-weight-normal"
                           style="text-align:left;" i18n>Search folder:</label>
                    <div class="col-sm-8">
                        <input [(ngModel)]="folder" name="folder-selector"
                               type="text" class="form-control" id="folder-selector"
                               placeholder="Input a folder to search"
                               [typeahead]="folderSuggestions"
                               [typeaheadMinLength]="1" i18n-placeholder>
                        <div class="small folder-links">
                            <a [tooltip]="defaultFolderLocation" (click)="folder = defaultFolderLocation" i18n>Default folder</a>
                            <a [tooltip]="currentFolderLocation" (click)="folder = currentFolderLocation" i18n>Current folder</a>
                        </div>
                    </div>
                </div>
                <div class="form-group">
                    <label for="max-doc-results-selector" class="col-sm-4 control-label font-weight-normal"
                           style="text-align:left;" i18n>Max results / doc: </label>
                    <div class="col-sm-3" title="Input maximum number of results to get from a single document" i18n-title>
                        <input [(ngModel)]="maxDocResults" name="max-doc-results-selector"
                               type="number" class="form-control" id="max-doc-results-selector">
                    </div>
                    <label for="min-relevance-selector" class="col-sm-2 control-label font-weight-normal"
                           style="text-align:left;" i18n>Relevance: </label>
                    <div class="col-sm-3" title="Input minimum relevance value to include in results" i18n-title>
                        <ng-template #customItemTemplate let-model="item">
                            {{ model.name }}
                        </ng-template>
                        <input [(ngModel)]="relevanceThreshold" name="min-relevance-selector" type="number"
                               class="form-control" id="min-relevance-selector"
                               [typeaheadMinLength]="0"
                               [typeaheadItemTemplate]="customItemTemplate"
                               typeaheadOptionField="value"
                               [typeaheadLatinize]="false"
                               [typeaheadIsFirstItemActive]="false"
                               [typeahead]="suggestions"
                               (keydown.arrowDown)="$event.preventDefault()"
                               (keydown.arrowUp)=" $event.preventDefault()"
                        >
                    </div>
                </div>

                <div class="flex rw space-between">
                    <div class="flex cl">
                        <label class="font-weight-normal"
                               title="Distinguish between upper and lower case letters" i18n-title>
                            <input type="checkbox" [(ngModel)]="caseSensitive"><ng-container i18n> Case sensitive</ng-container></label>
                        <label class="font-weight-normal" title="Allow regular expressions" i18n-title>
                            <input type="checkbox" [(ngModel)]="regex"><ng-container i18n> Regex</ng-container></label>
                        <label class="font-weight-normal" title="Leave plugin and setting contents out of the results" i18n-title>
                            <input type="checkbox" [(ngModel)]="ignorePlugins"><ng-container i18n> Ignore plugins and settings</ng-container></label>
                        <label class="font-weight-normal" title="Search only whole words with one or more character" i18n-title>
                            <input type="checkbox" [(ngModel)]="searchWholeWords"><ng-container i18n> Search whole words</ng-container></label>
                        <label class="font-weight-normal dropdown-item" title="Search from documents you own" i18n-title>
                            <input type="checkbox" [(ngModel)]="searchOwned"><ng-container i18n> Search owned documents</ng-container></label>
                        <label class="font-weight-normal" title="Show result of each search in new window" i18n-title>
                            <input type="checkbox" [(ngModel)]="createNewWindow"><ng-container i18n> Open new window for each search</ng-container></label>
                    </div>
                    <div class="flex cl">
                        <p i18n>Search scope:</p>
                        <label class="font-weight-normal" title="Search document content" i18n-title>
                            <input type="checkbox" [(ngModel)]="searchContent"><ng-container i18n> Contents</ng-container></label>
                        <label class="font-weight-normal" title="Search document titles" i18n-title>
                            <input type="checkbox" [(ngModel)]="searchTitles"><ng-container i18n> Titles</ng-container></label>
                        <label class="font-weight-normal" title="Search document tags" i18n-title>
                            <input type="checkbox" [(ngModel)]="searchTags"><ng-container> Tags</ng-container></label>
                        <label class="font-weight-normal" title="Search document paths" i18n-title>
                            <input type="checkbox" [(ngModel)]="searchPaths"><ng-container i18n> Paths</ng-container></label>
                    </div>
                </div>
            </div>
        </div>
    `,
    styleUrls: ["./search-box.component.scss"],
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
    relevanceThreshold = 10; // Exclude documents with < X relevance.
    suggestions = relevanceSuggestions;

    // Other attributes:
    errorMessage: string | undefined; // Message displayed only in search panel.
    loading = false; // Display loading icon.
    private item?: DocumentOrFolder = someglobals().curr_item;
    private storage = new TimStorage(
        "searchOpts",
        t.partial({
            maxDocResults: t.number,
            options: t.array(t.boolean),
            optionsValue: t.number,
            relevanceThreshold: t.number,
            searchWord: t.string,
        })
    );
    folderSuggestions: string[] = []; // A list of folder path suggestions.
    private resultsDialog: SearchResultsDialogComponent | undefined; // The most recent search result dialog.
    maxDocResults = 1000;
    private timeWarningLimit = 20; // Gives a warning about long search time if over this.
    private timeout = 120; // Default timeout for search.
    defaultFolderLocation?: string;
    currentFolderLocation?: string;

    ngOnInit() {
        this.loadLocalStorage();

        this.defaultFolderLocation = this.defaultFolder();
        this.currentFolderLocation = this.currentFolder();
        this.folder = this.defaultFolderLocation;
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

        await this.combinedSearch();
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
            await showSearchResultDialog(this);
        } else {
            if (!this.resultsDialog) {
                await showSearchResultDialog(this);
            } else {
                this.resultsDialog.updateAttributes(this);
            }
        }
    }

    /**
     * Sets a search result controller.
     * @param {SearchResultsDialogComponent} resultsDialog The currently active search result window.
     */
    registerResultsDialog(
        resultsDialog: SearchResultsDialogComponent | undefined
    ) {
        this.resultsDialog = resultsDialog;
    }

    /**
     * Saves options and search word to local storage.
     */
    private updateLocalStorage() {
        this.storage.set({
            maxDocResults: this.maxDocResults,
            options: [
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
            ],
            relevanceThreshold: this.relevanceThreshold,
            searchWord: this.query,
        });
    }

    /**
     * Fetches options and search word from local storage, if existent.
     */
    private loadLocalStorage() {
        const storage = this.storage.get();
        if (storage?.searchWord) {
            this.query = storage?.searchWord;
        }
        if (storage?.relevanceThreshold) {
            this.relevanceThreshold = storage?.relevanceThreshold;
        }
        if (storage?.maxDocResults) {
            this.maxDocResults = storage?.maxDocResults;
        }
        if (storage?.options && storage?.options.length > 10) {
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
            ] = storage?.options;
        }
    }

    currentFolder() {
        if (!this.item) {
            return "";
        }
        if (this.item.isFolder) {
            return this.item.path;
        } else {
            return this.item.location;
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
    defaultFolder() {
        const folder = this.currentFolder();
        const path = folder.split("/");
        if (path[0] === "users" && path.length >= 2) {
            return `${path[0]}/${path[1]}`;
        }
        if (path[0] === "kurssit" && path.length >= 3) {
            return `${path[0]}/${path[1]}/${path[2]}`;
        }
        if (path[0] === "kurssit" && path.length >= 2) {
            return folder;
        }
        if (path.length > 1) {
            return `${path[0]}`;
        }
        return "";
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

    private async combinedSearch() {
        const r = await to(
            $http<ISearchResultsInfo>({
                method: "GET",
                params: {
                    ignorePlugins: this.ignorePlugins,
                    maxDocResults: this.maxDocResults,
                    searchContent: this.searchContent,
                    searchTitles: this.searchTitles,
                    searchTags: this.searchTags,
                    searchPaths: this.searchPaths,
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
            this.tagResults = [];
            this.pathResults = [];
            return;
        }
        const response = r.result;
        this.results = response.data.content_results;
        this.titleResults = response.data.title_results;
        this.tagResults = response.data.tags_results;
        this.pathResults = response.data.paths_results;

        if (response.data.incomplete_search_reason) {
            this.incompleteSearchReason =
                response.data.incomplete_search_reason;
        }
        this.wordMatchCount = response.data.word_result_count;
        this.titleMatchCount = response.data.title_result_count;
        this.tagMatchCount = response.data.tags_result_count;
        this.pathMatchCount = response.data.paths_result_count;
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
