/**
 * A search box component.
 */

import {IController} from "angular";
import {ngStorage} from "ngstorage";
import {timApp} from "../app";
import {IItem, ITag, ITaggedItem} from "../item/IItem";
import {$http, $localStorage, $window} from "../util/ngimport";
import {Binding, to} from "../util/utils";
import {ShowSearchResultController, showSearchResultDialog} from "./searchResultsCtrl";

/**
 * All data title/word search route returns.
 */
export interface ISearchResultsInfo {
    results: IDocSearchResult[];
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

export class SearchBoxCtrl implements IController {
    // Results and variables search results dialog needs to know:
    public results: IDocSearchResult[] = [];
    public resultErrorMessage: string | undefined; // Message displayed only in results dialog.
    public tagMatchCount: number = 0;
    public wordMatchCount: number = 0;
    public titleMatchCount: number = 0;
    public tagResults: ITagSearchResult[] = [];
    public titleResults: IDocSearchResult[] = [];
    public incompleteSearchReason: string | undefined;
    public query: string = "";
    public folder!: Binding<string, "<">;

    // Route settings:
    private regex: boolean = false; // Regular expressions.
    private caseSensitive: boolean = false; // Take upper/lower case in account.
    private advancedSearch: boolean = false; // Toggle advanced options panel.
    private createNewWindow: boolean = false; // Open new dialog for each search.
    private ignorePlugins: boolean = false; // Leave plugins out of the results.
    private searchDocNames: boolean = true; // Doc title search. On by default.
    private searchTags: boolean = true; // Tag search. On by default.
    private searchWords: boolean = true; // Content search. On by default.
    private searchWholeWords: boolean = true; // Whole word search.
    private searchOwned: boolean = false; // Limit search to docs owned by the user.

    // Controller's private attributes:
    private errorMessage: string | undefined; // Message displayed only in search panel.
    private focusMe: boolean = true;
    private loading: boolean = false; // Display loading icon.
    private item: IItem = $window.item;
    private storage: ngStorage.StorageService & {
        searchWordStorage: null | string,
        optionsStorage: null | boolean[]};
    private folderSuggestions: string[] = []; // A list of folder path suggestions.
    private resultsDialog: ShowSearchResultController | undefined; // The most recent search result dialog.
    private timeWarningLimit: number = 20;
    private maxDocResults: number = 1000;

    // Gives a warning about long search time if over this.

    /**
     * SearchBox constructor.
     */
    constructor() {
        this.storage = $localStorage.$default({
            optionsStorage: null,
            optionsValueStorage: null,
            searchWordStorage: null,
        });
    }

    $onInit() {
        this.loadLocalStorage();
        this.defaultFolder();
        void this.loadFolderSuggestions();
    }

    $onDestroy() {
        this.updateLocalStorage();
    }

    /**
     * Word search on target folder.
     * @returns {Promise<void>}
     */
    async search() {
        if (this.loading) {
            return;
        }
        this.resetAttributes();
        this.loading = true;
        if (!this.searchDocNames && !this.searchTags && !this.searchWords) {
            this.errorMessage = (`All search scope options are unchecked.`);
            this.loading = false;
            return;
        }

        const start = new Date().getTime();

        // Each search type has separate route.
        if (this.searchTags) {
            await this.tagSearch();
        }
        if (this.searchWords || this.searchDocNames) {
            await this.wordSearch();
        }
        // if (this.searchDocNames) {
        //     await this.titleSearch();
        // }
        this.loading = false;
        if (this.results.length === 0 && this.tagResults.length === 0 &&
                this.titleResults.length === 0 && !this.errorMessage) {
            this.errorMessage = `Your search '${this.query}' did not match any documents.`;
            return;
        }
        if (this.errorMessage) {
            return;
        }
        // After successful search save search options to local storage.
        this.updateLocalStorage();

        // Give warnings if search was incomplete or took a long time.
        if (((new Date().getTime() - start) / 1000) > this.timeWarningLimit) {
            this.resultErrorMessage = "The search took a long time. " +
                "Use more specific search settings for a faster search.";
        }
        if (this.incompleteSearchReason) {
            this.resultErrorMessage = `Incomplete search: ${this.incompleteSearchReason}.` +
                ` For better results choose more specific search settings.`;
        }

        // Show results in result dialog.
        if (this.createNewWindow) {
            void showSearchResultDialog(this);
        } else {
            if (!this.resultsDialog) {
                void showSearchResultDialog(this);
            } else {
                this.resultsDialog.updateAttributes(this);
            }
        }
    }

    /**
     * Sets a search result controller.
     * @param {ShowSearchResultController} resultsDialog The currently active search result window.
     */
    registerResultsDialog(resultsDialog: ShowSearchResultController | undefined) {
        this.resultsDialog = resultsDialog;
    }

    /*
     * Calls search function when Enter is pressed.
     * @param event Keyboard event.
     */
    async keyPressed(event: KeyboardEvent) {
        // TODO: Causes "$digest already in progress" errors.
        if (event.which === 13) {
            await this.search();
        }
    }

    /**
     * Saves options and search word to local storage.
     */
    private updateLocalStorage() {
        if (this.query.trim().length > 0) {
            this.storage.searchWordStorage = this.query;
        }
        this.storage.optionsStorage = [];
        // Alphabetical order.
        this.storage.optionsStorage.push(this.advancedSearch);
        this.storage.optionsStorage.push(this.caseSensitive);
        this.storage.optionsStorage.push(this.createNewWindow);
        this.storage.optionsStorage.push(this.ignorePlugins);
        this.storage.optionsStorage.push(this.regex);
        this.storage.optionsStorage.push(this.searchDocNames);
        this.storage.optionsStorage.push(this.searchWholeWords);
        this.storage.optionsStorage.push(this.searchTags);
        this.storage.optionsStorage.push(this.searchOwned);
        this.storage.optionsStorage.push(this.searchWords);
    }

    /**
     * Fetches options and search word from local storage, if existent.
     */
    private loadLocalStorage() {
        if (this.storage.searchWordStorage) {
            this.query = this.storage.searchWordStorage;
        }
        if (this.storage.optionsStorage && this.storage.optionsStorage.length > 9) {
            this.advancedSearch = this.storage.optionsStorage[0];
            this.caseSensitive = this.storage.optionsStorage[1];
            this.createNewWindow = this.storage.optionsStorage[2];
            this.ignorePlugins = this.storage.optionsStorage[3];
            this.regex = this.storage.optionsStorage[4];
            this.searchDocNames = this.storage.optionsStorage[5];
            this.searchWholeWords = this.storage.optionsStorage[6];
            this.searchTags = this.storage.optionsStorage[7];
            this.searchOwned = this.storage.optionsStorage[8];
            this.searchWords = this.storage.optionsStorage[9];
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
        if (!this.folder) {
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
    }

    /**
     * Document title search.
     * @returns {Promise<void>}
     */
    private async titleSearch() {
        const [err, response] = await to($http<ISearchResultsInfo>({
            method: "GET",
            params: {
                caseSensitive: this.caseSensitive,
                folder: this.folder,
                query: this.query,
                regex: this.regex,
                searchOwned: this.searchOwned,
                searchWholeWords: this.searchWholeWords,
            },
            url: "/search/titles",
        }));
        if (err) {
            this.errorMessage = this.getErrorMessage(err);
            this.results = [];
            return;
        }
        if (response) {
            this.titleResults = response.data.results;
            if (response.data.incomplete_search_reason) {
                this.incompleteSearchReason = response.data.incomplete_search_reason;
            }
            this.titleMatchCount = response.data.title_result_count;
        }
    }

    /**
     * Document paragraph word search.
     * @returns {Promise<void>}
     */
    private async wordSearch() {
        const [err, response] = await to($http<ISearchResultsInfo>({
            method: "GET",
            params: {
                caseSensitive: this.caseSensitive,
                folder: this.folder,
                ignorePlugins: this.ignorePlugins,
                // maxResults: 100000,
                maxDocResults: this.maxDocResults,
                query: this.query,
                regex: this.regex,
                searchContent: this.searchWords,
                searchOwned: this.searchOwned,
                searchTitles: this.searchDocNames,
                searchWholeWords: this.searchWholeWords,
            },
            url: "/search",
        }));
        if (err) {
            this.errorMessage = this.getErrorMessage(err);
            this.results = [];
            return;
        }
        if (response) {
            this.results = response.data.results;
            if (response.data.incomplete_search_reason) {
                this.incompleteSearchReason = response.data.incomplete_search_reason;
            }
            this.wordMatchCount = response.data.word_result_count;
            this.titleMatchCount = response.data.title_result_count;
        }
    }

    /**
     * Search document tags.
     * @returns {Promise<void>}
     */
    private async tagSearch() {
        const [err, response] = await to($http<ITagSearchResultsInfo>({
            method: "GET",
            params: {
                caseSensitive: this.caseSensitive,
                folder: this.folder,
                query: this.query,
                regex: this.regex,
                searchOwned: this.searchOwned,
                searchWholeWords: this.searchWholeWords,
            },
            url: "/search/tags",
        }));
        if (response) {
            this.tagResults = response.data.results;
            this.tagMatchCount = response.data.tag_result_count;
            if (response.data.incomplete_search_reason) {
                this.incompleteSearchReason = response.data.incomplete_search_reason;
            }
        }
        if (err) {
            this.errorMessage = this.getErrorMessage(err);
            this.tagResults = [];
            return;
        }

    }

    /**
     * Parses unusual kinds of error messages.
     * @param {{data: {error: string}}} err Error response.
     * @returns {string} An error message.
     */
    private getErrorMessage(err: {data: {error: string}}) {
        let tempError = "";
        if (err.data.error) {
            tempError = err.data.error.toString();
        }
        if (err.data && tempError.length < 1) {
            // Proxy error data is in raw HTML format, so this is to make it more readable.
            tempError = removeHtmlTags(err.data.toString());
            if (tempError.indexOf("Proxy Error") > -1) {
                tempError = tempError.replace("Proxy ErrorProxy Error", "Proxy Error ").
                replace(".R", ". R").replace("&nbsp;", " ");
            }
        }
        if (tempError.length < 1) {
            tempError = "Unknown error";
        }
        return tempError;
    }

    /**
     * Makes a list of folder paths starting from the current default search folder.
     * @returns {Promise<void>}
     */
    private async loadFolderSuggestions() {
        const response = await $http<string[]>({
            method: "GET",
            params: {
                folder: this.folder,
            },
            url: "/search/getFolders",
        });
        this.folderSuggestions = response.data;
    }

    /**
     * Reset all search specific attributes to avoid them carrying over to following searches.
     */
    private resetAttributes() {
        this.tagMatchCount = 0;
        this.wordMatchCount = 0;
        this.titleMatchCount = 0;
        this.incompleteSearchReason = undefined;
        this.tagResults = [];
        this.titleResults = [];
        this.results = [];
        this.errorMessage = undefined;
        this.resultErrorMessage = undefined;
    }

    /**
     * Format search button tooltip based on the situation.
     * @returns {string} Tooltip, which is different depending on whether loading and input query states.
     */
    private searchButtonTooltip() {
        if (this.query.length < 1) {
            return "Input a search word to search";
        }
        if (this.loading) {
            return `Please wait, searching '${this.query}'`;
        } else {
            return `Search with '${this.query}'`;
        }
    }
}

/**
 * Removes HTML tags, linebreaks and extra white spaces.
 * @param {string} str Raw html string.
 * @returns {string} Plain text string.
 */
function removeHtmlTags(str: string) {
    return str.replace(/<{1}[^<>]{1,}>{1}/g, " ").
        replace(/(\r\n\t|\n|\r\t)/gm, " ").
        replace(/\s+/g, " ").trim();
}

timApp.component("searchBox", {
    bindings: {
        folder: "<",
    },
    controller: SearchBoxCtrl,
    template: `<div class="input-group">
        <input ng-model="$ctrl.query" name="searchField" ng-keypress="$ctrl.keyPressed($event)"
               type="text" focus-me="$ctrl.focusMe"
               title="Search documents with a key word"
               placeholder="Input a search word"
               class="form-control" autocomplete="on">
        <span class="input-group-addon btn" ng-click="$ctrl.search()" title="{{$ctrl.searchButtonTooltip()}}">
                <span ng-show="$ctrl.loading" class="glyphicon glyphicon-refresh glyphicon-refresh-animate">
                </span>
                <span ng-hide="$ctrl.loading" class="glyphicon glyphicon-search"></span>
        </span>
        <span class="input-group-addon btn" ng-click="$ctrl.advancedSearch = !$ctrl.advancedSearch"
            title="Toggle advanced search">
                <span class="glyphicon glyphicon-menu-hamburger"></span>
        </span>
   </div>
   <div ng-cloak ng-show="$ctrl.errorMessage" class="alert alert-warning">
    <span class="glyphicon glyphicon-exclamation-sign"></span> {{$ctrl.errorMessage}}
   </div>
   <div ng-if="$ctrl.advancedSearch" title="Advanced search options">
      <h5>Advanced search options</h5>
      <form class="form-horizontal">
           <div class="form-group" title="Write folder path to search from">
                <label for="folder-selector" class="col-sm-4 control-label font-weight-normal"
                style="text-align:left;">Search folder:</label>
                <div class="col-sm-8">
                    <input ng-model="$ctrl.folder" name="folder-selector"
                           type="text" class="form-control" id="folder-selector" placeholder="Input a folder to search"
           uib-typeahead="f as f for f in $ctrl.folderSuggestions | filter:$viewValue | limitTo:15 | orderBy:'length'"
                           typeahead-min-length="1">
                </div>
           </div>
           <div class="form-group" title="Input maximum number of results to get from a single document">
                <label for="max-doc-results-selector" class="col-sm-6 control-label font-weight-normal"
                style="text-align:left;">Max results per document:</label>
                <div class="col-sm-6">
                    <input ng-model="$ctrl.maxDocResults" name="max-doc-results-selector"
                           type="number" class="form-control" id="folder-selector"
                           placeholder="Input a result limit">
                </div>
           </div>

        <label class="font-weight-normal" title="Distinguish between upper and lower case letters">
            <input type="checkbox" ng-model="$ctrl.caseSensitive"> Case sensitive</label>
        <label class="font-weight-normal" title="Allow regular expressions">
            <input type="checkbox" ng-model="$ctrl.regex"> Regex</label>
        <label class="font-weight-normal" title="Leave plugin and setting contents out of the results">
            <input type="checkbox" ng-model="$ctrl.ignorePlugins"> Ignore plugins and settings</label>
        <label class="font-weight-normal" title="Search only whole words with one or more character">
            <input type="checkbox" ng-model="$ctrl.searchWholeWords"> Search whole words</label>
        <label class="font-weight-normal dropdown-item" title="Search from documents you own">
            <input type="checkbox" ng-model="$ctrl.searchOwned"> Search owned documents</label>
        <label class="font-weight-normal" title="Show result of each search in new window">
            <input type="checkbox" ng-model="$ctrl.createNewWindow"> Open new window for each search</label>
        <h5 class="font-weight-normal">Search scope:</h5>
        <label class="font-weight-normal" title="Search document titles">
            <input type="checkbox" ng-model="$ctrl.searchDocNames"> Title search</label>
        <label class="font-weight-normal" title="Search document tags">
            <input type="checkbox" ng-model="$ctrl.searchTags"> Tag search</label>
        <label class="font-weight-normal" title="Search document content">
            <input type="checkbox" ng-model="$ctrl.searchWords"> Content search</label>
      </form>
    </div>
`,
});
