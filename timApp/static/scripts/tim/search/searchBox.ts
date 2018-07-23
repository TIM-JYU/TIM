/**
 * A search box component.
 *
 * Search options:
 *
 * caseSensitive: distinguish between upper and lower case letters
 * folder: limit search to a folder and its subfolders
 * ignorePluginsSettings: leave plugin and setting paragraphs out of the results.
 * maxDocPars: search only X first paragraphs from each document
 * maxTime: end search after X seconds
 * maxTotalResults: end search after getting X results
 * regex: regular expressions
 * searchExactWords: search whole words
 * searchWords: basic word search
 * searchTags: tag search
 * searchDocNames: search document titles
 */

import {IController} from "angular";
import {timApp} from "../app";
import {$http, $localStorage, $window} from "../util/ngimport";
import {IItem, ITag, ITaggedItem} from "../item/IItem";
import {Binding, to} from "../util/utils";
import {ISearchResultParams, ShowSearchResultController, showSearchResultDialog} from "./searchResultsCtrl";
import {ngStorage} from "ngstorage";
import {IExtraData} from "../document/editing/edittypes";
import {BookmarksController} from "../bookmark/bookmarks";

export interface ISearchResultsInfo {
    results: ISearchResult[];
    complete: boolean; // Whether the search was completely finished in the folder.
    wordResultCount: number;
    titleResultCount: number;
    errors: ISearchError[];
}

export interface ISearchError {
    error: string;
    doc_path: string;
    par_id: string;
    tag_name: string;
}

export interface ITagSearchResultsInfo {
    results: ITagSearchResult[];
    complete: boolean;
    tagResultCount: number;
    errors: ISearchError[];
}

export interface ISearchResult {
    doc: IItem;
    par: IExtraData;
    match_start_index: number; // Index where the query match begins in the paragraph / title.
    match_end_index: number; // Index where the query match ends in the paragraph / title.
    match_word: string;
    in_title: boolean;
}

export interface ITagSearchResult{
    doc: ITaggedItem;
    // Number of matches in the document's tags (not matching_tags length, because same tag may contain match
    // more than once.)
    num_results: number;
    matching_tags: ITag[]; // List of tags that matched the query.
}

export class SearchBoxCtrl implements IController {
    private query: string = "";
    private folder!: Binding<string, "<">;
    private regex: boolean = false;
    private caseSensitive: boolean = false;
    private results: ISearchResult[] = [];
    private errorMessage: string = "";
    private tagMatchCount: number = 0;
    private wordMatchCount: number = 0;
    private titleMatchCount: number = 0;
    private advancedSearch: boolean = false;
    private createNewWindow: boolean = false;
    private ignorePluginsSettings: boolean = false;
    private searchDocNames: boolean = false;
    private searchTags: boolean = true; // Tag and word search are on by default.
    private searchWords: boolean = true;
    private searchExactWords: boolean = false;
    private focusMe: boolean = true;
    private loading: boolean = false; // Display loading icon.
    private item: IItem = $window.item;
    private storage: ngStorage.StorageService & {
        searchWordStorage: null | string,
        optionsStorage: null | boolean[],
        optionsValueStorage: null | number[]};
    private tagResults: ITagSearchResult[] = [];
    private folderSuggestions: string[] = []; // A list of folder path suggestions.
    private completeSearch: boolean = false;
    private maxDocResults: number = 100;
    private resultsDialog: ShowSearchResultController | null = null;

    constructor() {
        this.storage = $localStorage.$default({
            optionsValueStorage: null,
            optionsStorage: null,
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
        if (this.searchTags) {
            await this.tagSearch();
        }
        if (this.searchWords || this.searchDocNames) {
            // Server side has a 3 character minimum check.
            if (!this.folder.trim() && this.searchWords) {
                this.errorMessage = (`Content searches on root directory are not allowed.`);
                this.loading = false;
                return;
            }
            await this.wordSearch();
        }
        if (this.results.length === 0 && this.tagResults.length === 0 && !this.errorMessage) {
            this.errorMessage = `Your search '${this.query}' did not match any documents.`;
            this.loading = false;
            return;
        }
        if (this.errorMessage) {
            this.loading = false;
            return;
        }
        this.updateLocalStorage();
        let tempError = this.errorMessage;
        if (!this.completeSearch) {
            tempError = "Search was incomplete due to time or data constraints. " +
                "For better results choose more specific search options.";
        }
        const resultParams = {
            errorMessage: tempError,
            folder: this.folder,
            results: this.results,
            searchComponent: this,
            searchWord: this.query,
            tagMatchCount: this.tagMatchCount,
            tagResults: this.tagResults,
            titleMatchCount: this.titleMatchCount,
            wordMatchCount: this.wordMatchCount,
        };
        if (this.createNewWindow) {
            void showSearchResultDialog(resultParams);
        } else {
            if (!this.resultsDialog) {
                void showSearchResultDialog(resultParams);
            } else {
                this.resultsDialog.updateAttributes(resultParams);
            }
        }
        this.loading = false;
    }

    /**
     * Sets a search result controller.
     * @param {ShowSearchResultController} resultsDialog
     */
    registerResultsDialog(resultsDialog: ShowSearchResultController | null) {
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
        this.storage.optionsValueStorage = [];
        this.storage.optionsValueStorage.push(this.maxDocResults);

        this.storage.optionsStorage = [];
        // Alphabetical order.
        this.storage.optionsStorage.push(this.advancedSearch);
        this.storage.optionsStorage.push(this.caseSensitive);
        this.storage.optionsStorage.push(this.createNewWindow);
        this.storage.optionsStorage.push(this.ignorePluginsSettings);
        this.storage.optionsStorage.push(this.regex);
        this.storage.optionsStorage.push(this.searchDocNames);
        this.storage.optionsStorage.push(this.searchExactWords);
        this.storage.optionsStorage.push(this.searchTags);
        this.storage.optionsStorage.push(this.searchWords);
    }

    /**
     * Fetches options and search word from local storage, if existent.
     */
    private loadLocalStorage() {
        if (this.storage.searchWordStorage) {
            this.query = this.storage.searchWordStorage;
        }
        if (this.storage.optionsValueStorage && this.storage.optionsValueStorage.length > 0) {
            this.maxDocResults = this.storage.optionsValueStorage[0];
        }
        if (this.storage.optionsStorage && this.storage.optionsStorage.length > 8) {
            this.advancedSearch = this.storage.optionsStorage[0];
            this.caseSensitive = this.storage.optionsStorage[1];
            this.createNewWindow = this.storage.optionsStorage[2];
            this.ignorePluginsSettings = this.storage.optionsStorage[3];
            this.regex = this.storage.optionsStorage[4];
            this.searchDocNames = this.storage.optionsStorage[5];
            this.searchExactWords = this.storage.optionsStorage[6];
            this.searchTags = this.storage.optionsStorage[7];
            this.searchWords = this.storage.optionsStorage[8];
        }
    }

    /**
     * If the component doesn't get a default folder as parameter, decides it here.
     *
     * Rules:
     *
     * root -> kurssit
     * users/username/somesubfolders -> users/username
     * kurssit/faculty/course/somesubfolders -> kurssit/faculty/course
     * kurssit/faculty/course -> kurssit/faculty/course
     * somefolder/somesubfolders -> somefolder
     */
    private defaultFolder() {
        if (!this.folder) {
            if (!this.item) {
                this.folder = "kurssit";
                return;
            }
            if (this.item.isFolder) {
                this.folder = this.item.path;
            } else {
                this.folder = this.item.location;
            }
            if (!this.folder) {
                this.folder = "kurssit";
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
     * Document word and title search.
     * @returns {Promise<void>}
     */
    private async wordSearch() {
        const [err, response] = await to($http<ISearchResultsInfo>({
            method: "GET",
            params: {
                caseSensitive: this.caseSensitive,
                folder: this.folder,
                ignorePluginsSettings: this.ignorePluginsSettings,
                maxDocPars: 1000,
                maxDocResults: this.maxDocResults,
                maxTime: 10,
                maxTotalResults: 10000,
                query: this.query,
                regex: this.regex,
                searchDocNames: this.searchDocNames,
                searchExactWords: this.searchExactWords,
                searchWords: this.searchWords,
            },
            url: "/search",
        }));
        if (err) {
            let tempError = "";
            // Basic error message from server.
            if (err.data.error) {
                tempError = err.data.error.toString();
            }
            // Some errors don't have err.data.error and are in raw HTML.
            if (err.data && tempError.length < 1) {
                tempError = removeHtmlTags(err.data.toString());
                if (tempError.indexOf("Proxy Error") > -1) {
                    tempError = tempError.replace("Proxy Error Proxy Error", "Proxy Error:").
                    replace("&nbsp;", " ");
                }
            }
            if (tempError.length < 1) {
                tempError = "Unknown error";
            }
            this.errorMessage = tempError;
            this.results = [];
            return;
        }
        if (response) {
            this.results = response.data.results;
            this.completeSearch = response.data.complete;
            this.wordMatchCount = response.data.wordResultCount;
            this.titleMatchCount = response.data.titleResultCount;
            // if (response.data.errors.length > 0) {
            //     console.log("Errors were encountered during search:");
            //     console.log(response.data.errors);
            // }
        }
    }

    /**
     * Search document tags.
     * @returns {Promise<void>}
     */
    private async tagSearch() {
        const [err, response] = await to($http<ITagSearchResultsInfo>({
                method: "GET",
                url: "/search/tags",
                params: {
                    caseSensitive: this.caseSensitive,
                    folder: this.folder,
                    query: this.query,
                    regex: this.regex,
                    searchExactWords: this.searchExactWords,
                },
        }));
        if (response) {
            // if (response.data.errors.length > 0) {
            //     console.log("Errors were encountered during tag search:");
            //     console.log(response.data.errors);
            // }
            this.tagResults = response.data.results;
            this.tagMatchCount = response.data.tagResultCount;
        }
        if (err) {
            let tempError = "";
            if (err.data.error) {
                tempError = err.data.error.toString();
            }
            if (err.data && tempError.length < 1) {
                tempError = removeHtmlTags(err.data.toString());
                if (tempError.indexOf("Proxy Error") > -1) {
                    tempError = tempError.replace("Proxy ErrorProxy Error", "Proxy Error ").
                    replace(".R", ". R").replace("&nbsp;", " ");
                }
            }
            if (tempError.length < 1) {
                tempError = "Unknown error";
            }
            this.errorMessage = tempError;
            this.tagResults = [];
            return;
        }

    }

    /**
     * Make a list of folder paths.
     * @returns {Promise<void>}
     */
    private async loadFolderSuggestions() {
        // TODO: Load from an index to get all folders faster?
        // Currently goes only three levels deep to save time.
        const response = await $http<string[]>({
            method: "GET",
            params: {
                folder: "",
            },
            url: "/search/getFolders",
        });
        if (response) {
            this.folderSuggestions = response.data;
        }
    }

    /**
     * Reset all search specific attributes to avoid them carrying over to following searches.
     */
    private resetAttributes() {
        this.tagMatchCount = 0;
        this.wordMatchCount = 0;
        this.titleMatchCount = 0;
        this.tagResults = [];
        this.results = [];
        this.errorMessage = "";
    }

    /**
     * Format search button tooltip based on the situation.
     * @returns {string}
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
 * @param {string} str
 * @returns {string}
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
                           uib-typeahead="f as f for f in $ctrl.folderSuggestions | filter:$viewValue | limitTo:15"
                           typeahead-min-length="1">
                </div>
           </div>
            <div class="form-group" title="Input maximum number of results to give from a single document">
                <label for="max-doc-results-selector" class="col-sm-5 control-label font-weight-normal"
                style="text-align:left;">Max results / document:</label>
                <div class="col-sm-7">
                    <input ng-model="$ctrl.maxDocResults" name="max-doc-results-selector"
                           type="number" class="form-control" id="folder-selector"
                           placeholder="Input max # of results per document">
                </div>
            </div>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.caseSensitive"
            title="Distinguish between upper and lower case letters"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Case sensitive</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.regex"
            title="Allow regular expressions"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Regex</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.ignorePluginsSettings"
            title="Leave plugins and settings out of the results"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Ignore plugins</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.searchExactWords"
            title="Search only whole words with one or more character"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Search whole words</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.createNewWindow"
            title="Show result of each search in new window"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Open new window for each search</label>
        <h5 class="font-weight-normal">Search scope:</h5>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.searchDocNames"
            title="Search document titles"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Title search</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.searchTags"
            title="Search document tags"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Tag search</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.searchWords"
            title="Search document content"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Content search</label>
      </div>
      </form>
    </div>
`,
});
