/**
 * A search box component.
 *
 * Search options:
 *
 * caseSensitive: distinguish between upper and lower case letters
 * ignorePluginsSettings: leave plugin and setting paragraphs out of the results.
 * folder: limit search to a folder and its subfolders
 * onlyFirst: search only X first paragraphs from each document
 * regex: regular expressions
 */

import {IController} from "angular";
import {timApp} from "../app";
import {$http, $localStorage, $window} from "../util/ngimport";
import {IFolder, IItem, ITag, ITaggedItem} from "../item/IItem";
import {Binding, to} from "../util/utils";
import {showSearchResultDialog} from "./searchResultsCtrl";
import {ngStorage} from "ngstorage";
import {IExtraData} from "../document/editing/edittypes";

export interface ISearchResult {
    doc: IItem;
    par: IExtraData;
    match_start_index: number;
    match_end_index: number;
    match_word: string;
    num_results: number;
    num_pars: number;
    num_pars_found: number;
    in_title: boolean;
}

class SearchBoxCtrl implements IController {
    private query: string = "";
    private folder!: Binding<string, "<">;
    private regex: boolean = false;
    private caseSensitive: boolean = false;
    private results: ISearchResult[] = [];
    private errorMessage: string = "";
    private onlyfirst: number = 999; // # first paragraphs searched from the document
    private queryMinLength: number = 3;
    private tagMatchCount: number = 0;
    private wordMatchCount: number = 0;
    private titleMatchCount: number = 0;
    private advancedSearch: boolean = false;
    private ignorePluginsSettings: boolean = false;
    private searchDocNames: boolean = false;
    private searchTags: boolean = false;
    private searchWords: boolean = true;
    private focusMe: boolean = true;
    private loading: boolean = false;
    private item: IItem = $window.item;
    private storage: ngStorage.StorageService & {searchWordStorage: null | string, optionsStorage: null | boolean[]};
    private tagResults: ITaggedItem[] = [];
    private folderSuggestions: string[] = [];

    constructor() {
        this.storage = $localStorage.$default({
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
        if (!this.searchDocNames && !this.searchTags && !this.searchWords) {
            this.errorMessage = (`All search options are unchecked.`);
            return;
        }
        this.resetAttributes();
        this.loading = true;
        if (this.searchWords || this.searchDocNames) {
            // Server side has separate 3 character minimum check as well.
            if (this.query.trim().length < this.queryMinLength) {
                this.errorMessage = (`Search word must be at least ${this.queryMinLength} characters
                 long with whitespace stripped.`);
                this.loading = false;
                return;
            }
            if (!this.folder.trim() && this.searchWords) {
                this.errorMessage = (`Word searches on root directory are not allowed.`);
                this.loading = false;
                return;
            }
            await this.wordSearch();
        }
        if (this.results.length === 0 && this.tagResults.length === 0) {
            this.errorMessage = `Your search '${this.query}' did not match any documents.`;
            this.loading = false;
            return;
        }
        if (this.searchTags) {
            await this.tagSearch();
        }
        this.updateLocalStorage();
        void showSearchResultDialog({
            errorMessage: this.errorMessage,
            results: this.results,
            searchDocNames: this.searchDocNames,
            searchWord: this.query,
            tagResults: this.tagResults,
            wordMatchCount: this.wordMatchCount,
            tagMatchCount: this.tagMatchCount,
            titleMatchCount: this.titleMatchCount,
        });
        this.loading = false;
    }

    /*
     * Calls search function when Enter is pressed.
     * @param event Keyboard event.
     */
    async keyPressed(event: KeyboardEvent) {
        if (event.which === 13) {
            await this.search();
        }
    }

    /**
     * Saves options and search word to local storage.
     */
    private updateLocalStorage() {
        if (this.query.trim().length > this.queryMinLength) {
            this.storage.searchWordStorage = this.query;
        }
        this.storage.optionsStorage = [];
        // Alphabetical order.
        this.storage.optionsStorage.push(this.advancedSearch);
        this.storage.optionsStorage.push(this.caseSensitive);
        this.storage.optionsStorage.push(this.ignorePluginsSettings);
        this.storage.optionsStorage.push(this.regex);
        this.storage.optionsStorage.push(this.searchDocNames);
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
        if (this.storage.optionsStorage && this.storage.optionsStorage.length > 6) {
            this.advancedSearch = this.storage.optionsStorage[0];
            this.caseSensitive = this.storage.optionsStorage[1];
            this.ignorePluginsSettings = this.storage.optionsStorage[2];
            this.regex = this.storage.optionsStorage[3];
            this.searchDocNames = this.storage.optionsStorage[4];
            this.searchTags = this.storage.optionsStorage[5];
            this.searchWords = this.storage.optionsStorage[6];
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
        const [err, response] = await to($http<ISearchResult[]>({
            method: "GET",
            params: {
                caseSensitive: this.caseSensitive,
                folder: this.folder,
                ignorePluginsSettings: this.ignorePluginsSettings,
                onlyfirst: this.onlyfirst,
                query: this.query,
                regex: this.regex,
                searchDocNames: this.searchDocNames,
                searchWords: this.searchWords,
            },
            url: "/search",
        }));
        if (err) {
            this.errorMessage = err.data.error;
            this.results = [];
            return;
        }
        if (response) {
            this.errorMessage = "";
            this.results = response.data;
            this.countResults();
        }
    }

    /**
     * Search document tags.
     * @returns {Promise<void>}
     */
    private async tagSearch() {
        const tagResponse = await $http<ITaggedItem[]>({
                method: "GET",
                url: "/tags/getDocs",
                params: {
                    case_sensitive: this.caseSensitive,
                    exact_search: false,
                    list_doc_tags: true,
                    name: this.query,
                    regex: this.regex,
                },
            });
            const taggedDocList = tagResponse.data;
            for (const doc of taggedDocList) {
                if (doc.path.startsWith(this.folder)) {
                    const filteredTags: ITag[] = [];
                    for (const tag of doc.tags) {
                        if (this.caseSensitive) {
                            if (tag.name.indexOf(this.query) > -1) {
                                filteredTags.push(tag);
                                this.tagMatchCount++;
                            }
                        } else {
                            if (tag.name.toLowerCase().indexOf(this.query.toLowerCase()) > -1) {
                                filteredTags.push(tag);
                                this.tagMatchCount++;
                            }
                        }
                    }
                    if (filteredTags.length > 0) {
                        const filteredDoc = {
                            id: doc.id,
                            name: doc.name,
                            location: doc.location,
                            title: doc.title,
                            isFolder: doc.isFolder,
                            fulltext: doc.fulltext,
                            rights: doc.rights,
                            versions: doc.versions,
                            path: doc.path,
                            tags: filteredTags,
                        };
                        this.tagResults.push(filteredDoc);
                    }
                }
            }
    }

    /**
     * Make a list of all folder paths.
     * @returns {Promise<void>}
     */
    private async loadFolderSuggestions() {
        const response = await $http<IFolder[]>({
            method: "GET",
            params: {
                folder: "",
            },
            url: "/search/getFolders",
        });
        if (response) {
            for (const f of response.data) {
                this.folderSuggestions.push(f.path);
            }
        }
    }

    /**
     * Count types of results from search-route.
     */
    private countResults() {
        let wordTemp = 0;
        let titleTemp = 0;
        for (const r of this.results) {
            if (r.in_title) {
                titleTemp++;
            } else {
                wordTemp++;
            }
        }
        this.wordMatchCount = wordTemp;
        this.titleMatchCount = titleTemp;
    }

    /**
     * Reset all search specific attributes to avoid some carrying over to following searches.
     */
    private resetAttributes() {
        this.tagMatchCount = 0;
        this.wordMatchCount = 0;
        this.titleMatchCount = 0;
        this.tagResults = [];
        this.results = [];
        this.errorMessage = "";
    }
}

timApp.component("searchBox", {
    bindings: {
        folder: "<",
    },
    controller: SearchBoxCtrl,
    template: `<div class="input-group">
        <input ng-model="$ctrl.query" name="searchField" ng-keypress="$ctrl.keyPressed($event)"
               type="text" focus-me="$ctrl.focusMe"
               title="Search documents with key word"
               placeholder="Input a search word"
               class="form-control" autocomplete="on">
        <span class="input-group-addon btn" ng-click="$ctrl.search()">
                <span ng-show="$ctrl.loading" class="glyphicon glyphicon-refresh glyphicon-refresh-animate"></span>
                <span ng-hide="$ctrl.loading" class="glyphicon glyphicon-search"
                title="Search with word '{{$ctrl.query}}'"></span>
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
                <label for="folder-selector" class="col-sm-2 control-label">Folder:</label>
                <div class="col-sm-10">
                    <input ng-model="$ctrl.folder" name="folder-selector"
                           type="text" class="form-control" id="folder-selector" placeholder="Input a folder to search"
                           uib-typeahead="f as f for f in $ctrl.folderSuggestions | filter:$viewValue | limitTo:15"
                           typeahead-min-length="1">
                </div>
            </div>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.caseSensitive"
            title="Distinguishing between upper- and lower-case letters"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Case sensitive</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.ignorePluginsSettings"
            title="Leave plugins and settings out of the results"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Ignore plugins</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.regex"
            title="Regular expressions"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Regex</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.searchDocNames"
            title="Toggle document title search"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Document title search</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.searchTags"
            title="Toggle document tag search"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Tag search</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.searchWords"
            title="Toggle document word search"
            class="ng-pristine ng-untouched ng-valid ng-not-empty"> Word search</label>
      </div>
      </form>
    </div>
`,
});
