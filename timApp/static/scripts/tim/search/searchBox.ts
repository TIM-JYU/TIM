/**
 * A search box component.
 */

import {IController} from "angular";
import {timApp} from "../app";
import {$http, $localStorage} from "../util/ngimport";
import {Paragraph} from "../document/parhelpers";
import {IItem} from "../item/IItem";
import {Binding, to} from "../util/utils";
import {showSearchResultDialog} from "./searchResultsCtrl";
import {ngStorage} from "ngstorage";

export interface ISearchResult {
    doc: IItem;
    par: Paragraph;
    match_start_index: number;
    match_end_index: number;
    match_word: string;
    num_results: number;
    num_pars: number;
    num_pars_found: number;
}

class SearchBoxCtrl implements IController {
    private query: string = "";
    private folder!: Binding<string, "<">;
    private regex: boolean = false;
    private results: ISearchResult[] = [];
    private errorMessage: string = "";
    private beginning = true; // When search hasn't been used yet.
    private onlyfirst = 100; // # first results returned.
    private advancedSearch = false;
    private openByDefault = false;
    private storage: ngStorage.StorageService & {searchWordStorage: null | string, optionsStorage: null | boolean[]};

    constructor() {
        this.storage = $localStorage.$default({
            optionsStorage: null,
            searchWordStorage: null,
        });
    }

    $onInit() {
        if (this.storage.searchWordStorage) {
            this.query = this.storage.searchWordStorage;
        }
        if (this.storage.optionsStorage && this.storage.optionsStorage.length >= 2) {
            this.openByDefault = this.storage.optionsStorage[0];
            this.regex = this.storage.optionsStorage[1];
        }
        if (this.openByDefault) {
            this.advancedSearch = true;
        }
    }

    $onDestroy() {
        if (this.query.trim().length > 3) {
            this.storage.searchWordStorage = this.query;
        }
        this.storage.optionsStorage = [];
        this.storage.optionsStorage.push(this.openByDefault);
        this.storage.optionsStorage.push(this.regex);
    }

    /**
     * Word search on target folder.
     * @returns {Promise<void>}
     */
    async search() {
        if (this.query.trim().length < 3) {
            alert("Search text must be at least 3 characters long with whitespace stripped.");
            return;
        }
        this.beginning = false;
        const [err, response] = await to($http<ISearchResult[]>({
            method: "GET",
            params: {
                folder: this.folder,
                onlyfirst: this.onlyfirst,
                query: this.query,
                regex: this.regex,
            },
            url: "/search",
        }));
        if (err) {
            this.errorMessage = err.data.error;
            this.results = [];
        }
        if (response) {
            this.errorMessage = "";
            this.results = response.data;
        }
        void showSearchResultDialog({
            errorMessage: this.errorMessage,
            results: this.results,
            searchWord: this.query,
        });
    }

    /*
     * Calls tag search function when Enter is pressed.
     * @param event Keyboard event.
     */
    async keyPressed(event: KeyboardEvent) {
        if (event.which === 13) {
            await this.search();
        }
    }
}

timApp.component("searchBox", {
    bindings: {
        folder: "<",
    },
    controller: SearchBoxCtrl,
    template: `<div class="input-group">
        <input ng-model="$ctrl.query" name="searchField" ng-keypress="$ctrl.keyPressed($event)"
               type="text"
               title="Search documents with key word"
               placeholder="Input a search word"
               class="form-control" autocomplete="on">
        <span class="input-group-addon btn" ng-click="$ctrl.search()">
                <span class="glyphicon glyphicon-search" title="Search with word '{{$ctrl.query}}'"></span>
        </span>
        <span class="input-group-addon btn" ng-click="$ctrl.advancedSearch = !$ctrl.advancedSearch"
            title="Toggle advanced search">
                <span class="glyphicon glyphicon-menu-hamburger"></span>
        </span>
   </div>
   <div ng-if="$ctrl.advancedSearch" title="Advanced search options">
      <h5>Advanced search options</h5>
      <form class="form-horizontal">
           <div class="form-group" title="Write folder path to search from">
                <label for="folder-selector" class="col-sm-2 control-label">Folder:</label>
                <div class="col-sm-10">
                    <input ng-model="$ctrl.folder" name="folder-selector"
                           type="text" class="form-control" id="folder-selector" placeholder="Input a folder to search">
                </div>
            </div>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.regex"
                class="ng-pristine ng-untouched ng-valid ng-not-empty"> Regex</label>
        <label class="font-weight-normal"><input type="checkbox" ng-model="$ctrl.openByDefault"
                title="Always open search with advanced options visible"
                class="ng-pristine ng-untouched ng-valid ng-not-empty"> Always open in advanced</label>
      </div>
      </form>
    </div>
`,
});
