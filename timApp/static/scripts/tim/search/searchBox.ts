/**
 * A search box component.
 */

import {IController} from "angular";
import {timApp} from "../app";
import {$http} from "../util/ngimport";
import {Paragraph} from "../document/parhelpers";
import {IItem} from "../item/IItem";
import {to} from "../util/utils";
import {showSearchResultDialog} from "./searchResultsCtrl";

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
    private folder: string = "";
    private regex: boolean = false;
    private results: ISearchResult[] = [];
    private errorMessage: string = "";
    private beginning = true; // When search hasn't been used yet.
    private onlyfirst = 100; // # first results returned.
    private advancedSearch = false;
    //private caseSensitive = false;

    $onInit() {
    }

    $onDestroy() {
    }

    /**
     * Word search on target folder.
     * @returns {Promise<void>}
     */
    async search() {
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
    },
    controller: SearchBoxCtrl,
    template: `<div class="input-group">
        <input ng-model="$ctrl.query" name="searchField" ng-keypress="$ctrl.keyPressed($event)"
               type="text"
               title="Search documents with key word"
               placeholder="Search"
               class="form-control" autocomplete="on">
        <span class="input-group-addon btn" ng-click="$ctrl.search()">
                <span class="glyphicon glyphicon-search" title="Search with {{$ctrl.query}}"></span>
        </span>
        
   </div>
`,
});
