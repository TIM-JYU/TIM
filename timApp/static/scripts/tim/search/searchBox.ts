/**
 * A search box component.
 */

import {IController} from "angular";
import {timApp} from "../app";
import {$http} from "../util/ngimport";
import {Paragraph} from "../document/parhelpers";
import {IItem} from "../item/IItem";

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

    $onInit() {
    }

    $onDestroy() {
    }

    /**
     * Word search on target folder.
     * @returns {Promise<void>}
     */
    async search() {
        const response = await $http<ISearchResult[]>({
            method: "GET",
            params: {
                folder: this.folder,
                query: this.query,
                regex: this.regex,
            },
            url: "/search",
        });
        this.results = response.data;
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
           title="Search documents by entering  key words"
           placeholder="Search documents by entering key words"
           class="form-control" id="tagFilterField" autocomplete="on">
    <span class="input-group-addon btn" ng-click="$ctrl.search()">
            <span class="glyphicon glyphicon-search"
                  title="Search with {{$ctrl.query}}"></span>
        </span>
</div>
`,
});
