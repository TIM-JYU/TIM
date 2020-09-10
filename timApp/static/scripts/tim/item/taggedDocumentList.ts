/**
 * A component that shows and filters documents based on their tags.
 *
 * Contains a search field with visibility and exact/partial word search.
 */

import {IController} from "angular";
import {ngStorage} from "ngstorage";
import {Binding, to} from "tim/util/utils";
import {timApp} from "../app";
import {KEY_ENTER} from "../util/keycodes";
import {$http, $localStorage} from "../util/ngimport";
import {ITag, ITaggedItem, tagStyleClass} from "./IItem";

class TaggedDocumentListCtrl implements IController {
    public tagFilter!: Binding<string, "<">;
    public exactMatch!: Binding<boolean, "<">; // Get only documents with exactly matching tag.
    public caseSensitive!: Binding<boolean, "<">;
    public enableSearch!: Binding<boolean, "<">;
    private docList: ITaggedItem[] = [];
    private allUniqueTags: string[] = [];
    private listDocTags!: Binding<boolean, "<">;
    private tagToolTip: string = "";
    private storage: ngStorage.StorageService & {
        tagFilterStorage: null | string;
    };

    constructor() {
        this.storage = $localStorage.$default({
            tagFilterStorage: null,
        });
    }

    async $onInit() {
        if (this.enableSearch) {
            this.tagToolTip = "Search documents with tag ";
        } else {
            this.tagToolTip = "Document has tag ";
        }

        if (this.enableSearch && this.storage.tagFilterStorage) {
            this.tagFilter = this.storage.tagFilterStorage;
        } else {
            this.tagFilter = "";
        }
        await this.getAllTags();
        void this.getDocumentsByTag(this.tagFilter);
    }

    $onDestroy() {
        this.storage.tagFilterStorage = this.tagFilter;
    }

    private async getAllTags() {
        const r = await to($http.get<string[]>(`/tags/getAllTags`));
        if (r.ok) {
            this.allUniqueTags = r.result.data;
        }
    }

    /*
     * Filter documents by tag.
     * @param tagName Tag word to search with.
     * @param exactMatch Search only documents with the whole tag.
     * @param list_doc_tags Get also tags in each document.
     * If false will also search for partial matches.
     */
    private async getDocumentsByTag(tagName: string) {
        // Changes tag in input field to this in case the tagName is different.
        this.tagFilter = tagName;

        const response = await to(
            $http<ITaggedItem[]>({
                method: "GET",
                url: "/tags/getDocs",
                params: {
                    case_sensitive: this.caseSensitive,
                    exact_search: this.exactMatch,
                    list_doc_tags: this.listDocTags,
                    name: tagName,
                },
            })
        );
        if (!response.ok) {
            return;
        }
        this.docList = response.result.data;
    }

    /*
     * Calls tag search function when Enter is pressed.
     * @param event Keyboard event.
     */
    async keyPressed(event: KeyboardEvent) {
        if (event.which === KEY_ENTER) {
            await this.getDocumentsByTag(this.tagFilter);
        }
    }

    /*
     * Calls tag search function. Has option to searching for exactly matching
     * tags or all that containg parts of the tag.
     */
    async searchClicked(tagName: string) {
        if (!this.enableSearch) {
            return;
        } else {
            await this.getDocumentsByTag(tagName);
        }
    }

    /**
     * Changes tag css class depending on whether search is enabled and
     * if it's regular or special tag.
     * @param {ITag} tag
     * @returns {string}
     */
    private tagClass(tag: ITag) {
        let classes = tagStyleClass(tag, false);
        if (this.enableSearch) {
            classes += " cursor-pointer";
        }
        return classes;
    }
}

timApp.component("taggedDocumentList", {
    bindings: {
        caseSensitive: "<",
        enableSearch: "<",
        exactMatch: "<",
        listDocTags: "<",
        tagFilter: "<",
    },
    controller: TaggedDocumentListCtrl,
    template: `
    <div class="input-group" ng-show="$ctrl.enableSearch">
        <input ng-model="$ctrl.tagFilter" name="filterField" ng-keypress="$ctrl.keyPressed($event)"
                           type="text"
                           title="Search documents by entering a tag"
                           placeholder="Search documents by entering a tag"
                           class="form-control" id="tagFilterField" autocomplete="off"
    uib-typeahead="tag as tag for tag in $ctrl.allUniqueTags | filter:$viewValue | limitTo:15 | orderBy:'name'"
                           typeahead-min-length="1">
            <span class="input-group-addon btn">
                <span class="glyphicon glyphicon-search"
                ng-click="$ctrl.searchClicked($ctrl.tagFilter)"
                title="Search documents with tag '{{$ctrl.tagFilter}}'"></span>
            </span>
    </div>
        <ul ng-if="$ctrl.docList.length > 0">
            <li ng-repeat="d in $ctrl.docList | orderBy:'title'">
                <a href="/view/{{d.path}}" title="Open {{d.title}}">{{d.title}}</a>
                <span ng-repeat="tag in d.tags" ng-click="$ctrl.searchClicked(tag.name)">
                    <span class="btn-xs" ng-class="$ctrl.tagClass(tag)"
                        title="{{$ctrl.tagToolTip}}'{{tag.name}}'">{{tag.name}}</span>
                </span>
            </li>
        </ul>
    <span ng-if="$ctrl.docList.length == 0">No documents found!</span>
    `,
});
