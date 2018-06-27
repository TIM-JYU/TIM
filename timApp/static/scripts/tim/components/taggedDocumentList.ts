/**
 * A component that shows and filters documents based on their tags.
 *
 * Contains a search field with visibility and exact/partial word search.
 */

import {IController} from "angular";
import {ITag, ITaggedItem, TagType} from "../IItem";
import {to} from "tim/utils";
import {timApp} from "../app";
import {$http, $localStorage} from "../ngimport";
import {ngStorage} from "ngstorage";

class TaggedDocumentListCtrl implements IController {
    public tagFilter: string;
    public exactMatch: boolean; // Get only documents with exactly matching tag.
    public enableSearch: boolean;
    private docList: ITaggedItem[];
    private allUniqueTags: string[];
    private listDocTags: boolean;
    private tagToolTip: string;
    private storage: ngStorage.StorageService & {tagFilterStorage: null | string};

   async $onInit() {
        if (this.enableSearch) {
            this.tagToolTip = "Search documents with tag ";
        } else {
            this.tagToolTip = "Document has tag ";
        }
        this.storage = $localStorage.$default({
            tagFilterStorage: null,
        });
        if (this.enableSearch && this.storage.tagFilterStorage) {
            this.tagFilter = this.storage.tagFilterStorage;
        } else {
            this.tagFilter = "";
        }
        await this.getAllTags();
        void this.getDocumentsByTag(this.tagFilter, this.exactMatch, this.listDocTags);
    }

    $onDestroy() {
        this.storage.tagFilterStorage = this.tagFilter;
    }

    private async getAllTags() {
        const [err, response] = await to($http.get<string[]>(`/tags/getAllTags`));
        if (response) {
            this.allUniqueTags = response.data;
        }
    }

    /*
     * Filter documents by tag.
     * @param tagName Tag word to search with.
     * @param exactMatch Search only documents with the whole tag.
     * @param list_doc_tags Get also tags in each document.
     * If false will also search for partial matches.
     */
    private async getDocumentsByTag(tagName: string, exactMatch: boolean, listDocTags: boolean) {
        // Changes tag in input field to this in case the tagName is different.
        this.tagFilter = tagName;

        const response = await $http<ITaggedItem[]>({
            method: "GET",
            url: "/tags/getDocs",
            params: {
                exact_search: exactMatch,
                list_doc_tags: listDocTags,
                name: tagName,
            },
        });
        this.docList = response.data;

    }

    /*
     * Calls tag adding function when Enter is pressed.
     * @param event Keyboard event.
     */
    async keyPressed(event: KeyboardEvent) {
        if (event.which === 13) {
            await this.getDocumentsByTag(this.tagFilter, this.exactMatch, this.listDocTags);
        }
    }

    /*
     * Calls tag adding function. Has option to searching for exactly matching
     * tags or all that containg parts of the tag.
     */
    async searchClicked(tag: string) {
        if (!this.enableSearch) {
            return;
        } else {
            await this.getDocumentsByTag(tag, this.exactMatch, this.listDocTags);
        }
    }

    /**
     * Changes tag css style depending on whether search is enabled and
     * if it's regular or special tag.
     * @param {ITag} tag
     * @returns {string}
     */
    private tagStyle(tag: ITag) {
        let style = "";
        if (this.enableSearch) {
            style += "cursor-pointer ";
        }
        if (tag.type === TagType.Regular) {
            style += "btn-primary";
        } else {
            style += "btn-success";
        }
        return style;
    }
}

timApp.component("taggedDocumentList", {
    bindings: {
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
                <span ng-repeat="tag in d.tags">
                    <span class="btn-xs" ng-class="$ctrl.tagStyle(tag)"
                        ng-click="$ctrl.searchClicked(tag.name)"
                        title="{{$ctrl.tagToolTip}}'{{tag.name}}'">{{tag.name}}</span>
                </span>
            </li>
        </ul>
    <span ng-if="$ctrl.docList.length == 0">No documents found!</span>
    `,
});
