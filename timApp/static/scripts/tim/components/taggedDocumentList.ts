/*
 * A component that shows and filters documents based on their tags.
 */

import {IController} from "angular";
import {IItem, ITaggedItem} from "../IItem";
import {to} from "tim/utils";
import {timApp} from "../app";
import {showMessageDialog} from "../dialog";
import {$http, $window} from "../ngimport";

class TaggedDocumentListCtrl implements IController {
    public tagFilter: string;
    public exactMatch: boolean; // Get only documents with exactly matching tag.
    public showSearch: boolean;
    private doc: IItem;
    private docList: ITaggedItem[];
    private allUniqueTags: string[];

    private async loadTemplate(t: IItem) {
        const [err] = await to($http.post("/update/" + this.doc.id, {
            template_name: t.path,
        }));
        if (err) {
            await showMessageDialog(err.data.error);
        } else {
            $window.location.reload();
        }
    }

   async $onInit() {
        await this.getAllTags();
        void this.getDocumentsByTag(this.tagFilter, this.exactMatch);
    }

    private async getAllTags() {
        const [err, response] = await to($http.get<string[]>(`/tags/getAllTags`, {}));
        if (response) {
            this.allUniqueTags = response.data;
        }
    }

    /*
     * Filter documents by tag.
     * @param tagName: Tag word to search with.
     * @param exactMatch: Search only documents with the whole tag.
     * If false will also search for partial matches.
     */
    private async getDocumentsByTag(tagName: string, exactMatch: boolean) {
        const response = await $http<ITaggedItem[]>({
            method: "GET",
            url: "/tags/getDocs",
            params: {
                exact_search: exactMatch,
                tag: tagName,
            },
        });
        this.docList = response.data;
    }

    /*
     * Calls tag adding function when Enter is pressed.
     * @param event Keyboard event.
     */
    async chatEnterPressed(event: KeyboardEvent) {
        if (event.which === 13) {
            await this.getDocumentsByTag(this.tagFilter, this.exactMatch);
        }
    }

    /*
     * Calls tag adding function. Has option to searching for exactly matching
     * tags or all that containg parts of the tag.
     */
    async searchClicked() {
        await this.getDocumentsByTag(this.tagFilter, this.exactMatch);
    }
}

timApp.component("taggedDocumentList", {
    bindings: {
        doc: "<",
        closeFn: "&",
        showSearch: "<",
        exactMatch: "<",
        tagFilter: "@",
    },
    controller: TaggedDocumentListCtrl,
    template: `
<bootstrap-panel title="List of documents" show-close="true"
    ng-switch on="$ctrl.docList.length" close-fn="$ctrl.closeFn()">
    <div>
    <div class="input-group" ng-if="$ctrl.showSearch">
        <input ng-model="$ctrl.tagFilter" name="filterField" ng-keypress="$ctrl.chatEnterPressed($event)"
                           type="text" title="Search documents with a tag"
                           placeholder="Search documents by entering a tag"
                           class="form-control" id="tagFilterField" autocomplete="off"
                           uib-typeahead="tag as tag for tag in $ctrl.allUniqueTags | filter:$viewValue | limitTo:8"
                           typeahead-min-length="0">
        <span class="input-group-addon btn btn-default"
                            ng-click="$ctrl.searchClicked()"
                            title="Search other documents with a tag">
            <i class="glyphicon glyphicon-search"></i>
        </span>
    </div>
    <ul ng-switch-default>
        <li ng-repeat="doc in $ctrl.docList">
            <a href="view/{{doc.path}}">{{doc.title}}</a>
            <span ng-repeat="tag in doc.tags" ><span class="btn-primary btn-xs">{{tag.tag}}</span>&nbsp;</span>
        </li>
    </ul>
    <span ng-switch-when="0">No documents found!</span>
    </div>
</bootstrap-panel>
    `,
});
