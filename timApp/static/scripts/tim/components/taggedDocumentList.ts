/*
 * A component that shows and filters documents based on their tags.
 */

import {IController} from "angular";
import {to} from "tim/utils";
import {timApp} from "../app";
import {showMessageDialog} from "../dialog";
import {IItem} from "../IItem";
import {$http, $window} from "../ngimport";

class TaggedDocumentListCtrl implements IController {
    private doc: IItem;
    private docList: IItem[];
    private tagFilter: string;

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

    $onInit() {
        this.tagFilter = "kurssi";
        void this.getDocumentsByTag(this.tagFilter);
    }

    /*
     * Filter documents by tag.
     * @param tag
     */
    async getDocumentsByTag(tag: string) {
        const response = await $http<IItem[]>({
            method: "GET",
            url: "/tags/getDocs",
            params: {
                tag: tag,
            },
        });
        this.docList = response.data;
    }

    /*
     * Calls tag adding function when Enter is pressed.
     * @param event Keyboard event.
     */
    public chatEnterPressed(event: KeyboardEvent) {
        if (event.which === 13) {
            this.getDocumentsByTag(this.tagFilter);
        }
    }

    /*
     * Calls tag adding function.
     */
    public searchClicked() {
        this.getDocumentsByTag(this.tagFilter);
    }
}

timApp.component("taggedDocumentList", {
    bindings: {
        doc: "<",
        closeFn: "&",
    },
    controller: TaggedDocumentListCtrl,
    template: `
<bootstrap-panel title="List of documents" show-close="true"
    ng-switch on="$ctrl.docList.length" close-fn="$ctrl.closeFn()">
    <div>
    <div class="input-group">
        <input ng-model="$ctrl.tagFilter" name="filterField" ng-keypress="$ctrl.chatEnterPressed($event)"
                           type="text" title="Search documents with a tag"
                           class="form-control" id="tagFilterField" autocomplete="off">
        <span class="input-group-addon btn btn-default" ng-click="$ctrl.searchClicked()">
            <i class="glyphicon glyphicon-search"></i>
        </span>
    </div>
    <ul ng-switch-default>
        <li ng-repeat="doc in $ctrl.docList">
            <a href="view/{{doc.path}}">{{doc.title}}</a>
        </li>
    </ul>
    <span ng-switch-when="0">No documents found!</span>
    </div>
</bootstrap-panel>
    `,
});
