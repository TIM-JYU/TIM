import {IController} from "angular";
import {to} from "tim/utils";
import {timApp} from "../app";
import {showMessageDialog} from "../dialog";
import {IItem} from "../IItem";
import {$http, $window} from "../ngimport";

class TaggedDocumentListCtrl implements IController {
    private doc: IItem;
    private docList: IItem[];

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
        void this.getCourses();
    }

    async getCourses() {
        const response = await $http<IItem[]>({
            method: "GET",
            url: "/tags/getDocs",
            params: {
                tag: "kurssi",
            },
        });
        this.docList = response.data;
    }
}

timApp.component("taggedDocumentList", {
    bindings: {
        doc: "<",
    },
    controller: TaggedDocumentListCtrl,
    template: `
<bootstrap-panel title="List of documents" show-close="true" ng-switch on="$ctrl.docList.length">
    <ul ng-switch-default>
        <li ng-repeat="doc in $ctrl.docList">
            <a href="view/{{doc.path}}">{{doc.title}}</a>
        </li>
    </ul>
    <span ng-switch-when="0">No documents found!</span>
</bootstrap-panel>
    `,
});
