import {IController} from "angular";
import {to} from "tim/utils";
import {timApp} from "../app";
import {showMessageDialog} from "../dialog";
import {IItem} from "../IItem";
import {$http, $window} from "../ngimport";

class TemplateListCtrl implements IController {
    private doc: IItem;
    private templateList: IItem[];

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
        void this.getTemplates();
    }

    async getTemplates() {
        const response = await $http<IItem[]>({
            method: "GET",
            url: "/getTemplates",
            params: {
                item_path: this.doc.path,
            },
        });
        this.templateList = response.data;
    }
}

timApp.component("timTemplateList", {
    bindings: {
        doc: "<",
    },
    controller: TemplateListCtrl,
    template: `
<bootstrap-panel title="Choose a template" show-close="true" ng-switch on="$ctrl.templateList.length">
    <ul ng-switch-default>
        <li ng-repeat="template in $ctrl.templateList">
            {{template.title}}
            <button class="timButton btn-xs" ng-click="$ctrl.loadTemplate(template)">Load</button>
        </li>
    </ul>
    <span ng-switch-when="0">No templates found.</span>
</bootstrap-panel>
    `,
});
