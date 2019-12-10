import {IController} from "angular";
import {Binding, to} from "tim/util/utils";
import {timApp} from "../../app";
import {IItem} from "../../item/IItem";
import {showMessageDialog} from "../../ui/dialog";
import {$http} from "../../util/ngimport";

class TemplateListCtrl implements IController {
    private doc!: Binding<IItem, "<">;
    private templateList: IItem[] = [];

    private async loadTemplate(t: IItem) {
        const r = await to($http.post("/update/" + this.doc.id, {
            template_name: t.path,
        }));
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
        } else {
            window.location.reload();
        }
    }

    $onInit() {
        void this.getTemplates();
    }

    async getTemplates() {
        const response = await to($http<IItem[]>({
            method: "GET",
            url: "/getTemplates",
            params: {
                item_path: this.doc.path,
            },
        }));
        if (!response.ok) {
            return;
        }
        this.templateList = response.result.data;
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
