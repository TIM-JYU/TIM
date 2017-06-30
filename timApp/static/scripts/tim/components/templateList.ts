import {timApp} from "../app";
class TemplateListCtrl {

}

timApp.component("timTemplateList", {
    bindings: {
        onSelect: "&",
        templateList: "<",
    },
    controller: TemplateListCtrl,
    template: `
    <table>
        <thead>
        <tr>
            <td>
                <b>Template name</b>
            </td>
            <td></td>
        </tr>
        </thead>
        <tr ng-repeat="template in $ctrl.templateList">
            <td>
                {{ template.name }}
            </td>
            <td>
                <button class="timButton" ng-click="$ctrl.onSelect({template: template})">Load</button>
            </td>
        </tr>
    </table>
    `,
});
