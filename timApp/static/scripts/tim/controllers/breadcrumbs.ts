import {timApp} from "tim/app";
import {$window} from "../ngimport";

export class Breadcrumbs {
    private crumbs: any;
    private item: any;

    constructor() {
        this.crumbs = $window.breadcrumbs;
        this.item = $window.item;
    }
}

timApp.component("timBreadcrumbs", {
    controller: Breadcrumbs,
    template: "<div ng-transclude></div>",
    transclude: true,
});
