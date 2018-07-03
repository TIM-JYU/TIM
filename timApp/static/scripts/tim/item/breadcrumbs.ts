import {IController} from "angular";
import {timApp} from "tim/app";
import {$window} from "../util/ngimport";

export class Breadcrumbs implements IController {
    private crumbs: any;
    private item: any;

    constructor() {
        this.crumbs = $window.breadcrumbs;
        this.item = $window.item;
    }

    $onInit() {

    }
}

timApp.component("timBreadcrumbs", {
    controller: Breadcrumbs,
    template: "<div ng-transclude></div>",
    transclude: true,
});
