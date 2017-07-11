import {IController} from "angular";
import {timApp} from "tim/app";
import * as createItem from "tim/directives/createItem";
import {markAsUsed} from "tim/utils";

markAsUsed(createItem);

export class StartCtrl implements IController {
    private creatingNew: boolean;

    constructor() {
        this.creatingNew = false;
    }

    cancelCreate() {
        this.creatingNew = false;
    }

    enableCreate() {
        this.creatingNew = true;
    }
}

timApp.component("timStart", {
    controller: StartCtrl,
    template: "<div ng-transclude></div>",
    transclude: true,
});
