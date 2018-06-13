import {IController} from "angular";
import {timApp} from "tim/app";
import * as createItem from "tim/directives/createItem";
import {markAsUsed} from "tim/utils";

markAsUsed(createItem);

export class StartCtrl implements IController {
    private creatingNew: boolean;
    private docListOpen: boolean;

    constructor() {
        this.creatingNew = false;
        this.docListOpen = false;
    }

    $onInit() {

    }

    cancelCreate() {
        this.creatingNew = false;
    }

    enableCreate() {
        this.creatingNew = true;
    }

    enableDocList() {
        this.docListOpen = true;
    }

    disableDocList() {
        this.docListOpen = false;
    }
}

timApp.component("timStart", {
    controller: StartCtrl,
    template: "<div ng-transclude></div>",
    transclude: true,
});
