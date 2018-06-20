import {IController} from "angular";
import {timApp} from "tim/app";
import * as createItem from "tim/directives/createItem";
import {markAsUsed} from "tim/utils";
import {showCourseListDialog} from "./courseListDialogCtrl";
import {$window} from "../ngimport";

markAsUsed(createItem);

export class StartCtrl implements IController {
    private creatingNew: boolean;
    private docListOpen: boolean;
    private item = $window.item;

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

    /***
     * Opens course list dialog.
     */
    openCourseListDialog() {
        showCourseListDialog(this.item);
    }
}

timApp.component("timStart", {
    controller: StartCtrl,
    template: "<div ng-transclude></div>",
    transclude: true,
});
