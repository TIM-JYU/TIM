import {IController} from "angular";
import * as createItem from "tim/directives/createItem";
import {markAsUsed} from "tim/utils";
import {showCourseListDialog} from "./courseListDialogCtrl";
import {$http, $window} from "../ngimport";
import {ICourseSettings, IItem} from "../IItem";
import {to} from "../utils";
import {timApp} from "../app";
import {showMessageDialog} from "../dialog";

markAsUsed(createItem);

export class StartCtrl implements IController {
    private creatingNew: boolean;
    private docListOpen: boolean;
    private item: IItem = $window.item;

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

    /**
     * Opens 'Available courses' dialog.
     */
    async openCourseListDialog() {
        const [err, response] = await to($http.get<ICourseSettings>(`/courses/getCourseSettings`));
        if (response) {
            void showCourseListDialog({item: this.item, settings: response.data});
        }
        if (err) {
            void showMessageDialog(`Course settings not found: ${err.data.error}`);
        }
    }
}

timApp.component("timStart", {
    controller: StartCtrl,
    template: "<div ng-transclude></div>",
    transclude: true,
});
