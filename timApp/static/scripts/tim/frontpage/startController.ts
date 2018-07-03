import {IController} from "angular";
import * as createItem from "tim/item/createItem";
import {markAsUsed} from "tim/util/utils";
import {showCourseListDialog} from "../document/course/courseListDialogCtrl";
import {$http, $window} from "../util/ngimport";
import {ICourseSettings, IItem} from "../item/IItem";
import {to} from "../util/utils";
import {timApp} from "../app";
import {showMessageDialog} from "../ui/dialog";

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
        const [err, response] = await to($http.get<ICourseSettings>(`/courses/settings`));
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
