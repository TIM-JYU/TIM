import {IController} from "angular";
import * as createItem from "tim/item/createItem";
import {markAsUsed} from "tim/util/utils";
import {timApp} from "../app";
import {showCourseListDialog} from "../document/course/courseListDialogCtrl";
import {ICourseSettings, IItem} from "../item/IItem";
import {showMessageDialog} from "../ui/dialog";
import {$http, $window} from "../util/ngimport";
import {to} from "../util/utils";

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
        const r = await to($http.get<ICourseSettings>(`/courses/settings`));
        if (r.ok) {
            void showCourseListDialog({item: this.item, settings: r.result.data});
            return;
        }
        void showMessageDialog(`Course settings not found: ${r.result.data.error}`);
    }
}

timApp.component("timStart", {
    controller: StartCtrl,
    template: "<div ng-transclude></div>",
    transclude: true,
});
