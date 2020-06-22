import {IController} from "angular";
import {timApp} from "../app";
import {ViewCtrl} from "../document/viewctrl";
import {getItem, IItem} from "../item/IItem";
import {isScreenSizeOrLower, Require} from "../util/utils";
import {LectureController} from "./lectureController";

class LectureMenuController implements IController {
    private vctrl?: Require<ViewCtrl>;
    private item: IItem | undefined;
    private isOpen = false;
    private lctrl!: LectureController;

    async $onInit() {
        this.lctrl = this.vctrl && this.vctrl.lectureCtrl || LectureController.createAndInit(this.vctrl);
        if (this.lctrl.lecture) {
            this.item = await getItem(this.lctrl.lecture.doc_id);
        }
        if (!isScreenSizeOrLower("md") || this.lctrl.lectureSettings.inLecture) {
            this.isOpen = true;
        }
    }
}

timApp.component("timLectureMenu", {
    controller: LectureMenuController,
    require: {
        vctrl: "?^timView",
    },
    templateUrl: "/static/templates/lectureinfo_menu.html",
});
