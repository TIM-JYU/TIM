import {IController} from "angular";
import {timApp} from "../app";
import {getItem, IItem} from "../item/IItem";
import {isSmallScreen, Require} from "../util/utils";
import {LectureController} from "./lectureController";

class LectureMenuController implements IController {
    private lctrl!: Require<LectureController>;
    private item: IItem | undefined;
    private isOpen = false;

    async $onInit() {
        if (this.lctrl.lecture) {
            this.item = await getItem(this.lctrl.lecture.doc_id);
        }
        if (!isSmallScreen() || this.lctrl.lectureSettings.inLecture) {
            this.isOpen = true;
        }
    }
}

timApp.component("timLectureMenu", {
    controller: LectureMenuController,
    require: {
        lctrl: "^timLecture",
    },
    templateUrl: "/static/templates/lectureinfo_menu.html",
});
