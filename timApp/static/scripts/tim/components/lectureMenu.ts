import {IController} from "angular";
import {timApp} from "../app";
import {LectureController} from "../controllers/lectureController";
import {getItem, IItem} from "../IItem";

class LectureMenuController implements IController {
    private lctrl: LectureController;
    private item: IItem | undefined;

    async $onInit() {
        if (this.lctrl.lecture) {
            this.item = await getItem(this.lctrl.lecture.doc_id);
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
