import {IController} from "angular";
import {timApp} from "../app";
import {LectureController} from "./lectureController";
import {getItem, IItem} from "../item/IItem";
import {Require} from "../util/utils";

class LectureMenuController implements IController {
    private lctrl!: Require<LectureController>;
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
