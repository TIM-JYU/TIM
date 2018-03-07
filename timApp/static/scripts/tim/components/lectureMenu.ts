import {timApp} from "../app";

timApp.component("timLectureMenu", {
    require: {
        lctrl: "^timLecture",
    },
    templateUrl: "/static/templates/lectureinfo_menu.html",
});
