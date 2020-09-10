import {IController} from "angular";
import {timApp} from "../../app";

export class QuestionMatrixController implements IController {
    $onInit() {}
}

timApp.component("timQuestionMatrix", {
    bindings: {},
    controller: QuestionMatrixController,
    require: {
        qctrl: "^timEditQuestion",
    },
    templateUrl: "/static/templates/matrix.html",
});
