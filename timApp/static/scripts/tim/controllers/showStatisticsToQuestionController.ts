import angular, {IRootElementService, IScope} from "angular";
import {IController} from "angular";
import {timApp} from "tim/app";
import * as chart from "tim/directives/showChartDirective";
import {markAsUsed} from "tim/utils";
import {ParCompiler} from "../services/parCompiler";

markAsUsed(chart);

/**
 * Created by hajoviin on 6.5.2015.
 * FILL WITH SUITABLE TEXT
 * @module showStatisticsToQuestionController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

export class ShowStatisticsToQuestionController implements IController {
    private static $inject = ["$scope", "$element"];
    private scope: IScope;
    private canvas: string;
    private questionTitle: string;
    private lecturerAnswered: boolean;
    private element: IRootElementService;
    private dynamicAnswerShowControl: any;

    constructor(scope: IScope, element: IRootElementService) {
        this.scope = scope;
        this.element = element;
        this.dynamicAnswerShowControl = {};
        this.canvas = "";
        this.questionTitle = "";
        this.lecturerAnswered = false;

        this.scope.$on("closeAnswerSheetForGood", () => {
            this.scope.$emit("closeAnswerShow");
            this.dynamicAnswerShowControl.close();
        });

        this.scope.$on("lecturerAnswered", () => {
            this.lecturerAnswered = true;
            this.scope.$emit("showAnswers", true);
        });

        /**
         * Adds answer to statistic directive
         * @memberof module:showStatisticsToQuestionController
         */
        this.scope.$on("putAnswers", (event, answer) => {
            this.dynamicAnswerShowControl.addAnswer(answer.answers);
        });

        /**
         * Creates chart based on question json.
         * @memberof module:showStatisticsToQuestionController
         */
        this.scope.$on("createChart", async (event, question) => {
            this.lecturerAnswered = false;
            await this.dynamicAnswerShowControl.createChart(question);
            this.questionTitle = question.questionText;

            window.setTimeout(() => { // give time to html to change
                ParCompiler.processAllMath(this.element.parent());
            }, 200);

        });
    }

    $onInit() {

    }

    /**
     * Closes statistic window
     * @memberof module:showStatisticsToQuestionController
     */
    close() {
        this.scope.$emit("closeAnswerShow");
        if (this.lecturerAnswered) {
            this.dynamicAnswerShowControl.close();
        }
    }

    hide() {
        this.scope.$emit("closeAnswerShow");
    }
}
