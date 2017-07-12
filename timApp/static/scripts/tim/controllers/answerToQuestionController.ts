import {IController, IScope} from "angular";
import {timApp} from "tim/app";
import * as answerSheet from "tim/directives/dynamicAnswerSheet";
import {markAsUsed} from "tim/utils";
import {$http, $log, $rootScope} from "../ngimport";
import {IAskedQuestion, IAskedJson, IAskedJsonJson} from "../lecturetypes";

markAsUsed(answerSheet);

/**
 * Created by hajoviin on 22.4.2015
 * FILL WITH SUITABLE TEXT
 * @module answerToQuestionController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

export class AnswerToQuestionController implements IController {
    private scope: IScope;
    private questionHeaders: {}[];
    private answerTypes: {}[];
    private isLecturer: boolean;
    private questionTitle: string;
    private isAsking: boolean;
    constructor(scope: IScope) {
        this.scope = scope;
        this.questionHeaders = [];
        this.answerTypes = [];
        this.dynamicAnswerSheetControl = {};
        this.isLecturer = false;
        this.questionTitle = "";

        this.scope.$on("setQuestionJson", (event, args) => {
            this.result = args.result;
            this.previousAnswer = args.answer;
            this.askedId = args.askedId;
            this.questionId = args.questionId;
            this.questionParId = args.questionParId;
            this.isLecturer = args.isLecturer;
            this.markup = args.markup;
            this.questionTitle = args.markup.json.questionTitle;
            this.askedTime = args.askedTime;
            this.clockOffset = args.clockOffset;
            this.questionEnded = false;
            this.answered = false;
            this.buttonText = this.markup.button || this.markup.buttonText || "Answer";
            this.dynamicAnswerSheetControl.createAnswer(this);
            if (args.isAsking) {
                this.isAsking = true;
            }
        });
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:answerToQuestionController
     */
    answer() {
        this.dynamicAnswerSheetControl.answerToQuestion();
        this.answered = true;
        if (this.isLecturer) {
            $rootScope.$broadcast("lecturerAnswered");
        }
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:answerToQuestionController
     */
    close(callback) {
        if (this.isLecturer) {
            this.stopQuestion(callback);
        }
        if (this.result) {
            this.scope.$emit("pointsClosed", this.askedId);
        }
        this.dynamicAnswerSheetControl.closeQuestion();
    }

    stopQuestion(callback) {
        $http({
            url: "/stopQuestion",
            method: "POST",
            params: {
                asked_id: this.askedId,
                lecture_id: this.lectureId,
            },
        })
            .then(function() {
                this.scope.$emit("questionStopped");
                this.questionEnded = true;
                this.dynamicAnswerSheetControl.endQuestion();
                $log.info("Question ", this.askedId, " stopped");
                if (callback) {
                    callback();
                }
            }, () => {
                $log.info("Failed to stop question");
            });
    }

    /**
     *
     */
    showAnswers() {
        this.scope.$emit("showAnswers", true);
    }

    reAsk() {
        this.close(this.reAskEmit);
    }

    askAsNew() {
        this.close(this.askAsNewEmit);
    }

    edit() {
        $http<IAskedQuestion>({
            url: "/getAskedQuestionById",
            method: "GET",
            params: {asked_id: this.askedId},
        })
            .then((response) => {
                let markup: IAskedJsonJson = JSON.parse(response.data.json);
                if (!markup.json) markup = {json: markup} as any; // compability for old
                markup.points = response.data.points;
                $rootScope.$broadcast("changeQuestionTitle", {questionTitle: markup.json.questionTitle});
                $rootScope.$broadcast("editQuestion", {
                    asked_id: this.askedId,
                    markup,
                });
            }, () => {
                $log.info("There was some error getting question.");
            });
    }

    showPoints() {
        $http<{points, expl?}>({
            url: "/showAnswerPoints",
            method: "POST",
            params: {
                asked_id: this.askedId,
                lecture_id: this.lectureId,
                current_question_id: this.askedId,
            },
        })
            .then((response) => {
                this.current_points_id = this.askedId;
                this.result = true;
                this.points = response.data.points;
                if (response.data.expl) this.expl = JSON.parse(response.data.expl);
                this.dynamicAnswerSheetControl.createAnswer(this);
            }, () => {
                $log.info("Could not show points to students.");
            });
    }

    reAskEmit() {
        this.scope.$emit("askQuestion", {
            lecture_id: this.lectureId,
            asked_id: this.askedId,
            par_id: this.questionParId,
            question_id: this.questionId,
            doc_id: this.docId,
            markup: this.markup,
        });
    }

    askAsNewEmit() {
        this.scope.$emit("askQuestion", {
            lecture_id: this.lectureId,
            question_id: this.questionId,
            par_id: this.questionParId,
            doc_id: this.docId,
            markup: this.markup,
        });
    }
}
