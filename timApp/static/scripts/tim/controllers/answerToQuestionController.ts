import {IController, IScope} from "angular";
import {timApp} from "tim/app";
import * as answerSheet from "tim/directives/dynamicAnswerSheet";
import {markAsUsed} from "tim/utils";
import {$http, $interval, $log, $rootScope} from "../ngimport";
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
    private barFilled;
    private progressElem: JQuery;
    private progressText: JQuery;
    private timeLeft;
    private isLecturer: boolean;
    private questionTitle: string;
    private askedTime: number;
    private endTime: number;

    constructor(scope: IScope) {
        this.scope = scope;
        this.isLecturer = false;
        this.questionTitle = "";

        this.askedTime = params.askedTime - params.clockOffset;
        this.endTime = params.askedTime + this.json.timeLimit * 1000 - params.clockOffset;
        if (this.json.timeLimit && this.endTime && !this.preview && !this.result) {
            const progress = $("<progress>", {
                max: (this.endTime - this.askedTime),
                id: "progressBar",
            });
            htmlSheet.append(progress);
            htmlSheet.append($("<span>", {
                class: "progresslabel",
                id: "progressLabel",
                text: this.json.timeLimit + " s",
            }));
        }

        this.scope.$on("setQuestionJson", (event, args) => {
            this.result = args.result;
            this.previousAnswer = args.answer;
            this.askedId = args.askedId;
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

            if (!this.preview && !this.result) {
                const $table = this.element.find(".answer-sheet-table");
                window.setTimeout(() => {
                    const $table = this.element.find(".answer-sheet-table");
                    let $input = null;
                    if (this.json.answerFieldType !== "text") {
                        $input = $table.find("input:first");
                    } else {
                        $input = $table.find("textarea:first");
                    }
                    $input[0].focus();
                }, 0);
                //
                if (!this.isText) {
                    $table.on("keyup.send", this.answerWithEnter);
                }
                const now = new Date().valueOf();
                this.timeLeft = this.endTime - now;
                this.barFilled = 0;
                if (this.endTime && this.json.timeLimit) {
                    const timeBetween = 500;
                    const maxCount = this.timeLeft / timeBetween + 5 * timeBetween;
                    this.progressElem = $("#progressBar");
                    this.progressText = $("#progressLabel");
                    this.start(timeBetween, maxCount);
                }
            }
        });
    }

    $onInit() {

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
            .then(() => {
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
                if (!markup.json) markup = {json: markup} as any; // compatibility for old
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
            doc_id: this.docId,
            markup: this.markup,
        });
    }

    askAsNewEmit() {
        this.scope.$emit("askQuestion", {
            lecture_id: this.lectureId,
            par_id: this.questionParId,
            doc_id: this.docId,
            markup: this.markup,
        });
    }

    /**
     * Use time parameter to either close question/points window or extend question end time.
     * If time is null, question/points is closed.
     * Else time is set as questions new end time.
     * Event: update_end_time
     */
    updateEndTime(time) {
        if (time !== null) {
            this.endTime = time - this.$parent.vctrl.clockOffset;
            this.progressElem.attr("max", this.endTime - this.askedTime);
        } else {
            if (!this.$parent.lctrl.isLecturer) {
                $interval.cancel(this.promise);
                this.element.empty();
                scope.$emit("closeQuestion");
            }
        }
    }

    start(timeBetween, maxCount) {
        this.promise = $interval(this.updateBar, timeBetween, maxCount);
    }

    // createAnswer ends

    /**
     * Updates progressbar and time left text
     * @memberof module:dynamicAnswerSheet
     */
    updateBar() {
        //TODO: Problem with inactive tab.
        const now = new Date().valueOf();
        if (!this.endTime || !this.promise) {
            return;
        }
        this.timeLeft = this.endTime - now;
        this.barFilled = (this.endTime - this.askedTime) - (this.endTime - now);
        this.progressElem.attr("value", (this.barFilled));
        this.progressText.text(Math.max((this.timeLeft / 1000), 0).toFixed(0) + " s");
        this.progressElem.attr("content", (Math.max((this.timeLeft / 1000), 0).toFixed(0) + " s"));
        if (this.barFilled >= this.progressElem.attr("max")) {
            $interval.cancel(this.promise);
            if (!this.$parent.lctrl.isLecturer && !this.$parent.lctrl.questionEnded) {
                this.answerToQuestion();
            } else {
                this.progressText.text("Time's up");
            }
            this.$parent.questionEnded = true;
        }
    }

    endQuestion() {
        if (!this.promise) {
            return;
        }
        $interval.cancel(this.promise);
        const max = this.progressElem.attr("max");
        this.progressElem.attr("value", max);
        this.progressText.text("Time's up");
    }

    answerWithEnter = (e) => {
        if (e.keyCode === 13) {
            $(".answer-sheet-table").off("keyup.send"); // TODO
            this.$parent.answer();
        }
    }

    /**
     * Function to create question answer and send it to server.
     * @memberof module:dynamicAnswerSheet
     */
    answerToQuestion() {

        const answers = this.getAnswers();

        if (!this.$parent.lctrl.isLecturer) {
            this.element.empty();
            $interval.cancel(this.promise);
        }
        this.scope.$emit("answerToQuestion", {answer: answers, askedId: this.$parent.askedId});
    }

    /**
     * Closes question window and clears updateBar interval.
     * If user is lecturer, also closes answer chart window.
     * @memberof module:dynamicAnswerSheet
     */
    closeQuestion() {
        $interval.cancel(this.promise);
        this.element.empty();
        this.scope.$emit("closeQuestion");
        if (this.$parent.isLecturer) {
            $rootScope.$broadcast("closeAnswerSheetForGood");
        }
    }
}
