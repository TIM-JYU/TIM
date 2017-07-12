import {IController, IScope} from "angular";
import {timApp} from "tim/app";
import * as answerSheet from "tim/directives/dynamicAnswerSheet";
import {markAsUsed} from "tim/utils";
import {$http, $log, $rootScope, $window} from "../ngimport";
import {IAskedJsonJson} from "../lecturetypes";

markAsUsed(answerSheet);

/**
 * FILL WITH SUITABLE TEXT
 * @module questionPreviewController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

export class QuestionPreviewController implements IController {
    private static $inject = ["$scope"];
    private scope: IScope;
    //TODO parse json and set values from rows and columns to scope variables
    //TODO edit questionPreview.html to repeat rows and columns

    constructor(scope: IScope) {
        this.scope = scope;
        this.questionHeaders = [];
        this.answerTypes = [];
        this.dynamicAnswerSheetControl = {};
        this.isLecturer = false;
        this.questionTitle = "";

        this.scope.$on("setPreviewJson", function(event, args) {
            this.questionId = args.questionId;
            this.questionParId = args.questionParId;
            this.questionParIdNext = args.questionParIdNext;
            this.isLecturer = args.isLecturer;
            this.markup = args.markup;
            this.questionTitle = args.markup.json.questionTitle;
            this.dynamicAnswerSheetControl.createAnswer(this);
            // Tähän Texittää scope
            // ParCompiler.processAllMath(this.htmlSheet);

        });
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:questionPreviewController
     */
    /*
     this.editQuestion = function () {
     this.close();
     $rootScope.$broadcast("editQuestion", {
     "question_id": this.questionId,
     "par_id": this.questionParId,
     "par_id_next": this.questionParIdNext,
     "markup": this.markup,
     });
     };
     */

    editQuestion() {
        this.close();
        const parId = this.questionParId;
        const parNextId = this.questionParIdNext;
        const docId = this.docId;
        // $rootScope.$broadcast('toggleQuestion');
        $http<{markup: IAskedJsonJson}>({
            url: "/getQuestionByParId",
            method: "GET",
            params: {par_id: parId, doc_id: docId, edit: true},
        })
            .then((response) => {
                const data = response.data;
                if (!data.markup) {
                    return; // not a question
                }
                this.json = data.markup.json;  // TODO: näistä pitäisi päästä eroon, kaikki markupin kautta!
                this.markup = data.markup;
                // data.markup.qst = true;
                $rootScope.$broadcast("changeQuestionTitle", {questionTitle: this.json.questionTitle});
                $rootScope.$broadcast("editQuestion", {
                    par_id: parId,
                    par_id_next: parNextId,
                    markup: data.markup,
                });

            }, function() {
                console.error("Could not get question.");
            });

        // this.par = $par;
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:questionPreviewController
     */
    ask() {
        this.scope.$emit("askQuestion", {
            lecture_id: this.lectureId,
            question_id: this.questionId,
            par_id: this.questionParId,
            doc_id: this.docId,
            markup: this.markup,
        });
        this.close();
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:questionPreviewController
     */
    close() {
        this.dynamicAnswerSheetControl.closePreview();
        this.scope.$emit("closeQuestionPreview");
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:questionPreviewController
     */
    deleteQuestion() {
        const confirmDi = $window.confirm("Are you sure you want to delete this question?");
        if (confirmDi) {
            $http.post("/deleteParagraph/" + this.docId, {par: this.questionParId})
                .then(function(response) {
                    const data = response.data;
                    this.handleDelete(data, {par: this.questionParId, area_start: null, area_end: null});
                    this.$emit("closeQuestionPreview");
                    $log.info("Deleted question");
                }, function(error) {
                    this.$emit("closeQuestionPreview");
                    $log.info(error);
                });

        }
    }
}
