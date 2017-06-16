
import $ from "jquery";
import {$http, $log, $rootScope, $window} from "../../ngimport";
import {getElementByParId, getParId} from "./parhelpers";
import {onClick} from "./eventhandlers";
export function defineQuestions(sc) {
    "use strict";

    if (sc.lectureMode) {
        sc.noQuestionAutoNumbering = $window.noQuestionAutoNumbering;
    }

    sc.questionShown = false;
    sc.firstTimeQuestions = true;

    sc.showQuestion = function(questionId) {
        sc.markup = {};
        sc.markup.json = "No data";
        sc.qId = questionId;

        $http<{questionjson: string, points: any, expl: string}>({
            url: "/getQuestionById",
            method: "GET",
            params: {question_id: sc.qId},
        }).then(function(response) {
            sc.markup.json = JSON.parse(response.data.questionjson);
            sc.markup.points = response.data.points;
            sc.markup.expl = response.data.expl;
            $rootScope.$broadcast("changeQuestionTitle", {questionTitle: sc.json.questionTitle});
            $rootScope.$broadcast("setPreviewJson", {
                markup: sc.markup,
                questionId: sc.qId,
                isLecturer: sc.isLecturer,
            });
        }, function() {
            $log.error("There was some error creating question to database.");
        });

        sc.lectureId = -1;
        sc.inLecture = false;

        sc.$on("postLectureId", function(event, response) {
            sc.lectureId = response;
        });

        sc.$on("postInLecture", function(event, response) {
            sc.inLecture = response;
        });

        $rootScope.$broadcast("getLectureId");
        $rootScope.$broadcast("getInLecture");
        sc.showQuestionPreview = true;
    };

    sc.showQuestionNew = function(parId, parIdNext) {
        sc.markup = {};
        sc.markup.json = "No data";
        sc.questionParId = parId;
        sc.questionParIdNext = parIdNext;

        $http<{markup: string}>({
            url: "/getQuestionByParId",
            method: "GET",
            params: {par_id: sc.questionParId, doc_id: sc.docId},
        }).then(function(response) {
            sc.markup = response.data.markup;
            $rootScope.$broadcast("changeQuestionTitle", {questionTitle: sc.markup.json.questionTitle});
            $rootScope.$broadcast("setPreviewJson", {
                markup: sc.markup,
                questionParId: sc.questionParId,
                questionParIdNext: sc.questionParIdNext,
                isLecturer: sc.isLecturer,
            });
        }, function() {
            $log.error("Could not get question.");
        });

        sc.lectureId = -1;
        sc.inLecture = false;

        sc.$on("postLectureId", function(event, response) {
            sc.lectureId = response;
        });

        sc.$on("postInLecture", function(event, response) {
            sc.inLecture = response;
        });

        $rootScope.$broadcast("getLectureId");
        $rootScope.$broadcast("getInLecture");
        sc.showQuestionPreview = true;
    };

    // Event handler for "Add question below"
    // Opens pop-up window to create question.
    sc.addQuestionQst = function(e, $par) {
        const parId = getParId($par);
        const parNextId = parId; // getParId($par.next());
        const $newpar = sc.createNewPar();
        $par.before($newpar);
        $rootScope.$broadcast("toggleQuestion");
        $rootScope.$broadcast("newQuestion", {par_id: parId, par_id_next: parNextId, qst: true});
        sc.par = $par;
    };

    sc.editQst = async function(e, $par) {
        const parId = getParId($par);
        const parNextId = getParId($par.next());
        sc.par = $par;
        // $rootScope.$broadcast('toggleQuestion');
        try {
            var response = await $http<{markup: {json: string}}>({
                url: "/getQuestionByParId",
                method: "GET",
                params: {par_id: parId, doc_id: sc.docId, edit: true},
            });
        } catch (e) {
            $log.error("Could not get question.");
            return;
        }
        const data = response.data;
        if (!data.markup) {
            return; // not a question
        }
        sc.json = data.markup.json;  // TODO: näistä pitäisi päästä eroon, kaikki markupin kautta!
        sc.markup = data.markup;
        // data.markup.qst = true;
        $rootScope.$broadcast("changeQuestionTitle", {questionTitle: sc.json.questionTitle});
        $rootScope.$broadcast("editQuestion", {
            par_id: parId,
            par_id_next: parNextId,
            markup: data.markup,
        });
    };

    // Event handler for "Add question below"
    // Opens pop-up window to create question.
    sc.addQuestion = function(e, $par) {
        const parId = getParId($par);
        const parNextId = getParId($par.next());
        const $newpar = sc.createNewPar();
        $par.after($newpar);
        $rootScope.$broadcast("toggleQuestion");
        $rootScope.$broadcast("newQuestion", {par_id: parId, par_id_next: parNextId});
        sc.par = $par;
    };

    onClick(".questionAdded", function($this, e) {
        const question = $this;
        const questionId = question[0].getAttribute("id");
        sc.showQuestion(questionId);
        sc.par = ($(question).parent().parent());
    });

    onClick(".questionAddedNew", function($this, e) {
        const question = $this;
        const $par = $(question).parent().parent();
        const parId = $($par)[0].getAttribute("id");
        const parNextId = getParId($par.next());
        sc.showQuestionNew(parId, parNextId);
        sc.par = ($(question).parent());
    });

    sc.getQuestionHtml = function(questions) {
        const questionImage = "/static/images/show-question-icon.png";
        const $questionsDiv = $("<div>", {class: "questions"});

        // TODO: Think better way to get the ID of question.
        for (let i = 0; i < questions.length; i++) {

            const img = new Image(30, 30);
            img.src = questionImage;
            img.title = questions[i].question_title;
            const $questionDiv = $("<span>", {
                class: "questionAdded", html: img, id: questions[i].question_id,
            });
            /*
             var img = new Span(30, 30); // TODO: katso toimiiko
             // img.src = questionImage;
             img.title = questions[i].question_title;
             img.class = "glyphicon-question-sign";
             var $questionDiv = $("<span>", {
             class: 'questionAdded', html: img, id: questions[i].question_id
             });
             */
            $questionsDiv.append($questionDiv);
        }
        return $questionsDiv;
    };

    sc.processQuestions = function() {
        const questions = $(".questionPar");
        let n = 1;
        let separator = ")";
        if (sc.showQuestions()) {
            for (let i = 0; i < questions.length; i++) {
                const $par = questions.eq(i);
                const questionChildren = $par.children();
                const questionNumber = questionChildren.find(".questionNumber");
                // var questionTitle = getParAttributes($par).question;
                let questionTitle = questionNumber[0].innerHTML;
                if (questionTitle.length > 10) {
                    questionTitle = questionTitle.substr(0, 10) + "\r\n...";
                }
                const nt = parseInt(questionTitle);
                let nr = "";
                if (isNaN(nt)) {
                    nr = (n) + "" + separator + "\r\n";
                } else {
                    n = nt;
                    const nrt = "" + n;
                    if (questionTitle.length > nrt.length) {
                        separator = questionTitle[nrt.length];
                    }
                }
                if (questionNumber[0] && questionNumber[0].innerHTML) {
                    if (sc.noQuestionAutoNumbering) {
                        questionNumber[0].innerHTML = questionTitle;
                    } else {
                        questionNumber[0].innerHTML = nr + questionTitle;
                        n++;
                    }
                }
            }
        } else {
            questions.hide();
        }
    };

    sc.getQuestions = async function() {
        const response = await $http.get<Array<{par_id: string}>>("/questions/" + sc.docId);
        const data = response.data;
        const pars = {};
        const questionCount = data.length;
        for (let i = 0; i < questionCount; i++) {
            const pi = data[i].par_id;
            if (!(pi in pars)) {
                pars[pi] = {questions: []};
            }

            pars[pi].questions.push(data[i]);
        }

        Object.keys(pars).forEach(function(par_id, index) {
            const $par = getElementByParId(par_id);
            $par.find(".questions").remove();
            const $questionsDiv = sc.getQuestionHtml(pars[par_id].questions);
            $par.append($questionsDiv);
        });
    };

    sc.$on("getQuestions", function() {
        if (sc.firstTimeQuestions) {
            sc.getQuestions();
            sc.firstTimeQuestions = false;
        }
    });

    sc.$on("closeQuestionPreview", function() {
        sc.showQuestionPreview = false;
    });

}
