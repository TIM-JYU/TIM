
import $ from "jquery";
export function defineQuestions(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
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

        http({
            url: "/getQuestionById",
            method: "GET",
            params: {question_id: sc.qId},
        })
            .success(function(data) {
                sc.markup.json = JSON.parse(data.questionjson);
                sc.markup.points = data.points;
                sc.markup.expl = data.expl;
                $rootScope.$broadcast("changeQuestionTitle", {questionTitle: sc.json.questionTitle});
                $rootScope.$broadcast("setPreviewJson", {
                    markup: sc.markup,
                    questionId: sc.qId,
                    isLecturer: sc.isLecturer,
                });
            })

            .error(function() {
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

        http({
            url: "/getQuestionByParId",
            method: "GET",
            params: {par_id: sc.questionParId, doc_id: sc.docId},
        })
            .success(function(data) {
                sc.markup = data.markup;
                $rootScope.$broadcast("changeQuestionTitle", {questionTitle: sc.markup.json.questionTitle});
                $rootScope.$broadcast("setPreviewJson", {
                    markup: sc.markup,
                    questionParId: sc.questionParId,
                    questionParIdNext: sc.questionParIdNext,
                    isLecturer: sc.isLecturer,
                });
            })

            .error(function() {
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
        let parId = sc.getParId($par);
        let parNextId = parId; // sc.getParId($par.next());
        let $newpar = sc.createNewPar();
        $par.before($newpar);
        $rootScope.$broadcast("toggleQuestion");
        $rootScope.$broadcast("newQuestion", {par_id: parId, par_id_next: parNextId, qst: true});
        sc.par = $par;
    };

    sc.editQst = function(e, $par) {
        let parId = sc.getParId($par);
        let parNextId = sc.getParId($par.next());
        // $rootScope.$broadcast('toggleQuestion');
        http({
            url: "/getQuestionByParId",
            method: "GET",
            params: {par_id: parId, doc_id: sc.docId, edit: true},
        })
            .success(function(data) {
                if ( !data.markup ) return; // not a question
                sc.json = data.markup.json;  // TODO: näistä pitäisi päästä eroon, kaikki markupin kautta!
                sc.markup = data.markup;
                // data.markup.qst = true;
                $rootScope.$broadcast("changeQuestionTitle", {questionTitle: sc.json.questionTitle});
                $rootScope.$broadcast("editQuestion", {
                    par_id: parId,
                    par_id_next: parNextId,
                    markup: data.markup,
                });

            })

            .error(function() {
                $log.error("Could not get question.");
            });

        sc.par = $par;
    };

    // Event handler for "Add question below"
    // Opens pop-up window to create question.
    sc.addQuestion = function(e, $par) {
        let parId = sc.getParId($par);
        let parNextId = sc.getParId($par.next());
        let $newpar = sc.createNewPar();
        $par.after($newpar);
        $rootScope.$broadcast("toggleQuestion");
        $rootScope.$broadcast("newQuestion", {par_id: parId, par_id_next: parNextId});
        sc.par = $par;
    };

    sc.onClick(".questionAdded", function($this, e) {
        let question = $this;
        let questionId = question[0].getAttribute("id");
        sc.showQuestion(questionId);
        sc.par = ($(question).parent().parent());
    });

    sc.onClick(".questionAddedNew", function($this, e) {
        let question = $this;
        let $par = $(question).parent().parent();
        let parId = $($par)[0].getAttribute("id");
        let parNextId = sc.getParId($par.next());
        sc.showQuestionNew(parId, parNextId);
        sc.par = ($(question).parent());
    });

    sc.getQuestionHtml = function(questions) {
        let questionImage = "/static/images/show-question-icon.png";
        let $questionsDiv = $("<div>", {class: "questions"});

        // TODO: Think better way to get the ID of question.
        for (let i = 0; i < questions.length; i++) {

            let img = new Image(30, 30);
            img.src = questionImage;
            img.title = questions[i].question_title;
            let $questionDiv = $("<span>", {
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
        let questions = $(".questionPar");
        let n = 1;
        let separator = ")";
        if (sc.showQuestions()) {
            for (let i = 0; i < questions.length; i++) {
                let $par = questions.eq(i);
                let questionChildren = $par.children();
                let questionNumber = questionChildren.find(".questionNumber");
                // var questionTitle = sc.getParAttributes($par).question;
                let questionTitle = questionNumber[0].innerHTML;
                if (questionTitle.length > 10) {
                    questionTitle = questionTitle.substr(0, 10) + "\r\n...";
                }
                let nt = parseInt(questionTitle);
                let nr = "";
                if ( isNaN(nt) ) nr = (n) + "" + separator + "\r\n";
                else {
                    n = nt;
                    let nrt = "" + n;
                    if ( questionTitle.length  > nrt.length )
                        separator = questionTitle[nrt.length];
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

    sc.getQuestions = function() {
        http.get("/questions/" + sc.docId)
            .success(function(data) {
                let pars = {};
                let questionCount = data.length;
                for (let i = 0; i < questionCount; i++) {
                    let pi = data[i].par_id;
                    if (!(pi in pars)) {
                        pars[pi] = {questions: []};
                    }

                    pars[pi].questions.push(data[i]);
                }

                Object.keys(pars).forEach(function(par_id, index) {
                    let $par = sc.getElementByParId(par_id);
                    $par.find(".questions").remove();
                    let $questionsDiv = sc.getQuestionHtml(pars[par_id].questions);
                    $par.append($questionsDiv);
                });
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
