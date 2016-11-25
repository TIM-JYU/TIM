/* globals angular, $, timLogTime */

var timApp = angular.module('timApp');

timApp.defineQuestions = function (sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
    "use strict";

    if (sc.lectureMode) {
        sc.noQuestionAutoNumbering = $window.noQuestionAutoNumbering;
    }

    sc.questionShown = false;
    sc.firstTimeQuestions = true;

    sc.showQuestion = function (questionId) {
        sc.json = "No data";
        sc.qId = questionId;

        http({
            url: '/getQuestionById',
            method: 'GET',
            params: {'question_id': sc.qId}
        })
            .success(function (data) {
                sc.json = JSON.parse(data.questionjson);
                $rootScope.$broadcast('changeQuestionTitle', {'title': sc.json.title});
                $rootScope.$broadcast("setPreviewJson", {
                    questionjson: sc.json,
                    questionId: sc.qId,
                    points: data.points,
                    expl: JSON.parse(data.expl),
                    isLecturer: sc.isLecturer
                });
            })

            .error(function () {
                $log.error("There was some error creating question to database.");
            });


        sc.lectureId = -1;
        sc.inLecture = false;

        sc.$on('postLectureId', function (event, response) {
            sc.lectureId = response;
        });

        sc.$on('postInLecture', function (event, response) {
            sc.inLecture = response;
        });

        $rootScope.$broadcast('getLectureId');
        $rootScope.$broadcast('getInLecture');
        sc.showQuestionPreview = true;
    };

    sc.showQuestionNew = function (parId, parIdNext) {
        sc.json = "No data";
        sc.questionParId = parId;
        sc.questionParIdNext = parIdNext;

        http({
            url: '/getQuestionByParId',
            method: 'GET',
            params: {'par_id': sc.questionParId, 'doc_id': sc.docId}
        })
            .success(function (data) {
                sc.json = data.questionjson;
                $rootScope.$broadcast('changeQuestionTitle', {'title': sc.json.title});
                $rootScope.$broadcast("setPreviewJson", {
                    questionjson: sc.json,
                    questionParId: sc.questionParId,
                    questionParIdNext: sc.questionParIdNext,
                    points: data.points,
                    expl: data.expl,
                    isLecturer: sc.isLecturer
                });
            })

            .error(function () {
                $log.error("Could not get question.");
            });


        sc.lectureId = -1;
        sc.inLecture = false;

        sc.$on('postLectureId', function (event, response) {
            sc.lectureId = response;
        });

        sc.$on('postInLecture', function (event, response) {
            sc.inLecture = response;
        });

        $rootScope.$broadcast('getLectureId');
        $rootScope.$broadcast('getInLecture');
        sc.showQuestionPreview = true;
    };

    // Event handler for "Add question below"
    // Opens pop-up window to create question.
    sc.addQuestion = function (e, $par) {
        var parId = sc.getParId($par);
        var parNextId = sc.getParId($par.next());
        var $newpar = sc.createNewPar();
        $par.after($newpar);
        $rootScope.$broadcast('toggleQuestion');
        $rootScope.$broadcast('newQuestion', {'par_id': parId, 'par_id_next': parNextId});
        sc.par = $par;
    };

    sc.onClick(".questionAdded", function ($this, e) {
        var question = $this;
        var questionId = question[0].getAttribute('id');
        sc.showQuestion(questionId);
        sc.par = ($(question).parent().parent());
    });

    sc.onClick(".questionAddedNew", function ($this, e) {
        var question = $this;
        var $par = $(question).parent().parent();
        var parId = $($par)[0].getAttribute('id');
        var parNextId = sc.getParId($par.next());
        sc.showQuestionNew(parId, parNextId);
        sc.par = ($(question).parent());
    });

    sc.getQuestionHtml = function (questions) {
        var questionImage = '/static/images/show-question-icon.png';
        var $questionsDiv = $("<div>", {class: 'questions'});

        // TODO: Think better way to get the ID of question.
        for (var i = 0; i < questions.length; i++) {

            var img = new Image(30, 30);
            img.src = questionImage;
            img.title = questions[i].question_title;
            var $questionDiv = $("<span>", {
                class: 'questionAdded', html: img, id: questions[i].question_id
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

    sc.getAndEditQuestions = function () {
        $log.info(sc.settings);
        $log.info($window.sessionsettings);
        var questions = $('.editlineQuestion');
        for (var i = 0; i < questions.length; i++) {
            var questionParent = $(questions[i].parentNode);
            var questionChildren = $(questionParent.children());
            var questionNumber = $(questionChildren.find($('.questionNumber')));
            var questionTitle = sc.getParAttributes(questionParent).question;
            if (questionTitle === 'Untitled') {
                questionTitle = "";
            }
            if (questionTitle.length > 10) {
                questionTitle = questionTitle.substr(0, 10) + "\r\n...";
            }
            if (questionNumber.length > 0) {
                questionNumber[0].innerHTML = (i + 1) + ")\r\n" + questionTitle;
            }
            else {
                var parContent = $(questionChildren[0]);
                questionParent.addClass('questionPar');
                parContent.addClass('questionParContent');
                var questionTitleText;
                if (sc.noQuestionAutoNumbering) {
                    questionTitleText = questionTitle;
                } else {
                    questionTitleText = (i + 1) + ")\r\n" + questionTitle;
                }
                var p = $("<p>", {class: "questionNumber", text: questionTitleText});
                parContent.append(p);
                var editLine = $(questionChildren[1]);
                parContent.before(editLine);
            }
        }
    };

    sc.getQuestions = function () {
        http.get('/questions/' + sc.docId)
            .success(function (data) {
                var pars = {};
                var questionCount = data.length;
                for (var i = 0; i < questionCount; i++) {
                    var pi = data[i].par_id;
                    if (!(pi in pars)) {
                        pars[pi] = {questions: []};
                    }

                    pars[pi].questions.push(data[i]);
                }

                Object.keys(pars).forEach(function (par_id, index) {
                    var $par = sc.getElementByParId(par_id);
                    $par.find(".questions").remove();
                    var $questionsDiv = sc.getQuestionHtml(pars[par_id].questions);
                    $par.append($questionsDiv);
                });
            });
    };

    sc.$on("getQuestions", function () {
        if (sc.firstTimeQuestions) {
            sc.getQuestions();
            sc.firstTimeQuestions = false;
        }
    });

    sc.$on("closeQuestionPreview", function () {
        sc.showQuestionPreview = false;
    });

};
