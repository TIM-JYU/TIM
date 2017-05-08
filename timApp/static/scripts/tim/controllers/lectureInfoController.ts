import {timApp} from "tim/app";
import angular from "angular";
import * as showChart from "tim/directives/showChartDirective";
import {fixQuestionJson} from "tim/directives/dynamicAnswerSheet";
import $ from "jquery";
import {markAsUsed} from "tim/utils";
import {ParCompiler} from "../services/parCompiler";
import moment from "moment";

markAsUsed(showChart);

/**
 * Created by hajoviin on 11.5.2015.
 * Handles the controls of lecture info page.
 * @module lectureInfoController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @author Veli-Pekka Oksanen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

timApp.controller('LectureInfoController', ['$rootScope', '$scope', '$http', '$window', '$log', '$element', function ($rootScope, $scope, $http, $window, $log, $element) {
    "use strict";
    $scope.item = $window.item;
    $scope.docId = $scope.item.id;
    $scope.docName = $window.item.path;
    $scope.inLecture = $window.inLecture;
    $scope.lectureId = $window.lectureId;
    $scope.code = $window.lectureCode;
    $scope.lectureCode = "Lecture info: " + $window.lectureCode;
    $scope.lectureStartTime = moment($window.lectureStartTime);
    $scope.lectureEndTime = moment($window.lectureEndTime);
    $scope.msg = "";
    $scope.dynamicAnswerShowControls = [];
    $scope.dynamicAnswerShowControl = {};
    $scope.index = 0;
    $scope.isLecturer = false;
    $scope.answerers = [];
    $scope.showPoints = false;
    $scope.points = [];
    $scope.showLectureForm = false;
    $scope.element = $element;
    /**
     * Sends http request to get info about the specific lecture.
     * @memberof module:lectureInfoController
     */
    $scope.getLectureInfo = function () {
        $http({
            url: '/getLectureInfo',
            type: 'GET',
            params: {lecture_id: $scope.lectureId}
        })
            .success(function (answer) {

                /*Update the header links to correct urls*/
                $scope.updateHeaderLinks();
                /*Add a return link icon to lecture header if user is in lecture*/
                if($scope.inLecture) {
                    $scope.addReturnLinkToHeader();
                }

                angular.forEach(answer.messages, function (msg) {
                    $scope.msg = $scope.msg + msg.sender + " <" + msg.time + ">: " + msg.message + "\n";
                });
                $scope.answers = answer.answers;
                for (var i = 0; i < answer.questions.length; i++) {
                    $scope.dynamicAnswerShowControls.push({});
                    $scope.points.push(0);
                    var markup = JSON.parse(answer.questions[i].json);
                    if (!markup.json) markup = {json: markup}; // compability for old
                    var json = markup.json;
                    fixQuestionJson(json);
                    answer.questions[i].nr = i+1;
                    answer.questions[i].json = json;
                }
                $scope.questions = answer.questions;
                $scope.isLecturer = answer.isLecturer;
                $scope.answerers = answer.answerers;
                $scope.selectedUser = answer.user;
            })
            .error(function () {
                $window.console.log("fail");
            });
    };

    $scope.editPoints = function (asked_id) {
        $http({
            url: '/getAskedQuestionById',
            method: 'GET',
            params: {'asked_id': asked_id}
        })
            .success(function (data) {
                var markup = JSON.parse(data.json);
                if ( !markup.json ) markup = { json: markup}; // compability for old
                markup.points = data.points;
                $rootScope.$broadcast('changeQuestionTitle', {'questionTitle': markup.json.questionTitle});
                $rootScope.$broadcast("editQuestion", {
                    "asked_id": asked_id,
                    "markup": markup
                });
            })
            .error(function () {
                $log.error("There was some error creating question to database.");
            });
    };


    /*
     Gets the lecture info when loading page.
     */
    $scope.getLectureInfo();

    /*
     Updates the header links in base.html
     */
    $scope.updateHeaderLinks = function () {
        if (document.getElementById("headerView")) {
            document.getElementById("headerView").setAttribute("href", "/view/" + $scope.item.path);
        }
        if (document.getElementById("headerManage")) { document.getElementById("headerManage").setAttribute("href",  "/manage/" + $scope.item.path);
        }
        if (document.getElementById("headerTeacher")) {
            document.getElementById("headerTeacher").setAttribute("href",  "/teacher/" + $scope.item.path);
        }
        if (document.getElementById("headerAnswers")) {
            document.getElementById("headerAnswers").setAttribute("href",  "/answers/" + $scope.item.path);
        }
        if (document.getElementById("headerLecture")) {
            document.getElementById("headerLecture").setAttribute("href",  "/lecture/" + $scope.item.path);
        }
        if (document.getElementById("headerSlide")) {
            document.getElementById("headerSlide").setAttribute("href",  "/slide/" + $scope.item.path);
        }
    };

    /**
     * Adds a header to lectureMenu that allows user to return to lecture
     * if user is currently on the lecture.
     */
    $scope.addReturnLinkToHeader = function () {
        var menu = document.getElementById("inLectureIconSection");
        var linkToLecture = document.createElement("a");
        linkToLecture.setAttribute("href", /* "https://" + location.host + */ "/lecture/" + $scope.item.path + "?Lecture=" + $scope.lectureCode);
        linkToLecture.setAttribute("title", "Return to lecture");
        var returnImg = document.createElement("img");
        returnImg.setAttribute("src", "/static/images/join-icon3.png");
        returnImg.setAttribute("class", "icon");
        returnImg.setAttribute("title", "Return to lecture");
        linkToLecture.appendChild(returnImg);
        menu.appendChild(linkToLecture);
    };

    /**
     * Sends http request to delete the lecture.
     * @memberof module:lectureInfoController
     */
    $scope.deleteLecture = function () {
        var confirmAnswer = $window.confirm("Do you really want to delete this lecture?");
        if (confirmAnswer) {
            $http({
                url: '/deleteLecture',
                method: 'POST',
                params: {'doc_id': $scope.docId, lecture_id: $scope.lectureId}
            })
                .success(function () {
                    window.history.back();
                })
                .error(function () {
                    $window.console.log("Failed to delete the lecture");
                });
        }
    };

    $scope.editLecture = function () {
        $('#currentList').hide();
        $('#futureList').hide();
        $http({
            url: '/showLectureInfoGivenName',
            method: 'GET',
            params: {'lecture_code': $scope.code, 'doc_id': $scope.docId}
        })
            .success(function (lecture) {
                $scope.$broadcast("editLecture", {
                    "lecture_id": lecture.lectureId,
                    "lecture_name": lecture.lectureCode,
                    "start_date": moment(lecture.lectureStartTime),
                    "end_date": moment(lecture.lectureEndTime),
                    "password": lecture.password || "",
                    "editMode": true
                });
                $scope.showLectureForm = true;
            })
            .error(function () {
                $window.console.log("Failed to fetch lecture.");
            });
    };

    $scope.$on("lectureUpdated", function (event, data) {
        $http({
            url: '/showLectureInfoGivenName',
            method: 'GET',
            params: {'lecture_id': $scope.lectureId}
        })
            .success(function (lecture) {
                $scope.code = lecture.lectureCode;
                $scope.lectureCode = "Lecture info: " + lecture.lectureCode;
                $scope.lectureEndTime = moment(lecture.lectureEndTime);
                $scope.lectureStartTime = moment(lecture.lectureStartTime);
            })
            .error(function () {
                $window.console.log("Failed to fetch lecture.");
            });
        $scope.showLectureForm = false;
    });

    $scope.$on("closeLectureForm", function (event, data) {
        $scope.showLectureForm = false;
    });

    $scope.toggle = function() {
        $scope.dynamicAnswerShowControls[0].toggle();
        
    }
    
    
    /**
     * Draws charts from the answer of the current lecture.
     * @param userToShow Which users answers to shows. If undefined shows from every user.
     * @memberof module:lectureInfoController
     */
    $scope.drawCharts = async function (userToShow) {
        for (var p = 0; p < $scope.points.length; p++) {
            $scope.points[p] = 0;
        }
        $scope.showPoints = true;
        var user;
        if (typeof userToShow === 'undefined') {
            user = "";
        } else {
            user = userToShow.user_id;
        }
        var questionIndexes = [];
        for (var i = 0; i < $scope.dynamicAnswerShowControls.length; i++) {
            await $scope.dynamicAnswerShowControls[i].createChart($scope.questions[i].json);
            questionIndexes.push($scope.questions[i].asked_id);
        }

        for (var j = 0; j < $scope.answers.length; j++) {
            if (($scope.isLecturer && user === "") || $scope.answers[j].user_id === user) {
                $scope.dynamicAnswerShowControls[questionIndexes.indexOf($scope.answers[j].question_id)]
                    .addAnswer([{"answer": $scope.answers[j].answer}]);
                $scope.points[questionIndexes.indexOf($scope.answers[j].question_id)] += $scope.answers[j].points;
            }
        }

        if ($scope.answers.length <= 0) {
            var elem = $("#infoBox");
            elem.empty();
            elem.append("No answers from this lecture");
        }
        $window.setTimeout(function () { // give time to html to change
            ParCompiler.processAllMath($element.parent());
        }, 200);
    };
}]);
