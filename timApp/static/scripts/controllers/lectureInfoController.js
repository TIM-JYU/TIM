var docId, lectureId, lectureCode, lectureStartTime, lectureEndTime;
/**
 * Created by hajoviin on 11.5.2015.
 */
timApp.controller('LectureInfoController', ['$scope', '$http',  function ($scope, $http) {

    $scope.docId = docId;
    $scope.lectureId = lectureId;
    $scope.lectureCode = "Lecture info: " + lectureCode;
    $scope.lectureStartTime = lectureStartTime;
    $scope.lectureEndTime = lectureEndTime;
    $scope.msg = "";
    $scope.answers = "";
    $scope.userName = "";
    $scope.dynamicAnswerShowControls = [];
    $scope.dynamicAnswerShowControl = {};
    $scope.index = 0;
    $scope.isLecturer = false;
    $scope.answerers = [];

    $scope.getLectureInfo = function () {
        $http({
            url: '/getLectureInfo',
            type: 'GET',
            params: {lecture_id: $scope.lectureId}
        })
            .success(function (answer) {

                angular.forEach(answer.messages, function (msg) {
                    $scope.msg = $scope.msg + msg + "\n";
                });
                $scope.answers = answer.answers;
                for (var i = 0; i < answer.questions.length; i++) {
                    $scope.dynamicAnswerShowControls.push({});
                }

                $scope.questions = answer.questions;
                $scope.isLecturer = answer.isLecturer;
                $scope.answerers = answer.answerers;
                $scope.userName = answer.userName;
            })
            .error(function () {
                console.log("fail")
            })
    };

    $scope.getLectureInfo();

    $scope.deleteLecture = function () {
        var confirmAnswer = confirm("Do you really want to delete this lecture?");
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
                    console.log("Failed to delete the lecture");
                })

        }
    };

    $scope.drawCharts = function (userName) {
        var user;
        if (typeof userName === 'undefined') {
            user = ""
        } else {
            user = userName;
        }
        console.log("user: " + user);
        var questionIndexes = [];
        for (var i = 0; i < $scope.dynamicAnswerShowControls.length; i++) {
            $scope.dynamicAnswerShowControls[i].createChart(JSON.parse($scope.questions[i].questionJson));
            questionIndexes.push($scope.questions[i].question_id);
        }

        console.log($scope.dynamicAnswerShowControls);
         console.log($scope.answers);

        for (var j = 0; j < $scope.answers.length; j++) {
            if (($scope.isLecturer && user == "") || $scope.answers[j].user_name == user) {
                $scope.dynamicAnswerShowControls[questionIndexes.indexOf($scope.answers[j].question_id)]
                    .addAnswer([{"answer": $scope.answers[j].answer}]);
            }
        }

        if ($scope.answers.length <= 0) {
            var elem = $("#infoBox");
            elem.empty();
            elem.append("No answers from this lecture");
        }
    }
}]);