/**
 * Created by hajoviin on 6.5.2015.
 * @module showStatisticsToQuestionController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

var angular;

var timApp = angular.module('timApp');
timApp.controller('ShowStatisticsToQuestionController', ['$scope', '$http', function ($scope) {
    "use strict";
    $scope.dynamicAnswerShowControl = {};
    $scope.canvas = "";
    $scope.questionTitle = "";

    /**
     * @memberof module:showStatisticsToQuestionController
     */
    $scope.close = function () {
        $scope.$emit('closeAnswerShow');
        $scope.dynamicAnswerShowControl.close();
    };

    /**
     * @memberof module:showStatisticsToQuestionController
     */
    $scope.$on("putAnswers", function (event, answer) {
        $scope.dynamicAnswerShowControl.addAnswer(answer.answers);
    });

    /**
     * @memberof module:showStatisticsToQuestionController
     */
    $scope.$on("createChart", function (event, question) {
        $scope.dynamicAnswerShowControl.createChart(question);
        $scope.questionTitle = question.QUESTION;
    });

}]);

