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

timApp.controller("ShowStatisticsToQuestionController", ["$scope", "$element", function($scope, $element) {
    "use strict";
    $scope.dynamicAnswerShowControl = {};
    $scope.canvas = "";
    $scope.questionTitle = "";
    $scope.lecturerAnswered = false;

    $scope.$on("closeAnswerSheetForGood", function() {
        $scope.$emit("closeAnswerShow");
        $scope.dynamicAnswerShowControl.close();
    });
    /**
     * Closes statistic window
     * @memberof module:showStatisticsToQuestionController
     */
    $scope.close = function() {
        $scope.$emit("closeAnswerShow");
        if ($scope.lecturerAnswered) {
            $scope.dynamicAnswerShowControl.close();
        }
    };

    $scope.hide = function() {
        $scope.$emit("closeAnswerShow");
    };

    $scope.$on("lecturerAnswered", function() {
        $scope.lecturerAnswered = true;
        $scope.$emit("showAnswers", true);
    });

    /**
     * Adds answer to statistic directive
     * @memberof module:showStatisticsToQuestionController
     */
    $scope.$on("putAnswers", function(event, answer) {
        $scope.dynamicAnswerShowControl.addAnswer(answer.answers);
    });

    /**
     * Creates chart based on question json.
     * @memberof module:showStatisticsToQuestionController
     */
    $scope.$on("createChart", async function(event, question) {
        $scope.lecturerAnswered = false;
        await $scope.dynamicAnswerShowControl.createChart(question);
        $scope.questionTitle = question.questionText;

        window.setTimeout(function() { // give time to html to change
            ParCompiler.processAllMath($element.parent());
        }, 200);

    });

}]);
