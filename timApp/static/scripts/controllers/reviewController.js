/**
 * Created by sevitarv on 8.3.2016.
 * This controller requires phraseSelection directive to work.
 */

var angular;
var timApp = angular.module('timApp');
var markingColorPalette = ["palegreen", "khaki", "lightpink"];
console.log("reviewController.js added");

timApp.controller("ReviewController", ['$scope', '$http', '$window', '$compile', function ($scope, $http, $window, $compile) {
    "use strict";

    /**
     * Select text range
     */
    $scope.selectText = function () {
        var sel = $window.getSelection();
        if (sel.toString().length > 0) {
            console.log(sel.toString());
            var range = sel.getRangeAt(0);
            $scope.selectedArea = range;
        } else {
            $scope.selectedArea = undefined;
        }
    };

    /**
     * Get marking highlight style
     * @param points points given in marking
     * @returns marking color (should return some CSS-style)
     */
    $scope.getMarkingHighlight = function (points) {
        var highlightStyle = "positive";
        if (points == 0)
            highlightStyle = "neutral";
        else if (points < 0)
            highlightStyle = "negative";
        return highlightStyle;
    };

    /**
     * Uses selected phrase, if area is selected.
     * @param phrase phrase selected in phraseSelection directive
     */
    $scope.usePhrase = function (phrase) {
        if (typeof $scope.selectedArea != "undefined") {
            /*
             var span = angular.element("<span></span>");
             console.log(span);
             */
            //var span = $scope.createPopOverElement(phrase.content);
            var span = document.createElement("span");
            span.classList.add("marking");
            span.classList.add($scope.getMarkingHighlight(phrase.points));

            $compile(span)($scope);

            $scope.selectedArea.surroundContents(span);
            $scope.selectedArea = undefined;
            phrase.used += 1;

            $scope.open(phrase.id);
        }
    };

}]);