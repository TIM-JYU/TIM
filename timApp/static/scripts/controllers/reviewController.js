/**
 * Created by sevitarv on 8.3.2016.
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
        var colorIndex = 0;
        if (points == 0)
            colorIndex = 1;
        else if (points < 0)
            colorIndex = 2;
        return markingColorPalette[colorIndex];
    };

        $scope.usePhrase = function (phrase) {
        if (typeof $scope.selectedArea != "undefined") {
            /*
             var span = angular.element("<span></span>");
             console.log(span);
             */
            //var span = $scope.createPopOverElement(phrase.content);
            var span = document.createElement("span");
            span.classList.add("marking");

            // color should be by points
            span.style.background = $scope.getMarkingHighlight(phrase.points);
            //span.style.background = $scope.tags[phrase.tags[0]].color;
            $compile(span)($scope);

            $scope.selectedArea.surroundContents(span);
            $scope.selectedArea = undefined;
            phrase.used += 1;

            $scope.open(phrase.id);
        }
    };

}]);