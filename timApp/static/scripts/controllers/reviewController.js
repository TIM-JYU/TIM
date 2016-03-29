/**
 * Created by sevitarv on 8.3.2016.
 * NOTE: This controller requires phraseSelection directive to work!!!
 */

var angular;
var timApp = angular.module('timApp');
console.log("reviewController.js added");

timApp.controller("ReviewController", ['$scope', '$http', '$window', '$compile', function ($scope, $http, $window, $compile) {
    "use strict";

    $scope.showSource = false;
    $scope.markingsAdded = false;
    $scope.selectedMarking = {"comment": "", "velp": "", "points": 0};


    $http.get('/static/test_data/markings.json').success(function (data) {
        $scope.markings = data;
       //$scope.loadMarkings();
    });

    $scope.toggleShowSource = function(){
        $scope.showSource = !$scope.showSource;
    };

    /**
     * Loads used markings into view
     */
    $scope.loadMarkings = function() {
        var elements = document.getElementById("previewContent").getElementsByTagName("p");

        if (elements.length > 0 && !$scope.markingsAdded && typeof $scope.markings != "undefined" && $scope.markings.length > 0) {

            for (var i=$scope.markings.length-1;i>=0; i--){
                var placeInfo = $scope.markings[i]["coord"];
                console.log($scope.markings[i]);
                var el = elements.item(placeInfo["el"]).childNodes[0];
                var range = document.createRange();

                range.setStart(el, placeInfo["start"]);
                range.setEnd(el, placeInfo["end"]);

                $scope.addMarkingToCoord(range, $scope.markings[i]);
            }
            $scope.markingsAdded = true;
        }
    };

    /**
     * Adds marking to given element on given coordinate
     * @param el element
     * @param start start coordinate
     * @param end end coordinate
     */
    $scope.addMarkingToCoord = function(range, marking){
        var span = document.createElement("span");
        span.setAttribute("ng-click", "selectMarking(" + marking.id + ")");
        span.classList.add("marking");
        span.classList.add($scope.getMarkingHighlight( marking.points));
        $compile(span)($scope);

        range.surroundContents(span);
    };

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
     * Method for showing marking information in view (reviewEditor.html)
     * @param marking marking info to show
     */
    $scope.selectMarking = function(markingId){
        $scope.selectedMarking["points"] = $scope.markings[markingId].points;
        $scope.selectedMarking["comment"] = $scope.markings[markingId].comment;
        $scope.selectedMarking["velp"] = $scope.velps[ $scope.markings[markingId].velp ]["content"];
    };

    /**
     * Get marking highlight style
     * @param points points given in marking
     * @returns highlight style
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
     * Uses selected velp, if area is selected.
     * @param velp velp selected in velpSelection directive
     */
    $scope.usePhrase = function (velp) {
        if (typeof $scope.selectedArea != "undefined") {
            /*
             var span = angular.element("<span></span>");
             console.log(span);
            */
            //var span = $scope.createPopOverElement(velp.content);

            var newMarking = {
                "id": $scope.markings.length,
                "velp": velp.id,
                "points": velp.points,
                "coord": {"el": 0, "start": 0, "end":0 }, // TODO: get coordinates from selectedArea
                "comment": ""
            };

            $scope.markings.push(newMarking);
            $scope.addMarkingToCoord($scope.selectedArea, newMarking );

            $scope.selectedArea = undefined;
            velp.used += 1;
        }
    };


}]);