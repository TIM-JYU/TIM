/**
 * Created by sevitarv on 8.3.2016.
 * NOTE: This controller requires phraseSelection directive to work!!!
 */

var angular;
var timApp = angular.module('timApp');
console.log("reviewController.js added");


timApp.controller("ReviewController", ['$scope', '$http', '$window', '$compile', function ($scope, $http, $window, $compile) {
    "use strict";

    $scope.testi = "TESTI";
    $scope.showSource = false;
    $scope.markingsAdded = false;
    $scope.selectedMarking = {"comment": "", "velp": "", "points": 0};

    $http.get('/static/test_data/markings.json').success(function (data) {
        $scope.markings = [];
        //$scope.markings = data;
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

                //$scope.addMarkingToCoord(range, $scope.markings[i], false);
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
    $scope.addMarkingToCoord = function(range, marking, show){
        console.log("marking added");
        var span = $scope.createPopOverElement(marking, show);
        range.surroundContents(span);
        $compile(span)($scope);
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
        $scope.selectedMarking["velp"] = $scope.getVelpById( $scope.markings[markingId].velp).content;
    };

    $scope.getVelpById = function(id){
        for (var i=0; i<$scope.velps.length; i++)
            if ($scope.velps[i].id == "" +id)
                return $scope.velps[i];

        return undefined;
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

            var newMarking = {
                "id": $scope.markings.length,
                "velp": velp.id,
                "points": velp.points,
                "coord": {"el": 0, "start": 0, "end":0 }, // TODO: get coordinates from selectedArea
                "comments": []
            };

            $scope.markings.push(newMarking);
            //$scope.selectMarking(newMarking['id']);

            $scope.addMarkingToCoord($scope.selectedArea, newMarking, true );
            $scope.selectedArea = undefined;

            velp.used += 1;
        }
        $scope.markingsAdded = true;
    };

    $scope.createPopOverElement = function (marking, show) {
        var element = document.createElement('marking');
        var velp_content = String($scope.getVelpById(marking.velp).content);


        element.setAttribute("velp", velp_content);
        element.setAttribute("points", marking.points);
        element.setAttribute("comments", marking.comments);

        return element;
    };

}]);