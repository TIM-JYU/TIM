/**
 * Created by sevitarv on 8.3.2016.
 * NOTE: This controller requires phraseSelection directive to work!!!
 */

var angular;
var timApp = angular.module('timApp');
console.log("reviewController.js added");


timApp.controller("ReviewController", ['$scope', '$http', '$window', '$compile', function ($scope, $http, $window, $compile) {
    "use strict";

    $scope.markingsAdded = false;
    $scope.selectedMarking = {"comments": [], "velp": "", "points": 0};
    $scope.markings = [];

    //var username = $scope.$parent.$parent.users[0].name;
    var username = $scope.$parent.users[0].name;

    /*
    $http.get('/static/test_data/markings.json').success(function (data) {
        //$scope.markings = [];
        $scope.markings = data;
        $scope.loadMarkings();
    });
    */

    /**
     * Loads used markings into view
     */
    $scope.loadMarkings = function() {
        // TODO: Change this when editor is removed
        var elements = document.getElementById("previewContent").getElementsByTagName("p");

        if (elements.length > 0 && !$scope.markingsAdded && typeof $scope.markings != "undefined" && $scope.markings.length > 0) {
            for (var i=$scope.markings.length-1; i>=0; i--){
                var placeInfo = $scope.markings[i]["coord"];
                var el = elements.item(placeInfo["el"]).childNodes[0];
                var range = document.createRange();

                range.setStart(el, placeInfo["start"]);
                range.setEnd(el, placeInfo["end"]);

                $scope.addMarkingToCoord(range, $scope.markings[i], false);
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
        var span = $scope.createPopOverElement(marking, show);
        console.log(range);
        try {
            range.surroundContents(span);
        } catch(err) {
            var new_range = document.createRange();
            var el = range.startContainer;
            var start = range.startOffset;
            var end = range.startContainer.parentNode.innerHTML.length;

            new_range.setStart(el, start);
            new_range.setEnd(el, end);
            $scope.addMarkingToCoord(new_range, marking, show);
        }
        $compile(span)($scope); // Gives error [$compile:nonassign]

        var parelement = range.commonAncestorContainer.parentNode;
        var startElement = range.startContainer;

        var i = 0;
        while(!parelement.hasAttribute("id")){
            parelement = parelement.parentNode;
            console.log(parelement);
            i++;
        }
    };

    $scope.deleteAnnotation = function(id){
        console.log(id);
        var markingParents = document.querySelectorAll('[aid="{0}"]'.replace('{0}', id));
        var markingHighlights = markingParents[0].getElementsByClassName("ng-scope");
        var savedHTML = markingHighlights[0].innerHTML;

        for (var i=1; i<markingHighlights.length-1; i++){
            savedHTML += markingHighlights[0].outerHTML;
        }

        if (markingHighlights.length>1){
            savedHTML += markingHighlights[markingHighlights.length-1].innerHTML;
        }

        console.log(savedHTML);

        markingParents[0].parentNode.replaceChild(savedHTML, markingHighlights[0]);
        /*
        var savedHTML = marking[0].innerHTML;
        var parent = marking[0].parentNode;
        console.log(parent);
        for (var i=1; i<marking.length-1; i++){
            savedHTML += marking[i].outerHTML;
        }
        if (marking.length > 1)
            savedHTML += marking[marking.length-1].innerHTML;

        console.log(savedHTML);
        */
        /*
        var annotations = markings.getElementsByClassName("marking");
        var savedHTML = children[0].innerHTML;
        for (var i = 1; i<markings.length; i++) {
            console.log(markings[i].getElementsByClassName("marking")[0].childNodes);
        }
        */
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
     * Get velp by its id
     * @param id velp to find
     * @returns velp or undefined
     */
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
                "coord": {
                    "el": 0,
                    "start": $scope.selectedArea["startOffset"] ,
                    "end": $scope.selectedArea["endOffset"]
                }, // TODO: get coordinates from selectedArea
                "comments": []
            };

            console.log("SELECTED AREA");
            //console.log($scope.selectedArea["commonAncestorContainer"].parentElement.parentElement);
            var parent = document.querySelectorAll("#previewContent p");
            console.log(parent);

            console.log($scope.selectedArea["endOffset"]);

            $scope.markings.push(newMarking);
            //$scope.selectMarking(newMarking['id']);

            $scope.addMarkingToCoord($scope.selectedArea, newMarking, true);
            $scope.selectedArea = undefined;

            velp.used += 1;
        }
        $scope.markingsAdded = true;
    };

    /**
     * Get comments of given marking
     * @param id marking id
     * @returns {Array|*|string|boolean}
     */
    $scope.getMarkingComments = function (id){
        for (var i=0; i<$scope.markings.length; i++){
            if (id == $scope.markings[i].id)
                return $scope.markings[i].comments;
        }
    };

    /**
     * Create pop over marking element
     * @param marking marking info
     * @param show wether to show marking or not
     * @returns {Element}
     */
    $scope.createPopOverElement = function (marking, show) {
        var element = document.createElement('marking');
        var velp_content = String($scope.getVelpById(marking.velp).content);

        element.setAttribute("velp", velp_content);
        element.setAttribute("points", marking.points);
        element.setAttribute("aid", marking.id);
        element.setAttribute("user", username);
        element.setAttribute("show", show);
        element.setAttribute("comments", JSON.stringify(marking.comments));

        return element;
    };

}]);