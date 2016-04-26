/**
 * Created by sevitarv on 8.3.2016.
 * NOTE: This controller requires velpSelection directive to work!!!
 */

var angular;
var timApp = angular.module('timApp');
console.log("reviewController.js added");


timApp.controller("ReviewController", ['$scope', '$http', '$window', '$compile', '$timeout', function ($scope, $http, $window, $compile, $timeout) {
    "use strict";

    $scope.annotationsAdded = false;
    $scope.selectedAnnotation = {"comments": [], "velp": "", "points": 0};
    $scope.selectionParent = null;

    var username = $scope.$parent.users[0].name;

    /**
     * Makes post request to given url
     * @param url request url
     * @param params query parameters
     */
    $scope.makePostRequest = function (url, params, succesMethod) {
        var response = null;
        $http({
            method: 'POST',
            url: url,
            data: params
        }).then(function (data) {
            succesMethod(data);
        });
    };

    /**
     * Loads used annotations into view
     */
    $scope.loadAnnotations = function() {
        for (var i=$scope.annotations.length -1; i>=0; i--){
            var placeInfo = $scope.annotations[i]["coord"];

            var parent = document.getElementById(placeInfo["start"]["par_id"]).querySelector(".parContent");
            var elements = parent;

            // Find element
            var elpath = placeInfo["start"]["el_path"];

            console.log(elpath);
            console.log(elpath.length);

            for (var j=0; j<elpath.length; j++){
                elements = elements.children.item(elpath[j]);
            }

            var el = elements.childNodes[0];
            console.log(el);

            var range = document.createRange();
            range.setStart(el, placeInfo["start"]["offset"]);
            range.setEnd(el, placeInfo["end"]["offset"]);
            $scope.addAnnotationToCoord(range, $scope.annotations[i], false);
        }

        $scope.annotationsAdded = true;
    };

    /**
     * Adds annotation to given element on given coordinate
     * @param range annotation location
     * @param annotation annotation info
     * @param show show by default
     */
    $scope.addAnnotationToCoord = function(range, annotation, show){
        var span = $scope.createPopOverElement(annotation, show);
        try {
            console.log(range);
            range.surroundContents(span);
        } catch(err) {
            // Add annotation to the "club of missing velps"
            /*
            var new_range = document.createRange();
            var el = range.startContainer;
            var start = range.startOffset;
            var end = range.startContainer.parentNode.innerHTML.length;

            new_range.setStart(el, start);
            new_range.setEnd(el, end);
            $scope.addAnnotationToCoord(new_range, annotation, show);
            */
        }

        $compile(span)($scope); // Gives error [$compile:nonassign]
    };

    /**
     * Delete annotation
     * TODO: Make query to database
     * @param id annotation id
     */
    $scope.deleteAnnotation = function(id){
        var annotationParents = document.querySelectorAll('[aid="{0}"]'.replace('{0}', id));
        var annotationHighlights = annotationParents[0].getElementsByClassName("highlighted");
        var savedHTML = "";

        for (var i=0; i<annotationHighlights.length; i++){
            var addHTML = annotationHighlights[i].innerHTML.replace('<span class="ng-scope">', '');
            addHTML = addHTML.replace('</span>', '');
            savedHTML += addHTML;
        }

        annotationParents[0].outerHTML = savedHTML;
    };

    /**
     * Delete annotation
     * TODO: Make query to database
     * @param id annotation id
     */
    $scope.changeAnnotationPoints = function(id, points){
        for (var i=0; i<$scope.annotations.length; i++){
            if ($scope.annotations[i].id == id) {
                $scope.annotations[i].points = points;
                break;
            }
        }
    };

    /**
     * Select text range
     */
    $scope.selectText = function () {
        var sel = $window.getSelection();

        if (sel.toString().length > 0) {
            /*
            if ($scope.selectedArea != null && $scope.selectionParent != null) {
                console.log("hei");
                $scope.selectedArea.startContainer.parentNode.replaceChild($scope.selectionParent, $scope.selectedArea.startContainer);
            }
            */
            //console.log(sel);

            var range = sel.getRangeAt(0);
            //$scope.selectionParent = range.startContainer.parentNode.cloneNode(true);
            /*
            var selection = document.createElement("span");
            selection.classList.add("text-selection");
            range.surroundContents(selection);
            */
            //$scope.getRealRange(range);
            $scope.selectedArea = range;
        } else {
            $scope.selectedArea = undefined;
        }
    };

    $scope.getRealRange = function(currentRange){
        var startparent = currentRange.startContainer.parentNode;
        for (var i=0; i<startparent.childNodes.length; i++){
            //console.log(startparent.childNodes[i])
        }
    };

    /**
     * Get velp by its id
     * @param id velp to find
     * @returns velp or undefined
     */
    $scope.getVelpById = function(id){
        for (var i=0; i<$scope.velps.length; i++)
            if ($scope.velps[i].id == id)
                return $scope.velps[i];

        return null;
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
     * TODO: When annotations can cross taglines, change end coordinate according to end element
     * @param velp velp selected in velpSelection directive
     */
    $scope.useVelp = function (velp) {
        if (typeof $scope.selectedArea != "undefined") {
            var parelement = $scope.selectedArea["commonAncestorContainer"].parentElement;
            var startElement = $scope.selectedArea["startContainer"].parentElement;
            var startElementNum = null;

            while (!parelement.hasAttribute("id")) {
                parelement = parelement.parentElement;
            }

            var element_path = $scope.getElementPositionInTree(startElement, []);

            var newMarking = {
                id: $scope.annotations.length,
                velp: velp.id,
                points: velp.points,
                doc_id: $scope.docId,
                coord: {
                    start: {
                        par_id: parelement.id,
                        t: parelement.getAttribute("t"),
                        el_path: element_path,
                        offset: $scope.selectedArea["startOffset"]
                    } ,
                    end: {
                        par_id: parelement.id,
                        t: parelement.getAttribute("t"),
                        el_path: element_path,
                        offset:  $scope.selectedArea["endOffset"]
                    }
                },
                comments: [
                   //{"content": "Pre-printed comment", "author": username}
                ]
            };

            $scope.makePostRequest("/addannotation", newMarking, function (data) {
                console.log("Added annotation");
                console.log(data);
                $scope.annotations.push(newMarking);
                $scope.addAnnotationToCoord($scope.selectedArea, newMarking, true);
                $scope.selectedArea = undefined;
                velp.used += 1;
            });
        }
        $scope.annotationsAdded = true;
    };

    /**
     * Gets array of element indexes from parent to start
     * @param start
     * @param array
     * @returns {*}
     */
    $scope.getElementPositionInTree = function(start, array){
        var myparent = start.parentElement;

        if (myparent.hasAttribute("id"))
            return array.reverse();

        for (var i = 0; i<myparent.children.length; i++){
            console.log(myparent);
            if (myparent.children[i] == start){
                array.push(i);
                console.log(array);
                return $scope.getElementPositionInTree(myparent, array)
            }
        }

        return null
    };



    /**
     * Get comments of given marking
     * @param id marking id
     * @returns {Array|*|string|boolean}
     */
    $scope.getMarkingComments = function (id){
        for (var i=0; i<$scope.annotations.length; i++){
            if (id == $scope.annotations[i].id)
                return $scope.annotations[i].comments;
        }
    };

    /**
     * Create pop over marking element
     * @param marking marking info
     * @param show wether to show marking or not
     * @returns {Element}
     */
    $scope.createPopOverElement = function (marking, show) {
        var element = document.createElement('annotation');
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