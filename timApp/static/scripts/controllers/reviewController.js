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
    $scope.annotationids = {0:0};

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
        console.log($scope.annotations);

        for (var i=0; i<$scope.annotations.length; i++){
            var placeInfo = $scope.annotations[i]["coord"];
            console.log(placeInfo);
            var parent = document.getElementById(placeInfo["start"]["par_id"]).querySelector(".parContent");
            var elements = parent;

            // Find element
            var elpath = placeInfo["start"]["el_path"];

            console.log(elpath);
            console.log(elpath.length);

            for (var j=0; j<elpath.length; j++){
                if (elements.children.item(elpath[j]) != null)
                    elements = elements.children.item(elpath[j]);
            }

            var startel = elements.childNodes[placeInfo["start"]["node"]];
            var endel = elements.childNodes[placeInfo["end"]["node"]];

            var range = document.createRange();
            range.setStart(startel, placeInfo["start"]["offset"]);
            range.setEnd(endel, placeInfo["end"]["offset"]);
            addAnnotationToCoord(range, $scope.annotations[i], false);
        }

        $scope.annotationsAdded = true;
    };

    /**
     * Adds annotation to given element on given coordinate
     * @param range annotation location
     * @param annotation annotation info
     * @param show show by default
     */
    var addAnnotationToCoord = function(range, annotation, show){
        var span = $scope.createPopOverElement(annotation, show);
        try {
            console.log(range);
            range.surroundContents(span);
        } catch(err) {
            // Add annotation to the "club of missing velps"
            var new_range = document.createRange();
            var el = range.startContainer;
            var start = range.startOffset;
            var end = range.startContainer.parentNode.innerHTML.length -1;

            new_range.setStart(el, start);
            new_range.setEnd(el, end);
            addAnnotationToCoord(new_range, annotation, show);

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

        for (var a=0; a<$scope.annotations.length; a++){
            if (id == $scope.annotations[a].id)
                $scope.annotations.splice(a,1);
        }

        if (id < 0) id = $scope.annotationids[id];

        $scope.makePostRequest("/deleteannotation", id, function(){});
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
        console.log(sel);
        console.log(sel.toString());
        if (sel.toString().length > 0) {
            var range = sel.getRangeAt(0);
            console.log(range);

            /*
            if ($scope.selectedArea != null && $scope.selectionParent != null) {
                console.log("hei");
                $scope.selectedArea.startContainer.parentNode.replaceChild($scope.selectionParent, $scope.selectedArea.startContainer);
            }
            */
            //console.log(sel);

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

            var innerDiv = document.createElement('div');
            var cloned = $scope.selectedArea.cloneContents();
            innerDiv.appendChild(cloned);

            while (!parelement.hasAttribute("id")) {
                parelement = parelement.parentElement;
            }

            var element_path = getElementPositionInTree(startElement, []);

            var startoffset = getRealStartOffset($scope.selectedArea["startContainer"], $scope.selectedArea["startOffset"]);
            var endOffset =  $scope.selectedArea["endOffset"];
            if (innerDiv.childElementCount == 0)
                endOffset = startoffset + innerDiv.childNodes[innerDiv.childNodes.length-1].length;
            console.log(Object.keys($scope.annotationids).length);

            var newMarking = {
                id: (Object.keys($scope.annotationids).length+1)*(-1),
                velp: velp.id,
                points: velp.points,
                doc_id: $scope.docId,
                visible_to: 4,
                coord: {
                    start: {
                        par_id: parelement.id,
                        t: parelement.getAttribute("t"),
                        el_path: element_path,
                        offset: startoffset,
                        depth: element_path.length
                    } ,
                    end: {
                        par_id: parelement.id,
                        t: parelement.getAttribute("t"),
                        el_path: element_path,
                        offset: endOffset,
                        depth: element_path.length
                    }
                },
                comments: [
                   //{"content": "Pre-printed comment", "author": username}
                ]
            };

            addAnnotationToCoord($scope.selectedArea, newMarking, true);
            $scope.annotations.push(newMarking);
            $scope.annotationids[newMarking.id] = newMarking.id;

            var nodeNums = getNodeNumbers($scope.selectedArea["startContainer"], newMarking.id, innerDiv);
            newMarking.coord.start.node = nodeNums[0];
            newMarking.coord.end.node = nodeNums[1];

            console.log(newMarking);

            $scope.makePostRequest("/addannotation", newMarking, function(json){
                $scope.annotationids[newMarking.id] = json.data;
            });

            $scope.selectedArea = undefined;
            velp.used += 1;
        }
        $scope.annotationsAdded = true;
    };

    /**
     * Gets array of element indexes from parent to start
     * TODO: ignore annotations
     * @param start
     * @param array
     * @returns {*}
     */
    var getElementPositionInTree = function(start, array){
        var myparent = start.parentElement;

        if (myparent.hasAttribute("t")) {
            return array.reverse();
        }

        var count = 0;
        for (var i = 0; i<myparent.children.length; i++){

            if (myparent.children[i] == start){
                array.push(count);
                return getElementPositionInTree(myparent, array)
            }

            if (myparent.children[i].nodeName == "ANNOTATION"){
                var innerElements = myparent.children[i].getElementsByClassName("highlighted")[0];
                if (innerElements.children.length > 2){
                    count += innerElements.children.length - 2;
                }
                continue;
            }

            count++;
        }

        throw "Element position in tree was not found";
    };

    /**
     * Get start offset according to "original state" of DOM tree.
     * Ignores annoations elements, but not elements inside it
     * @param el start container
     * @param startoffset original start offset
     * @returns {*}
     */
    var getRealStartOffset = function(el, startoffset){

        var startType = el.nodeName;
        var storedOffset = startoffset;

        while (el.previousSibling != null){
            el = el.previousSibling;
            if (el.nodeName == "ANNOTATION"){

                var innerElements = el.getElementsByClassName("highlighted")[0];
                storedOffset += innerElements.lastChild.innerHTML.length;

                if (innerElements.childNodes.length > 1){
                    return storedOffset;
                }
            }
            else if (el.nodeName != startType){
                return storedOffset;
            } else {
                storedOffset += el.length;
            }
        }

        return storedOffset;
    };

    /**
     * Get start and end node numbers of created annotation element.
     * Ignores annoations elements, but not elements inside it.
     * @param el start container
     * @param aid annotation id
     * @param innerElement annotation content
     * @returns {*[]}
     */
    var getNodeNumbers = function(el, aid, innerElement){
        var parent = el;
        var num = 0;

        var prevNodeName = parent.childNodes[0].nodeName;
        var aidFound = 0;

        for (var i = 0; i < parent.childNodes.length; i++) {

            if (parent.childNodes[i].nodeName == "ANNOTATION") {

                if (parent.childNodes[i].getAttribute("aid") == aid) {
                    
                    var startnum = num - 1;
                    num += innerElement.childNodes.length;
                    
                    if (innerElement.firstChild.nodeName == prevNodeName) num--;
                    if (i < parent.childNodes.length - 1 && innerElement.lastChild.nodeName == parent.childNodes[i + 1].nodeName) num--;
                    
                    return [startnum, num];
                    
                } else {
                    var innerEl = parent.childNodes[i].getElementsByClassName("highlighted")[0];
                    num += innerEl.childNodes.length;

                    if (innerEl.firstChild.firstChild.nodeName == prevNodeName) num--;
                    if (i < parent.childNodes.length - 1 && innerEl.lastChild.lastChild.nodeName == parent.childNodes[i + 1].nodeName) num--;
                    
                    continue;
                }
            }

            num++;
            prevNodeName = parent.childNodes[i].nodeName;
        }

        throw "No node found";
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