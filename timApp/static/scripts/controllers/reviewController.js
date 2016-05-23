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
    //$scope.selectionParent = null;

    var username = $scope.$parent.users[0].name;
    $scope.annotationids = {0: 0};

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
     * Loads document annotations into view
     */
    $scope.loadDocumentAnnotations = function () {

        for (var i = 0; i < $scope.annotations.length; i++) {

            var placeInfo = $scope.annotations[i]["coord"];
            var parent = document.getElementById(placeInfo["start"]["par_id"]);

            if ($scope.annotations[i].answer_id !== null){
                if (!parent.classList.contains("has-annotation")){
                    parent.classList.add("has-annotation");
                }
                continue;
            }

            if (parent.getAttribute("t") == placeInfo["start"]["t"]) {
                var elements = parent.querySelector(".parContent");

                var start_elpath = placeInfo["start"]["el_path"];

                for (var j = 0; j < start_elpath.length; j++) {
                    var elementChildren = getElementChildren(elements);
                    if (elementChildren[start_elpath[j]] != null)
                        elements = elementChildren[start_elpath[j]];
                }

                var startel = elements.childNodes[placeInfo["start"]["node"]];
                var endel = elements.childNodes[placeInfo["end"]["node"]];

                var range = document.createRange();
                range.setStart(startel, placeInfo["start"]["offset"]);
                range.setEnd(endel, placeInfo["end"]["offset"]);
                addAnnotationToCoord(range, $scope.annotations[i], false);
            } else {
                addAnnotationToElement(parent, $scope.annotations[i], false, "Paragraph has been modified");
            }
        }

        $scope.annotationsAdded = true;
    };

    /**
     * Get children (not childNodes) of the element.
     *
     * @param element
     * @returns {Array}
     */
    var getElementChildren = function(element){
        /*if (typeof element.children !== "undefined")
            return element.children;
        */
        var children = [];
        for (var i=0; i<element.childNodes.length; i++){
            if (typeof element.childNodes[i].tagName !== "undefined"){
                children.push(element.childNodes[i]);
            }
        }
        return children;
    };

    /**
     * Get parent element of the given element
     * @param element
     * @returns {*}
     */
    var getElementParent = function(element){
        /*
        if (typeof element.parentElement !== "undefined")
            return element.parentElement;
        */
        var parent = element.parentNode;
        if (typeof parent.tagName !== "undefined"){
            return parent;
        }

        getElementParent(parent);
    };

    /**
     * Checks if given element is an annotation
     * @param element element to check
     * @returns boolean
     */
    var checkIfAnnotation = function(element){
        if (element.nodeName == "ANNOTATION")
            return true;

        if (element.nodeName == "SPAN"){
            console.log(element);
            console.log(element.hasAttribute("annotation"));
            return element.hasAttribute("annotation");
        }

        return false;
    };

    /**
     * Loads annotations to given answer
     * @param answer_id
     */
    $scope.loadAnnotationsToAnswer = function(answer_id, par_id){
        var par = document.getElementById(par_id);
        var annotations = $scope.getAnnotationsByAnswerId(answer_id);

        for (var i = 0; i < annotations.length; i++) {
            var placeInfo = annotations[i]["coord"];

            var element = par.getElementsByTagName("PRE")[0].firstChild;
            console.log(element);
            var range = document.createRange();
            range.setStart(element, placeInfo["start"]["offset"]);
            range.setEnd(element, placeInfo["end"]["offset"]);
            addAnnotationToCoord(range, annotations[i], false);
        }
    };

    $scope.getAnnotationsByAnswerId = function(id) {
        var annotations = [];
        angular.forEach($scope.annotations, function(a){
            console.log(a);
            if (a.answer_id !== null && a.answer_id === id)
                annotations.push(a);
        });

        annotations.sort(function (a, b) {
            console.log(b);
            console.log(a);
            return b.coord.start.offset - a.coord.start.offset;
        });

        return annotations;
    };

    /**
     * Adds annotation to given element on given coordinate
     * @param range annotation location
     * @param annotation annotation info
     * @param show show by default
     */
    var addAnnotationToCoord = function (range, annotation, show) {
        var span = $scope.createPopOverElement(annotation, show);
        try {
            range.surroundContents(span);
        } catch (err) {
            // TODO: Add annotation to the "club of missing velps"
            console.log(err);
            var new_range = document.createRange();

            var el = range.startContainer;
            var start = range.startOffset;
            var end = range.startContainer.parentNode.innerHTML.length - 1;

            new_range.setStart(el, start);
            new_range.setEnd(el, end);
            addAnnotationToCoord(new_range, annotation, show);
        }

        $compile(span)($scope); // Gives error [$compile:nonassign]
    };

    /**
     * Adds annotation to given element. Puts annotation on the "club of missing velps".
     * @param id element id
     * @param annotation annotation info
     * @param show show by default
     * @param reason why annotation is put to here
     */
    var addAnnotationToElement = function (el, annotation, show, reason){
        var span = $scope.createPopOverElement(annotation, show);
        var text = document.createTextNode("\u00A0"+annotation.content+"\u00A0");
        span.appendChild(text);

        var container = el.getElementsByClassName("missing-velps");
        if (container.length > 0){
            container[0].appendChild(span);
        } else {
            container = document.createElement("div");
            container.classList.add("missing-velps");
            container.appendChild(span);
            el.appendChild(container);
        }

        console.log(reason);
        $compile(span)($scope); // Gives error [$compile:nonassign]
    };

    $scope.getRealAnnotationId = function(id){
        if (id < 0){
            return $scope.annotationids[id];
        }
        return id;
    };

    /**
     * Delete annotation
     * TODO: Make query to database
     * @param id annotation id
     */
    $scope.deleteAnnotation = function (id) {
        var annotationParents = document.querySelectorAll('[aid="{0}"]'.replace('{0}', id));
        var annotationHighlights = annotationParents[0].getElementsByClassName("highlighted");
        var savedHTML = "";

        for (var i = 0; i < annotationHighlights.length; i++) {
            var addHTML = annotationHighlights[i].innerHTML.replace('<span class="ng-scope">', '');
            addHTML = addHTML.replace('</span>', '');
            savedHTML += addHTML;
        }

        annotationParents[0].outerHTML = savedHTML;

        for (var a = 0; a < $scope.annotations.length; a++) {
            if (id == $scope.annotations[a].id)
                $scope.annotations.splice(a, 1);
        }

        var current_id = $scope.getRealAnnotationId(id);

        $scope.makePostRequest("/invalidate_annotation", {annotation_id: current_id}, function (json) {
            console.log(json);
        });
    };

    /**
     * Delete annotation
     * TODO: Make query to database
     * @param id annotation id
     */
    $scope.changeAnnotationPoints = function (id, points) {
        for (var i = 0; i < $scope.annotations.length; i++) {
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

        try {
            var range;
            if ($window.getSelection) {
                range = $window.getSelection();
            } else {
                range = document.getSelection();
            }
            console.log(range.toString());
            if (range.toString().length > 0){
                $scope.selectedArea = range.getRangeAt(0);
            } else {
                $scope.selectedArea = undefined;
            }

        } catch (err) {
            console.log(err);
        }
    };


    /**
     * Get velp by its id
     * @param id velp to find
     * @returns velp or undefined
     */
    $scope.getVelpById = function (id) {
        for (var i = 0; i < $scope.velps.length; i++)
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
     * TODO: When annotations can cross HTML-tags, change end coordinate according to end element
     * @param velp velp selected in velpSelection directive
     */
    $scope.useVelp = function (velp) {
        if (typeof $scope.selectedArea != "undefined") {

            var parelement = getElementParent($scope.selectedArea["commonAncestorContainer"]);
            var startElement = getElementParent($scope.selectedArea["startContainer"]);

            var innerDiv = document.createElement('div');
            var cloned = $scope.selectedArea.cloneContents();
            innerDiv.appendChild(cloned);

            while (!parelement.hasAttribute("t")) {
                parelement = getElementParent(parelement);
            }

            var element_path = getElementPositionInTree(startElement, []);
            var answer_id = getAnswerInfo(startElement);

            var startoffset = getRealStartOffset($scope.selectedArea["startContainer"], $scope.selectedArea["startOffset"]);
            var endOffset = $scope.selectedArea["endOffset"];
            if (innerDiv.childElementCount == 0)
                endOffset = startoffset + innerDiv.childNodes[innerDiv.childNodes.length - 1].length;

            var newAnnotation = {
                id: -($scope.annotations.length + 1),
                velp: velp.id,
                points: velp.points,
                doc_id: $scope.docId,
                visible_to: 4,
                content: velp.content,
                coord: {
                    start: {
                        par_id: parelement.id,
                        t: parelement.getAttribute("t"),
                        el_path: element_path,
                        offset: startoffset,
                        depth: element_path.length
                    },
                    end: {
                        par_id: parelement.id,
                        t: parelement.getAttribute("t"),
                        el_path: element_path,
                        offset: endOffset,
                        depth: element_path.length
                    }
                },
                comments: [
                    //{"content": "", "author": username}
                ]
            };

            if (answer_id !== null)
                newAnnotation.answer_id = answer_id.selectedAnswer.id;

            addAnnotationToCoord($scope.selectedArea, newAnnotation, true);
            $scope.annotations.push(newAnnotation);
            $scope.annotationids[newAnnotation.id] = newAnnotation.id;

            var nodeNums = getNodeNumbers($scope.selectedArea["startContainer"], newAnnotation.id, innerDiv);
            newAnnotation.coord.start.node = nodeNums[0];
            newAnnotation.coord.end.node = nodeNums[1];

            console.log(newAnnotation);

            $scope.makePostRequest("/add_annotation", newAnnotation, function (json) {
                $scope.annotationids[newAnnotation.id] = json.data;
                console.log("Annotation");
                console.log(json);
            });

            $scope.selectedArea = undefined;
            velp.used += 1;
        }
        $scope.annotationsAdded = true;
    };

    /**
     * Gets answer info of element. Returns null if no answer found.
     * @param start
     * @returns {*}
     */
    var getAnswerInfo = function (start){
        var myparent = getElementParent(start);

        if (myparent.tagName == "ANSWERBROWSER") {
            return angular.element(myparent).isolateScope();
        }

        if (myparent.hasAttribute("t"))
            return null;

        return getAnswerInfo(myparent);
    };

    /**
     * Gets array of element indexes from parent to start
     * @param start
     * @param array
     * @returns {*}
     */
    var getElementPositionInTree = function (start, array) {
        var myparent = getElementParent(start);

        if (myparent.hasAttribute("t")) {
            return array.reverse();
        }

        var count = 0;

        var children = getElementChildren(myparent);
        for (var i = 0; i < children.length; i++) {

            if (children[i] == start) {
                array.push(count);
                return getElementPositionInTree(myparent, array);
            }

            if (checkIfAnnotation(children[i])) {
                var innerElements = children[i].getElementsByClassName("highlighted")[0];
                var innerChildren = getElementChildren(innerElements);
                if (innerChildren.length > 2) {
                    count += innerChildren.length - 2;
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
    var getRealStartOffset = function (el, startoffset) {

        var startType = el.nodeName;
        var storedOffset = startoffset;

        while (el.previousSibling != null) {
            el = el.previousSibling;
            if (checkIfAnnotation(el)) {

                var innerElements = el.getElementsByClassName("highlighted")[0];
                storedOffset += innerElements.lastChild.innerHTML.length;

                if (innerElements.childNodes.length > 1) {
                    return storedOffset;
                }
            }
            else if (el.nodeName != startType) {
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
    var getNodeNumbers = function (el, aid, innerElement) {
        var parent = el;
        while(parent.nodeName == "#text"){
            parent = parent.parentNode;
        }
        var num = 0;

        var prevNodeName = parent.childNodes[0].nodeName;
        var aidFound = 0;

        for (var i = 0; i < parent.childNodes.length; i++) {

            if (checkIfAnnotation(parent.childNodes[i])) {

                if (parseInt(parent.childNodes[i].getAttribute("aid")) === aid) {

                    var startnum = num - 1;
                    num += innerElement.childNodes.length;

                    if (innerElement.firstChild.nodeName == prevNodeName) num--;
                    if (i < parent.childNodes.length - 1 && innerElement.lastChild.nodeName == parent.childNodes[i + 1].nodeName) num--;

                    if (startnum < 0) startnum = 0;
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
    $scope.getMarkingComments = function (id) {
        for (var i = 0; i < $scope.annotations.length; i++) {
            if (id == $scope.annotations[i].id)
                return $scope.annotations[i].comments;
        }
    };

    /**
     * Create pop over marking element
     * @param annotation marking info
     * @param show wether to show marking or not
     * @returns {Element}
     */
    $scope.createPopOverElement = function (annotation, show) {
        var element = document.createElement("span");
        element.setAttribute("annotation", "");
        //element.classList.add("annotation-element");
        var velp_data = $scope.getVelpById(annotation.velp);

        var velp_content;

        if (velp_data !== null){
            velp_content = velp_data.content;
        } else {
            velp_content = annotation.content;
        }
        console.log(annotation);
        element.setAttribute("velp", velp_content);
        element.setAttribute("points", annotation.points);
        element.setAttribute("aid", annotation.id);
        element.setAttribute("user", username);
        element.setAttribute("visibleto", annotation.visible_to);
        element.setAttribute("show", show);
        element.setAttribute("comments", JSON.stringify(annotation.comments));

        return element;
    };

    $scope.toggleAnnotation = function(annotation) {
        var parent = document.getElementById(annotation.coord.start.par_id);

        try {
            var annotationElement = parent.querySelectorAll("span[aid='{0}']".replace("{0}", annotation.id))[0];
            angular.element(annotationElement).isolateScope().showAnnotation();
            scrollToElement(annotationElement);
        } catch (e){
            // Find answer browser and isolate its scope
            // go to nextAnswer until answerid = annotation.answer_id
            // query selector element -> toggle annotation
            if (e.name == "TypeError"){
                scrollToElement(parent);
            }
        }
    };


    var scrollToElement = function (element) {
        if (!!element && element.scrollIntoView) {
            element.scrollIntoView();
        }
    }
}]);
