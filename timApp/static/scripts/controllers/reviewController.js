/**
 * Created by sevitarv on 8.3.2016.
 * NOTE: This controller requires velpSelection directive to work!!!
 */

var angular;
var timApp = angular.module('timApp');

var UNDEFINED = "undefined";
var console = window.console;

console.log("reviewController.js added");

timApp.controller("ReviewController", ['$scope', '$http', '$window', '$compile', function ($scope, $http, $window, $compile) {
    "use strict";

    var illegalClasses = ["annotation-element", "highlighted", "editorArea", "previewcontent"];

    $scope.annotationsAdded = false;
    $scope.selectedArea = null;
    $scope.selectedElement = null;
    $scope.rights = $window.rights;

    $scope.annotationids = {0: 0};

    /**
     * Makes post request to given url
     * @param url request url
     * @param params query parameters
     * @param successMethod
     */
    $scope.makePostRequest = function (url, params, successMethod) {
        $http({
            method: 'POST',
            url: url,
            data: params
        }).then(function (data) {
            successMethod(data);
        });
    };


    /**
     * Loads document annotations into view
     */
    $scope.loadDocumentAnnotations = function () {
        var annotationsToRemove = [];

        for (var i = 0; i < $scope.annotations.length; i++) {

            var placeInfo = $scope.annotations[i].coord;
            var parent = document.getElementById(placeInfo.start.par_id);

            if (parent === null){
                // TODO: Decide what to do, when parent element has been deleted, for now remove annotation from list
                annotationsToRemove.push($scope.annotations[i]);
                continue;
            }

            if ($scope.annotations[i].answer_id !== null){
                if (!parent.classList.contains("has-annotation")){
                    parent.classList.add("has-annotation");
                }
                continue;
            }

            if (parent.getAttribute("t") === placeInfo.start.t && placeInfo.start.offset !== null) {

                try{
                    var elements = parent.querySelector(".parContent");

                    var start_elpath = placeInfo.start.el_path;

                    for (var j = 0; j < start_elpath.length; j++) {
                        var elementChildren = getElementChildren(elements);
                        if (elementChildren[start_elpath[j]] !== null)
                            elements = elementChildren[start_elpath[j]];
                    }

                    var startel = elements.childNodes[placeInfo.start.node];
                    var endel = elements.childNodes[placeInfo.end.node];

                    var range = document.createRange();
                    range.setStart(startel, placeInfo.start.offset);
                    range.setEnd(endel, placeInfo.end.offset);
                    $scope.addAnnotationToCoord(range, $scope.annotations[i], false);
                } catch (err) {
                    addAnnotationToElement(parent, $scope.annotations[i], false, "Could not show annotation in correct place");
                }
            } else {
                addAnnotationToElement(parent, $scope.annotations[i], false, "Paragraph has been modified");
            }
        }

        for (var k=0; k<annotationsToRemove.length; k++){
            console.log("Deleted annotation:");
            console.log(annotationsToRemove[k]);
            var index = $scope.annotations.indexOf(annotationsToRemove[k]);
            $scope.annotations.splice(index, 1);
        }

        $scope.annotationsAdded = true;
    };

    /**
     * Get children (not childNodes) of the element.
     * @param element
     * @returns {Array}
     */
    var getElementChildren = function (element) {
        /*if (typeof element.children !== "undefined")
         return element.children;
         */
        var children = [];
        for (var i = 0; i < element.childNodes.length; i++) {
            if (typeof element.childNodes[i].tagName !== "undefined") {
                children.push(element.childNodes[i]);
            }
        }
        return children;
    };

    /**
     * Get parent element of the given element
     * @param element element which parent is queried
     * @returns element parent
     */
    var getElementParent = function (element) {
        /*
         if (typeof element.parentElement !== "undefined")
         return element.parentElement;
         */
        var parent = element.parentNode;
        if (typeof parent.tagName !== "undefined") {
            return parent;
        }

        getElementParent(parent);
    };


    /**
     * Gets element parent element when certain attribute is present.
     * @param element element which parent is queried
     * @param attribute attribute as string
     * @returns first element parent that has given attribute
     */
    var getElementParentUntilAttribute = function(element, attribute){
        //console.log(element);
        element = getElementParent(element);
        while (!element.hasAttribute(attribute)) {
            element = getElementParent(element);
        }
        return element;
    };

    /**
     * Checks if given element is an annotation
     * @param element element to check
     * @returns boolean
     */
    var checkIfAnnotation = function (element) {
        if (element.nodeName === "ANNOTATION")
            return true;

        if (element.nodeName === "SPAN") {
            return element.hasAttribute("annotation");
        }

        return false;
    };

    /**
     * Loads annotations to given answer.
     * @param answer_id answer id
     * @param par_id paragraph id
     */
    $scope.loadAnnotationsToAnswer = function (answer_id, par_id) {
        var par = document.getElementById(par_id);
        var annotations = $scope.getAnnotationsByAnswerId(answer_id);

        for (var i = 0; i < annotations.length; i++) {
            var placeInfo = annotations[i].coord;

            var element = par.getElementsByTagName("PRE")[0].firstChild;
            console.log(element);
            console.log(annotations[i]);

            var range = document.createRange();
            range.setStart(element, placeInfo.start.offset);
            range.setEnd(element, placeInfo.end.offset);
            $scope.addAnnotationToCoord(range, annotations[i], false);
        }
    };

    /**
     * Get all annotaions by given answer id.
     * @param id answer id
     * @returns {Array} answer's annotation
     */
    $scope.getAnnotationsByAnswerId = function (id) {
        var annotations = [];
        $scope.annotations.forEach(function (a) {
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
    $scope.addAnnotationToCoord = function (range, annotation, show) {
        var span = $scope.createPopOverElement(annotation, show);
        try {
            range.surroundContents(span);
        } catch (err) {
            // TODO: Add annotation to the "club of missing velps"

            addAnnotationToElement(span, annotation,true, "Annotation crosses taglines");
            $scope.selectedArea = null;

            /*
            console.log(err);
            var new_range = document.createRange();

            var el = range.startContainer;
            var start = range.startOffset;
            var end = range.startContainer.parentNode.innerHTML.length - 1;

            new_range.setStart(el, start);
            new_range.setEnd(el, end);
            $scope.addAnnotationToCoord(new_range, annotation, show);
            */
        }

        $compile(span)($scope); // Gives error [$compile:nonassign]
    };

    /**
     * Adds annotation to given element. Puts annotation to the "club of missing velps".
     * @param el given element
     * @param annotation annotation info
     * @param show show by default
     * @param reason why annotation is put to here
     */
    var addAnnotationToElement = function (el, annotation, show, reason) {
        annotation.reason = reason;
        var span = $scope.createPopOverElement(annotation, show);
        var text = document.createTextNode("\u00A0" + annotation.content + "\u00A0");
        span.appendChild(text);
        addElementToParagraphMargin(el, span);

        console.log(reason);
        $compile(span)($scope); // Gives error [$compile:nonassign]
    };

    /**
     * Adds element to paragraph margin.
     * @param par paragraph
     * @param el element to add
     */
    var addElementToParagraphMargin = function (par, el) {
        var container = par.getElementsByClassName("notes");
        if (container.length > 0) {
            container[0].appendChild(el);
        } else {
            container = document.createElement("div");
            container.classList.add("notes");
            container.appendChild(el);
            par.appendChild(container);
        }
    };

    /**
     * Stores element for velping.
     */
    var createVelpBadge = function () {
        var btn = document.createElement("input");
        btn.type = "button";
        btn.classList.add("velp-badge");
        btn.classList.add("timButton");
        btn.value = "V";
        btn.id = "velpBadge";
        btn.setAttribute("ng-click", "clearVelpBadge($event)");
        $compile(btn)($scope);
        return btn;
    };

    /**
     * Updates velp badge to correct element
     * @param oldElment element, where badge was
     * @param newElement element, where badge needs to go
     * @returns {null}
     */
    $scope.updateVelpBadge = function(oldElment, newElement){
        if (newElement === null) {
            return null;
        } else if (oldElment === null){
            addElementToParagraphMargin(newElement, createVelpBadge(newElement.id));
        } else if (oldElment.id !== newElement.id){
            $scope.clearVelpBadge(null);
            addElementToParagraphMargin(newElement, createVelpBadge(newElement.id));
        }
    };

    /**
     * Closes element selection
     */
    $scope.clearVelpBadge = function (e) {
        var btn = document.getElementById("velpBadge");
        if (btn !== null){
            var parent = getElementParent(btn);
            parent.removeChild(btn);
        }

        if (e !== null) {
            console.log(e);
            $scope.selectedElement = null;
            $scope.selectedArea = null;
            e.stopPropagation();

            while (!parent.hasAttribute("t")) {
                parent = getElementParent(parent);
            }
        }
    };


    /**
     * Returns real ids of annotations
     * @param id
     * @returns {*}
     */
    $scope.getRealAnnotationId = function (id) {
        if (id < 0) {
            return $scope.annotationids[id];
        }
        return id;
    };

    /**
     * Delete annotation
     * TODO: Make query to database
     * @param id annotation id
     */
    $scope.deleteAnnotation = function (id, inmargin) {
        console.log(inmargin);
        var annotationParents = document.querySelectorAll('[aid="{0}"]'.replace('{0}', id));
        var annotationHighlights = annotationParents[0].getElementsByClassName("highlighted");

        if (!inmargin) {
            var savedHTML = "";
            for (var i = 0; i < annotationHighlights.length; i++) {
                var addHTML = annotationHighlights[i].innerHTML.replace('<span class="ng-scope">', '');
                addHTML = addHTML.replace('</span>', '');
                savedHTML += addHTML;
            }
            annotationParents[0].outerHTML = savedHTML;
        } else {
            annotationParents[0].parentNode.removeChild(annotationParents[0]);
        }

        for (var a = 0; a < $scope.annotations.length; a++) {
            if (id === $scope.annotations[a].id)
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
     * @param points annotation points
     */
    $scope.changeAnnotationPoints = function (id, points) {
        for (var i = 0; i < $scope.annotations.length; i++) {
            if ($scope.annotations[i].id === id) {
                $scope.annotations[i].points = points;
                break;
            }
        }
    };

    /**
     * Select text range. If range breaks taglines, method sets range "as long as possible"
     * // TODO: When annotations can cross taglines, check annotations from all elements in selection
     */
    $scope.selectText = function () {

        var oldElement = null;
        if ($scope.selectedElement !== null)
            oldElement = $scope.selectedElement;

        try {
            var range;
            if ($window.getSelection) {
                range = $window.getSelection();
            } else {
                range = document.getSelection();
            }
            if (range.toString().length > 0) {
                $scope.selectedArea = range.getRangeAt(0);
                $scope.selectedElement = getElementParentUntilAttribute($scope.selectedArea.startContainer, "t");
            } else {
                $scope.selectedArea = null;
            }
        } catch (err) {
            console.log("error in method selectText");
            console.log(err);
            //return;
        }

        if ($scope.selectedArea !== null){
            // Check if selection breaks tags, has annotation as a parent or as a child.
            if (isSelectionTagParentsUnequal($scope.selectedArea) ||
                hasSelectionParentAnnotation($scope.selectedArea) ||
                hasSelectionChildrenAnnotation($scope.selectedArea)){
                $scope.selectedArea = null;
            }
        } else if($scope.selectedArea === null) {
            var elements = document.getElementsByClassName("lightselect");
            console.log(elements);
            if (elements.length > 0)
                $scope.selectedElement = elements[0];
        }

        var newElement = $scope.selectedElement;
        $scope.updateVelpBadge(oldElement, newElement);
        if (newElement !== null)
            $scope.updateVelpList();
    };

    /**
     * Checks if given range object breaks taglines. Returns true if taglines are broken.
     * @param range Range object
     * @returns {boolean}
     */
    var isSelectionTagParentsUnequal = function(range){
        console.log("check tags");
        return getElementParent(range.startContainer) !== getElementParent(range.endContainer);
    };

    /**
     * Checks iteratively if element has annotation as parent
     * @param range Range object
     * @returns {boolean}
     */
    var hasSelectionParentAnnotation = function(range){
        console.log("check parents");
        var startcont = getElementParent(range.startContainer);
        while (!startcont.hasAttribute("t")){
            startcont = getElementParent(startcont);
            if (checkIfAnnotation(startcont) || hasAnyIllegalClass(startcont))
                return true;
        }

        var endcont = getElementParent(range.endContainer);
        while (!endcont.hasAttribute("t")){
            endcont = getElementParent(endcont);
            if (checkIfAnnotation(endcont))
                return true;
        }

        return false;
    };

    /**
     * Checks if element has any class in illegalClasses array.
     * @param element element to be checked
     * @returns {boolean} whether illegal class was found or not.
     */
    var hasAnyIllegalClass = function(element){
        for (var i=0; i<illegalClasses.length; i++){
            if (element.classList.contains(illegalClasses[i]))
                return true;
        }
        return false;
    };

    /**
     * Checks recursevily if selection has annotations as children.
     * @param range selection
     * @returns {boolean}
     */
    var hasSelectionChildrenAnnotation = function(range){
        console.log("check children");
        var div = document.createElement("div");
        var clone = range.cloneContents();
        div.appendChild(clone);
        var children = div.childNodes;
        console.log(div);
        for (var i=0; i<children.length; i++){
            if (hasElementChildrenAnnotation(children[i])){
                console.log("Child has annotation");
                return true;
            }
        }
        return false;
    };

    /**
     * Check if element children has annotation
     * @param element element to check
     * @returns {boolean}
     */
    var hasElementChildrenAnnotation = function(element){

        if (checkIfAnnotation(element)){
            return true;
        }

        var children = element.childNodes;

        for (var i=0; i<children.length; i++)
            if(hasElementChildrenAnnotation(children[i]))
                return true;

        return false;
    };

    /**
     * Get velp by its id
     * @param id velp to find
     * @returns velp or undefined
     */
    $scope.getVelpById = function (id) {
        for (var i = 0; i < $scope.velps.length; i++)
            if ($scope.velps[i].id === id)
                return $scope.velps[i];

        return null;
    };

    /**
     * Get marking highlight style
     * @param points points given in marking
     * @returns string highligt style
     */
    $scope.getMarkingHighlight = function (points) {
        var highlightStyle = "positive";
        if (points === 0)
            highlightStyle = "neutral";
        else if (points < 0)
            highlightStyle = "negative";
        return highlightStyle;
    };

    /**
     * Uses selected velp, if area is selected.
     * TODO: When annotations can cross HTML-tags, change end coordinate according to end element
     * TODO: Also get parelement according to endContainer
     * @param velp velp selected in velpSelection directive
     */
    $scope.useVelp = function (velp) {

        if ($scope.velpToEdit.id >= 0) return;

        var newAnnotation = {
            id: -($scope.annotations.length + 1),
            velp: velp.id,
            points: velp.points,
            doc_id: $scope.docId,
            visible_to: 4,
            content: velp.content,
            annotator_name: "me",
            email: "",
            timesince: "just now",
            creationtime: "now",
            coord: {},
            comments: []
        };


        if ($scope.selectedArea !== null) {

            var parelement = getElementParent($scope.selectedArea.startContainer);
            var startElement = getElementParent($scope.selectedArea.startContainer);

            var innerDiv = document.createElement('div');
            var cloned = $scope.selectedArea.cloneContents();
            innerDiv.appendChild(cloned);

            while (!parelement.hasAttribute("t")) {
                parelement = getElementParent(parelement);
            }

            var element_path = getElementPositionInTree(startElement, []);
            var answer_id = getAnswerInfo(startElement);

            var startoffset = getRealStartOffset($scope.selectedArea.startContainer, $scope.selectedArea.startOffset);
            var endOffset = $scope.selectedArea.endOffset;
            if (innerDiv.childElementCount === 0)
                endOffset = startoffset + innerDiv.childNodes[innerDiv.childNodes.length - 1].length;

            newAnnotation.coord = {
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
            };

            if (answer_id !== null)
                newAnnotation.answer_id = answer_id.selectedAnswer.id;

            $scope.addAnnotationToCoord($scope.selectedArea, newAnnotation, true);
            $scope.annotations.push(newAnnotation);
            $scope.annotationids[newAnnotation.id] = newAnnotation.id;

            var nodeNums = getNodeNumbers($scope.selectedArea.startContainer, newAnnotation.id, innerDiv);
            newAnnotation.coord.start.node = nodeNums[0];
            newAnnotation.coord.end.node = nodeNums[1];

            console.log(newAnnotation);

            $scope.makePostRequest("/add_annotation", newAnnotation, function (json) {
                $scope.annotationids[newAnnotation.id] = json.data.id;
                console.log("Annotation to text");
                console.log(json);
            });

            $scope.selectedArea = undefined;
            velp.used += 1;
        } else if ($scope.selectedElement !== null) {
            newAnnotation.coord = {
                start: {
                    par_id: $scope.selectedElement.id,
                    t: $scope.selectedElement.getAttribute("t")
                },
                end: {
                    par_id: $scope.selectedElement.id,
                    t: $scope.selectedElement.getAttribute("t")
                }
            };
            addAnnotationToElement($scope.selectedElement, newAnnotation, true, "No coordinate found");
            $scope.annotations.push(newAnnotation);
            $scope.annotationids[newAnnotation.id] = newAnnotation.id;

            $scope.makePostRequest("/add_annotation", newAnnotation, function (json) {
                $scope.annotationids[newAnnotation.id] = json.data.id;
                console.log("Annotation to element");
                console.log(json);
            });
            velp.used += 1;
        }

        $scope.annotationsAdded = true;
    };

    /**
     * Gets answer info of element. Returns null if no answer found.
     * @param start
     * @returns {*}
     */
    var getAnswerInfo = function (start) {
        var myparent = getElementParent(start);

        if (myparent.tagName === "ANSWERBROWSER") {
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

            if (children[i] === start) {
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
     * Ignores annoations elements, but not elements inside annotation
     * @param el start container
     * @param startoffset original start offset
     * @returns {*}
     */
    var getRealStartOffset = function (el, startoffset) {

        var startType = el.nodeName;
        var storedOffset = startoffset;

        while (el.previousSibling !== null) {
            el = el.previousSibling;
            if (checkIfAnnotation(el)) {

                var innerElements = el.getElementsByClassName("highlighted")[0];
                storedOffset += innerElements.lastChild.innerHTML.length;

                if (innerElements.childNodes.length > 1) {
                    return storedOffset;
                }
            }
            else if (el.nodeName !== startType) {
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
        while (parent.nodeName === "#text") {
            parent = parent.parentNode;
        }
        var num = 0;

        var prevNodeName = parent.childNodes[0].nodeName;

        for (var i = 0; i < parent.childNodes.length; i++) {

            if (checkIfAnnotation(parent.childNodes[i])) {

                if (parseInt(parent.childNodes[i].getAttribute("aid")) === aid) {

                    var startnum = num - 1;
                    num += innerElement.childNodes.length;

                    if (innerElement.firstChild.nodeName === prevNodeName) num--;
                    if (i < parent.childNodes.length - 1 && innerElement.lastChild.nodeName === parent.childNodes[i + 1].nodeName) num--;

                    if (startnum < 0) startnum = 0;
                    return [startnum, num];

                } else {
                    var innerEl = parent.childNodes[i].getElementsByClassName("highlighted")[0];
                    num += innerEl.childNodes.length;

                    if (innerEl.firstChild.firstChild.nodeName === prevNodeName) num--;
                    if (i < parent.childNodes.length - 1 && innerEl.lastChild.lastChild.nodeName === parent.childNodes[i + 1].nodeName) num--;

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
            if (id === $scope.annotations[i].id)
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

        //element.setAttribute("style", "line-height: 1em;");
        element.setAttribute("annotation", "");
        //element.classList.add("annotation-element");
        var velp_data = $scope.getVelpById(annotation.velp);

        var velp_content;

        if (velp_data !== null) {
            velp_content = velp_data.content;
        } else {
            velp_content = annotation.content;
        }
        console.log(annotation);
        element.setAttribute("velp", velp_content);
        element.setAttribute("points", annotation.points);
        element.setAttribute("aid", annotation.id);
        element.setAttribute("annotator", annotation.annotator_name);
        element.setAttribute("timesince", annotation.timesince);
        element.setAttribute("creationtime", annotation.creationtime);
        element.setAttribute("email", annotation.email);
        element.setAttribute("visibleto", annotation.visible_to);
        element.setAttribute("show", show);
        if (typeof annotation.reason !== "undefined")
            element.setAttribute("ismargin", true);
        else
            element.setAttribute("ismargin", false);
        element.setAttribute("comments", JSON.stringify(annotation.comments));

        return element;
    };

    $scope.toggleAnnotation = function (annotation) {
        var parent = document.getElementById(annotation.coord.start.par_id);

        try {
            var annotationElement = parent.querySelectorAll("span[aid='{0}']".replace("{0}", annotation.id))[0];
            angular.element(annotationElement).isolateScope().showAnnotation();
            scrollToElement(annotationElement);
        } catch (e) {
            // Find answer browser and isolate its scope
            // set answer id -> change answer to that
            // query selector element -> toggle annotation
            if (e.name === "TypeError" && annotation.answer_id !== null) {
                //var abl = angular.element(parent.getElementsByTagName("ANSWERBROWSERLAZY")[0]);
                var ab = parent.getElementsByTagName("ANSWERBROWSER")[0];

                if (typeof ab === UNDEFINED) {
                    var abl = angular.element(parent.getElementsByTagName("ANSWERBROWSERLAZY")[0]);
                    abl.isolateScope().loadAnswerBrowser();
                }

                setTimeout(function () {
                    ab = angular.element(parent.getElementsByTagName("ANSWERBROWSER")[0]);
                    var abscope = ab.isolateScope();
                    abscope.review = true;
                    abscope.setAnswerById(annotation.answer_id);

                    setTimeout(function () {
                        var annotationElement = parent.querySelectorAll("span[aid='{0}']".replace("{0}", annotation.id))[0];
                        angular.element(annotationElement).isolateScope().showAnnotation();
                        $scope.$apply();
                        scrollToElement(annotationElement);
                        console.log(ab);
                    }, 500);
                }, 100);
            }
        }
    };


    var scrollToElement = function (element) {
        if (!!element && element.scrollIntoView) {
            element.scrollIntoView();
        }
    };
}]);
