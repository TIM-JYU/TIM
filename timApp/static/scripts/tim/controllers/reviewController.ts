import angular from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {getElementParent, scrollToElement} from "../utils";
import {$compile, $http, $window} from "../ngimport";

/**
 * The controller handles the logic related to adding and removing annotations. It also handles the way how
 * the selected area is defined in the view. Requires `velpSelection` directive.
 *
 * @module reviewController
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
 */

const UNDEFINED = "undefined";

/**
 * Angular controller for handling annotations.
 * @lends module:reviewController
 */
timApp.controller("ReviewController", ["$scope", function($scope) {
    "use strict";
    const console = $window.console;

    const illegalClasses = ["annotation-element", "highlighted", "editorArea", "previewcontent"];

    $scope.annotationsAdded = false;
    $scope.selectedArea = null;
    $scope.selectedElement = null;
    $scope.item = $window.item;
    $scope.annotations = [];
    $scope.annotationids = {0: 0};
    $scope.zIndex = 1;
    const docId = $scope.docId;

    /**
     * Makes a post request to the given URL.
     * @method makePostRequest
     * @param url - Request URL
     * @param params - Query parameters
     * @param successMethod - Method to run if the request was successful.
     */
    $scope.makePostRequest = function(url, params, successMethod) {
        if (params === null) {
            throw new Error("'params' can not be null");
        }

        $http({
            method: "POST",
            url,
            data: params,
        }).then(function(data) {
            successMethod(data);
        });
    };

    $http.get("/{0}/get_annotations".replace("{0}", docId)).then(function(response) {
        $scope.annotations = response.data;
        $scope.loadDocumentAnnotations();
    });

    /**
     * Loads the document annotations into the view.
     * @method loadDocumentAnnotations
     */
    $scope.loadDocumentAnnotations = function() {
        const annotationsToRemove = [];

        for (let i = 0; i < $scope.annotations.length; i++) {

            const placeInfo = $scope.annotations[i].coord;
            const parent = document.getElementById(placeInfo.start.par_id);

            if (parent === null) {
                // TODO: Decide what to do, when parent element has been deleted, for now remove annotation from list
                annotationsToRemove.push($scope.annotations[i]);
                continue;
            }

            if ($scope.annotations[i].answer_id !== null) {
                if (!parent.classList.contains("has-annotation")) {
                    parent.classList.add("has-annotation");
                }
                continue;
            }

            if (parent.getAttribute("t") === placeInfo.start.t && placeInfo.start.offset !== null) {

                try {
                    let elements = parent.querySelector(".parContent");

                    const startElpath = placeInfo.start.el_path;

                    for (let j = 0; j < startElpath.length; j++) {
                        const elementChildren = getElementChildren(elements);
                        if (elementChildren[startElpath[j]] !== null) {
                            elements = elementChildren[startElpath[j]];
                        }
                    }

                    const startel = elements.childNodes[placeInfo.start.node];
                    const endel = elements.childNodes[placeInfo.end.node];

                    const range = document.createRange();
                    range.setStart(startel, placeInfo.start.offset);
                    range.setEnd(endel, placeInfo.end.offset);
                    $scope.addAnnotationToCoord(range, $scope.annotations[i], false);
                    addAnnotationToElement(parent, $scope.annotations[i], false, "Added also margin annotation");
                } catch (err) {
                    addAnnotationToElement(parent, $scope.annotations[i], false, "Could not show annotation in correct place");
                }
            } else {
                addAnnotationToElement(parent, $scope.annotations[i], false, "Paragraph has been modified");
            }
        }

        for (let k = 0; k < annotationsToRemove.length; k++) {
            const index = $scope.annotations.indexOf(annotationsToRemove[k]);
            $scope.annotations.splice(index, 1);
        }

        $scope.annotationsAdded = true;
    };

    /**
     * Gets the children (not childNodes) of the element.
     * @method getElementChildren
     * @param element - Element whose children are requested
     * @returns {Array} Element children
     */
    const getElementChildren = function(element) {
        /*if (typeof element.children !== "undefined")
         return element.children;
         */
        const children = [];
        for (let i = 0; i < element.childNodes.length; i++) {
            if (typeof element.childNodes[i].tagName !== "undefined") {
                children.push(element.childNodes[i]);
            }
        }
        return children;
    };

    /**
     * Gets element parent element until given attribute is present.
     * @method getElementParentUntilAttribute
     * @param element - Element whose parent is queried for
     * @param attribute - Attribute as a string
     * @returns {Element} First element that has the given attribute
     */
    const getElementParentUntilAttribute = function(element, attribute) {
        element = getElementParent(element);
        while (!element.hasAttribute(attribute)) {
            element = getElementParent(element);
        }
        return element;
    };

    const getFirstChildUntilNull = function(element) {
        if (typeof element.firstChild === UNDEFINED || element.firstChild === null) {
            return element;
        }
        return getFirstChildUntilNull(element.firstChild);
    };

    /**
     * Get last inner last child of the element.
     * @param element
     * @returns Element
     */
    const getLastChildUntilNull = function(element) {
        if (typeof element.lastChild === UNDEFINED || element.lastChild === null) {
            return element;
        }
        return getFirstChildUntilNull(element.lastChild);
    };

    /**
     * Checks if the given element is an annotation or not.
     * @method checkIfAnnotation
     * @param element - Element to check
     * @returns {boolean} - Whether the element is an annotation or not
     */
    const checkIfAnnotation = function(element) {
        if (element.nodeName === "ANNOTATION") {
            return true;
        }

        if (element.nodeName === "SPAN") {
            return element.hasAttribute("annotation");
        }

        return false;
    };

    /**
     * Loads the annotations to the given answer.
     * @method loadAnnotationsToAnswer
     * @param answer_id - Answer ID
     * @param par - Paragraph element
     */
    $scope.loadAnnotationsToAnswer = function(answer_id, par, showInPlace) {
        const annotations = $scope.getAnnotationsByAnswerId(answer_id);

        const oldAnnotations = par.querySelectorAll(".notes [aid]");
        if (oldAnnotations.length > 0) {
            const annotationParent = getElementParent(oldAnnotations[0]);
            for (let a = 0; a < oldAnnotations.length; a++) {
                annotationParent.removeChild(oldAnnotations[a]);
            }
        }

        for (let i = 0; i < annotations.length; i++) {
            const placeInfo = annotations[i].coord;

            const preElem = par.getElementsByTagName("PRE")[0];
            if (!preElem) {
                continue;
            }
            const element = preElem.firstChild;

            if (!showInPlace || placeInfo.start.offset === null) {
                addAnnotationToElement(par, annotations[i], false, "Added as margin annotation");
            } else {
                const range = document.createRange();
                range.setStart(element, placeInfo.start.offset);
                range.setEnd(element, placeInfo.end.offset);
                $scope.addAnnotationToCoord(range, annotations[i], false);
                addAnnotationToElement(par, annotations[i], false, "Added also margin annotation");
            }

        }
    };

    /**
     * Gets all the annotations with a given answer ID.
     * @method getAnnotationsByAnswerId
     * @param id - Answer ID
     * @returns {Array} Annotations of the answer
     */
    $scope.getAnnotationsByAnswerId = function(id) {
        const annotations = [];
        $scope.annotations.forEach(function(a) {
            if (a.answer_id !== null && a.answer_id === id) {
                annotations.push(a);
            }
        });

        annotations.sort(function(a, b) {
            return b.coord.start.offset - a.coord.start.offset;
        });

        return annotations;
    };

    /**
     * Adds an annotation to the given element in a given coordinate.
     * @method addAnnotationToCoord
     * @param range - Annotation coordinate
     * @param annotation - Annotation info
     * @param show - Whether annotation is shown when created or not
     */
    $scope.addAnnotationToCoord = function(range, annotation, show) {
        const span = $scope.createPopOverElement(annotation, show);
        try {
            range.surroundContents(span);
        } catch (err) {
            addAnnotationToElement(span, annotation, true, "Annotation crosses taglines");
            $scope.selectedArea = null;

            /*
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
     * Adds an annotation to the given element. The annotation will be placed in the margin.
     * @method addAnnotationToElement
     * @param el - Given element
     * @param annotation - Annotation info
     * @param show - Whether annotation is shown when created or not
     * @param reason - The reason why the annotation is put here (not implemented yet)
     */
    const addAnnotationToElement = function(el, annotation, show, reason) {
        annotation.reason = reason;
        const span = $scope.createPopOverElement(annotation, show);
        const text = document.createTextNode("\u00A0" + annotation.content + "\u00A0");
        span.appendChild(text);
        addElementToParagraphMargin(el, span);

        $compile(span)($scope); // Gives error [$compile:nonassign]
    };

    /**
     * Adds an element to the paragraph margin.
     * @method addElementToParagraphMargin
     * @param par - Paragraph where the element will be added
     * @param el - Element to add
     */
    const addElementToParagraphMargin = function(par, el) {
        let container = par.getElementsByClassName("notes");
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
     * Creates the velp badge button (the button with letter 'V' on it).
     * @method createVelpBadge
     */
    $scope.createVelpBadge = function(par) {
        $scope.velpBadgePar = par;
        if ($scope.velpBadge) {
            //$compile($scope.velpBadge)($scope);
            return $scope.velpBadge;
        }
        const btn = document.createElement("input");
        btn.type = "button";
        btn.classList.add("velp-badge");
        btn.classList.add("timButton");
        btn.value = "V";
        btn.id = "velpBadge";
        btn.onclick = function(e) {
            $scope.clearVelpBadge(e);
        };
        //btn.setAttribute("ng-click", "clearVelpBadge($event)");
        //$compile(btn)($scope);
        $scope.velpBadge = btn;
        return btn;
    };

    /**
     * Moves the velp badge to the correct element.
     * @method updateVelpBadge
     * @param oldElement - Element where the badge was
     * @param newElement - Element where the badge needs to be attached
     */
    $scope.updateVelpBadge = function(oldElement, newElement) {
        if (newElement === null) {
            return null;
        } else if (oldElement === null) {
            addElementToParagraphMargin(newElement, $scope.createVelpBadge(newElement.id));
        } else if (oldElement.id !== newElement.id) {
            $scope.clearVelpBadge(null);
            addElementToParagraphMargin(newElement, $scope.createVelpBadge(newElement.id));
        }
    };

    /**
     * Removes the velp badge and clears the element selection.
     * @param e - Current click event
     */
    $scope.clearVelpBadge = function(e) {
        const btn = $scope.velpBadge;
        if (btn) {
            const parent = getElementParent(btn);
            if (parent) {
                parent.removeChild(btn);
            }
        }

        if (e !== null) {
            $scope.selectedElement = null;
            $scope.selectedArea = null;
            $scope.updateVelpList();
            e.stopPropagation();
        }
    };

    /**
     * Returns the real ID of an annotation.
     * @method getRealAnnotationId
     * @param id - Annotation ID generated by the client
     * @returns {int} Annotation ID generated by the server
     */
    $scope.getRealAnnotationId = function(id) {
        if (id < 0) {
            return $scope.annotationids[id];
        }
        return id;
    };

    /**
     * Deletes the given annotation.
     * @method deleteAnnotation
     * @param id - Annotation ID
     * @param inmargin - Whether annotation is a margin annotation or not
     */
    $scope.deleteAnnotation = function(id, inmargin) {
        const annotationParents = document.querySelectorAll('[aid="{0}"]'.replace("{0}", id));
        const annotationHighlights = annotationParents[0].getElementsByClassName("highlighted");

        if (annotationParents.length > 1) {
            let savedHTML = "";
            for (let i = 0; i < annotationHighlights.length; i++) {
                let addHTML = annotationHighlights[i].innerHTML.replace('<span class="ng-scope">', "");
                addHTML = addHTML.replace("</span>", "");
                savedHTML += addHTML;
            }
            annotationParents[0].outerHTML = savedHTML;
            annotationParents[1].parentNode.removeChild(annotationParents[1]);
        } else {
            annotationParents[0].parentNode.removeChild(annotationParents[0]);
        }

        for (let a = 0; a < $scope.annotations.length; a++) {
            if (id === $scope.annotations[a].id) {
                $scope.annotations.splice(a, 1);
            }
        }

        const currentId = $scope.getRealAnnotationId(id);

        $scope.makePostRequest("/invalidate_annotation", {annotation_id: currentId}, function(json) {
        });
    };

    /**
     * Updates annotation data.
     * @method updateAnnotation
     * @param id - Annotation ID
     * @param inmargin - Whether the annotation is to be placed in the margin or not
     */
    $scope.updateAnnotation = function(id, inmargin) {
        const annotationParents = document.querySelectorAll('[aid="{0}"]'.replace("{0}", id));
        const annotationElement = $('[aid="{0}"]'.replace("{0}", id));
        const par = annotationElement.parents(".par");
        const annotationHighlights = annotationElement[0].getElementsByClassName("highlighted");
        if (!inmargin) {
            for (let a = 0; a < $scope.annotations.length; a++) {
                if (id === $scope.annotations[a].id) {
                    annotationElement[1].parentNode.removeChild(annotationElement[1]);
                    addAnnotationToElement(par[0], $scope.annotations[a], false, "Added also margin annotation");
                    //addAnnotationToElement($scope.annotations[a], false, "Added also margin annotation");
                }
            }

        } else {
            if (annotationParents.length > 1) {
                let savedHTML = "";
                for (let i = 0; i < annotationHighlights.length; i++) {
                    let addHTML = annotationHighlights[i].innerHTML.replace('<span class="ng-scope">', "");
                    addHTML = addHTML.replace("</span>", "");
                    savedHTML += addHTML;
                }
                annotationParents[0].outerHTML = savedHTML;

                // TODO: add redraw annotation text
            }
        }

    };

    /**
     * Changes annotation points.
     * @method changeAnnotationPoints
     * @param id - Annotation ID
     * @param points - Annotation points
     */
    $scope.changeAnnotationPoints = function(id, points) {
        for (let i = 0; i < $scope.annotations.length; i++) {
            if ($scope.annotations[i].id === id) {
                $scope.annotations[i].points = points;
                break;
            }
        }
    };

    $scope.changeAnnotationColor = function(id, color) {
        for (let i = 0; i < $scope.annotations.length; i++) {
            if ($scope.annotations[i].id === id) {
                $scope.annotations[i].color = color;
                break;
            }
        }
    };

    /**
     * Adds a comment to the given annotation.
     * @method addComment
     * @param id - Annotation ID
     * @param name - Commenter name
     * @param comment - Content of the given comment
     */
    $scope.addComment = function(id, name, comment) {
        for (let i = 0; i < $scope.annotations.length; i++) {
            if ($scope.annotations[i].id === id) {
                $scope.annotations[i].comments.push({
                    commenter_username: name,
                    content: comment,
                    comment_time: "now",
                    comment_relative_time: "just now",
                });
                break;
            }
        }
    };

    /**
     * Changes the visibility of an annotation. Visibility can be one of the following:
     *
     * - 1 = Myself
     * - 2 = Document owner
     * - 3 = Teacher
     * - 4 = Everyone.
     *
     * @method changeVisibility
     * @param id - Annoation ID
     * @param visibility - Annotation visibility (1, 2, 3 or 4)
     */
    $scope.changeVisibility = function(id, visiblity) {
        for (let i = 0; i < $scope.annotations.length; i++) {
            if ($scope.annotations[i].id === id) {
                $scope.annotations[i].visible_to = visiblity;
                break;
            }
        }
    };

    /**
     * Selects text range or just the element.
     * @method selectText
     * @todo When annotations can break tags, check annotations from all elements in the selection.
     */
    $scope.selectText = function($event) {
        const $par = $($event.target).parents(".par")[0];

        let oldElement = null;
        if ($scope.selectedElement !== null) {
            oldElement = $scope.selectedElement;
        }

        try {
            let range;
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
            //return;
        }

        if ($scope.selectedArea !== null) {
            // Check if selection breaks tags, has annotation as a parent or as a child.
            if (isSelectionTagParentsUnequal($scope.selectedArea) ||
                hasSelectionParentAnnotation($scope.selectedArea) ||
                hasSelectionChildrenAnnotation($scope.selectedArea)) {
                $scope.selectedArea = null;
            }
        } else {
            /*
            var elements = document.getElementsByClassName("lightselect");
            if (elements.length > 0)
                $scope.selectedElement = elements[0];
            */
            if ($par && $par.id) {
                $scope.selectedElement = $par;
            }

        }

        const newElement = $scope.selectedElement;
        $scope.updateVelpBadge(oldElement, newElement);
        if (newElement !== null) {
            $scope.updateVelpList();
        }
    };

    /**
     * Checks if the selection breaks HTML tags. Returns true if the tags were broken.
     * @method isSelectionTagParentsUnequal
     * @param range - Range object containing the user's selection
     * @returns {boolean} Whether the HTML tags were broken or not.
     */
    const isSelectionTagParentsUnequal = function(range) {
        return getElementParent(range.startContainer) !== getElementParent(range.endContainer);
    };

    /**
     * Checks iteratively if the element has an annotation element as its parent.
     * @method hasSelectionParentAnnotation
     * @param range - Range object containing the user's selection
     * @returns {boolean} Whether the element has an annotation element as its parent or not
     */
    const hasSelectionParentAnnotation = function(range) {
        let startcont = getElementParent(range.startContainer);
        while (!startcont.hasAttribute("t")) {
            startcont = getElementParent(startcont);
            if (checkIfAnnotation(startcont) || hasAnyIllegalClass(startcont)) {
                return true;
            }
        }

        let endcont = getElementParent(range.endContainer);
        while (!endcont.hasAttribute("t")) {
            endcont = getElementParent(endcont);
            if (checkIfAnnotation(endcont)) {
                return true;
            }
        }

        return false;
    };

    /**
     * Checks if the element has any class in `illegalClasses` array.
     * @method hasAnyIllegalClass
     * @param element - Element to be checked
     * @returns {boolean} Whether illegal classes were found or not.
     */
    const hasAnyIllegalClass = function(element) {
        for (let i = 0; i < illegalClasses.length; i++) {
            if (element.classList.contains(illegalClasses[i])) {
                return true;
            }
        }
        return false;
    };

    /**
     * Checks recursively if the selection has any annotation elements as children.
     * @method hasSelectionChildrenAnnotation
     * @param range - Range object containing the user's selection
     * @returns {boolean} Whether the selection has any annotation elements as children or not
     */
    const hasSelectionChildrenAnnotation = function(range) {
        const div = document.createElement("div");
        const clone = range.cloneContents();
        div.appendChild(clone);
        const children = div.childNodes;
        for (let i = 0; i < children.length; i++) {
            if (hasElementChildrenAnnotation(children[i])) {
                return true;
            }
        }
        return false;
    };

    /**
     * Checks if the element children has an annotation element.
     * @method hasElementChildrenAnnotation
     * @param element - Element to check
     * @returns {boolean} Whether annotation was found or not
     */
    const hasElementChildrenAnnotation = function(element) {

        if (checkIfAnnotation(element)) {
            return true;
        }

        const children = element.childNodes;

        for (let i = 0; i < children.length; i++)
            if (hasElementChildrenAnnotation(children[i])) {
                return true;
            }

        return false;
    };

    /**
     * Gets the velp by its ID. If no velps are found, this method returns null.
     * @method getVelpById
     * @param id - Velp to be found
     * @returns {Object|null} Velp or null
     */
    $scope.getVelpById = function(id) {
        if (typeof $scope.velps === UNDEFINED) {
            return null;
        }

        for (let i = 0; i < $scope.velps.length; i++) {
            if ($scope.velps[i].id === id) {
                return $scope.velps[i];
            }
        }

        return null;
    };

    /**
     * Get marking highlight style.
     * @method getAnnotationHighlight
     * @param points - Points given in marking
     * @returns {string} Highlight style
     */
    $scope.getAnnotationHighlight = function(points) {
        let highlightStyle = "positive";
        if (points === 0) {
            highlightStyle = "neutral";
        } else if (points < 0) {
            highlightStyle = "negative";
        }
        return highlightStyle;
    };

    /**
     * Detect user right to annotation to document.
     * @param points - Points given in velp or annotation
     * @returns {boolean} - Right to make annotations

    $scope.notAnnotationRights = function (points) {
        if ($scope.item.rights.teacher) {
            return false;
        } else {
            if (points === null) {
                return false;
            } else {
                return true;
            }
        }
    };
    */
    /**
     * Return caption text if the user has no rights to the annotation.
     * @param state - Whether the annotation is disabled to the user or not
     * @returns {string} - Caption text
     */

    $scope.showDisabledText = function(state) {
        if (state) {
            return "You need to have teacher rights to make annotations with points.";
        }
    };

    /**
     * Adds an annotation with the selected velp's data to
     * the selected text area or element.
     * @method useVelp
     * @todo When the annotations can cross HTML tags, end coordinate needs to be changed according to the end element.
     * @todo Also get the paragraph element (parelement) according to endContainer.
     * @param velp - Velp selected in the `velpSelection` directive
     */
    $scope.useVelp = function(velp) {

        if ($scope.velpToEdit.edit || $scope.selectedElement === null && $scope.selectedArea === null) {
            return;
        }

        /*
        if (velp.default_comment !== null && velp.default_comment.length > 0){
            comment.push({
                content: velp.default_comment,
                commenter_username: "me",
                comment_time: "now",
                comment_relative_time: "just now"
            });
        }
        */

        const newAnnotation: any = {
            id: -($scope.annotations.length + 1),
            velp: velp.id,
            points: velp.points,
            doc_id: $scope.docId,
            visible_to: velp.visible_to,
            content: velp.content,
            annotator_name: "me",
            edit_access: true,
            email: "",
            timesince: "just now",
            creationtime: "now",
            coord: {},
            color: velp.color,
            comments: [],
            default_comment: velp.default_comment,
            newannotation: true,
            user_id: -1,
        };

        if ($scope.selectedArea !== null) {

            let parelement = getElementParent($scope.selectedArea.startContainer);
            const startElement = getElementParent($scope.selectedArea.startContainer);

            const innerDiv = document.createElement("div");
            const cloned = $scope.selectedArea.cloneContents();
            innerDiv.appendChild(cloned);

            while (!parelement.hasAttribute("t")) {
                parelement = getElementParent(parelement);
            }

            const elementPath = getElementPositionInTree(startElement, []);
            const answerInfo = getAnswerInfo(startElement);

            if (answerInfo === null) {
                newAnnotation.user_id = null;
            } else {
                newAnnotation.user_id = $scope.$parent.selectedUser.id;
            }

            const startoffset = getRealStartOffset($scope.selectedArea.startContainer, $scope.selectedArea.startOffset);
            let endOffset = $scope.selectedArea.endOffset;
            if (innerDiv.childElementCount === 0) {
                endOffset = startoffset + (innerDiv.childNodes[innerDiv.childNodes.length - 1] as any).length;
            }

            newAnnotation.coord = {
                start: {
                    par_id: parelement.id,
                    t: parelement.getAttribute("t"),
                    el_path: elementPath,
                    offset: startoffset,
                    depth: elementPath.length,
                },
                end: {
                    par_id: parelement.id,
                    t: parelement.getAttribute("t"),
                    el_path: elementPath,
                    offset: endOffset,
                    depth: elementPath.length,
                },
            };

            if (answerInfo !== null) {
                newAnnotation.answer_id = answerInfo.selectedAnswer.id;
            }

            addAnnotationToElement($scope.selectedElement, newAnnotation, false, "Added also margin annotation");
            $scope.addAnnotationToCoord($scope.selectedArea, newAnnotation, true);
            $scope.annotations.push(newAnnotation);
            $scope.annotationids[newAnnotation.id] = newAnnotation.id;

            const nodeNums = getNodeNumbers($scope.selectedArea.startContainer, newAnnotation.id, innerDiv);
            newAnnotation.coord.start.node = nodeNums[0];
            newAnnotation.coord.end.node = nodeNums[1];

            $scope.selectedArea = undefined;

        } else if ($scope.selectedElement !== null) {

            newAnnotation.coord = {
                start: {
                    par_id: $scope.selectedElement.id,
                    t: $scope.selectedElement.getAttribute("t"),
                    offset: null,
                },
                end: {
                    par_id: $scope.selectedElement.id,
                    t: $scope.selectedElement.getAttribute("t"),
                },
            };

            const answerInfo = getAnswerInfo($scope.selectedElement);

            if (answerInfo !== null) {
                newAnnotation.answer_id = answerInfo.selectedAnswer.id;
            }

            addAnnotationToElement($scope.selectedElement, newAnnotation, true, "No coordinate found");
            $scope.annotationids[newAnnotation.id] = newAnnotation.id;
        }

        $scope.makePostRequest("/add_annotation", newAnnotation, function(json) {
            $scope.annotationids[newAnnotation.id] = json.data.id;
        });

        velp.used += 1;

        $scope.annotationsAdded = true;
    };

    /**
     * Gets the answer info of the element. Returns null if no answer found.
     * @method getAnswerInfo
     * @param start - Paragraph where the answerbrowser element is searched for.
     * @returns {Element|null} answerbrowser element or null.
     */
    const getAnswerInfo = function(start) {

        if (start.hasAttribute("attrs") && start.hasAttribute("t")) {
            const answ = start.getElementsByTagName("answerbrowser");
            if (answ.length > 0) {
                const answScope = angular.element(answ[0]).isolateScope();
                return answScope;
            }
            return null;
        }

        const myparent = getElementParent(start);

        if (myparent.tagName === "ANSWERBROWSER") {
            return angular.element(myparent).isolateScope();
        }

        if (myparent.hasAttribute("t")) {
            return null;
        }

        return getAnswerInfo(myparent);
    };

    /**
     * Gets an array of element indexes from the TIM paragraph element to the given element.
     * TIM paragraph element is defined as an element containing a 't' attribute.
     * If the given element is inside the TIM paragraph element, this method returns the following array: [0].
     * If the given element is inside the second child element of the TIM paragraph element, the following
     * array is returned: [0, 1].
     *
     * @method getElementPositionInTree
     * @param start - Starting element
     * @param array - Array of indexes
     * @returns {Array} Array of element indexes
     */
    const getElementPositionInTree = function(start, array) {
        const myparent = getElementParent(start);

        if (myparent.hasAttribute("t")) {
            return array.reverse();
        }

        let count = 0;

        const children = getElementChildren(myparent);
        for (let i = 0; i < children.length; i++) {

            if (children[i] === start) {
                array.push(count);
                return getElementPositionInTree(myparent, array);
            }

            if (checkIfAnnotation(children[i])) {
                const innerElements = children[i].getElementsByClassName("highlighted")[0];
                const innerChildren = getElementChildren(innerElements);
                if (innerChildren.length > 2) {
                    count += innerChildren.length - 2;
                }
                continue;
            }

            count++;
        }

        throw new Error("Element position in tree was not found");
    };

    /**
     * Get start offset according to the "original state" of DOM.
     * Ignores `annotation` elements, but not the elements inside the annotation.
     *
     * @method getRealStartOffset
     * @param el - Start container
     * @param startoffset - Original start offset
     * @returns {int} Start offset according to the "original state" of the DOM.
     */
    const getRealStartOffset = function(el, startoffset) {

        const startType = el.nodeName;
        let storedOffset = startoffset;

        while (el.previousSibling !== null) {
            el = el.previousSibling;
            if (checkIfAnnotation(el)) {

                const innerElements = el.getElementsByClassName("highlighted")[0];
                const lastInnerLastChild = getLastChildUntilNull(innerElements);
                storedOffset += lastInnerLastChild.length;

                // if (typeof innerElements.lastChild.innerHTML !== UNDEFINED)
                //     storedOffset += innerElements.lastChild.innerHTML.length;
                // else storedOffset += innerElements.lastChild.length;

                if (innerElements.childNodes.length > 1) {
                    return storedOffset;
                }
            } else if (el.nodeName !== startType) {
                return storedOffset;
            } else {
                storedOffset += el.length;
            }
        }

        return storedOffset;
    };

    /**
     * Gets the start and end node numbers of created annotation element.
     * Ignores annoations elements, but not elements inside it.
     *
     * @method getNodeNumbers
     * @param el - Start container
     * @param aid - Annotation ID
     * @param innerElement - Annotation content
     * @returns {Array} array with the start and end node numbers
     */
    const getNodeNumbers = function(el, aid, innerElement) {
        let parent = el;
        const lastInnerFirstChild = getFirstChildUntilNull(innerElement);
        const lastInnerLastChild = getLastChildUntilNull(innerElement);

        while (parent.nodeName === "#text") {
            parent = parent.parentNode;
        }

        let num = 0;

        let prevNodeName = parent.childNodes[0].nodeName;

        for (let i = 0; i < parent.childNodes.length; i++) {

            if (checkIfAnnotation(parent.childNodes[i])) {

                if (parseInt(parent.childNodes[i].getAttribute("aid")) === aid) {

                    let startnum = num - 1;
                    num += innerElement.childNodes.length;

                    if (lastInnerFirstChild.nodeName === prevNodeName) {
                        num--;
                    }
                    if (i < parent.childNodes.length - 1 && lastInnerLastChild.nodeName === parent.childNodes[i + 1].nodeName) {
                        num--;
                    }

                    if (startnum < 0) {
                        startnum = 0;
                    }
                    return [startnum, num];

                } else {
                    const innerEl = parent.childNodes[i].getElementsByClassName("highlighted")[0];
                    num += innerEl.childNodes.length;

                    if (lastInnerFirstChild.nodeName === prevNodeName) {
                        num--;
                    }

                    if (i < parent.childNodes.length - 1 && lastInnerLastChild.nodeName === parent.childNodes[i + 1].nodeName) {
                        num--;
                    }

                    continue;
                }
            }

            num++;
            prevNodeName = parent.childNodes[i].nodeName;
        }

        throw new Error("No node found");
    };

    /**
     * Gets the comments of the given annotation.
     * @method getAnnotationComments
     * @param id - Annotation ID
     * @returns Object - Annotation
     */
    $scope.getAnnotation = function(id) {
        for (let i = 0; i < $scope.annotations.length; i++) {
            if (id === $scope.annotations[i].id) {
                return $scope.annotations[i];
            }
        }
    };

    /**
     * Creates the actual (pop over) annotation element.
     * @method createPopOverElement
     * @param annotation - Annotation info
     * @param show - Whether to show the annotation on creation or not
     * @returns {Element} - Annotation element
     */
    $scope.createPopOverElement = function(annotation, show) {
        const element = document.createElement("span");

        //element.setAttribute("style", "line-height: 1em;");
        element.setAttribute("annotation", "");
        //element.classList.add("annotation-element");
        const velpData = $scope.getVelpById(annotation.velp);

        let velpContent;

        if (velpData !== null) {
            velpContent = velpData.content;
        } else {
            velpContent = annotation.content;
        }

        if (isUndefinedOrNull(annotation.default_comment)) {
            annotation.default_comment = "";
        }

        if (isUndefinedOrNull(annotation.color)) {
            annotation.color = "";
        }

        element.setAttribute("velp", velpContent);
        element.setAttribute("points", annotation.points);
        element.setAttribute("aid", annotation.id);
        element.setAttribute("newcomment", annotation.default_comment);
        element.setAttribute("annotator", annotation.annotator_name);
        element.setAttribute("editaccess", annotation.edit_access);
        element.setAttribute("timesince", annotation.timesince);
        element.setAttribute("creationtime", annotation.creationtime);
        element.setAttribute("email", annotation.email);
        element.setAttribute("visibleto", annotation.visible_to);
        element.setAttribute("color", annotation.color);
        element.setAttribute("show", show);
        element.setAttribute("newannotation", annotation.newannotation);
        if (typeof annotation.reason !== "undefined") {
            element.setAttribute("ismargin", "true");
        } else {
            element.setAttribute("ismargin", "false");
        }
        element.setAttribute("comments", JSON.stringify(annotation.comments));

        return element;
    };

    const isUndefinedOrNull = function(attr) {
        return typeof attr === UNDEFINED || attr === null;
    };

    /**
     * Shows the annotation (despite the name).
     * @todo If the annotation should be toggled, change all `showAnnotation()` methods to `toggleAnnotation()`.
     * @method toggleAnnotation
     * @param annotation - Annotation to be showed.
     */
    $scope.toggleAnnotation = function(annotation) {
        const parent = document.getElementById(annotation.coord.start.par_id);

        try {
            const annotationElement = parent.querySelectorAll("span[aid='{0}']".replace("{0}", annotation.id))[0];
            angular.element(annotationElement).isolateScope().showAnnotation();
            if (annotation.parentNode.classname === "notes") {
                const abl = angular.element(parent.getElementsByTagName("ANSWERBROWSERLAZY")[0]);
                abl.isolateScope().loadAnswerBrowser();
            }
            scrollToElement(annotationElement);
            //addAnnotationToElement(par, annotation, false, "Added also margin annotation");

        } catch (e) {
            // Find answer browser and isolate its scope
            // set answer id -> change answer to that
            // query selector element -> toggle annotation
            if (e.name === "TypeError" && annotation.answer_id !== null) {
                //var abl = angular.element(parent.getElementsByTagName("ANSWERBROWSERLAZY")[0]);
                let ab: any = parent.getElementsByTagName("ANSWERBROWSER")[0];

                if (typeof ab === UNDEFINED) {
                    const abl = angular.element(parent.getElementsByTagName("ANSWERBROWSERLAZY")[0]);
                    abl.isolateScope().loadAnswerBrowser();
                }
                if (this.selectedUser.id !== annotation.user_id) {
                    for (let i = 0; i < this.users.length; i++) {
                        if (this.users[i].id === annotation.user_id) {
                            $scope.changeUser(this.users[i], false);
                            break;
                        }
                    }

                }

                setTimeout(function() {
                    ab = angular.element(parent.getElementsByTagName("ANSWERBROWSER")[0]);
                    const abscope = ab.isolateScope();
                    abscope.review = true;
                    abscope.setAnswerById(annotation.answer_id);

                    setTimeout(function() {
                        const annotationElement = parent.querySelectorAll("span[aid='{0}']".replace("{0}", annotation.id))[0];
                        angular.element(annotationElement).isolateScope().showAnnotation();
                        $scope.$apply();
                        scrollToElement(annotationElement);
                    }, 500);
                }, 300);
            }
        }
    };
}]);
