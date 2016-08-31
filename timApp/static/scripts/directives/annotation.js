/**
 * The directive handles the logic behind a single annotation.
 * The annotation must be implemented to the element as a directive declaration,
 * because IE does not support custom elements reliably. In example, use:
 * `<span annotation="">...</span>` instead of `<annotation>...</annotation>`.
 * 
 * @module annotation
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
 */

/*
 var angular;
 var timApp = angular.module('timApp');
 */

/** Directive for a single annotation.
 * @lends module:reviewController
 */
timApp.directive("annotation",['$window', function ($window, $timeout) {
    "use strict";
    var console = $window.console;
    return {
        templateUrl: "/static/templates/annotation.html",
        transclude: true,
        scope: {
            show: '=',
            points: '=',
            visibleto: '=',
            comments: '=',
            aid: '=',
            ismargin: '=',
            annotator: '@',
            editaccess: '=',
            //email: '@',
            timesince: '@',
            creationtime: '@',
            velp: '@',
            newannotation: '@',
            showHidden: '@'
        },

        link: function (scope, element) {
            scope.newComment = "";
            scope.velpElement = null;
            scope.ctrlDown = false;
            scope.ctrlKey = 17;
            scope.visible_options = {
                "type": "select",
                "value": scope.visibleto,
                "values": [1, 2, 3, 4],
                "names": ["Just me", "Document owner", "Teachers", "Everyone"]
            };
            scope.newannotation = false;
            scope.marginonly = false;


            // Original visibility, or visibility in session
            // TODO: origin visibility
            scope.original = {
                points: scope.points,
                velp: scope.velp,
                visible_to: scope.visibleto,
                comment: scope.newComment,
                annotation_id: scope.aid
            };

            /**
             * Toggles the visibility of the annotation.
             * @method toggleAnnotation
             */
            scope.toggleAnnotation = function () {
                if (scope.velpElement === null) {
                    scope.velpElement = element[0].getElementsByClassName("annotation-info")[0];
                }
                var elementName = scope.velpElement.parentNode.offsetParent.className;
                var annotationParents = document.querySelectorAll('[aid="{0}"]'.replace('{0}', scope.aid));
                if ( elementName === "notes" &&  annotationParents.length > 1 ){
                    if (scope.aid > 0)
                    {
                        if (true) {// todo: detect is scope.$$prevSibling visible $(scope.$$prevSibling).is(':visible')
                            scope.$$prevSibling.show = !scope.$$prevSibling.show;
                            scope.$$prevSibling.updateVelpZIndex();
                        } else {
                             scope.show = !scope.show;
                            if (scope.show) {
                                scope.updateVelpZIndex();
                            }
                        }
                    } else {
                        scope.$$nextSibling.show = !scope.$$nextSibling.show;
                        scope.$$nextSibling.updateVelpZIndex();
                        //scope.show = !scope.show;
                        //scope.velpElement.parentNode. = !scope.velpElement.parentNode.$$prevSibling.show;
                        //annotationParents.show = !annotationParents[0].show;
                        //scope.show = !scope.show;
                    }

                    //scope.isolateScope.show = !scope.isolateScope.show;
                } else {
                    scope.show = !scope.show;
                    if (scope.show) {
                        scope.updateVelpZIndex();
                    } else {
                     console.log(scope.$parent.annotations);
                    }
                }
            };


            /**
             * Updates the z-index attribute of the annotation.
             * @method updateVelpZIndex
             */
            scope.updateVelpZIndex = function () {
                if (scope.velpElement === null) {
                    scope.velpElement = element[0].getElementsByClassName("annotation-info")[0];
                    scope.velpElement.style.zIndex = scope.$parent.zIndex.toString();
                    scope.$parent.zIndex++;
                } else {
                    scope.velpElement.style.zIndex = scope.$parent.zIndex.toString();
                    scope.$parent.zIndex++;
                }
            };

            /**
             * Shows the annotation.
             * @method showAnnotation
             */
            scope.showAnnotation = function () {
                scope.showHidden = false;
                scope.newannotation = false;
                scope.show = true;

                scope.updateVelpZIndex();
            };

            /**
             * Focuses on the comment field of the annotation.
             * @method focusTextarea
             */
            scope.focusTextarea =function(){
               return true;
            };



            /**
             * Deletes the selected annotation. Queries parent scope and deletes
             * the corresponding annotation from there.
             * @method deleteAnnotation
             */
            scope.deleteAnnotation = function () {

                if (scope.comments.length < 2) {
                    if (!$window.confirm("Delete - are you sure?")) {
                        return;
                    }
                    scope.$parent.deleteAnnotation(scope.aid, scope.ismargin);
                    scope.toggleAnnotation();
                }
            };

            /**
             * Updates the annotation and toggles its visibility in the margin.
             * @method updateAnnotation
             */
            scope.updateAnnotation = function () {
                var margin = false;
                if (scope.velpElement.parentNode.offsetParent.className ==="notes"){
                    margin = true;
                }
                scope.$parent.updateAnnotation(scope.aid, margin);
            };

            /**
             * Changes points of the selected annotation. Qeries parent scope
             * and changes the points of the corresponding annotation.
             * @method changePoints
             */
            scope.changePoints = function () {
                console.log(scope.points);
                scope.$parent.changeAnnotationPoints(scope.aid, scope.points);
            };

            /**
             * Saves the changes made to the annotation. Queries parent scope
             * and updates the changes of the corresponding annotation.
             * @method saveChanges
             */
            scope.saveChanges = function () {
                var id = scope.$parent.getRealAnnotationId(scope.aid);

                // Add comment
                if (scope.newComment.length > 0) {
                    var comment = scope.newComment;

                    var data = {annotation_id: id, content: scope.newComment};
                    scope.$parent.makePostRequest("/add_annotation_comment", data, function (json) {
                        scope.comments.push({
                            commenter_username: json.data.name,
                            content: comment,
                            comment_time: "now",
                            comment_relative_time: "just now"
                        });
                        scope.$parent.addComment(scope.aid, json.data.name, comment);
                        scope.updateAnnotation();
                    });

                } else {
                    scope.updateAnnotation();
                }
                scope.newComment = "";
                if (scope.visible_options.value !== scope.original.visible_to) {
                    scope.$parent.changeVisibility(scope.aid, scope.visible_options.value);
                }
                scope.original = {
                    points: scope.points,
                    annotation_id: id,
                    visible_to: scope.visible_options.value,
                    velp: scope.velp,
                    comment: scope.newComment,
                    doc_id: scope.$parent.docId
                };

                scope.$parent.makePostRequest("/update_annotation", scope.original, function (json) {
                    console.log(json);
                });
            };

            /**
             * Checks if the user has rights to edit the annotation.
             * @method checkRights
             * @returns {boolean} Whether the user has rights or not
             */
            scope.checkRights = function () {
                return scope.editaccess !== 1;
            };

            /**
             * Detect user right to annotation to document.
             * @param points - Points given in velp or annotation
             * @returns {boolean} - Whether user has rights to make annotations
             */

            scope.notAnnotationRights = function (points) {
                if (scope.$parent.rights.teacher) {
                    return false;
                } else {
                    if (points === null) {
                        return false;
                    } else {
                        return true;
                    }
                }
            };

            /**
             * Return true if user has teacher rights.
             * @returns {boolean} Whether the user has teacher rights or not
             */
            scope.allowChangePoints = function () {
                return scope.$parent.rights.teacher;
            };



            /**
             * Checks if the annotation is changed compared to its last saved state.
             * @method checkIfChanged
             * @returns {boolean} - Whether any modifications were made or not
             */
            scope.checkIfChanged = function () {
                if (scope.original.points !== scope.points)
                    return true;
                if (scope.original.comment !== scope.newComment)
                    return true;
                if (scope.original.visible_to !== scope.visible_options.value)
                    return true;
                if (scope.original.velp !== scope.velp)
                    return true;
                return false;
            };

            /**
             * Detects the `Ctrl + S` and `Ctrl+Enter` key strokes on the text area.
             * @method keyDownFunc
             * @param event - Current event
             */
            scope.keyDownFunc = function (event) {
                 if (event.keyCode === scope.ctrlKey) {
                     scope.ctrlDown = true;
                 }
                if (scope.ctrlDown && (String.fromCharCode(event.which).toLowerCase() === 's' || event.keyCode === 13 )) {
                    event.preventDefault();
                    scope.ctrlDown = false;
                    if (scope.checkIfChanged()){
                        scope.saveChanges();
                    } else {
                        scope.toggleAnnotation();
                    }
                }
		    };

            /**
             * Detects if `Ctrl`-key is released.
             * @method keyUpFunc
             * @param event - Current event
             */
            scope.keyUpFunc = function (event) {
                 if (event.keyCode === scope.ctrlKey) {
                     scope.ctrlDown = false;
                 }
		    };

           scope.$watch('newannotation', function(value) {
                if(value) {
                    element.find('textarea').focus();
                    scope.newannotation = false;
                    console.log("focus on new annotation");
                }
            });

            setTimeout(function(){
                if (scope.show) scope.updateVelpZIndex();
            }, 0);
        }
    };
}]);


