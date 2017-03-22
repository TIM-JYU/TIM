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

 var angular;
 var timApp = angular.module('timApp');

/** Directive for a single annotation.
 * @lends module:reviewController
 */
timApp.directive("annotation",['$window', function ($window) {
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
            newcomment: '@',
            //email: '@',
            timesince: '@',
            creationtime: '@',
            velp: '@',
            color: '@',
            newannotation: '@',
            showHidden: '@'
        },

        link: function (scope, element) {
            scope.velpElement = null;
            scope.ctrlDown = false;
            scope.ctrlKey = 17;
            scope.visible_options = {
                "type": "select",
                "value": scope.visibleto,
                "title": "Visible to",
                "values": [1, 2, 3, 4],
                "names": ["Just me", "Document owner", "Teachers", "Everyone"]
            };
            scope.newannotation = false;
            scope.marginonly = false;

            if (scope.newcomment === null) scope.newcomment = "";

            scope.isvalid = {
                points: {value: true, msg: ""}
            };

            // Original visibility, or visibility in session
            // TODO: origin visibility
            scope.original = {
                points: scope.points,
                velp: scope.velp,
                color: scope.color,
                visible_to: scope.visibleto,
                comment: "",//scope.newcomment,
                aid: scope.aid
            };

            /**
             * Toggles the visibility of the annotation.
             * @method toggleAnnota tion
             */
            scope.toggleAnnotation = function () {
                if (scope.velpElement === null) {
                    scope.velpElement = element[0].getElementsByClassName("annotation-info")[0];
                }
                var elementName = scope.velpElement.parentNode.offsetParent.className;
                var annotationElements = document.querySelectorAll('[aid="{0}"]'.replace('{0}', scope.aid));

                scope.toggleAnnotationShow();


                if ( elementName === "notes" &&  annotationElements.length > 1 ){
                    for (var i=0; i<annotationElements.length; i++){
                        if (annotationElements[i].parentNode.offsetParent.className !== "notes"){
                            angular.element(
                                annotationElements[i]
                            ).isolateScope().toggleAnnotationShow();
                            scope.toggleAnnotationShow();
                        }
                    }
                }

            };

            scope.toggleAnnotationShow = function () {
                scope.show = !scope.show;
                scope.updateVelpZIndex();
            };

            scope.clearColor = function () {
                scope.color = "";
                scope.changeColor()
            };

            scope.isVelpCustomColor = function () {
                return scope.color.length === 7; // hex colors are 7 characters long
            }


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
                if (typeof scope.points !== UNDEFINED){
                    scope.isvalid.points.value = true;
                    scope.$parent.changeAnnotationPoints(scope.aid, scope.points);
                } else {
                    scope.isvalid.points.value = false;
                    scope.isvalid.points.msg = "Insert a number or leave empty"
                }
            };
            
            scope.changeColor = function () {
                scope.$parent.changeAnnotationColor(scope.aid, scope.color);
            };

            /**
             * Saves the changes made to the annotation. Queries parent scope
             * and updates the changes of the corresponding annotation.
             * @method saveChanges
             */
            scope.saveChanges = function () {
                var id = scope.$parent.getRealAnnotationId(scope.aid);

                // Add comment
                if (scope.newcomment.length > 0) {
                    var comment = scope.newcomment;

                    var data = {annotation_id: id, content: scope.newcomment};
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
                scope.newcomment = "";
                if (scope.visible_options.value !== scope.original.visible_to) {
                    scope.$parent.changeVisibility(scope.aid, scope.visible_options.value);
                }
                scope.original = {
                    points: scope.points,
                    annotation_id: id,
                    visible_to: scope.visible_options.value,
                    velp: scope.velp,
                    color: scope.color,
                    comment: scope.newcomment,
                    doc_id: scope.$parent.docId
                };

                scope.$parent.makePostRequest("/update_annotation", scope.original, function (json) {
                    //console.log(json);
                });
            };

            scope.getCustomColor = function () {
                if (typeof scope.color !== UNDEFINED || scope.color !== null)
                    return scope.color;
            };


            /**
             * Checks if the user has rights to edit the annotation.
             * @method checkRights
             * @returns {boolean} Whether the user has rights or not
             */
            scope.checkRights = function () {
                return scope.editaccess !== true;
            };

            /**
             * Detect user right to annotation to document.
             * @param points - Points given in velp or annotation
             * @returns {boolean} - Whether user has rights to make annotations


            scope.notAnnotationRights = function (points) {
                if (scope.$parent.item.rights.teacher) {
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
             * Return true if user has teacher rights.
             * @returns {boolean} Whether the user has teacher rights or not
             */
            scope.allowChangePoints = function () {
                return scope.$parent.item.rights.teacher;
            };



            /**
             * Checks if the annotation is changed compared to its last saved state.
             * @method checkIfChanged
             * @returns {boolean} - Whether any modifications were made or not
             */
            scope.checkIfChanged = function () {
                if (!scope.showHidden)
                    return false;
                if (scope.original.points !== scope.points || scope.original.comment !== scope.newcomment ||
                    scope.original.visible_to !== scope.visible_options.value || scope.original.velp !== scope.velp ||
                    scope.original.color !== scope.color)
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

            /**
             * Watches changes on newannotation attribute. Shoulc scroll window
             * only if annotation is not inside browser window.
             * TODO: Check scroll positions according to textarea element
             */
           scope.$watch('newannotation', function(newValue) {
               if (newValue === "true" && scope.show) { // this check is necessary

                   var x = window.scrollX, y = window.scrollY;

                   var pos = element[0].getBoundingClientRect().top;
                   element.find('textarea').focus();

                   if (0 < pos && pos < window.innerHeight){
                        window.scrollTo(x, y);
                   }

                   scope.newannotation = false;
               }

            });

            setTimeout(function(){
                if (scope.show) scope.updateVelpZIndex();
            }, 0);
        }
    };
}]);

