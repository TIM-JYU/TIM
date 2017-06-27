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

import angular from "angular";
import {timApp} from "tim/app";
import {$window} from "../ngimport";

const UNDEFINED = "undefined";

class AnnotationController {
    private static $inject = ["$scope", "$element"];

    private $parent: any; // TODO require ReviewController
    private ctrlKey: number;
    private ctrlDown: boolean;
    private visible_options: {
        type: string; value: number; title: string;
        values: [number, number, number, number];
        names: [string, string, string, string]
    };
    private visibleto: number;
    private newcomment: string;
    private newannotation: boolean;
    private marginonly: boolean;
    private isvalid: {points: {value: boolean; msg: string}};
    private element: angular.IRootElementService;
    private velpElement: HTMLElement;
    private showHidden: boolean;
    private color: string;
    private show: boolean;
    private editaccess: boolean;
    private comments: {}[];
    private original: {
        points: number;
        velp: {};
        color: string;
        visible_to: number;
        comment: string;
        aid: string,
        annotation_id: number,
        doc_id: number
    };
    private points: number;
    private velp: {};
    private aid: string;
    private ismargin: boolean;

    constructor(scope: angular.IScope, element: angular.IRootElementService) {
        this.element = element;
        this.velpElement = element[0].getElementsByClassName("annotation-info")[0] as HTMLElement;
        this.ctrlDown = false;
        this.ctrlKey = 17;
        this.visible_options = {
            type: "select",
            value: this.visibleto,
            title: "Visible to",
            values: [1, 2, 3, 4],
            names: ["Just me", "Document owner", "Teachers", "Everyone"],
        };
        this.newannotation = false;
        this.marginonly = false;

        if (this.newcomment === null) {
            this.newcomment = "";
        }

        this.isvalid = {
            points: {value: true, msg: ""},
        };

        // Original visibility, or visibility in session
        // TODO: origin visibility
        this.original = {
            annotation_id: null,
            doc_id: null,
            points: this.points,
            velp: this.velp,
            color: this.color,
            visible_to: this.visibleto,
            comment: "", //this.newcomment,
            aid: this.aid,
        };

        /**
         * Watches changes on newannotation attribute. Should scroll window
         * only if annotation is not inside browser window.
         * TODO: Check scroll positions according to textarea element
         */
        scope.$watch(() => this.newannotation, function(newValue) {
            if (newValue && this.show) { // this check is necessary

                const x = window.scrollX, y = window.scrollY;

                const pos = element[0].getBoundingClientRect().top;
                element.find("textarea").focus();

                if (0 < pos && pos < window.innerHeight) {
                    window.scrollTo(x, y);
                }

                this.newannotation = false;
            }

        });

        setTimeout(function() {
            if (this.show) {
                this.updateVelpZIndex();
            }
        }, 0);
    }

    /**
     * Toggles the visibility of the annotation.
     * @method toggleAnnotation
     */
    toggleAnnotation() {
        const elementName = this.velpElement.parentElement.offsetParent.className;
        const annotationElements = document.querySelectorAll('[aid="{0}"]'.replace("{0}", this.aid));

        this.toggleAnnotationShow();

        if (elementName === "notes" && annotationElements.length > 1) {
            for (let i = 0; i < annotationElements.length; i++) {
                const e = annotationElements[i].parentNode as HTMLElement;
                if (e.offsetParent.className !== "notes") {
                    angular.element(
                        annotationElements[i],
                    ).isolatethis().toggleAnnotationShow();
                    this.toggleAnnotationShow();
                }
            }
        }

    }

    toggleAnnotationShow() {
        this.show = !this.show;
        this.updateVelpZIndex();
    }

    clearColor() {
        this.color = "";
        this.changeColor();
    }

    isVelpCustomColor() {
        return this.color.length === 7; // hex colors are 7 characters long
    }

    /**
     * Updates the z-index attribute of the annotation.
     * @method updateVelpZIndex
     */
    updateVelpZIndex() {
        this.velpElement.style.zIndex = this.$parent.zIndex.toString();
        this.$parent.zIndex++;
    }

    /**
     * Shows the annotation.
     * @method showAnnotation
     */
    showAnnotation() {
        this.showHidden = false;
        this.newannotation = false;
        this.show = true;

        this.updateVelpZIndex();
    }

    /**
     * Focuses on the comment field of the annotation.
     * @method focusTextarea
     */
    focusTextarea() {
        return true;
    }

    /**
     * Deletes the selected annotation. Queries parent this and deletes
     * the corresponding annotation from there.
     * @method deleteAnnotation
     */
    deleteAnnotation() {

        if (this.comments.length < 2) {
            if (!$window.confirm("Delete - are you sure?")) {
                return;
            }
            this.$parent.deleteAnnotation(this.aid, this.ismargin);
        }
    }

    /**
     * Updates the annotation and toggles its visibility in the margin.
     * @method updateAnnotation
     */
    updateAnnotation() {
        let margin = false;
        if (this.velpElement.parentElement.offsetParent.className === "notes") {
            margin = true;
        }
        this.$parent.updateAnnotation(this.aid, margin);
    }

    /**
     * Changes points of the selected annotation. Qeries parent this
     * and changes the points of the corresponding annotation.
     * @method changePoints
     */
    changePoints() {
        if (typeof this.points !== UNDEFINED) {
            this.isvalid.points.value = true;
            this.$parent.changeAnnotationPoints(this.aid, this.points);
        } else {
            this.isvalid.points.value = false;
            this.isvalid.points.msg = "Insert a number or leave empty";
        }
    }

    changeColor() {
        this.$parent.changeAnnotationColor(this.aid, this.color);
    }

    /**
     * Saves the changes made to the annotation. Queries parent this
     * and updates the changes of the corresponding annotation.
     * @method saveChanges
     */
    saveChanges() {
        const id = this.$parent.getRealAnnotationId(this.aid);

        // Add comment
        if (this.newcomment.length > 0) {
            const comment = this.newcomment;

            const data = {annotation_id: id, content: this.newcomment};
            this.$parent.makePostRequest("/add_annotation_comment", data, function(json) {
                this.comments.push({
                    commenter_username: json.data.name,
                    content: comment,
                    comment_time: "now",
                    comment_relative_time: "just now",
                });
                this.$parent.addComment(this.aid, json.data.name, comment);
                this.updateAnnotation();
            });

        } else {
            this.updateAnnotation();
        }
        this.newcomment = "";
        if (this.visible_options.value !== this.original.visible_to) {
            this.$parent.changeVisibility(this.aid, this.visible_options.value);
        }
        this.original = {
            aid: this.aid,
            points: this.points,
            annotation_id: id,
            visible_to: this.visible_options.value,
            velp: this.velp,
            color: this.color,
            comment: this.newcomment,
            doc_id: this.$parent.vctrl.docId,
        };

        this.$parent.makePostRequest("/update_annotation", this.original, function(json) {
            //$log.info(json);
        });
    }

    getCustomColor() {
        if (typeof this.color !== UNDEFINED || this.color !== null) {
            return this.color;
        }
    }

    /**
     * Checks if the user has rights to edit the annotation.
     * @method checkRights
     * @returns {boolean} Whether the user has rights or not
     */
    checkRights() {
        return this.editaccess !== true;
    }

    /**
     * Detect user right to annotation to document.
     * @param points - Points given in velp or annotation
     * @returns {boolean} - Whether user has rights to make annotations

     this.notAnnotationRights = function (points) {
            if (this.$parent.item.rights.teacher) {
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
    allowChangePoints() {
        return this.$parent.vctrl.item.rights.teacher;
    }

    /**
     * Checks if the annotation is changed compared to its last saved state.
     * @method checkIfChanged
     * @returns {boolean} - Whether any modifications were made or not
     */
    checkIfChanged() {
        if (!this.showHidden) {
            return false;
        }
        if (this.original.points !== this.points || this.original.comment !== this.newcomment ||
            this.original.visible_to !== this.visible_options.value || this.original.velp !== this.velp ||
            this.original.color !== this.color) {
            return true;
        }
        return false;
    }

    /**
     * Detects the `Ctrl + S` and `Ctrl+Enter` key strokes on the text area.
     * @method keyDownFunc
     * @param event - Current event
     */
    keyDownFunc(event) {
        if (event.keyCode === this.ctrlKey) {
            this.ctrlDown = true;
        }
        if (this.ctrlDown && (String.fromCharCode(event.which).toLowerCase() === "s" || event.keyCode === 13)) {
            event.preventDefault();
            this.ctrlDown = false;
            if (this.checkIfChanged()) {
                this.saveChanges();
            } else {
                this.toggleAnnotation();
            }
        }
    }

    /**
     * Detects if `Ctrl`-key is released.
     * @method keyUpFunc
     * @param event - Current event
     */
    keyUpFunc(event) {
        if (event.keyCode === this.ctrlKey) {
            this.ctrlDown = false;
        }
    }
}

/** Directive for a single annotation.
 * @lends module:reviewController
 */
timApp.component("annotation", {
    templateUrl: "/static/templates/annotation.html",
    transclude: true,
    bindings: {
        show: "=",
        points: "=",
        visibleto: "=",
        comments: "=",
        aid: "=",
        ismargin: "=",
        annotator: "@",
        editaccess: "=",
        newcomment: "@",
        //email: '@',
        timesince: "@",
        creationtime: "@",
        velp: "@",
        color: "@",
        newannotation: "<", // TODO: should this be "@" ?
        showHidden: "@",
    },

    controller: AnnotationController,
    controllerAs: "actrl",
});
