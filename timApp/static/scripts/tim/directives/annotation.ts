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

import {IController, IRootElementService, IScope} from "angular";
import {timApp} from "tim/app";
import {ReviewController} from "../controllers/reviewController";
import {showMessageDialog} from "../dialog";
import {IUser} from "../IUser";
import {$http, $window} from "../ngimport";
import {Binding, Require} from "../utils";
import {IAnnotationCoordless} from "./velptypes";

const UNDEFINED = "undefined";

export class AnnotationController implements IController {
    private static $inject = ["$scope", "$element"];
    private ctrlKey: number;
    private ctrlDown: boolean;
    private visible_options!: {
        type: string; value: number; title: string;
        values: [number, number, number, number];
        names: [string, string, string, string]
    }; // $onInit
    private newcomment: string;
    private isvalid: {points: {value: boolean; msg: string}};
    private element: IRootElementService;
    private velpElement!: HTMLElement; // $postLink
    private showHidden: boolean;
    public show: boolean = false;
    private showStr!: Binding<string, "@">;
    private original!: {
        points: number | null;
        velp: {};
        color: string | null;
        visible_to: number;
        comment: string;
        aid: number,
        annotation_id: number | null,
        doc_id: number | null
    }; // $onInit
    private velp!: Binding<string, "@">;
    private rctrl!: Require<ReviewController>;
    private scope: IScope;
    private annotationdata!: Binding<string, "@">;
    private annotation!: IAnnotationCoordless; // $onInit

    constructor(scope: IScope, element: IRootElementService) {
        this.scope = scope;
        this.element = element;
        this.ctrlDown = false;
        this.ctrlKey = 17;
        this.newcomment = "";
        this.showHidden = false;
        this.isvalid = {
            points: {value: true, msg: ""},
        };
    }

    $onInit() {
        this.show = this.showStr === "true";
        this.annotation = JSON.parse(this.annotationdata);
        this.visible_options = {
            type: "select",
            value: this.annotation.visible_to,
            title: "Visible to",
            values: [1, 2, 3, 4],
            names: ["Just me", "Document owner", "Teachers", "Everyone"],
        };
        this.annotation.newannotation = false;

        if (this.annotation.default_comment != null) {
            this.newcomment = this.annotation.default_comment;
        }

        // Original visibility, or visibility in session
        // TODO: origin visibility
        this.original = {
            annotation_id: null,
            doc_id: null,
            points: this.annotation.points,
            velp: this.velp,
            color: this.annotation.color,
            visible_to: this.annotation.visible_to,
            comment: "", //this.newcomment,
            aid: this.annotation.id,
        };
    }

    $postLink() {
        this.velpElement = this.element.find(".fulldiv")[0].parentElement as HTMLElement;
        if (!this.velpElement) {
            throw new Error("Could not find velpElement from template");
        }
        /**
         * Watches changes on newannotation attribute. Should scroll window
         * only if annotation is not inside browser window.
         * TODO: Check scroll positions according to textarea element
         */
        this.scope.$watch(() => this.annotation.newannotation, (newValue) => {
            if (newValue && this.show) { // this check is necessary

                const x = window.scrollX, y = window.scrollY;

                const pos = this.element[0].getBoundingClientRect().top;
                this.element.find("textarea").focus();

                if (0 < pos && pos < window.innerHeight) {
                    window.scrollTo(x, y);
                }

                this.annotation.newannotation = false;
            }

        });

        setTimeout(() => {
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
        if (this.velpElement.parentElement == null) {
            return;
        }
        const a = this.rctrl.getAnnotationById(this.annotation.id);
        if (!a) {
            showMessageDialog(`Could not annotation with id ${this.annotation.id}`);
            return;
        }
        this.rctrl.toggleAnnotation(a, false);
    }

    toggleAnnotationShow() {
        this.show = !this.show;
        this.updateVelpZIndex();
    }

    clearColor() {
        this.annotation.color = "";
        this.changeColor();
    }

    isVelpCustomColor(): boolean {
        if (this.annotation.color == null) {
            return false;
        }
        return this.annotation.color.length === 7; // hex colors are 7 characters long
    }

    /**
     * Updates the z-index attribute of the annotation.
     * @method updateVelpZIndex
     */
    updateVelpZIndex() {
        this.velpElement.style.zIndex = this.rctrl.zIndex.toString();
        this.rctrl.zIndex++;
    }

    /**
     * Shows the annotation.
     * @method showAnnotation
     */
    showAnnotation() {
        this.showHidden = false;
        this.annotation.newannotation = false;
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

        if (this.annotation.comments.length < 2) {
            if (!$window.confirm("Delete - are you sure?")) {
                return;
            }
            this.rctrl.deleteAnnotation(this.annotation.id);
        }
    }

    /**
     * Updates the annotation and toggles its visibility in the margin.
     * @method updateAnnotation
     */
    updateAnnotation() {
        let margin = false;
        if (this.velpElement.parentElement == null) {
            return;
        }
        if (this.velpElement.parentElement.offsetParent.className === "notes") {
            margin = true;
        }
        this.rctrl.updateAnnotation(this.annotation.id, margin);
    }

    /**
     * Changes points of the selected annotation. Qeries parent this
     * and changes the points of the corresponding annotation.
     * @method changePoints
     */
    changePoints() {
        if (typeof this.annotation.points !== UNDEFINED) {
            this.isvalid.points.value = true;
            this.rctrl.changeAnnotationPoints(this.annotation.id, this.annotation.points);
        } else {
            this.isvalid.points.value = false;
            this.isvalid.points.msg = "Insert a number or leave empty";
        }
    }

    changeColor() {
        this.rctrl.changeAnnotationColor(this.annotation.id, this.annotation.color);
    }

    /**
     * Saves the changes made to the annotation. Queries parent this
     * and updates the changes of the corresponding annotation.
     * @method saveChanges
     */
    async saveChanges() {
        const id = this.rctrl.getRealAnnotationId(this.annotation.id);

        // Add comment
        if (this.newcomment.length > 0) {
            const comment = this.newcomment;

            const data = {annotation_id: id, content: this.newcomment};
            const response = await $http.post<IUser>("/add_annotation_comment", data);
            this.annotation.comments.push({
                commenter_username: response.data.name,
                content: comment,
                comment_time: "now",
                comment_relative_time: "just now",
            });
            this.rctrl.addComment(this.annotation.id, response.data.name, comment);
            this.updateAnnotation();
        } else {
            this.updateAnnotation();
        }
        this.newcomment = "";
        if (this.visible_options.value !== this.original.visible_to) {
            this.rctrl.changeVisibility(this.annotation.id, this.visible_options.value);
        }
        this.original = {
            aid: this.annotation.id,
            points: this.annotation.points,
            annotation_id: id,
            visible_to: this.visible_options.value,
            velp: this.velp,
            color: this.annotation.color,
            comment: this.newcomment,
            doc_id: this.rctrl.docId,
        };

        $http.post("/update_annotation", this.original);
    }

    getCustomColor() {
        if (typeof this.annotation.color !== UNDEFINED || this.annotation.color != null) {
            return this.annotation.color;
        }
    }

    /**
     * Checks if the user has rights to edit the annotation.
     * @method checkRights
     * @returns {boolean} Whether the user has rights or not
     */
    checkRights() {
        return this.annotation.edit_access !== true;
    }

    /**
     * Return true if user has teacher rights.
     * @returns {boolean} Whether the user has teacher rights or not
     */
    allowChangePoints() {
        return this.rctrl.item.rights.teacher;
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
        if (this.original.points !== this.annotation.points || this.original.comment !== this.newcomment ||
            this.original.visible_to !== this.visible_options.value || this.original.velp !== this.velp ||
            this.original.color !== this.annotation.color) {
            return true;
        }
        return false;
    }

    /**
     * Detects the `Ctrl + S` and `Ctrl+Enter` key strokes on the text area.
     * @method keyDownFunc
     * @param event - Current event
     */
    keyDownFunc(event: KeyboardEvent) {
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
    keyUpFunc(event: KeyboardEvent) {
        if (event.keyCode === this.ctrlKey) {
            this.ctrlDown = false;
        }
    }
}

/** Directive for a single annotation.
 * @lends module:reviewController
 */
timApp.component("annotation", {
    bindings: {
        annotationdata: "@",
        showStr: "@",
        velp: "@",
    },
    controller: AnnotationController,
    controllerAs: "actrl",
    require: {
        rctrl: "^timReview",
    },
    templateUrl: "/static/templates/annotation.html",
    transclude: true,
});
