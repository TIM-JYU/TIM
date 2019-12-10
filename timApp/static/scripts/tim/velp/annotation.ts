/**
 * The directive handles the logic behind a single annotation.
 *
 * @module annotation
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
 */

import {IController, IScope} from "angular";
import {timApp} from "tim/app";
import * as focusme from "tim/ui/focusMe";
import {ViewCtrl} from "../document/viewctrl";
import {DestroyScope} from "../ui/destroyScope";
import {showMessageDialog} from "../ui/dialog";
import {IUser} from "../user/IUser";
import {KEY_CTRL, KEY_ENTER, KEY_S} from "../util/keycodes";
import {$http} from "../util/ngimport";
import {Binding, isInViewport, markAsUsed, Require, scrollToElement, to} from "../util/utils";
import {IAnnotationCoordless} from "./velptypes";

markAsUsed(focusme);

export class AnnotationController extends DestroyScope implements IController {
    static $inject = ["$scope", "$element"];
    private ctrlDown: boolean;
    private visibleOptions!: {
        type: string; value: number; title: string;
        values: [number, number, number, number];
        names: [string, string, string, string]
    }; // $onInit
    private newcomment: string;
    private isvalid: {points: {value: boolean; msg: string}};
    private velpElement!: HTMLElement; // $postLink
    private showFull = false;
    private focus = false;
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
    private vctrl!: Require<ViewCtrl>;
    private annotationdata!: Binding<string, "@">;
    public annotation!: IAnnotationCoordless; // $onInit
    private prefix = "x";

    constructor(private scope: IScope, public element: JQLite) {
        super(scope, element);
        this.ctrlDown = false;
        this.newcomment = "";
        this.isvalid = {
            points: {value: true, msg: ""},
        };
    }

    setShowFull(v: boolean) {
        this.showFull = v;
        this.focus = v;
    }

    $onDestroy() {
        this.vctrl.unRegisterAnnotation(this);
    }

    getKeyPrefix() {
        return this.prefix;
    }

    $onInit() {
        this.show = this.showStr === "true";
        this.annotation = JSON.parse(this.annotationdata) as IAnnotationCoordless;
        this.visibleOptions = {
            type: "select",
            value: this.annotation.visible_to,
            title: "Visible to",
            values: [1, 2, 3, 4],
            names: ["Just me", "Document owner", "Teachers", "Everyone"],
        };

        if (this.annotation.default_comment != null) {
            this.newcomment = this.annotation.default_comment;
        }

        this.setShowFull(this.annotation.newannotation);

        // Original visibility, or visibility in session
        // TODO: origin visibility
        this.original = {
            annotation_id: null,
            doc_id: null,
            points: this.annotation.points,
            velp: this.velp,
            color: this.annotation.color,
            visible_to: this.annotation.visible_to,
            comment: "", // this.newcomment,
            aid: this.annotation.id,
        };
        const e = this.element;
        const isInMargin = e.parent().hasClass("notes") && e.parent().parent().hasClass("par");
        this.prefix = isInMargin ? "m" : "t"; // as in "margin" or "text"
        this.vctrl.registerAnnotation(this);
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

                const x = window.scrollX;
                const y = window.scrollY;

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

    get rctrl() {
        return this.vctrl.reviewCtrl;
    }

    /**
     * Toggles the visibility of the annotation.
     */
    toggleAnnotation() {
        if (this.velpElement.parentElement == null) {
            return;
        }
        const a = this.rctrl.getAnnotationById(this.annotation.id);
        if (!a) {
            showMessageDialog(`Could not find annotation with id ${this.annotation.id}`);
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
     */
    updateVelpZIndex() {
        this.velpElement.style.zIndex = this.rctrl.zIndex.toString();
        this.rctrl.zIndex++;
    }

    /**
     * Shows the annotation.
     */
    showAnnotation() {
        this.setShowFull(false);
        this.annotation.newannotation = false;
        this.show = true;

        this.updateVelpZIndex();
    }

    scrollToIfNotInViewport() {
        const e = this.element[0];
        if (!isInViewport(e)) {
            scrollToElement(e);
        }
    }

    /**
     * Deletes the selected annotation. Queries parent this and deletes
     * the corresponding annotation from there.
     */
    deleteAnnotation() {

        if (this.annotation.comments.length < 2) {
            if (!window.confirm("Delete - are you sure?")) {
                return;
            }
            this.rctrl.deleteAnnotation(this.annotation.id);
        }
    }

    /**
     * Updates the annotation and toggles its visibility in the margin.
     */
    updateAnnotation() {
        let margin = false;
        if (this.velpElement.parentElement == null || this.velpElement.parentElement.offsetParent == null) {
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
     */
    changePoints() {
        if (this.annotation.points != null) {
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
     */
    async saveChanges() {
        const id = this.rctrl.getRealAnnotationId(this.annotation.id);

        // Add comment
        if (this.newcomment.length > 0) {
            const comment = this.newcomment;

            const data = {annotation_id: id, content: this.newcomment};
            const r = await to($http.post<IUser>("/add_annotation_comment", data));
            if (!r.ok) {
                return;
            }
            const response = r.result;
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
        if (this.visibleOptions.value !== this.original.visible_to) {
            this.rctrl.changeVisibility(this.annotation.id, this.visibleOptions.value);
        }
        this.original = {
            aid: this.annotation.id,
            points: this.annotation.points,
            annotation_id: id,
            visible_to: this.visibleOptions.value,
            velp: this.velp,
            color: this.annotation.color,
            comment: this.newcomment,
            doc_id: this.vctrl.docId,
        };

        $http.post("/update_annotation", this.original);
    }

    getCustomColor() {
        return this.annotation.color;
    }

    /**
     * Checks if the user has rights to edit the annotation.
     * @returns {boolean} Whether the user has rights or not
     */
    checkRights() {
        return !this.annotation.edit_access;
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
     * @returns {boolean} - Whether any modifications were made or not
     */
    checkIfChanged() {
        if (!this.showFull) {
            return false;
        }
        if (this.original.points !== this.annotation.points || this.original.comment !== this.newcomment ||
            this.original.visible_to !== this.visibleOptions.value || this.original.velp !== this.velp ||
            this.original.color !== this.annotation.color) {
            return true;
        }
        return false;
    }

    /**
     * Detects the `Ctrl + S` and `Ctrl+Enter` key strokes on the text area.
     * @param event - Current event
     */
    keyDownFunc(event: KeyboardEvent) {
        if (event.keyCode === KEY_CTRL) {
            this.ctrlDown = true;
        }
        if (this.ctrlDown && (event.which === KEY_S || event.keyCode === KEY_ENTER)) {
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
     * @param event - Current event
     */
    keyUpFunc(event: KeyboardEvent) {
        if (event.keyCode === KEY_CTRL) {
            this.ctrlDown = false;
        }
    }
}

/**
 * Directive for a single annotation.
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
        vctrl: "^timView",
    },
    templateUrl: "/static/templates/annotation.html",
    transclude: true,
});
