/**
 * The directive handles the logic behind a single annotation.
 *
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
 */

import {Users} from "tim/user/userService";
import deepEqual from "deep-equal";
import {AfterViewInit, Component, ElementRef, Input, OnDestroy, OnInit, ViewChild} from "@angular/core";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {deserialize} from "typescript-json-serializer";
import {DrawObject} from "tim/plugin/drawCanvas";
import {ViewCtrl} from "../document/viewctrl";
import {showMessageDialog} from "../ui/dialog";
import {KEY_CTRL, KEY_ENTER, KEY_S} from "../util/keycodes";
import {$http} from "../util/ngimport";
import {angularWait, clone, isInViewport, Result, scrollToElement, to} from "../util/utils";
import {Annotation, IAnnotationEditableValues, IAnnotationInterval} from "./velptypes";

/**
 * Lists the possible reasons why an annotation is added to the document.
 */
export enum AnnotationAddReason {
    // Annotation is added because the user selected some text and clicked a velp to add a new annotation.
    AddingNew,
    // An existing annotation is being loaded.
    LoadingExisting,
}

export enum AnnotationPlacement {
    InText,
    InMargin,
    InMarginOnly,
    InMarginAndUnknownIfItWillBeInText,
    InPicture,
}

export interface IAnnotationBindings {
    reason: AnnotationAddReason;
    placement: AnnotationPlacement;
    annotation: Annotation;
    defaultcomment: string;
}

export async function updateAnnotationServer(updatevalues: IAnnotationEditableValues & { id: number, coord?: IAnnotationInterval, draw_data?: DrawObject[] }): Promise<Result<Annotation, string>> {
    const r2 = await to($http.post<Record<string, unknown>>("/update_annotation", updatevalues));
    if (!r2.ok) {
        return {ok: false, result: r2.result.data.error};
    }
    return {ok: true, result: deserialize(r2.result.data, Annotation)};
}

@Component({
    selector: "annotation",
    template: `
        <span #contentSpan (click)="setShowFull(false);toggleAnnotation()"
              class="no-popup-menu"
              [ngClass]="['highlighted', 'clickable', 'emphasise', 'default', getClass()]"
              [ngStyle]="{backgroundColor: getCustomColor()}">
            <ng-content></ng-content>
        </span>
        <span #inlineSpan [ngClass]="placement != 4 ? 'inlineAnnotation' : 'inlineImageAnnotation'">
            <div #inlineDiv *ngIf="show"
                 (click)="updateZIndex()"
                 class="no-popup-menu annotation-info emphasise default"
                 [ngClass]="getClass()"
                 (keydown)="keyDownFunc($event)"
                 (keyup)="keyUpFunc($event)"
                 [style.backgroundColor]="getCustomColor()"
                [style.z-index]="zIndex">

    <span class="fulldiv">
        <span class="div-90 annTopSection" (mouseenter)="setShowFull(true)">
            <p><span class="annHeader"><strong>{{ annotation.getContent() }}</strong></span></p>
            <p *ngIf="showFull">
                <tim-signature [user]="annotation.annotator" [time]="annotation.creation_time"></tim-signature>
            </p>
        </span>
            <tim-close-button class="clickable-icon"
                  (click)="toggleAnnotationShow();setShowFull(false)"></tim-close-button>
    </span>

                <div>
                    <div *ngIf="showFull">
                        <p [hidden]="!isAnnotator() || !allowChangePoints()"><label>
                            Points: <input type="number"
                                           [(ngModel)]="values.points"
                                           step="any"
                                           [disabled]="!isAnnotator()"/></label>
                        </p>
                        <p *ngIf="!isAnnotator()"><label
                                [hidden]="values.points == null">Points:  {{ values.points }}</label></p>
                    </div>
                    <div [hidden]="showFull">
                        <div *ngIf="values.points != null">
                            <p><label>Points: {{ values.points }}</label></p>
                        </div>
                    </div>

                    <div class="fulldiv">
                        <ul class="comments">
                            <li *ngFor="let comment of annotation.comments">
                                <p>{{ comment.content }}
                                    &mdash;
                                    <tim-signature [time]="comment.comment_time"
                                                   [user]="comment.commenter"></tim-signature>
                                </p>
                            </li>
                        </ul>

                    </div>
                    <div *ngIf="showFull">
        <textarea focusMe class="form-control" placeholder="Add comment"
                  [(ngModel)]="newcomment"></textarea>
                        <div>
                            <div class="form-inline adjustAnnForm">
                                <div>
                    <span [hidden]="!isAnnotator()">
                        <label [ngClass]="{disabled: !isAnnotator()}"><div title="Visible to"
                                                                           [ngClass]="['glyphicon', 'glyphicon-eye-open' ]"></div></label>
                        <select [(ngModel)]="values.visible_to" [disabled]="!isAnnotator()">
                            <option *ngFor="let o of visibleOptions.values" [ngValue]="o.id">{{o.name}}</option>
                        </select>
                        <div [ngClass]="['glyphicon', 'glyphicon-question-sign', 'clickable-icon']"
                             (click)="openHelp()">
                        </div>
                    </span>

                                    <span *ngIf="!isAnnotator()" class="annotationVisibleText">

                        <span title="Visible to" [ngClass]="['annqmark', 'glyphicon', 'glyphicon-eye-open' ]"></span>
                                        {{ getSelectedVisibleOption().name }}

                                        <span [ngClass]="['annqmark', 'glyphicon', 'glyphicon-question-sign', 'clickable-icon']"
                                              (click)="openHelp()">
                        </span>

                    </span>
                                    <span style="float: right">
                        <input type="color" [(ngModel)]="values.color"
                               class="colorchange-button" title="Change annotation color">
                        <button *ngIf="isVelpCustomColor()"
                                class="smallButton"
                                (click)="clearColor()"
                                title="Reset color to original value">R</button>
                    </span>
                                </div>
                            </div>
                        </div>
                        <span>
        <button class="saveChanges timButton" *ngIf="hasChanged()" (click)="saveChanges()">
            Save
        </button>
    </span>
                    </div>
                    <div *ngIf="showFull">
        <span class="pull-right glyphicon glyphicon-trash clickable-icon" title="Delete annotation"
              [hidden]="!isAnnotator()"
              (click)="deleteAnnotation()"></span>
                    </div>
                    <div class="annotationVisibleText" [hidden]="showFull">
                        {{ getSelectedVisibleOption().name }}
                    </div>
                </div>

            </div>
        </span>
    `,
    styleUrls: ["./annotation.component.scss"],
})
export class AnnotationComponent implements OnDestroy, OnInit, AfterViewInit, IAnnotationBindings {
    private ctrlDown = false;
    visibleOptions = {
        values: [
            {id: 1, name: "Just me"},
            {id: 2, name: "Document owner"},
            {id: 3, name: "Teachers"},
            {id: 4, name: "Everyone"},
        ],
    };
    newcomment = "";
    showFull = false;
    public show = false;
    @Input()
    public defaultcomment!: string;
    @Input()
    public reason!: AnnotationAddReason;
    @Input()
    public annotation!: Annotation;
    @Input() placement!: AnnotationPlacement;
    private original!: Readonly<IAnnotationEditableValues>;
    values!: IAnnotationEditableValues;
    private vctrl!: ViewCtrl;
    private prefix = "x";
    private readonly element: JQuery<HTMLElement>;
    zIndex = 0;
    @ViewChild("inlineSpan") inlineSpan!: ElementRef<HTMLSpanElement>;
    @ViewChild("contentSpan") contentSpan!: ElementRef<HTMLSpanElement>;

    @ViewChild("inlineDiv") set inlineDiv(div: ElementRef<HTMLDivElement>)  {
        if (div && this.placement == AnnotationPlacement.InPicture) {
            this.adjustInPicturePosition(div.nativeElement);
        }
    }

    constructor(public e: ElementRef) {
        this.element = $(e.nativeElement);
    }

    setShowFull(v: boolean) {
        this.showFull = v;
        if (v) {
            this.show = true;
            this.updateZIndex();
            if (this.inlineDiv) {
                this.adjustInPicturePosition(this.inlineDiv.nativeElement);
            }
        }
        this.toggleElementBorder();
    }

    ngOnDestroy() {
        this.vctrl.unRegisterAnnotation(this);
    }

    getClass() {
        if (this.values.points == null) {
            return "";
        } else if (this.values.points == 0) {
            return "neutral";
        } else if (this.values.points > 0) {
            return "positive";
        } else {
            return "negative";
        }
    }

    getKeyPrefix() {
        return this.prefix;
    }

    getSelectedVisibleOption() {
        return this.visibleOptions.values.find((o) => o.id === this.values.visible_to)!;
    }

    ngOnInit() {
        this.vctrl = vctrlInstance!;
        this.setAnnotation(this.annotation);
        this.setShowFull(this.reason == AnnotationAddReason.AddingNew);
        this.newcomment = this.defaultcomment;
        this.prefix = this.isInMargin() ? "m" : "t"; // as in "margin" or "text"
        this.vctrl.registerAnnotation(this);
        if (this.placement == AnnotationPlacement.InMarginOnly) {
            this.vctrl.rejectTextAnnotation(this);
        }
    }

    setAnnotation(a: Annotation) {
        this.annotation = a;
        this.original = a.getEditableValues();
        this.values = clone(this.original);
    }

    private isInMargin() {
        return (this.placement !== AnnotationPlacement.InText && this.placement !== AnnotationPlacement.InPicture);
    }

    ngAfterViewInit() {
        if (this.reason == AnnotationAddReason.AddingNew && this.show) {
            this.scrollToIfNotInViewport();
        }
    }

    getInPictureLeft() {
        if (!(this.placement == AnnotationPlacement.InPicture && this.inlineDiv)) {
            console.log(this.inlineDiv, "not found");
            return 0 + "px";
        }
        if (this.element.parent()[0].clientWidth - (this.element[0].offsetLeft + this.inlineDiv.nativeElement.clientWidth) < 0) {
            if (this.element[0].offsetLeft - this.inlineDiv.nativeElement.clientWidth + this.element[0].clientWidth >= 0) {
                return -this.inlineDiv.nativeElement.clientWidth + this.element[0].clientWidth + "px";
            }
        }
    }

    getInPictureTop() {
        if (!(this.placement == AnnotationPlacement.InPicture && this.inlineDiv)) {
            console.log(this.inlineDiv, "not found");
            return;
        }
        if (this.element.parent()[0].clientHeight - (this.element[0].offsetTop + this.inlineDiv.nativeElement.clientHeight) < 0) {
            if (this.element[0].offsetTop - this.inlineDiv.nativeElement.clientHeight + this.element[0].clientHeight >= 0) {
                return -this.inlineDiv.nativeElement.clientHeight + this.element[0].clientWidth;
            }
        }
    }

    adjustInPicturePosition(div: HTMLDivElement) {
        if (this.placement != AnnotationPlacement.InPicture) {
            return;
        }
        let left = 0;
        let top = this.element[0].clientHeight;
        if (this.element[0].offsetLeft + div.clientWidth > this.element.parent()[0].clientWidth) {
            const overlap = this.element.parent()[0].clientWidth - this.element[0].offsetLeft + this.element[0].clientWidth > 0;
            if (overlap) {
                if (this.element.parent()[0].clientWidth - div.clientWidth >= 0) {
                    left = -(this.element[0].offsetLeft + div.clientWidth - this.element.parent()[0].clientWidth);
                }
            } else if (this.element[0].offsetLeft + this.element[0].clientWidth - div.clientWidth >= 0) {
                left = -div.clientWidth + this.element[0].clientWidth;
            }
        }
        const spanBottom = this.element[0].offsetTop + this.element[0].clientHeight;
        if (this.element[0].offsetTop + this.element[0].clientHeight + div.clientHeight > this.element.parent()[0].clientHeight) {
            if (this.element[0].offsetTop - div.clientHeight >= 0) {
                top = -div.clientHeight;
            }
        }
        this.inlineSpan.nativeElement.style.left = left + "px";
        this.inlineSpan.nativeElement.style.top = top + "px";
    }

    get rctrl() {
        return this.vctrl.reviewCtrl;
    }

    /**
     * Toggles the visibility of the annotation.
     */
    async toggleAnnotation() {
        await this.rctrl.toggleAnnotation(this, false);
    }

    toggleAnnotationShow() {
        this.show = !this.show;
        this.toggleElementBorder();
        this.updateZIndex();
    }

    toggleElementBorder() {
        if (this.placement == AnnotationPlacement.InPicture) {
            const innerRectangle = this.element.find(".annotation-picture-element");
            if (innerRectangle[0]) {
                innerRectangle[0].style.border = this.show ? "1px solid #000000" : "none";
            }
        }
    }

    clearColor() {
        this.values.color = "";
    }

    isVelpCustomColor() {
        if (this.values.color == null) {
            return false;
        }
        return this.values.color.length === 7; // hex colors are 7 characters long
    }

    /**
     * Updates the z-index of the annotation.
     */
    updateZIndex() {
        this.zIndex = this.rctrl.zIndex;
        this.rctrl.zIndex++;
    }

    /**
     * Shows the annotation.
     */
    showAnnotation() {
        this.setShowFull(false);
        this.show = true;
        this.toggleElementBorder();
        this.updateZIndex();
    }

    scrollToIfNotInViewport() {
        const e = this.element[0];
        if (!isInViewport(e)) {
            scrollToElement(e);
        }
    }

    /**
     * Deletes this annotation.
     */
    deleteAnnotation() {
        if (!window.confirm("Delete - are you sure?")) {
            return;
        }
        this.rctrl.deleteAnnotation(this.annotation.id);
    }

    /**
     * Saves the changes made to the annotation.
     */
    async saveChanges() {
        const id = this.annotation.id;

        // Add comment
        if (this.newcomment.length > 0) {
            const data = {id: id, content: this.newcomment};
            const r = await to($http.post<Record<string, unknown>>(
                "/add_annotation_comment",
                {id: id, content: this.newcomment},
            ));
            if (!r.ok) {
                return;
            }
        }
        this.newcomment = "";
        const updatevalues = {
            id: id,
            ...this.values,
        };
        const r2 = await updateAnnotationServer(updatevalues);
        if (!r2.ok) {
            return;
        }
        const ann = r2.result;
        this.annotation = ann;
        this.original = this.annotation.getEditableValues();
        this.rctrl.updateAnnotation(ann);
    }

    getCustomColor() {
        return this.values.color;
    }

    /**
     * Checks if the user has rights to edit the annotation.
     */
    isAnnotator() {
        return this.annotation.annotator.id == Users.getCurrent().id;
    }

    /**
     * Return true if the user has teacher rights.
     */
    allowChangePoints() {
        return this.rctrl.item.rights.teacher;
    }

    /**
     * Checks if there are unsaved changes.
     */
    hasChanged() {
        return !deepEqual(this.original, this.values) || this.newcomment.length > 0;
    }

    /**
     * Detects the `Ctrl + S` and `Ctrl+Enter` key strokes on the text area.
     */
    keyDownFunc(event: KeyboardEvent) {
        if (event.keyCode === KEY_CTRL) {
            this.ctrlDown = true;
        }
        if (this.ctrlDown && (event.which === KEY_S || event.keyCode === KEY_ENTER)) {
            event.preventDefault();
            this.ctrlDown = false;
            if (this.hasChanged()) {
                this.saveChanges();
            } else {
                this.toggleAnnotation();
            }
        }
    }

    /**
     * Detects if `Ctrl`-key is released.
     */
    keyUpFunc(event: KeyboardEvent) {
        if (event.keyCode === KEY_CTRL) {
            this.ctrlDown = false;
        }
    }

    openHelp() {
        showMessageDialog(`
        'Just me' means that the annotation is visible only to yourself.
        'Document owner' refers to the person or group who has been named as the document owner.
        'Teachers' refers to the users that have teacher access to this document.
        'Everyone' means that the annotation is visible to everyone who can view the assessed content.`);
    }
}
