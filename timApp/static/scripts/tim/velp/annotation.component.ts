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
import type {
    AfterViewInit,
    OnDestroy,
    OnInit,
    SimpleChanges,
} from "@angular/core";
import {Component, ElementRef, Input, ViewChild} from "@angular/core";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import type {DrawItem} from "tim/plugin/draw-canvas/draw-canvas.components";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {ParCompiler} from "tim/editor/parCompiler";
import {parseHexColor, shouldUseDarkText} from "tim/util/colorUtils";
import type {ViewCtrl} from "tim/document/viewctrl";
import {KEY_CTRL, KEY_ENTER, KEY_S} from "tim/util/keycodes";
import {$http} from "tim/util/ngimport";
import type {Result} from "tim/util/utils";
import {clone, isInViewport, scrollToElement, to} from "tim/util/utils";
import type {
    IAnnotationEditableValues,
    IAnnotationInterval,
} from "tim/velp/velptypes";
import {Annotation, jsonSerializer} from "tim/velp/velptypes";

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
    AccuratelyPositioned,
    InMargin,
    InMarginOnly,
    InMarginAndUnknownIfItWillBeAccuratelyPositioned,
}

export interface IAnnotationBindings {
    reason: AnnotationAddReason;
    placement: AnnotationPlacement;
    annotation: Annotation;
    defaultcomment: string;
}

export async function updateAnnotationServer(
    updatevalues: IAnnotationEditableValues & {
        id: number;
        coord?: IAnnotationInterval;
        draw_data?: DrawItem[];
    }
): Promise<Result<Annotation, string>> {
    const r2 = await to(
        $http.post<Record<string, unknown>>("/update_annotation", updatevalues)
    );
    if (!r2.ok) {
        return {ok: false, result: r2.result.data.error};
    }
    const annotation = jsonSerializer.deserialize(r2.result.data, Annotation);
    if (!annotation || Array.isArray(annotation)) {
        return {ok: false, result: "Invalid annotation data"};
    }
    return {ok: true, result: annotation};
}

@Component({
    selector: "annotation",
    template: `
        <span #contentSpan (click)="setShowFull(false);toggleAnnotation()"
              class="no-popup-menu highlighted clickable emphasise default"
              [ngClass]="getClass()"
              [style.background-color]="getCustomColor()"
              [class.dark-bg]="darkBg">
            <ng-content></ng-content>
        </span>
        <span #inlineSpan [ngClass]="(isImageAnnotation()) ? 'inlineImageAnnotation' : 'inlineAnnotation'">
            <div #inlineDiv *ngIf="showTransparentText()"
                 class="emphasise image-text default"
                 style="white-space: nowrap; position: absolute;"
                 (click)="setShowFull(true)"
                 [style.color]="getCustomColor()"
                 [ngClass]="getClass()"
                 [style.z-index]="zIndex">
                <p class="math" style="font-size: x-large; margin: 0">
                    {{ annotation.getContent() }}
                </p>
                <p class="math" *ngFor="let comment of annotation.comments"
                   style="margin: 0">
                    {{ comment.content }}
                </p>
            </div>
            <div #inlineDiv *ngIf="show && !showTransparentText()"
                 class="no-popup-menu annotation-info emphasise default"
                 [ngClass]="getClass()"
                 (keydown)="keyDownFunc($event)"
                 (keyup)="keyUpFunc($event)"
                 [style.backgroundColor]="getCustomColor()"
                 [class.dark-bg]="darkBg"
                 [style.z-index]="zIndex">
                
                <span class="fulldiv">
                    <span class="div-90 annTopSection" (mouseenter)="setShowFull(true)">
                        <p><span class="annHeader math"><strong>{{ annotation.getContent() }}</strong></span></p>
                        <p *ngIf="showFull">
                            <tim-signature [user]="annotation.annotator"
                                           [time]="annotation.creation_time"></tim-signature>
                        </p>
                    </span>
                    <tim-close-button class="clickable-icon"
                                      (click)="toggleAnnotationShow(); setShowFull(false)"></tim-close-button>
                </span>
                
                <div>
                    <div *ngIf="showFull">
                        <p class="points-input">
                            <ng-container *ngIf="canEditAnnotation() && allowChangePoints()">
                                <label>
                                Points: <input type="number"
                                               [(ngModel)]="values.points"
                                               step="any"
                                               [disabled]="!canEditAnnotation()"/>
                                </label>
                            </ng-container>
                            <span *ngIf="canEditAnnotation()" style="float: right">
                                <input type="color" [(ngModel)]="values.color"
                                       (ngModelChange)="onColorUpdate($event)"
                                       title="Change annotation color"
                                       class="annotation-color-selector">
                                <button *ngIf="isVelpCustomColor()"
                                        class="btn-restore-annotation-color"
                                        (click)="clearColor()"
                                        title="Reset color to original value">R</button>
                                <button class="btn-restore-annotation-color"
                                        (click)="whiteColor()"
                                        title="Reset color to white">W</button>
                            </span>
                        </p>
                        <p *ngIf="!canEditAnnotation()"><label
                                [hidden]="values.points == null">Points:  {{ values.points }}</label></p>
                    </div>
                    <div [hidden]="showFull">
                        <div *ngIf="values.points != null">
                            <p class="points-input"><label>Points: {{ values.points }}</label></p>
                        </div>
                    </div>
                    
                    <div class="fulldiv" #commentsDiv>
                        <ul class="comments">
                            <li *ngFor="let comment of annotation.comments">
                                <p class="math">{{ comment.content }}
                                    &mdash;
                                    <tim-signature [time]="comment.comment_time"
                                                   [user]="comment.commenter"></tim-signature>
                                </p>
                            </li>
                        </ul>
                    </div>
                    <div *ngIf="showFull">
                        <textarea focusMe class="form-control comment-area" placeholder="Add comment"
                                  [(ngModel)]="newcomment"></textarea>
                        <div>
                            <div class="form-inline adjustAnnForm">
                                <div>
                                    <span [hidden]="!canEditAnnotation()">
                                        <label [class.disabled]="!canEditAnnotation()">
                                            <div title="Visible to"
                                                 class="glyphicon glyphicon-eye-open visible-icon"></div></label>
                                        <select [(ngModel)]="values.visible_to" [disabled]="!canEditAnnotation()">
                                            <option *ngFor="let o of visibleOptions.values"
                                                    [ngValue]="o.id">{{o.name}}</option>
                                        </select>
                                        <div class="glyphicon glyphicon-question-sign clickable-icon help-icon"
                                             (click)="openHelp()">
                                        </div>
                                    </span>
                                    <span *ngIf="!canEditAnnotation()" class="annotationVisibleText">
                                        <span title="Visible to" class="annqmark glyphicon glyphicon-eye-open"></span>
                                        {{ getSelectedVisibleOption().name }}
                                        <span class="annqmark glyphicon glyphicon-question-sign clickable-icon"
                                              (click)="openHelp()">
                                        </span>

                                    </span>
                                    <p>
                                        <span *ngIf="canEditAnnotation() && isImageAnnotation()">
                                            Style&nbsp;
                                            <select [(ngModel)]="values.style" [disabled]="!canEditAnnotation()">
                                                <option *ngFor="let o of styleOptions.values"
                                                        [ngValue]="o.id">{{o.name}}</option>
                                            </select>
                                        </span>
                                    </p>
                                </div>
                            </div>
                        </div>
                        <span>
                            <!--
                            <button class="saveChanges timButton" *ngIf="hasChanged()" (click)="saveChanges()"> 
                            -->
                            <button class="saveChanges timButton" [disabled]="!hasChanged()" (click)="saveChanges()">
                                Save
                            </button>
                        </span>
                    </div>
                    <div *ngIf="showFull">
                        <span class="pull-right glyphicon glyphicon-trash clickable-icon glyph-delete-annotation" title="Delete annotation"
                              [hidden]="!canEditAnnotation()"
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
export class AnnotationComponent
    implements OnDestroy, OnInit, AfterViewInit, IAnnotationBindings
{
    private ctrlDown = false;
    visibleOptions = {
        values: [
            {id: 1, name: "Just me"},
            {id: 2, name: "Document owner"},
            {id: 3, name: "Teachers"},
            {id: 4, name: "Users with access"},
        ],
        default: 1,
    };
    styleOptions = {
        values: [
            {id: 1, name: "Default"},
            {id: 2, name: "Text"},
            {id: 3, name: "Text (always visible)"},
        ],
        default: 1,
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
    private refreshMath = false;
    zIndex = 0;
    darkBg = false;

    imageCoordinates = {top: 0, left: 0}; // location of drawn velp on drawCanvas
    imageDimensions = {width: 0, height: 0}; // size of annotation border div on drawCanvas

    @ViewChild("inlineSpan") inlineSpan!: ElementRef<HTMLSpanElement>;
    @ViewChild("contentSpan") contentSpan!: ElementRef<HTMLSpanElement>;
    inlineDivRef?: ElementRef<HTMLDivElement>;

    @ViewChild("inlineDiv") set inlineDiv(div: ElementRef<HTMLDivElement>) {
        if (div && this.isImageAnnotation()) {
            this.inlineDivRef = div;
            this.adjustAnnotationInPicturePosition(
                this.inlineDivRef.nativeElement
            );
            ParCompiler.processAllMath($(div.nativeElement));
        }
    }

    private commentsDivRef?: ElementRef<HTMLDivElement>;
    @ViewChild("commentsDiv") set commentsDiv(
        el: ElementRef<HTMLDivElement> | undefined
    ) {
        if (!el || this.commentsDivRef == el) {
            return;
        }
        this.commentsDivRef = el;
        ParCompiler.processAllMath($(el.nativeElement));
    }

    constructor(public e: ElementRef) {
        this.element = $(e.nativeElement);
    }

    setShowFull(v: boolean) {
        this.showFull = v;
        if (v) {
            this.show = true;
            this.updateZIndex();
            if (this.inlineDivRef) {
                this.adjustAnnotationInPicturePosition(
                    this.inlineDivRef.nativeElement
                );
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
        return this.visibleOptions.values.find(
            (o) => o.id === this.values.visible_to
        )!;
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
        if (this.isImageAnnotation()) {
            const ele = this.element.get()[0];
            this.imageCoordinates = {top: ele.offsetTop, left: ele.offsetLeft};
            this.imageDimensions = {
                width: ele.clientWidth,
                height: ele.clientHeight,
            };
            const aid = this.annotation.getAnswerId();
            if (aid) {
                const canvas = this.vctrl.getVelpCanvas(aid);
                if (canvas) {
                    this.resizeElementBorder(canvas.zoomLevel);
                }
            }
        }
    }

    setAnnotation(a: Annotation) {
        this.annotation = a;
        this.original = a.getEditableValues();
        this.values = clone(this.original);
        this.onColorUpdate(this.values.color ?? "");
        this.refreshMath = true;
    }

    ngOnChanges(changes: SimpleChanges) {
        if (changes.annotation) {
            this.refreshMath = true;
        }
    }

    ngAfterViewChecked() {
        if (this.refreshMath) {
            this.refreshMath = false;
            if (this.commentsDivRef) {
                ParCompiler.processAllMath(
                    $(this.commentsDivRef.nativeElement)
                );
            }
            if (this.inlineDivRef) {
                ParCompiler.processAllMath($(this.inlineDivRef.nativeElement));
            }
        }
    }

    private isInMargin() {
        return this.placement !== AnnotationPlacement.AccuratelyPositioned;
    }

    isImageAnnotation(): boolean {
        return (
            this.placement == AnnotationPlacement.AccuratelyPositioned &&
            this.annotation.draw_data != undefined
        );
    }

    showTransparentText(): boolean {
        return (
            // {id: 1, name: "Default/Hidden"},
            // {id: 2, name: "Text"},
            // {id: 3, name: "Text (always visible"},
            this.isImageAnnotation() &&
            !this.showFull &&
            ((this.values.style == 2 && this.show) || this.values.style == 3)
        );
    }

    ngAfterViewInit() {
        if (this.reason == AnnotationAddReason.AddingNew && this.show) {
            this.scrollToIfNotInViewport();
        }
    }

    /**
     * Checks if in-picture annotation would fit better if opened upwards or leftwards
     * @param div Annotation info div
     */
    adjustAnnotationInPicturePosition(div: HTMLDivElement) {
        // TODO: Update coordinate from canvas if direct click, or check scroll pos if opened via margin
        //  Then adjust position near click / visible part of large drawn velp
        if (!this.isImageAnnotation()) {
            return;
        }
        const aid = this.annotation.getAnswerId();
        if (aid == undefined) {
            return;
        }
        const canvas = this.vctrl.getVelpCanvas(aid);
        if (!canvas) {
            return;
        }
        let left = 0;
        const container = canvas.getWrapper();
        const drawingLeft = this.element[0].offsetLeft;
        const annWidth = div.clientWidth;
        const containerWidth = container.clientWidth + container.scrollLeft;
        if (drawingLeft + annWidth > containerWidth) {
            if (containerWidth - annWidth >= container.scrollLeft) {
                left = containerWidth - annWidth - drawingLeft;
            }
        }

        const drawingHeight = this.element[0].clientHeight;
        const drawingTop = this.element[0].offsetTop;
        const annHeight = div.clientHeight;
        const containerHeight = container.clientHeight + container.scrollTop;
        let top = drawingHeight;
        if (drawingTop + drawingHeight + annHeight > containerHeight) {
            if (drawingTop - annHeight >= container.scrollTop) {
                top = -annHeight;
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
        if (this.isImageAnnotation()) {
            const innerRectangle = this.element.find(
                ".annotation-picture-element"
            );
            if (innerRectangle[0]) {
                innerRectangle[0].style.border = this.show
                    ? "1px solid #000000"
                    : "none";
            }
        }
    }

    /**
     * Resize the div around which a drawn velp is wrapped
     * @param scale new size
     */
    resizeElementBorder(scale: number) {
        if (this.isImageAnnotation()) {
            const innerRectangle = this.element.find(
                ".annotation-picture-element"
            );
            const rect = innerRectangle[0];
            const ele = this.element.get()[0];
            if (!ele || !rect) {
                return;
            }
            rect.style.width = this.imageDimensions.width * scale + "px";
            rect.style.height = this.imageDimensions.height * scale + "px";
            ele.style.left = this.imageCoordinates.left * scale + "px";
            ele.style.top = this.imageCoordinates.top * scale + "px";
            if (this.inlineDivRef) {
                this.adjustAnnotationInPicturePosition(
                    this.inlineDivRef.nativeElement
                );
            }
        }
    }

    clearColor() {
        this.values.color = this.original.color ?? "";
        this.onColorUpdate(this.values.color);
    }

    whiteColor() {
        this.values.color = "#ffffff";
        this.onColorUpdate(this.values.color);
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

        let ann = this.annotation;

        // Add comment
        if (this.commentChanged()) {
            const r = await to(
                $http.post<Record<string, unknown>>("/add_annotation_comment", {
                    id: id,
                    content: this.newcomment,
                })
            );
            if (!r.ok) {
                return;
            }
            const annotation = jsonSerializer.deserializeObject(
                r.result.data,
                Annotation
            );
            if (!annotation) {
                return;
            }
            ann = annotation;
            this.newcomment = "";
        }
        if (this.valuesChanged()) {
            const updatevalues = {
                id: id,
                ...this.values,
            };
            const r2 = await updateAnnotationServer(updatevalues);
            if (!r2.ok) {
                return;
            }
            ann = r2.result;
        }
        this.annotation = ann;
        this.original = this.annotation.getEditableValues();
        this.rctrl.updateAnnotation(ann);
        this.refreshMath = true;
    }

    getCustomColor() {
        return this.values.color;
    }

    /**
     * Checks if the user is the original annotator.
     */
    isAnnotator() {
        return this.annotation.annotator.id == Users.getCurrent().id;
    }

    /**
     * Checks if the user has rights to edit the annotation.
     */
    canEditAnnotation(): boolean {
        return this.isAnnotator() || this.vctrl.item.rights.teacher;
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
        return this.valuesChanged() || this.commentChanged();
    }

    valuesChanged() {
        return !deepEqual(this.original, this.values);
    }

    commentChanged() {
        // in test environment newcomment might end up undefined
        return this.newcomment?.length > 0;
    }

    /**
     * Detects the `Ctrl + S` and `Ctrl+Enter` key strokes on the text area.
     */
    keyDownFunc(event: KeyboardEvent) {
        if (event.keyCode === KEY_CTRL) {
            this.ctrlDown = true;
        }
        if (
            this.ctrlDown &&
            (event.which === KEY_S || event.keyCode === KEY_ENTER)
        ) {
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
        'Users with access' means that the annotation is visible to everyone who can view the assessed content.`);
    }

    onColorUpdate(newColor: string) {
        const rgb = parseHexColor(newColor);
        if (!rgb) {
            return;
        }
        this.darkBg = !shouldUseDarkText(rgb);
    }
}
