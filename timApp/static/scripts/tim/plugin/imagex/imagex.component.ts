import deepmerge from "deepmerge";
import type * as t from "io-ts";
import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {
    ChangeDetectorRef,
    Component,
    ElementRef,
    NgModule,
} from "@angular/core";
import {DomSanitizer} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {$http, $sce} from "tim/util/ngimport";
import {TimDefer} from "tim/util/timdefer";
import type {MouseOrTouch} from "tim/util/utils";
import {
    defaultTimeout,
    posToRelative,
    timeout,
    to,
    touchEventToTouch,
    valueOr,
} from "tim/util/utils";
import {editorChangeValue} from "tim/editor/editorScope";
import type {ITimComponent, IVelpableComponent} from "tim/document/viewctrl";
import {ChangeType} from "tim/document/viewctrl";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import type {
    IDrawOptions,
    IDrawVisibleOptions,
} from "tim/plugin/draw-canvas/draw-toolbar.component";
import {
    DrawToolbarModule,
    DrawType,
} from "tim/plugin/draw-canvas/draw-toolbar.component";
import type {DrawItem} from "tim/plugin/draw-canvas/draw-canvas.components";
import {Drawing} from "tim/plugin/draw-canvas/draw-canvas.components";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {
    CommonPropsT,
    DefaultPropsT,
    DragObjectPropsT,
    FixedObjectPropsT,
    ImageXMarkup,
    IPinPosition,
    IPoint,
    ISized,
    ISizedPartial,
    ObjectTypeT,
    OptionalCommonPropNames,
    OptionalDragObjectPropNames,
    OptionalFixedObjPropNames,
    OptionalPropNames,
    OptionalTargetPropNames,
    PinAlign,
    PinPropsT,
    RequiredNonNull,
    RequireExcept,
    RightAnswerT,
    SizeT,
    TargetPropsT,
    TextboxPropsT,
    TuplePoint,
} from "tim/plugin/imagex/imagextypes";
import {ImageXAll, SingleSize, ValidCoord} from "tim/plugin/imagex/imagextypes";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {CommonModule} from "@angular/common";

let globalPreviewColor = "#fff";

export function tupleToCoords(p: TuplePoint) {
    return {x: p[0], y: p[1]};
}

function tupleToSizedOrDef(
    p: SizeT | undefined | null,
    def: ISizedPartial | undefined
) {
    return p ? {width: p[0], height: p[1]} : def;
}

function drawFillCircle(
    ctx: CanvasRenderingContext2D,
    r: number,
    p1: number,
    p2: number,
    c: string,
    tr: number
) {
    const p = ctx;
    p.globalAlpha = tr;
    p.beginPath();
    p.fillStyle = c;
    p.arc(p1, p2, r, 0, 2 * Math.PI);
    p.fill();
    p.globalAlpha = 1;
}

function toRange(range: TuplePoint, p: number) {
    if (p < range[0]) {
        return range[0];
    }
    if (p > range[1]) {
        return range[1];
    }
    return p;
}

function isObjectOnTopOf(
    position: IPoint,
    object: DrawObject,
    grabOffset: number
) {
    return object.isPointInside(position, grabOffset);
}

function areObjectsOnTopOf<T extends DrawObject>(
    position: IPoint,
    objects: T[],
    grabOffset: number | undefined
): T | undefined {
    for (const o of objects) {
        const collision = isObjectOnTopOf(position, o, grabOffset ?? 0);
        if (collision) {
            return o;
        }
    }
    return undefined;
}

enum TargetState {
    Normal,
    Snap,
    Drop,
}

class DragTask {
    public bgCtx: CanvasRenderingContext2D; // for drags, task-related lines etc
    public frontCtx: CanvasRenderingContext2D; // for custom drawing
    private mousePosition: IPoint;
    private drawing: Drawing;
    private mouseDown = false;
    private activeDragObject?: {
        obj: DragObject;
        xoffset: number;
        yoffset: number;
    };
    public drawObjects: DrawObject[];
    public lines?: Line[];

    constructor(
        private bgCanvas: HTMLCanvasElement,
        private frontCanvas: HTMLCanvasElement,
        private imgx: ImageXComponent
    ) {
        this.bgCtx = bgCanvas.getContext("2d")!;
        this.frontCtx = frontCanvas.getContext("2d")!;
        this.drawObjects = [];
        this.mousePosition = {x: 0, y: 0};
        this.drawing = imgx.drawing;

        this.frontCanvas.style.touchAction = "double-tap-zoom"; // To get IE and EDGE touch to work

        this.frontCanvas.addEventListener("mousemove", (event) => {
            event.preventDefault();
            this.moveEvent(event, event);
        });
        this.frontCanvas.addEventListener("touchmove", (event) => {
            event.preventDefault();
            this.moveEvent(event, touchEventToTouch(event));
        });
        this.frontCanvas.addEventListener("mousedown", (event) => {
            event.preventDefault();
            this.downEvent(event, event);
        });
        this.frontCanvas.addEventListener("touchstart", (event) => {
            event.preventDefault();
            // this.imgx.getScope().$evalAsync(() => {
            //     this.downEvent(event, this.te(event));
            // });
            this.downEvent(event, touchEventToTouch(event));
        });
        this.frontCanvas.addEventListener("mouseup", (event) => {
            event.preventDefault();
            this.upEvent(event, event);
        });
        this.frontCanvas.addEventListener("touchend", (event) => {
            event.preventDefault();
            this.upEvent(event, touchEventToTouch(event));
        });

        if (imgx.markup.freeHandShortCuts) {
            this.frontCanvas.addEventListener(
                "keypress",
                (event) => {
                    const c = String.fromCharCode(event.keyCode);
                    if (event.keyCode === 26) {
                        this.imgx.undo();
                    }
                    if (c === "c") {
                        this.imgx.resetExercise();
                    }
                    if (c === "r") {
                        this.setColor("#f00");
                    }
                    if (c === "b") {
                        this.setColor("#00f");
                    }
                    if (c === "y") {
                        this.setColor("#ff0");
                    }
                    if (c === "g") {
                        this.setColor("#0f0");
                    }
                    if (c === "+") {
                        this.incWidth(+1);
                    }
                    if (c === "-") {
                        this.incWidth(-1);
                    }
                    if (c === "1") {
                        this.setWidth(1);
                    }
                    if (c === "2") {
                        this.setWidth(2);
                    }
                    if (c === "3") {
                        this.setWidth(3);
                    }
                    if (c === "4") {
                        this.setWidth(4);
                    }
                    // if (c === "l") {
                    //     this.drawing.flipLineMode();
                    // }
                    if (c === "f" && imgx.freeHandShortCut) {
                        imgx.drawSettings.enabled = !imgx.drawSettings.enabled;
                    }
                },
                false
            );
        }

        // Lisatty eventlistenereiden poistamiseen.
        /*
        this.removeEventListeners = function() {
            this.canvas.removeEventListener('mousemove', moveEvent);
            this.canvas.removeEventListener('touchmove', moveEvent);
            this.canvas.removeEventListener('mousedown', downEvent);
            this.canvas.removeEventListener('touchstart', downEvent);
            this.canvas.removeEventListener('mouseup', upEvent);
            this.canvas.removeEventListener('touchend', upEvent);
        }
        */
    }

    get targets(): Target[] {
        const result = [];
        for (const o of this.drawObjects) {
            if (o.name === "target") {
                result.push(o);
            }
        }
        return result;
    }

    get dragobjects(): DragObject[] {
        const result = [];
        for (const o of this.drawObjects) {
            if (o.name === "dragobject") {
                result.push(o);
            }
        }
        return result;
    }

    drawDragTask() {
        const canvas = this.bgCanvas;
        this.bgCtx.globalCompositeOperation = "source-over";
        setIdentityTransform(this.bgCtx);
        this.bgCtx.fillStyle = "white"; // TODO get from markup?
        this.bgCtx.globalAlpha = 1;
        this.bgCtx.clearRect(0, 0, canvas.width, canvas.height);

        if (this.activeDragObject) {
            const dobj = this.activeDragObject;
            const xlimits: [number, number] = dobj.obj.xlimits ?? [0, 1e100];
            const ylimits: [number, number] = dobj.obj.ylimits ?? [0, 1e100];
            if (dobj.obj.lock !== "x") {
                dobj.obj.x = toRange(
                    xlimits,
                    this.mousePosition.x - dobj.xoffset
                );
            }
            if (dobj.obj.lock !== "y") {
                dobj.obj.y = toRange(
                    ylimits,
                    this.mousePosition.y - dobj.yoffset
                );
            }
        }
        const targets = this.targets;
        for (const o of this.drawObjects) {
            o.draw();
            if (o.name === "dragobject") {
                if (this.activeDragObject) {
                    const onTopOf = areObjectsOnTopOf(
                        this.activeDragObject.obj,
                        targets,
                        this.imgx.grabOffset
                    );
                    if (onTopOf && onTopOf.objectCount < onTopOf.maxObjects) {
                        onTopOf.state = TargetState.Snap;
                    }
                    const onTopOfB = areObjectsOnTopOf(
                        o,
                        targets,
                        this.imgx.grabOffset
                    );
                    if (onTopOfB && o !== this.activeDragObject.obj) {
                        onTopOfB.state = TargetState.Drop;
                    }
                } else {
                    const onTopOfA = areObjectsOnTopOf(
                        o,
                        targets,
                        this.imgx.grabOffset
                    );
                    if (onTopOfA) {
                        onTopOfA.state = TargetState.Drop;
                    }
                }
            } else if (o.name === "target") {
                o.state = TargetState.Normal;
            }
        }
        setIdentityTransform(this.bgCtx);
        if (this.lines) {
            for (const l of this.lines) {
                l.draw(this.bgCtx);
            }
        }
    }

    downEvent(event: Event, p: MouseOrTouch) {
        event.preventDefault();
        this.mousePosition = posToRelative(this.frontCanvas, p);
        let active = areObjectsOnTopOf(this.mousePosition, this.dragobjects, 0);
        if (!active) {
            active = areObjectsOnTopOf(
                this.mousePosition,
                this.dragobjects,
                this.imgx.grabOffset
            );
        }

        if (this.imgx.emotion) {
            drawFillCircle(
                this.frontCtx,
                30,
                this.mousePosition.x,
                this.mousePosition.y,
                "black",
                0.5
            );
            this.drawing.startSegmentDraw(this.mousePosition);
            if (this.imgx.markup.autosave) {
                this.imgx.save();
            }
        }

        if (active) {
            const array = this.drawObjects;
            array.push(array.splice(array.indexOf(active), 1)[0]); // put active last so that it gets drawn on top of others
            this.frontCanvas.style.cursor = "pointer";
            this.activeDragObject = {
                obj: active,
                xoffset: this.mousePosition.x - active.x,
                yoffset: this.mousePosition.y - active.y,
            };
            this.drawDragTask();
            this.drawing.redrawAll();
        } else if (this.imgx.drawSettings.enabled) {
            this.frontCanvas.style.cursor = "pointer";
            this.mouseDown = true;
            if (!this.imgx.emotion) {
                this.drawing.downEvent(this.mousePosition);
            }
        }

        if (this.imgx.attrsall.preview) {
            this.imgx.coords = `[${Math.round(
                this.mousePosition.x
            )}, ${Math.round(this.mousePosition.y)}]`;
            editorChangeValue(["position:"], this.imgx.coords);
        }
    }

    moveEvent(event: Event, p: MouseOrTouch) {
        event.preventDefault();
        if (this.activeDragObject) {
            if (event != p) {
                event.preventDefault();
            }
            this.mousePosition = posToRelative(this.frontCanvas, p);
            if (!this.imgx.emotion) {
                this.drawDragTask();
                this.drawing.redrawAll();
            }
        } else if (this.mouseDown) {
            if (event != p) {
                event.preventDefault();
            }
            if (!this.imgx.emotion) {
                this.drawDragTask();
                this.drawing.moveEvent(posToRelative(this.frontCanvas, p));
            }
        }
    }

    upEvent(event: Event, p: MouseOrTouch) {
        if (this.activeDragObject) {
            this.frontCanvas.style.cursor = "default";
            if (event != p) {
                event.preventDefault();
            }
            this.mousePosition = posToRelative(this.frontCanvas, p);

            const isTarget = areObjectsOnTopOf(
                this.activeDragObject.obj,
                this.targets,
                undefined
            );

            if (isTarget) {
                if (isTarget.objectCount < isTarget.maxObjects) {
                    if (isTarget.snap) {
                        this.activeDragObject.obj.x =
                            isTarget.x + isTarget.snapOffset.x;
                        this.activeDragObject.obj.y =
                            isTarget.y + isTarget.snapOffset.y;
                    }
                }
            }

            this.activeDragObject = undefined;
            if (!this.imgx.emotion) {
                this.drawDragTask();
                this.drawing.redrawAll();
            }
        } else if (this.mouseDown) {
            this.frontCanvas.style.cursor = "default";
            this.mouseDown = false;
            this.drawing.upEvent(posToRelative(this.frontCanvas, p));
        }
        this.imgx.checkChanges();
    }

    addRightAnswers(answers: RightAnswerT[]) {
        this.lines = [];
        const objects = this.drawObjects;
        for (const d of answers) {
            const o = objects.find((obj) => obj.id === d.id);
            if (o) {
                const line = new Line("green", 2, o, tupleToCoords(d.position));
                this.lines.push(line);
            }
        }
        this.drawDragTask();
        this.drawing.redrawAll();
    }

    setColor(newColor: string) {
        this.imgx.drawSettings.color = newColor;
    }

    setWidth(newWidth: number) {
        this.imgx.drawSettings.w = newWidth;
        if (this.imgx.drawSettings.w < 1) {
            this.imgx.drawSettings.w = 1;
        }
    }

    incWidth(dw: number) {
        this.setWidth(this.imgx.drawSettings.w + dw);
    }
}

function alignToDir(a: PinAlign, diagonalFactor: number) {
    let alignOffset;
    switch (a) {
        case "north":
            alignOffset = {x: 0, y: -1};
            break;
        case "northeast":
            alignOffset = {x: diagonalFactor, y: -diagonalFactor};
            break;
        case "northwest":
            alignOffset = {x: -diagonalFactor, y: -diagonalFactor};
            break;
        case "south":
            alignOffset = {x: 0, y: 1};
            break;
        case "southeast":
            alignOffset = {x: diagonalFactor, y: diagonalFactor};
            break;
        case "southwest":
            alignOffset = {x: -diagonalFactor, y: diagonalFactor};
            break;
        case "east":
            alignOffset = {x: 1, y: 0};
            break;
        case "west":
            alignOffset = {x: -1, y: 0};
            break;
        case "center":
            alignOffset = {x: 0, y: 0};
            break;
    }
    return alignOffset;
}

class Pin {
    private pos: RequiredNonNull<IPinPosition>;

    constructor(
        private values: RequiredNonNull<PinPropsT>,
        defaultAlign: PinAlign
    ) {
        const a = values.position.align ?? defaultAlign;
        const {x, y} = alignToDir(a, 1 / Math.sqrt(2));
        const lngth = values.length;
        this.pos = {
            align: a,
            coord: tupleToCoords(
                values.position.coord ?? [x * lngth, y * lngth]
            ),
            start: tupleToCoords(values.position.start ?? [0, 0]),
        };
    }

    /**
     * Returns the offset for pinned object. The returned offset refers to the center of the object.
     * @param o
     */
    getOffsetForObject(o: ISized) {
        const wp2 = o.width / 2;
        const hp2 = o.height / 2;
        const alignOffset = alignToDir(this.pos.align, 1);
        return {
            x: -(this.pos.coord.x + alignOffset.x * wp2),
            y: -(this.pos.coord.y + alignOffset.y * hp2),
        };
    }

    draw(ctx: CanvasRenderingContext2D) {
        if (this.values.visible) {
            ctx.strokeStyle = this.values.color;
            ctx.fillStyle = this.values.color;
            ctx.beginPath();
            ctx.arc(0, 0, this.values.dotRadius, 0, 2 * Math.PI);
            ctx.fill();
            ctx.beginPath();
            ctx.moveTo(0, 0);
            ctx.lineWidth = this.values.linewidth;
            ctx.lineTo(
                -(this.pos.coord.x - this.pos.start.x),
                -(this.pos.coord.y - this.pos.start.y)
            );
            ctx.stroke();
        }
    }
}

function point(x: number, y: number) {
    return {x, y};
}

export type DrawObject = Target | FixedObject | DragObject;

interface IChildShape {
    s: Shape;
    p: IPoint;
}

const toRadians = Math.PI / 180;

type ImageLoadResult = HTMLImageElement | Event;

abstract class ObjBase<
    T extends RequireExcept<CommonPropsT, OptionalCommonPropNames>
> {
    public x: number;
    public y: number;
    public a: number;
    protected childShapes: IChildShape[] = [];
    public pendingImage?: Promise<ImageLoadResult>;

    public mainShape: Shape;

    get overrideColor(): string | undefined {
        return undefined;
    }

    // Center point of object ignoring possible pin; used in drag & drop checks.
    get center(): IPoint {
        const off = this.mainShapeOffset;
        const rotOff = this.getRotatedPoint(off.x, off.y, this.a);
        return point(this.x + rotOff.x, this.y + rotOff.y);
    }

    protected constructor(
        protected ctx: CanvasRenderingContext2D,
        protected values: T,
        public did: string
    ) {
        const z = values.position;
        this.x = z[0];
        this.y = z[1];
        this.a = -valueOr(this.values.a, 0);
        let s;
        if (ValidCoord.is(this.values.size)) {
            const [width, height] = this.values.size;
            s = {width, height};
        } else if (SingleSize.is(this.values.size)) {
            const [width] = this.values.size;
            s = {width};
        }
        switch (values.type) {
            case "img":
                let img;
                if (this.values.imgproperties) {
                    [this.pendingImage, img] = loadImage(
                        this.values.imgproperties.src
                    );
                    if (this.values.imgproperties.textbox) {
                        this.childShapes.push({
                            p: this.values.textboxproperties
                                ? tupleToCoords(
                                      this.values.textboxproperties
                                          .position ?? [0, 0]
                                  )
                                : {
                                      x: 0,
                                      y: 0,
                                  },
                            s: textboxFromProps(
                                this.values,
                                s,
                                () => this.overrideColor,
                                this.id
                            ),
                        });
                    }
                } else {
                    [this.pendingImage, img] = loadImage(
                        "/static/images/Imagex.png"
                    );
                }
                this.mainShape = new DImage(img, s);
                break;
            case "ellipse":
                this.mainShape = new Ellipse(
                    () => this.overrideColor ?? this.values.color ?? "black",
                    2,
                    s
                );
                break;
            case "textbox":
                this.mainShape = textboxFromProps(
                    this.values,
                    s,
                    () => this.overrideColor,
                    this.id
                );
                break;
            case "vector":
                const vprops = this.values.vectorproperties ?? {};
                this.mainShape = new Vector(
                    () =>
                        this.overrideColor ??
                        vprops.color ??
                        this.values.color ??
                        "black",
                    2,
                    s,
                    vprops.arrowheadwidth,
                    vprops.arrowheadlength
                );
                break;
            default:
                // rectangle
                this.mainShape = new Rectangle(
                    () => this.overrideColor ?? this.values.color ?? "black",
                    "transparent",
                    this.values.borderWidth ?? 2,
                    0,
                    s
                );
                break;
        }
    }

    isPointInside(p: IPoint, offset?: number) {
        // TODO check child shapes too
        return this.mainShape.isNormalizedPointInsideShape(
            this.normalizePoint(p),
            offset
        );
    }

    normalizePoint(p: IPoint) {
        const {x, y} = this.center;
        const xdiff = p.x - x;
        const ydiff = p.y - y;
        return this.getRotatedPoint(xdiff, ydiff, -this.a);
    }

    protected getRotatedPoint(x: number, y: number, angle: number) {
        const sina = Math.sin(angle * toRadians);
        const cosa = Math.cos(angle * toRadians);
        const rx = cosa * x - sina * y;
        const ry = cosa * y + sina * x;
        return point(rx, ry);
    }

    get id(): string {
        return this.values.id ?? this.did;
    }

    get mainShapeOffset() {
        return {x: 0, y: 0};
    }

    draw() {
        this.initialTransform();
        const {x, y} = this.mainShapeOffset;
        this.ctx.translate(x, y);
        this.ctx.save();
        this.mainShape.draw(this.ctx);
        this.ctx.restore();
        this.ctx.translate(
            -this.mainShape.size.width / 2,
            -this.mainShape.size.height / 2
        );
        for (const s of this.childShapes) {
            this.ctx.save();
            this.ctx.translate(
                s.p.x + s.s.size.width / 2,
                s.p.y + s.s.size.height / 2
            );
            s.s.draw(this.ctx);
            this.ctx.restore();
        }
    }

    protected initialTransform() {
        setIdentityTransform(this.ctx);
        this.ctx.translate(this.x, this.y);
        this.ctx.rotate(this.a * toRadians);
    }
}

function setIdentityTransform(ctx: CanvasRenderingContext2D) {
    ctx.setTransform(1, 0, 0, 1, 0, 0);
}

function textboxFromProps(
    values: {textboxproperties?: TextboxPropsT | null; color?: string | null},
    s: ISizedPartial | undefined,
    overrideColorFn: () => string | undefined,
    defaultText: string
) {
    const props = values.textboxproperties ?? {};
    return new Textbox(
        () => overrideColorFn() ?? props.borderColor ?? values.color ?? "black",
        props.textColor ?? "black",
        props.fillColor ?? "white",
        valueOr(props.borderWidth, 2),
        valueOr(props.cornerradius, 2),
        tupleToSizedOrDef(props.size, s),
        props.text ?? defaultText,
        props.font ?? "14px Arial"
    );
}

class DragObject extends ObjBase<
    RequireExcept<DragObjectPropsT, OptionalDragObjectPropNames>
> {
    public name = "dragobject" as const;
    private pin: Pin;

    get lock() {
        return this.values.lock;
    }

    get xlimits() {
        return this.values.xlimits;
    }

    get ylimits() {
        return this.values.ylimits;
    }

    constructor(
        ctx: CanvasRenderingContext2D,
        values: RequireExcept<DragObjectPropsT, OptionalDragObjectPropNames>,
        defId: string
    ) {
        super(ctx, values, defId);
        this.pin = new Pin(
            {
                color: values.pin.color ?? "blue",
                dotRadius: valueOr(values.pin.dotRadius, 3),
                length: valueOr(values.pin.length, 15),
                linewidth: valueOr(values.pin.linewidth, 2),
                position: values.pin.position ?? {},
                visible: valueOr(values.pin.visible, true),
            },
            this.values.type === "vector" ? "west" : "northwest"
        );
    }

    draw() {
        this.initialTransform();
        this.pin.draw(this.ctx);
        super.draw();
    }

    get mainShapeOffset() {
        return this.pin.getOffsetForObject(this.mainShape.size);
    }

    resetPosition() {
        this.x = this.values.position[0];
        this.y = this.values.position[1];
    }
}

class Target extends ObjBase<
    RequireExcept<TargetPropsT, OptionalTargetPropNames>
> {
    public name = "target" as const;
    objectCount = 0;
    maxObjects = 1000;
    state = TargetState.Normal;

    get snapOffset() {
        return tupleToCoords(this.values.snapOffset);
    }

    get snap() {
        return this.values.snap;
    }

    get overrideColor() {
        switch (this.state) {
            case TargetState.Normal:
                return undefined;
            case TargetState.Snap:
                return this.values.snapColor;
            case TargetState.Drop:
                return this.values.dropColor;
        }
    }

    constructor(
        ctx: CanvasRenderingContext2D,
        values: RequireExcept<TargetPropsT, OptionalTargetPropNames>,
        defId: string
    ) {
        super(ctx, values, defId);
    }
}

class FixedObject extends ObjBase<
    RequireExcept<FixedObjectPropsT, OptionalFixedObjPropNames>
> {
    public name = "fixedobject" as const;

    constructor(
        ctx: CanvasRenderingContext2D,
        values: RequireExcept<FixedObjectPropsT, OptionalFixedObjPropNames>,
        defId: string
    ) {
        super(ctx, values, defId);
    }

    get mainShapeOffset() {
        const s = this.mainShape.size;
        return {x: s.width / 2, y: s.height / 2}; // for FixedObject, position is top-left corner
    }
}

function isSized(s: ISizedPartial): s is ISized {
    return s.height != null;
}

abstract class Shape {
    protected constructor(protected explicitSize: ISizedPartial | undefined) {}

    /**
     * Size of the object if no size has been explicitly set.
     */
    get preferredSize() {
        return {width: 10, height: 10};
    }

    get size(): ISized {
        if (this.explicitSize) {
            if (isSized(this.explicitSize)) {
                return this.explicitSize;
            } else {
                const p = this.preferredSize;
                return {
                    width: this.explicitSize.width,
                    height: (this.explicitSize.width * p.height) / p.width,
                };
            }
        } else {
            return this.preferredSize;
        }
    }

    /**
     * Checks if a normalized point is inside this shape.
     * Default implementation assumes a rectangular shape.
     * @param p point to check
     * @param offset how near the shape the point should be to be considered inside the shape. Default 0.
     */
    isNormalizedPointInsideShape(p: IPoint, offset?: number) {
        const s = this.size;
        const r1 = s.width + (offset ?? 0);
        const r2 = s.height + (offset ?? 0);
        return (
            p.x >= -r1 / 2 && p.x <= r1 / 2 && p.y >= -r2 / 2 && p.y <= r2 / 2
        );
    }

    abstract draw(ctx: CanvasRenderingContext2D): void;
}

class Line {
    constructor(
        private readonly color: string,
        private readonly lineWidth: number,
        private readonly beg: IPoint,
        private readonly end: IPoint
    ) {}

    draw(ctx: CanvasRenderingContext2D) {
        ctx.beginPath();
        ctx.moveTo(this.beg.x, this.beg.y);
        ctx.lineTo(this.end.x, this.end.y);
        ctx.lineWidth = this.lineWidth;
        ctx.strokeStyle = this.color;
        ctx.stroke();
    }
}

class Ellipse extends Shape {
    constructor(
        private readonly color: () => string,
        private readonly lineWidth: number,
        size: ISizedPartial | undefined
    ) {
        super(size);
    }

    draw(ctx: CanvasRenderingContext2D) {
        const {width, height} = this.size;
        ctx.strokeStyle = this.color();
        ctx.lineWidth = this.lineWidth;
        ctx.save();
        ctx.beginPath();
        ctx.scale(width / 2, height / 2);
        ctx.arc(0, 0, 1, 0, 2 * Math.PI);

        // Restore before stroke is intentional; otherwise the ellipse will look filled.
        ctx.restore();
        ctx.stroke();
    }

    isNormalizedPointInsideShape(p: IPoint, offset?: number) {
        const {width, height} = this.size;
        const r1 = width + (offset ?? 0);
        const r2 = height + (offset ?? 0);
        return (
            Math.pow(p.x, 2) / Math.pow(r1 / 2, 2) +
                Math.pow(p.y, 2) / Math.pow(r2 / 2, 2) <=
            1
        );
    }
}

class Rectangle extends Shape {
    constructor(
        protected readonly color: () => string,
        protected readonly fillColor: string,
        protected readonly lineWidth: number,
        protected readonly cornerRadius: number,
        size: ISizedPartial | undefined
    ) {
        super(size);
        const {width, height} = this.size;
        if (this.cornerRadius > width / 2 || this.cornerRadius > height / 2) {
            if (this.cornerRadius > width / 2) {
                this.cornerRadius = width / 2;
            }
            if (this.cornerRadius > height / 2) {
                this.cornerRadius = height / 2;
            }
        }
    }

    draw(ctx: CanvasRenderingContext2D) {
        const {width, height} = this.size;
        ctx.strokeStyle = this.color();
        ctx.fillStyle = this.fillColor;
        ctx.lineWidth = this.lineWidth;
        ctx.beginPath();
        ctx.moveTo(-width / 2 + this.cornerRadius, -height / 2);
        ctx.lineTo(width / 2 - this.cornerRadius, -height / 2);
        ctx.arc(
            width / 2 - this.cornerRadius,
            -height / 2 + this.cornerRadius,
            this.cornerRadius,
            1.5 * Math.PI,
            0
        );
        ctx.lineTo(width / 2, height / 2 - this.cornerRadius);
        ctx.arc(
            width / 2 - this.cornerRadius,
            height / 2 - this.cornerRadius,
            this.cornerRadius,
            0,
            0.5 * Math.PI
        );
        ctx.lineTo(-width / 2 + this.cornerRadius, height / 2);
        ctx.arc(
            -width / 2 + this.cornerRadius,
            height / 2 - this.cornerRadius,
            this.cornerRadius,
            0.5 * Math.PI,
            Math.PI
        );
        ctx.lineTo(-width / 2, -height / 2 + this.cornerRadius);
        ctx.arc(
            -width / 2 + this.cornerRadius,
            -height / 2 + this.cornerRadius,
            this.cornerRadius,
            Math.PI,
            1.5 * Math.PI
        );
        ctx.closePath();
        ctx.fill();
        ctx.stroke();
    }
}

const auxctx = document.createElement("canvas").getContext("2d")!;

class Textbox extends Shape {
    private lines: string[];
    private textwidth: number;
    private textHeight: number;
    private rect: Rectangle;
    private leftMargin = 3;
    private rightMargin = 3;
    private topMargin = 3;
    private bottomMargin = 3;

    constructor(
        protected readonly borderColor: () => string,
        protected readonly textColor: string,
        protected readonly fillColor: string,
        protected readonly lineWidth: number,
        protected readonly cornerRadius: number,
        size: ISizedPartial | undefined,
        text: string,
        protected readonly font: string
    ) {
        super(size);
        this.lines = text.split("\n");
        auxctx.font = this.font;
        const lineWidths = this.lines.map(
            (line) => auxctx.measureText(line).width
        );
        this.textwidth = Math.max(...lineWidths);
        this.textHeight = parseInt(auxctx.font, 10);

        const {width, height} = this.size;
        if (this.cornerRadius > width / 2 || this.cornerRadius > height / 2) {
            if (this.cornerRadius > width / 2) {
                this.cornerRadius = width / 2;
            }
            if (this.cornerRadius > height / 2) {
                this.cornerRadius = height / 2;
            }
        }
        this.rect = new Rectangle(
            borderColor,
            fillColor,
            lineWidth,
            cornerRadius,
            this.size
        );
    }

    get preferredSize() {
        return {
            width: this.textwidth + this.leftMargin + this.rightMargin,
            height:
                this.textHeight * this.lines.length +
                this.topMargin +
                this.bottomMargin,
        };
    }

    draw(ctx: CanvasRenderingContext2D) {
        this.rect.draw(ctx);
        let textStart = this.topMargin;
        ctx.translate(-this.size.width / 2, -this.size.height / 2);
        ctx.font = this.font;
        ctx.textBaseline = "top";
        ctx.fillStyle = this.textColor;
        for (const line of this.lines) {
            ctx.fillText(line, this.leftMargin, textStart);
            textStart += this.textHeight;
        }
    }
}

class Vector extends Shape {
    private arrowHeadWidth: number;
    private arrowHeadLength: number;

    constructor(
        private readonly color: () => string,
        private readonly lineWidth: number,
        size: ISizedPartial | undefined,
        arrowHeadWidth: number | undefined,
        arrowHeadLength: number | undefined
    ) {
        super(size);
        this.arrowHeadWidth = valueOr(arrowHeadWidth, this.size.height * 3);
        this.arrowHeadLength = valueOr(arrowHeadLength, this.size.height * 5);
    }

    get preferredSize() {
        return {
            width: 50,
            height: 4,
        };
    }

    draw(ctx: CanvasRenderingContext2D) {
        const {width, height} = this.size;
        ctx.strokeStyle = this.color();
        ctx.fillStyle = ctx.strokeStyle;
        ctx.translate(-width / 2, -height / 2);
        ctx.beginPath();
        ctx.lineTo(0, height);
        ctx.lineTo(width - this.arrowHeadLength, height);
        ctx.lineTo(
            width - this.arrowHeadLength,
            height / 2 + this.arrowHeadWidth / 2
        );
        ctx.lineTo(width, height / 2);
        ctx.lineTo(
            width - this.arrowHeadLength,
            height / 2 - this.arrowHeadWidth / 2
        );
        ctx.lineTo(width - this.arrowHeadLength, 0);
        ctx.lineTo(0, 0);
        ctx.fill();
    }
}

class DImage extends Shape {
    constructor(
        private readonly image: HTMLImageElement,
        size: ISizedPartial | undefined
    ) {
        super(size);
    }

    get preferredSize() {
        return {
            width: this.image.width,
            height: this.image.height,
        };
    }

    draw(ctx: CanvasRenderingContext2D) {
        const {width, height} = this.size;
        ctx.translate(-width / 2, -height / 2);
        ctx.drawImage(this.image, 0, 0, width, height);
    }
}

function isTouchDevice() {
    return typeof window.ontouchstart !== "undefined";
}

function loadImage(src: string): [Promise<ImageLoadResult>, HTMLImageElement] {
    const deferred = new TimDefer<ImageLoadResult>();
    const sprite = new Image();
    sprite.onload = () => {
        deferred.resolve(sprite);
    };
    sprite.onerror = (e) => {
        deferred.resolve(e as Event);
    };
    sprite.src = src;
    return [deferred.promise, sprite];
}

function baseDefs(
    objType: ObjectTypeT,
    color: string
): RequireExcept<DefaultPropsT, OptionalPropNames> {
    return {
        a: 0,
        color,
        dropColor: "cyan",
        pin: {},
        position: [0, 0],
        snap: true,
        snapColor: "cyan",
        snapOffset: [0, 0],
        type: objType,
    };
}

interface IAnswerResponse {
    rightanswers?: RightAnswerT[];
}

@Component({
    selector: "imagex-runner",
    // changeDetection: ChangeDetectionStrategy.OnPush,
    template: `
        <div class="csRunDiv no-popup-menu" [class.cs-has-header]="header">
            <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <div class="pluginError" *ngIf="imageLoadError" [textContent]="imageLoadError"></div>
            <h4 *ngIf="header" [innerHtml]="header"></h4>
            <p *ngIf="stem" class="stem" [innerHtml]="stem"></p>
            <div class="imagex-content" [style.height.px]="canvasheight">
                <canvas class="canvas no-popup-menu"
                        tabindex="1"
                        [width]="canvaswidth"
                        [height]="canvasheight"></canvas>
                <canvas class="canvas no-popup-menu"
                        tabindex="1"
                        [width]="canvaswidth"
                        [height]=canvasheight></canvas>
                <div class="content"></div>
            </div>
            <p class="csRunMenu"><span *ngIf="button" class="save">&nbsp;<button
                                               class="timButton"
                                               [disabled]="(disableUnchanged && !isUnSaved()) || isRunning"
                                               (click)="save()">{{button}}
            </button></span>
                <span  *ngIf="buttonPlay" class="videoPlay">
                &nbsp;&nbsp;
                <button
                        [disabled]="isRunning"
                        (click)="videoPlay()">{{buttonPlay}}
                </button>
                </span>
                <span *ngIf="buttonRevert" class="videoBeginning">
                &nbsp;&nbsp;
                <button
                        [disabled]="isRunning"
                        (click)="videoBeginning()">{{buttonRevert}}
                </button>
                    </span>
                <span [hidden]="!(finalanswer && userHasAnswered)" class="showAnswer">
                &nbsp;&nbsp;
                <button class="timButton"
                        [disabled]="isRunning"
                        (click)="showAnswer()">
                    Show correct answer
                </button>
                    </span>
                <span *ngIf="undoButton && isUnSaved() && undoButton">
                    &nbsp;
                    <a href="" title="{{undoTitle}}"
                        (click)="tryResetChanges($event);">{{undoButton}}</a>
                </span>
                <span *ngIf="button" class="resetExercise">
                    &nbsp;
                    <a (click)="resetExercise()">{{resetText}}</a>
                </span>
                <span *ngIf="muokattu" class="initCode">
                &nbsp;&nbsp;<a
                        href="" (click)="initCode()">{{resetText}}</a>
                    </span>
                &nbsp;&nbsp;
                <draw-toolbar *ngIf="isFreeHandInUse" [drawSettings]="drawSettings" [undo]="passUndo"
                              [drawVisibleOptions]="drawVisibleOptions"></draw-toolbar>
            </p>
            <div class="plgfooter"></div>
            <div [hidden]="!preview"><span><span [ngStyle]="{'background-color': previewColor}"
                                                 style="display: table-cell; text-align: center; width: 30px;"
                                                 (click)="getPColor()">&lt;-</span>
        <input [(ngModel)]="previewColor"
               colorpicker="hex"
               type="text"
               (click)="getPColor()"
               size="10"/> <label> Coord:
            <input [(ngModel)]="coords" (click)="getPColor()" size="10"/></label></span></div>
            <pre class="" *ngIf="error && preview">{{error}}</pre>
            <pre class="" [hidden]="!result">{{result}}</pre>
            <div class="replyHTML" *ngIf="replyHTML"><span [innerHtml]="svgImageSnippet()"></span></div>
            <img *ngIf="replyImage" class="grconsole" [src]="replyImage" alt=""/>
            <p class="plgfooter" *ngIf="footer" [innerHtml]="footer"></p>
        </div>
    `,
    styleUrls: ["./imagex.component.scss"],
})
export class ImageXComponent
    extends AngularPluginBase<
        t.TypeOf<typeof ImageXMarkup>,
        t.TypeOf<typeof ImageXAll>,
        typeof ImageXAll
    >
    implements OnInit, ITimComponent, IVelpableComponent
{
    public imageLoadError?: string | null;

    get emotion() {
        return this.markup.emotion;
    }

    get max_tries() {
        return this.markup.answerLimit;
    }

    get freeHandVisible() {
        const r = this.markup.freeHandVisible;
        if (r != null) {
            return r;
        }
        return this.isFreeHandInUse;
    }

    get freeHandShortCut() {
        const r = this.markup.freeHandShortCut;
        if (r != null) {
            return r;
        }
        return this.isFreeHandInUse;
    }

    get isFreeHandInUse() {
        return this.markup.freeHand === "use" || this.markup.freeHand === true;
    }

    get videoPlayer(): HTMLVideoElement | undefined {
        if (this.markup.followid) {
            return this.vctrl.getVideo(this.markup.followid);
        }
    }

    get buttonPlay() {
        return (
            this.markup.buttonPlay ??
            (this.videoPlayer ? "Aloita/pysäytä" : null)
        );
    }

    get buttonRevert() {
        return (
            this.markup.buttonRevert ??
            (this.videoPlayer ? "Video alkuun" : null)
        );
    }

    get teacherMode() {
        return this.vctrl.teacherMode;
    }

    get grabOffset() {
        return isTouchDevice() ? this.markup.extraGrabAreaHeight : 0;
    }

    // TODO: Choose visible options from markup
    public drawVisibleOptions: IDrawVisibleOptions = {
        enabled: true,
        freeHand: true,
        lineMode: true,
        rectangleMode: true,
        centerEllipseMode: true,
        cornerEllipseMode: true,
        arrowMode: true,
        w: true,
        color: true,
        fill: true,
        opacity: true,
        eraser: true,
    };
    public drawSettings: IDrawOptions = {
        enabled: false,
        drawType: DrawType.Freehand,
        w: 0,
        color: "",
        fill: false,
        opacity: 1,
        eraser: false,
    };
    // public color = "";
    public drawing!: Drawing;
    private previousDrawing: DrawItem[] = [];

    public coords = "";
    public drags: DragObject[] = [];
    private previousDrags: (IPoint & {did: string})[] = [];
    public userHasAnswered = false;

    public muokattu: boolean;
    public result: string;
    public error?: string;
    private cursor: string;
    private tries = 0;
    public previewColor = "";
    public isRunning: boolean = false;

    public replyImage?: string;
    public replyHTML?: string;
    private bgCanvas!: HTMLCanvasElement;
    private frontCanvas!: HTMLCanvasElement;
    private dt!: DragTask;
    private answer?: IAnswerResponse;
    private prevAnswer?: string;
    private embed: boolean = false;
    private init = false;

    draw() {
        this.dt.drawDragTask();
    }

    constructor(
        el: ElementRef,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        private cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer);
        this.muokattu = false;
        this.result = "";
        this.cursor = "\u0383"; // "\u0347"; // "\u02FD";
    }

    async getVelpImages() {
        while (!this.init) {
            await timeout();
        }
        const canvas = document.createElement("canvas");
        canvas.height = this.canvasheight;
        canvas.width = this.canvaswidth;
        const ctx = canvas.getContext("2d")!;
        ctx.drawImage(this.bgCanvas, 0, 0);
        ctx.drawImage(this.frontCanvas, 0, 0);
        return [canvas.toDataURL()];
    }

    getFrontCanvas(): HTMLCanvasElement {
        return this.frontCanvas;
    }

    getBgCanvasCtx() {
        return this.bgCanvas.getContext("2d")!;
    }

    getFrontCanvasCtx() {
        return this.frontCanvas.getContext("2d")!;
    }

    async ngOnInit() {
        super.ngOnInit();

        if (!this.attrsall.preview) {
            this.vctrl.addTimComponent(this);
        }
        // timeout required; otherwise the canvas element will be overwritten with another by Angular
        await timeout();
        this.bgCanvas = this.element.find(".canvas")[0] as HTMLCanvasElement;
        this.frontCanvas = this.element.find(".canvas")[1] as HTMLCanvasElement;
        this.element[0].addEventListener("touchstart", (event) => {
            // event.preventDefault();
        });

        this.element[0].addEventListener("touchmove", (event) => {
            // event.preventDefault();
        });

        this.tries = this.attrsall.info?.earlier_answers ?? 0;
        const drawings: DrawItem[] = [];
        if (this.attrsall.state?.freeHandData) {
            for (const obj of this.attrsall.state.freeHandData) {
                drawings.push({type: "freehand", drawData: obj});
            }
        }
        this.drawing = new Drawing(this.drawSettings, [this.frontCanvas]);
        this.drawing.drawData = this.attrsall.state?.drawings ?? drawings;
        if (this.isFreeHandInUse) {
            this.drawSettings.enabled = true;
        }

        this.drawSettings.w = this.markup.freeHandWidth;
        this.drawSettings.color = this.markup.freeHandColor;
        // this.lineMode = this.markup.freeHandLine;
        if (this.markup.freeHandLine) {
            this.drawSettings.drawType = DrawType.Line;
        }

        const dt = new DragTask(this.bgCanvas, this.frontCanvas, this);
        this.dt = dt;

        const userObjects = this.markup.objects;

        const userTargets = this.markup.targets;
        const userFixedObjects = this.markup.fixedobjects;
        const fixedobjects = [];
        const targets = [];
        const objects = [];
        const ctx = this.getBgCanvasCtx();

        if (this.markup.background) {
            const background = new FixedObject(
                ctx,
                {
                    a: this.markup.background.a ?? 0,
                    imgproperties: {
                        src: this.markup.background.src,
                        textbox: false,
                    },
                    position: this.markup.background.position ?? [0, 0],
                    size: this.markup.background.size,
                    type: "img",
                },
                "background"
            );
            dt.drawObjects.push(background);
        }

        this.error = "";

        // We don't ever want to concatenate arrays when merging.
        const opts: deepmerge.Options = {
            arrayMerge: (destinationArray, sourceArray, options) => sourceArray,
        };

        let fixedDef = deepmerge<
            RequireExcept<FixedObjectPropsT, OptionalFixedObjPropNames>
        >(baseDefs("rectangle", "blue"), this.markup.defaults ?? {}, opts);

        if (userFixedObjects) {
            for (let i = 0; i < userFixedObjects.length; i++) {
                fixedDef = deepmerge(fixedDef, userFixedObjects[i], opts);
                fixedobjects.push(
                    new FixedObject(ctx, fixedDef, "fix" + (i + 1))
                );
            }
        }

        let targetDef = deepmerge<
            RequireExcept<TargetPropsT, OptionalTargetPropNames>
        >(baseDefs("rectangle", "blue"), this.markup.defaults ?? {}, opts);
        if (userTargets) {
            for (let i = 0; i < userTargets.length; i++) {
                targetDef = deepmerge(targetDef, userTargets[i], opts);

                // Don't deepmerge points object because it's undesirable.
                // Currently this doesn't matter either way because points is not used in client side.
                if (userTargets[i].points != null) {
                    targetDef.points = userTargets[i].points;
                } else {
                    delete targetDef.points;
                }
                targets.push(new Target(ctx, targetDef, "trg" + (i + 1)));
            }
        }

        let dragDef = deepmerge<
            RequireExcept<DragObjectPropsT, OptionalDragObjectPropNames>
        >(baseDefs("textbox", "black"), this.markup.defaults ?? {}, opts);
        if (userObjects) {
            for (let i = 0; i < userObjects.length; i++) {
                dragDef = deepmerge(dragDef, userObjects[i], opts);
                const newObject = new DragObject(ctx, dragDef, "obj" + (i + 1));
                objects.push(newObject);
                this.drags.push(newObject);
            }
        }

        if (this.attrsall.state?.userAnswer) {
            this.userHasAnswered = true;
            const userDrags = this.attrsall.state.userAnswer.drags;
            if (userDrags && userDrags.length > 0) {
                for (const o of objects) {
                    const ud = userDrags.find((u) => u.did === o.did);
                    if (ud) {
                        o.x = ud.position[0];
                        o.y = ud.position[1];
                    }
                }
            }
        }

        dt.drawObjects = [
            ...dt.drawObjects,
            ...fixedobjects,
            ...targets,
            ...objects,
        ];
        for (const d of dt.drawObjects) {
            if (d.pendingImage) {
                const r = await d.pendingImage;
                if (r instanceof Event) {
                    this.imageLoadError = `Failed to load image ${
                        (r.target as Element).getAttribute("src") ?? "(no src)"
                    }`;
                    break;
                }
            }
        }
        if (!this.imageLoadError) {
            this.imageLoadError = null;
        }
        dt.drawDragTask();
        this.drawing.redrawAll();

        this.previewColor = globalPreviewColor;
        this.previousDrags = this.drags.map((drag) => ({
            did: drag.did,
            x: drag.x,
            y: drag.y,
        }));
        this.previousDrawing = [...this.drawing.getDrawing()];
        this.prevAnswer = this.getContent();
        this.init = true;
    }

    ngOnDestroy() {
        if (!this.attrsall.preview) {
            this.vctrl.removeTimComponent(this);
        }
    }

    initCode() {
        this.error = "";
        this.result = "";
    }
    passUndo = (e?: Event) => {
        if (e) {
            e.preventDefault();
        }
        this.undo(e);
    };

    undo(e?: Event) {
        if (e) {
            e.preventDefault();
        }
        this.drawing.undo();
        this.dt.drawDragTask();
        this.drawing.redrawAll();
        this.checkChanges();
    }

    resetChanges() {
        this.drawing.setDrawData([...this.previousDrawing]);
        for (const o of this.dt.drawObjects) {
            const prevPos = this.previousDrags.find(
                (prev) => prev.did == o.did
            );
            if (prevPos) {
                o.x = prevPos.x;
                o.y = prevPos.y;
            }
        }
        this.dt.drawDragTask();
        this.drawing.redrawAll();
        this.prevAnswer = this.getContent();
        this.checkChanges();
    }

    checkChanges() {
        this.result = "";
        this.cdr.detectChanges();
        this.updateListeners(
            this.isUnSaved() ? ChangeType.Modified : ChangeType.Saved
        );
    }

    lineMode(): boolean {
        return this.drawSettings.drawType == DrawType.Line;
    }

    getPColor() {
        if (this.attrsall.preview) {
            globalPreviewColor = this.previewColor;
            editorChangeValue(
                ["[a-zA-Z]*[cC]olor[a-zA-Z]*:"],
                `"${globalPreviewColor}"`
            );
        }
    }

    async save() {
        return await this.doSave(false);
    }

    showAnswer() {
        this.doshowAnswer();
    }

    // Get the important stuff from dragobjects
    getDragObjectJson() {
        return this.drags.map((d) => ({
            did: d.did,
            id: d.id,
            position: [d.x, d.y],
        }));
    }

    // This is pretty much identical to the normal save except that a query to
    // show correct answer is also sent.
    async doshowAnswer() {
        if (this.answer?.rightanswers) {
            this.dt.addRightAnswers(this.answer.rightanswers);
            return;
        }

        this.error = "... saving ...";
        this.isRunning = true;
        this.result = "";

        const params = {
            input: {
                drags: this.getDragObjectJson(),
                finalanswerquery: true,
                drawings: this.drawing.getDrawing(),
            },
        };
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to(
            $http<{
                error?: string;
                web: {
                    error?: string;
                    result: string;
                    tries: number;
                    answer: IAnswerResponse;
                };
            }>({method: "PUT", url, data: params, timeout: defaultTimeout})
        );
        this.isRunning = false;
        if (r.ok) {
            const data = r.result.data;
            if (data.error?.includes("exceeded")) {
                data.error = "";
            }
            this.error = data.web.error;
            this.result = data.web.result;
            this.tries = data.web.tries;
            // for showing right answers.
            this.answer = data.web.answer;
            const ra = this.answer.rightanswers;
            if (ra) {
                this.dt.addRightAnswers(ra);
            }
        } else {
            this.error = "Ikuinen silmukka tai jokin muu vika?";
        }
    }

    // Resets the positions of dragobjects.
    resetExercise() {
        this.error = "";
        this.drawing.resetDrawing();

        for (const obj of this.drags) {
            obj.resetPosition();
        }

        this.dt.lines = undefined;

        // Draw the exercise so that reset appears instantly.
        this.dt.drawDragTask();
        this.drawing.redrawAll();
        this.checkChanges();
    }

    svgImageSnippet() {
        return $sce.trustAsHtml(this.replyHTML);
    }

    videoPlay() {
        const video = this.videoPlayer;
        if (!video) {
            return;
        }
        if (video.paused) {
            video.play();
        } else {
            video.pause();
        }
    }

    videoBeginning() {
        const video = this.videoPlayer;
        if (!video) {
            return;
        }
        if (video.paused) {
            video.currentTime = 0;
        }
    }

    async doSave(nosave: boolean) {
        this.error = "... saving ...";
        this.isRunning = true;
        this.result = "";

        const params = {
            input: {
                drags: this.getDragObjectJson(),
                drawings: this.drawing.getDrawing(),
                nosave: false,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const url = this.pluginMeta.getAnswerUrl();

        const r = await to(
            $http<{
                web: {
                    error?: string;
                    result: string;
                    tries: number;
                    "-replyImage": string;
                    "-replyHTML": string;
                };
            }>({method: "PUT", url, data: params, timeout: defaultTimeout})
        );
        this.isRunning = false;
        if (r.ok) {
            const data = r.result.data;
            this.error = data.web.error;
            this.result = data.web.result;
            this.tries = data.web.tries;
            this.userHasAnswered = true;
            this.replyImage = data.web["-replyImage"];
            this.replyHTML = data.web["-replyHTML"];
            this.previousDrags = this.drags.map((drag) => ({
                did: drag.did,
                x: drag.x,
                y: drag.y,
            }));
            this.previousDrawing = [...this.drawing.getDrawing()];
            this.prevAnswer = this.getContent();
            this.updateListeners(
                this.isUnSaved() ? ChangeType.Modified : ChangeType.Saved
            );
            this.cdr.detectChanges();
            return {saved: true, message: undefined};
        } else {
            this.error = "Ikuinen silmukka tai jokin muu vika?";
            this.cdr.detectChanges();
            return {saved: false, message: this.error};
        }
    }

    getDefaultMarkup() {
        return {};
    }

    getAttributeType() {
        return ImageXAll;
    }

    get canvaswidth(): number {
        return this.markup.canvaswidth;
    }

    get canvasheight(): number {
        return this.markup.canvasheight;
    }

    get preview() {
        return this.attrsall.preview;
    }

    get button() {
        return this.markup.button ?? "Save";
    }

    get resetText() {
        return this.markup.resetText ?? "Reset";
    }

    get freeHandLineVisible() {
        return this.markup.freeHandLineVisible;
    }

    get freeHandToolbar() {
        return this.markup.freeHandToolbar;
    }

    get finalanswer() {
        // must have finalanswer-attribute
        // if limit exeeded or in no limit case at least one answer
        return (
            this.markup.finalanswer &&
            ((this.max_tries && this.tries >= this.max_tries) ||
                (!this.max_tries && this.tries > 0))
        );
    }

    getContent(): string | undefined {
        return JSON.stringify({
            drags: this.getDragObjectJson(),
            drawings: this.drawing.getDrawing(),
        });
    }

    isUnSaved(): boolean {
        if (!this.init) {
            return false;
        }
        return this.getContent() !== this.prevAnswer;
    }

    get markup(): Readonly<t.TypeOf<typeof ImageXMarkup>> {
        return {...super.markup, answerBrowser: {showValidOnly: false}};
    }
}

@NgModule({
    declarations: [ImageXComponent],
    imports: [
        CommonModule,
        DrawToolbarModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
    ],
})
export class ImagexModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
registerPlugin("imagex-runner", ImagexModule, ImageXComponent);
