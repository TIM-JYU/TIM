import type {
    ApplicationRef,
    DoBootstrap,
    OnChanges,
    OnDestroy,
    OnInit,
    SimpleChanges,
} from "@angular/core";
import {
    Component,
    ElementRef,
    Input,
    NgModule,
    QueryList,
    ViewChild,
    ViewChildren,
} from "@angular/core";
import type {SafeResourceUrl} from "@angular/platform-browser";
import {BrowserModule, DomSanitizer} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";
import type {IUnsavedComponent} from "tim/document/viewctrl";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {
    IDrawOptions,
    IFillAndWidth,
} from "tim/plugin/draw-canvas/draw-toolbar.component";
import {
    DrawToolbarModule,
    DrawType,
} from "tim/plugin/draw-canvas/draw-toolbar.component";
import type {
    ILine,
    ILineSegment,
    IPoint,
    IRectangleOrEllipse,
    TuplePoint,
} from "tim/plugin/imagex/imagextypes";
import type {MouseOrTouch} from "tim/util/utils";
import {
    isTouchEvent,
    numOrStringToNumber,
    posToRelative,
    touchEventToTouch,
} from "tim/util/utils";

export type IRectangle = {
    type: "rectangle";
    drawData: IRectangleOrEllipse;
};

export type IEllipse = {
    type: "ellipse";
    drawData: IRectangleOrEllipse;
};

export type IFreeHand = {
    type: "freehand";
    drawData: ILineSegment;
};

export type IArrow = {
    type: "arrow";
    drawData: ILine;
};

// TODO: a variable for reporting MouseDown, TouchMove etc could be useful for fine-tuning the desired
//  update behaviour - for now this is called on certain MouseUp/TouchEnd events, and the behaviour
//  is not optimal for mobile (e.g contained objects register clicks when user just wanted to scroll the page)
export interface IDrawUpdate {
    x?: number;
    y?: number;
    drawingUpdated: boolean;
    scaleChange?: number;
    deleted?: boolean;
}

export type DrawItem = IRectangle | IEllipse | IFreeHand | IArrow;

export interface IDrawingWithID {
    id: number;
    drawData: DrawItem[];
}

const defaultDrawOptions: IDrawOptions = {
    enabled: true,
    drawType: DrawType.Freehand,
    color: "red",
    w: 3,
    opacity: 1,
    fill: false,
    eraser: false,
};

/**
 * Gets dimensions (start coordinates, width, height) from given drawing
 * @param drawing DrawItem[] to check
 * @param minSize if width/height is less than this, then add extra padding (at both ends)
 * @param adjustByEraser if false, include erased parts in the dimensions
 */
export function getDrawingDimensions(
    drawing: DrawItem[],
    minSize = 0,
    adjustByEraser = true
): {x: number; y: number; w: number; h: number} {
    let x = Number.MAX_SAFE_INTEGER;
    let y = Number.MAX_SAFE_INTEGER;
    let w = 0;
    let h = 0;
    let erasers = false;
    for (const obj of drawing) {
        if (obj.drawData.eraser && adjustByEraser) {
            erasers = true;
        }
        if (obj.type == "freehand") {
            for (const line of obj.drawData.lines) {
                const width = obj.drawData.w
                    ? numOrStringToNumber(obj.drawData.w) / 2
                    : 0;
                if (x == null || x > line[0] - width) {
                    x = line[0] - width;
                }
                if (y == null || y > line[1] - width) {
                    y = line[1] - width;
                }
                if (w < line[0] + width) {
                    w = line[0] + width;
                }
                if (h < line[1] + width) {
                    h = line[1] + width;
                }
            }
        } else if (obj.type == "rectangle" || obj.type == "ellipse") {
            const shape = obj.drawData;
            let width = 0;
            if (shape.fillColor == undefined && shape.lineWidth != undefined) {
                width = shape.lineWidth / 2;
            }
            if (x > shape.x) {
                x = shape.x - width;
            }
            if (y > shape.y) {
                y = shape.y - width;
            }
            if (w < shape.x + shape.w) {
                w = shape.x + shape.w + width;
            }
            if (h < shape.y + shape.h) {
                h = shape.y + shape.h + width;
            }
        } else if (obj.type == "arrow") {
            const shape = obj.drawData;
            const width = shape.w / 2;
            const sides = getArrowPoints(shape);
            const minX = Math.min(
                shape.start.x,
                shape.end.x,
                sides.right.x,
                sides.left.x
            );
            const minY = Math.min(
                shape.start.y,
                shape.end.y,
                sides.right.y,
                sides.left.y
            );
            const maxX = Math.max(
                shape.start.x,
                shape.end.x,
                sides.right.x,
                sides.left.x
            );
            const maxY = Math.max(
                shape.start.y,
                shape.end.y,
                sides.right.y,
                sides.left.y
            );
            if (x > minX) {
                x = minX - width;
            }
            if (y > minY) {
                y = minY - width;
            }
            if (h < maxY) {
                h = maxY + width;
            }
            if (w < maxX) {
                w = maxX + width;
            }
        }
    }
    w = w - x;
    h = h - y;
    if (adjustByEraser && erasers) {
        ({x, y, w, h} = getEraserAdjustedDrawingDimensions(drawing, {
            x,
            y,
            w,
            h,
        }));
    }
    if (w < minSize) {
        x = Math.max(x - minSize, 0);
        w = w + 2 * minSize;
    }
    if (h < minSize) {
        y = Math.max(y - minSize, 0);
        h = h + 2 * minSize;
    }
    return {x: x, y: y, w: w, h: h};
}

/**
 * Reduce dimensions (start coordinates, width, height) from given drawing by skipping erased pixels
 * @param drawing DrawItem[] to check
 * @param originalDims original dimensions to reduce
 */
export function getEraserAdjustedDrawingDimensions(
    drawing: DrawItem[],
    originalDims: {x: number; y: number; w: number; h: number}
) {
    const dims = {
        x: Math.floor(originalDims.x),
        y: Math.floor(originalDims.y),
        w: Math.ceil(originalDims.w),
        h: Math.ceil(originalDims.h),
    };
    const canvas = document.createElement("canvas");
    canvas.height = dims.h;
    canvas.width = dims.w;
    const ctx = canvas.getContext("2d")!;
    ctx.lineCap = "round";
    ctx.translate(-dims.x, -dims.y);
    drawFromArray(drawing, defaultDrawOptions, ctx);

    const output = ctx.getImageData(0, 0, dims.w, dims.h);
    const data = output.data;
    const getOpacity = function (x: number, y: number) {
        const offset = dims.w * y + x;
        return data[offset * 4 + 3];
    };
    const sides = {
        left: 0,
        right: dims.w,
        top: 0,
        bottom: dims.h,
    };
    let found = false;
    for (let i = 0; i < dims.w; i++) {
        for (let j = 0; j < dims.h; j++) {
            if (getOpacity(i, j) > 0) {
                sides.left = i;
                found = true;
                break;
            }
        }
        if (found) {
            break;
        }
    }
    found = false;
    for (let i = 0; i < dims.h; i++) {
        for (let j = 0; j < dims.w; j++) {
            if (getOpacity(j, i) > 0) {
                sides.top = i;
                found = true;
                break;
            }
        }
        if (found) {
            break;
        }
    }
    found = false;
    // -1 is intentional
    for (let i = dims.w - 1; i >= 0; i--) {
        for (let j = dims.h; j >= 0; j--) {
            if (getOpacity(i, j) > 0) {
                sides.right = i;
                found = true;
                break;
            }
        }
        if (found) {
            break;
        }
    }
    found = false;
    for (let i = dims.h; i >= 0; i--) {
        for (let j = dims.w; j >= 0; j--) {
            if (getOpacity(j, i) > 0) {
                sides.bottom = i;
                found = true;
                break;
            }
        }
        if (found) {
            break;
        }
    }
    return {
        x: dims.x + sides.left,
        y: dims.y + sides.top,
        w: sides.right - sides.left,
        h: sides.bottom - sides.top,
    };
}

/**
 * Checks if a coordinate exists within given drawing
 * @param drawing DrawItem[] to check
 * @param x start coordinate
 * @param y start coordinate
 * @param minSize if width/height is less than this, then add extra padding (at both ends)
 */
export function isCoordWithinDrawing(
    drawing: DrawItem[],
    x: number,
    y: number,
    minSize = 0
): boolean {
    const dimensions = getDrawingDimensions(drawing);
    const horizontalPadding = dimensions.w < minSize ? minSize : 0;
    const verticalPadding = dimensions.h < minSize ? minSize : 0;
    return (
        x > dimensions.x - horizontalPadding &&
        x < dimensions.x + dimensions.w + horizontalPadding &&
        y > dimensions.y - verticalPadding &&
        y < dimensions.y + dimensions.h + verticalPadding
    );
}

/**
 * Prepare canvas options (line widths, colors, opacity) to suit given object
 * @param obj IRectangleOrEllipse object with options
 * @param options default options to use if missing from object
 * @param ctx canvas context to edit
 */
export function setContextSettingsFromObject(
    obj: IRectangleOrEllipse,
    options: IDrawOptions,
    ctx: CanvasRenderingContext2D
) {
    ctx.strokeStyle = obj.color ?? options.color;
    ctx.lineWidth = obj.lineWidth ?? options.w;
    ctx.fillStyle = obj.fillColor ?? "transparent";
    ctx.globalAlpha = obj.eraser ? 1 : obj.opacity ?? options.opacity;
    ctx.globalCompositeOperation = obj.eraser
        ? "destination-out"
        : "source-over";
}

/**
 * Prepare canvas options (line widths, colors, opacity) for a given line
 * @param line ILine line with options
 * @param options default options to use if missing from object
 * @param ctx canvas context to edit
 */
export function setContextSettingsFromLine(
    line: ILine,
    options: IDrawOptions,
    ctx: CanvasRenderingContext2D
) {
    ctx.strokeStyle = line.color ?? options.color;
    ctx.lineWidth = line.w ?? options.w;
    ctx.globalAlpha = line.eraser ? 1 : line.opacity ?? options.opacity;
    ctx.globalCompositeOperation = line.eraser
        ? "destination-out"
        : "source-over";
}

/**
 * Draw freehand data on canvas
 * @param ctx canvas context to draw on
 * @param dr array of ILineSegment to draw
 */
export function drawFreeHand(
    ctx: CanvasRenderingContext2D,
    dr: ILineSegment[]
): void {
    ctx.lineJoin = "round";
    for (const seg of dr) {
        if (seg.lines.length < 2) {
            continue;
        }
        ctx.beginPath();
        applyStyleAndWidth(ctx, seg);
        ctx.moveTo(seg.lines[0][0], seg.lines[0][1]);
        for (let lni = 1; lni < seg.lines.length; lni++) {
            ctx.lineTo(seg.lines[lni][0], seg.lines[lni][1]);
        }
        ctx.stroke();
    }
}

/**
 * Calculate arrow head points
 * @param line arrow data to inspect
 */
export function getArrowPoints(line: ILine): {
    right: {x: number; y: number};
    left: {x: number; y: number};
} {
    const baseLength = 10; // minimum arrow side length
    const widthMultiplier = 2; // increase arrow side length for wider arrows

    const headlen = baseLength + line.w * widthMultiplier;
    const angle = Math.atan2(
        line.end.y - line.start.y,
        line.end.x - line.start.x
    );
    const arrowAngle = 5; // increase to make arrow more narrow
    return {
        right: {
            x: line.end.x - headlen * Math.cos(angle - Math.PI / arrowAngle),
            y: line.end.y - headlen * Math.sin(angle - Math.PI / arrowAngle),
        },
        left: {
            x: line.end.x - headlen * Math.cos(angle + Math.PI / arrowAngle),
            y: line.end.y - headlen * Math.sin(angle + Math.PI / arrowAngle),
        },
    };
}

/**
 * Draw an arrow on canvas
 * @param line arrow data in ILine format
 * @param ctx canvas context to draw on
 */
export function drawArrow(line: ILine, ctx: CanvasRenderingContext2D) {
    ctx.lineJoin = "round";
    ctx.beginPath();
    ctx.moveTo(line.start.x, line.start.y);
    ctx.lineTo(line.end.x, line.end.y);
    const sides = getArrowPoints(line);
    ctx.lineTo(sides.right.x, sides.right.y);
    ctx.moveTo(line.end.x, line.end.y);
    ctx.lineTo(sides.left.x, sides.left.y);
    ctx.stroke();
}

/**
 * Draws given input on canvas
 * @param data array of freehand drawings, ellipses and rectangles
 * @param options default options to use if missing from object
 * @param ctx canvas context to edit
 */
export function drawFromArray(
    data: DrawItem[],
    options: IDrawOptions,
    ctx: CanvasRenderingContext2D
) {
    for (const object of data) {
        if (object.type == "ellipse") {
            setContextSettingsFromObject(object.drawData, options, ctx);
            drawEllipse(object.drawData, ctx);
        } else if (object.type == "rectangle") {
            setContextSettingsFromObject(object.drawData, options, ctx);
            drawRectangle(object.drawData, ctx);
        } else if (object.type == "arrow") {
            setContextSettingsFromLine(object.drawData, options, ctx);
            drawArrow(object.drawData, ctx);
        } else {
            drawFreeHand(ctx, [object.drawData]);
        }
    }
}

/**
 * Draws a rectangle
 * @param rectangle in IRectangleOrEllipse format
 * @param ctx CanvasRenderingContext2D to draw on
 */
export function drawRectangle(
    rectangle: IRectangleOrEllipse,
    ctx: CanvasRenderingContext2D
) {
    // TODO: Draw border with own settings but custom fill color
    ctx.lineJoin = "miter";
    const h = Math.max(rectangle.h, 1);
    const w = Math.max(rectangle.w, 1);
    if (rectangle.fillColor) {
        ctx.fillRect(rectangle.x, rectangle.y, w, h);
    } else {
        ctx.strokeRect(rectangle.x, rectangle.y, w, h);
    }
}

/**
 * Draws an ellipse
 * @param ellipse in IRectangleOrEllipse format
 * @param ctx CanvasRenderingContext2D to draw on
 */
export function drawEllipse(
    ellipse: IRectangleOrEllipse,
    ctx: CanvasRenderingContext2D
) {
    const h = Math.max(ellipse.h, 1);
    const w = Math.max(ellipse.w, 1);
    const ratio = w / h;
    ctx.save();
    ctx.beginPath();
    ctx.scale(ratio, 1);
    const r = Math.min(w / ratio, h) / 2;
    ctx.arc((ellipse.x + w / 2) / ratio, ellipse.y + h / 2, r, 0, 2 * Math.PI);
    ctx.restore();
    if (ellipse.fillColor) {
        ctx.fill();
    } else {
        ctx.stroke();
    }
}

/**
 * Sets canvas options (line widths, colors, opacity) based on given settings
 * @param options settings to use
 * @param ctx canvas context to edit
 */
export function setContextSettingsFromOptions(
    options: IDrawOptions,
    ctx: CanvasRenderingContext2D
) {
    ctx.globalAlpha = options.eraser ? 1 : options.opacity;
    ctx.strokeStyle = options.color;
    ctx.lineWidth = options.w;
    ctx.fillStyle = options.color;
    ctx.globalCompositeOperation = options.eraser
        ? "destination-out"
        : "source-over";
}

function applyStyleAndWidth(ctx: CanvasRenderingContext2D, seg: ILineSegment) {
    ctx.strokeStyle = seg.color ?? ctx.strokeStyle;
    ctx.lineWidth = numOrStringToNumber(seg.w ?? ctx.lineWidth);
    ctx.globalAlpha = seg.eraser ? 1 : seg.opacity ?? 1;
    ctx.globalCompositeOperation = seg.eraser
        ? "destination-out"
        : "source-over";
}

interface IImageSizes {
    height: number;
    width: number;
}

interface IOffsetContext {
    ctx: CanvasRenderingContext2D;
    width: number;
    height: number;
    yOffset: number;
}

/**
 * Draw basic shapes on canvas via manually called movement events
 */
export class Drawing {
    drawOptions: IDrawOptions;
    // original canvas elements
    canvases: HTMLCanvasElement[];
    // CanvasRenderingContexts on canvases and their respective offsets
    ctxs: IOffsetContext[] = [];

    /**
     * @param drawOptions settings used when drawing new items
     * @param canvases CanvasElements to draw on
     */
    constructor(drawOptions: IDrawOptions, canvases: HTMLCanvasElement[]) {
        if (canvases.length < 1) {
            console.error("Failed to receive canvases");
        }
        this.drawOptions = drawOptions;
        this.canvases = canvases;
        this.ctxs = this.canvases.map((c) => {
            const context = c.getContext("2d")!;
            // When using multiple, dynamic canvases,
            // then setting lineCap or calling setOffset seems to fail in constructor.
            // In that case init dummy values here and set these externally later
            context.lineCap = "round";
            return {
                ctx: context,
                height: c.height,
                width: c.width,
                yOffset: 0,
            };
        });
        this.activeContexts = this.ctxs;
    }

    // keep track of mousedown while drawing enabled
    drawStarted = false;
    drawMoved = false;
    // drawings that can altered with undo (TODO: Redo, erase...?)
    drawData: DrawItem[] = [];
    // drawings that cannot be altered via undo, e.g background or permanent drawings
    persistentDrawData: DrawItem[] = [];

    // freehand drawing that is built while mouse is pressed
    freeDrawing?: ILineSegment;
    // previous mouse position when drawing freehand
    private prevPos?: TuplePoint;

    // click start position
    private startX: number = 0;
    private startY: number = 0;

    // click end position
    private endX: number = 0;
    private endY: number = 0;

    // previous move event coordinate
    private prevStepY: number = 0;

    // y-coordinate limits for move event from previous preview shape
    private prevMinY: number = 0;
    private prevMaxY: number = 0;

    // dimension used when drawing shapes
    private itemX: number = 0;
    private itemY: number = 0;
    private itemW: number = 0;
    private itemH: number = 0;

    // which contexts need to be re-drawn on draw events
    private activeContexts: IOffsetContext[] = [];

    // y-coordinate limits for current move event
    private minY: number = 0;
    private maxY: number = 0;

    /**
     * Move CanvasRenderingContext by offset
     * @param index Canvas / ctx position in this.canvases / this.ctxs
     * @param dimensions visible dimensions of CanvasRenderingContexts
     * @param offset amount of pixels to move down
     */
    setOffSet(
        index: number,
        dimensions: {w: number; h: number},
        offset: number
    ) {
        const context = this.ctxs[index];
        if (!context) {
            return;
        }
        context.ctx.lineCap = "round";
        context.ctx.translate(0, offset);
        context.yOffset = offset;
        context.width = dimensions.w;
        context.height = dimensions.h;
    }

    /**
     * Begin drawing sequence
     * @param coords starting location
     */
    public downEvent(coords: {x: number; y: number}) {
        if (this.drawOptions.enabled) {
            this.drawStarted = true;
            this.drawMoved = false;
            this.startX = coords.x;
            this.startY = coords.y;
            this.minY = coords.y;
            this.maxY = coords.y;
            this.startSegmentDraw(coords);
        }
    }

    /**
     * Update maximum and minimum y according to current and previous move event
     * Draw preview object based on mouse movement, min/max y-coordinate and current drawing settings
     * @param coords current location
     */
    public moveEvent(coords: {x: number; y: number}) {
        // TODO: Drawings with calculations (ellipse/arrow) could be pre-calculated here (or in preview draw function)
        //  for optimization
        if (!this.drawStarted) {
            return;
        }
        this.drawMoved = true;
        const {x, y} = coords;
        const w = this.drawOptions.w;
        // TODO: Adding more DrawTypes later could be easier with classes
        if (
            this.drawOptions.drawType == DrawType.Rectangle ||
            this.drawOptions.drawType == DrawType.CornerEllipse
        ) {
            this.minY = Math.min(this.startY, y, this.prevStepY) - w;
            this.maxY = Math.max(this.startY, y, this.prevStepY) + w;
            this.setActiveContexts();
            this.redrawAll();
            this.setContextSettingsFromOptions();
            this.itemX = Math.min(x, this.startX);
            this.itemY = Math.min(y, this.startY);
            this.itemW = Math.abs(x - this.startX);
            this.itemH = Math.abs(y - this.startY);
            if (this.drawOptions.drawType == DrawType.CornerEllipse) {
                this.drawPreviewEllipse();
            } else {
                this.drawPreviewRectangle();
            }
        } else if (this.drawOptions.drawType == DrawType.CenterEllipse) {
            const oppositeX = 2 * this.startX - x;
            const oppositeY = 2 * this.startY - y;
            this.itemX = Math.min(x, oppositeX);
            this.itemY = Math.min(y, oppositeY);
            this.itemW = Math.abs(x - oppositeX);
            this.itemH = Math.abs(y - oppositeY);
            this.minY = Math.min(oppositeY, this.prevMinY) - w;
            this.maxY = Math.max(oppositeY, this.prevMaxY) + w;
            this.prevMinY = this.itemY;
            this.prevMaxY = this.itemY + this.itemH;
            this.setActiveContexts();
            this.redrawAll();
            this.setContextSettingsFromOptions();
            this.drawPreviewEllipse();
        } else if (this.drawOptions.drawType == DrawType.Arrow) {
            const points = getArrowPoints({
                start: {x: this.startX, y: this.startY},
                end: coords,
                w: this.drawOptions.w,
            });
            this.minY =
                Math.min(
                    this.startY,
                    y,
                    this.prevStepY,
                    points.left.y,
                    points.right.y
                ) - w;
            this.maxY =
                Math.max(
                    this.startY,
                    y,
                    this.prevStepY,
                    points.left.y,
                    points.right.y
                ) + w;
            this.setActiveContexts();
            this.redrawAll();
            this.setContextSettingsFromOptions();
            this.drawPreviewArrow({
                start: {x: this.startX, y: this.startY},
                end: coords,
                w: this.drawOptions.w,
            });
        } else if (this.prevPos) {
            this.minY = Math.min(this.startY, this.minY, coords.y) - w;
            this.maxY = Math.max(this.startY, this.maxY, coords.y) + w;
            this.setActiveContexts();
            if (this.drawOptions.drawType == DrawType.Line) {
                this.popPoint(1);
            }
            this.setContextSettingsFromOptions();
            this.addPoint(coords);
            // TODO: If we're not in linemode, then just adding a new piece of freehand line to coord might suffice
            this.redrawAll();
        }
        this.prevStepY = coords.y;
    }

    /**
     * Finish drawing sequence and store the drawn object
     * @param coords end location
     */
    public upEvent(coords: {x: number; y: number}) {
        this.endX = coords.x;
        this.endY = coords.y;
        if (this.drawStarted) {
            this.drawStarted = false;
        }
        if (this.drawMoved) {
            if (
                this.drawOptions.drawType == DrawType.CenterEllipse ||
                this.drawOptions.drawType == DrawType.CornerEllipse
            ) {
                const ellipse: IEllipse = {
                    type: "ellipse",
                    drawData: this.makeFullRectangleOrEllipse(),
                };
                this.drawData.push(ellipse);
            } else if (this.drawOptions.drawType == DrawType.Rectangle) {
                const rect: IRectangle = {
                    type: "rectangle",
                    drawData: this.makeFullRectangleOrEllipse(),
                };
                this.drawData.push(rect);
            } else if (this.drawOptions.drawType == DrawType.Arrow) {
                const arrow: IArrow = {
                    type: "arrow",
                    drawData: this.makeLine(),
                };
                this.drawData.push(arrow);
            } else if (this.freeDrawing) {
                const freeDrawing: IFreeHand = {
                    type: "freehand",
                    drawData: this.freeDrawing,
                };
                this.drawData.push(freeDrawing);
                this.freeDrawing = undefined;
            }
        }
    }

    /**
     * Update CanvasRenderingContext2d settings to match current drawing options
     */
    setContextSettingsFromOptions() {
        this.activeContexts.forEach((c) =>
            setContextSettingsFromOptions(this.drawOptions, c.ctx)
        );
    }

    /**
     * Create ILine based on draw coordinates and current settings
     */
    makeLine(): ILine {
        const eraser = this.drawOptions.eraser ? {eraser: true} : {};
        return {
            start: {x: this.startX, y: this.startY},
            end: {x: this.endX, y: this.endY},
            w: this.drawOptions.w,
            opacity: this.drawOptions.opacity,
            color: this.drawOptions.color,
            ...eraser,
        };
    }

    /**
     * Returns full shape information for ellipse or rectangle based on current settings
     */
    makeFullRectangleOrEllipse(): IRectangleOrEllipse {
        let fillOrBorder = {};
        if (this.drawOptions.fill) {
            fillOrBorder = {fillColor: this.drawOptions.color};
        } else {
            fillOrBorder = {lineWidth: this.drawOptions.w};
        }
        const eraser = this.drawOptions.eraser ? {eraser: true} : {};
        return {
            x: this.itemX,
            y: this.itemY,
            w: this.itemW,
            h: this.itemH,
            opacity: this.drawOptions.opacity,
            color: this.drawOptions.color,
            ...fillOrBorder,
            ...eraser,
        };
    }

    /**
     * Start drawing freehand
     */
    startSegmentDraw(pxy: IPoint) {
        const p: TuplePoint = [Math.round(pxy.x), Math.round(pxy.y)];
        const ns: ILineSegment = {lines: [p]};
        ns.color = this.drawOptions.color;
        ns.w = this.drawOptions.w;
        if (this.drawOptions.eraser) {
            ns.eraser = true;
        }
        if (this.drawOptions.opacity < 1) {
            ns.opacity = this.drawOptions.opacity;
        }
        this.freeDrawing = ns;
        this.prevPos = p;
    }

    /**
     * Draw a rectangle during mouse move
     */
    drawPreviewRectangle() {
        const shape = this.makeShape();
        this.activeContexts.forEach((c) => drawRectangle(shape, c.ctx));
    }

    /**
     * Draw an ellipse during mouse move
     */
    drawPreviewEllipse() {
        const shape = this.makeShape();
        this.activeContexts.forEach((c) => drawEllipse(shape, c.ctx));
    }

    /**
     * Draw an arrow during mouse move
     * @param line current start/end coordinates
     */
    drawPreviewArrow(line: ILine) {
        this.activeContexts.forEach((c) => drawArrow(line, c.ctx));
    }

    /**
     * Return dimension for current shape dimensions based on last draw event
     */
    makeShape(): IRectangleOrEllipse {
        return {
            x: this.itemX,
            y: this.itemY,
            w: this.itemW,
            h: this.itemH,
            fillColor: this.drawOptions.fill
                ? this.drawOptions.color
                : undefined,
        };
    }

    /**
     * Clear current non-permanent drawings
     */
    resetDrawing(): void {
        this.drawData = [];
        this.freeDrawing = undefined;
        this.clearCanvas();
    }

    /**
     * Clears the canvases by clearing the visible part of each canvasContext
     */
    clearCanvas(): void {
        this.activeContexts.forEach((ctx) => {
            ctx.ctx.clearRect(0, -ctx.yOffset, ctx.width, ctx.height);
        });
    }

    /**
     * Redraws everything (current drawings, permanent drawings, possible freehand drawing)
     */
    redrawAll(): void {
        this.clearCanvas();
        this.drawFromArray(this.persistentDrawData);
        this.drawFromArray(this.drawData);
        if (this.freeDrawing) {
            this.drawFreeHand([this.freeDrawing]);
        }
    }

    /**
     * Check which canvas contexts are required to update in current draw event
     * (e.g canvases between this.minY and this.maxY)
     */
    setActiveContexts() {
        const ret: IOffsetContext[] = [];
        for (const ctx of this.ctxs) {
            const top = -ctx.yOffset;
            const bottom = top + ctx.height;
            if (top <= this.maxY && bottom >= this.minY) {
                ret.push(ctx);
            } else if (top > this.maxY) {
                break;
            }
        }
        this.activeContexts = ret;
    }

    drawFromArray(data: DrawItem[]) {
        this.activeContexts.forEach((ctx) =>
            drawFromArray(data, this.drawOptions, ctx.ctx)
        );
    }

    drawFreeHand(data: ILineSegment[]) {
        this.activeContexts.forEach((ctx) => drawFreeHand(ctx.ctx, data));
    }

    /**
     * Removes a piece of line from current freehand drawing in progress
     * @param minlen
     */
    popPoint(minlen: number) {
        if (!this.freeDrawing) {
            return;
        }
        if (this.freeDrawing.lines.length > minlen) {
            this.freeDrawing.lines.pop();
        }
    }

    /**
     * Adds and draws a point to current freehand drawing
     * @param pxy
     */
    addPoint(pxy: IPoint) {
        if (!this.freeDrawing) {
            return;
        }

        const p: TuplePoint = [Math.round(pxy.x), Math.round(pxy.y)];
        this.freeDrawing.lines.push(p);
        if (this.drawOptions.drawType != DrawType.Line) {
            this.prevPos = p;
        }
    }

    /**
     * Remove last drawn piece of non-permanent drawings
     */
    undo(): DrawItem | undefined {
        const dimensions = getDrawingDimensions(this.drawData, 0, false);
        this.minY = dimensions.y;
        this.maxY = dimensions.y + dimensions.h;
        this.setActiveContexts();
        const ret = this.drawData.pop();
        this.redrawAll();
        return ret;
    }

    /**
     * Returns current drawing progress in an array
     */
    getDrawing(): DrawItem[] {
        return this.drawData;
    }

    /**
     * Add current non-permanent drawings to permanent drawings
     */
    storeDrawing() {
        this.persistentDrawData = this.persistentDrawData.concat(this.drawData);
        this.drawData = [];
    }

    /**
     * Set permanent drawings
     * @param data drawings to set
     */
    setPersistentDrawData(data: DrawItem[]) {
        this.activeContexts = this.ctxs;
        this.ctxs.forEach((c) => (c.ctx.lineCap = "round"));
        this.persistentDrawData = data;
        this.redrawAll();
    }

    /**
     * Set non-permanent drawings
     * @param data drawings to set
     */
    setDrawData(data: DrawItem[]) {
        this.drawData = data;
    }
}

// Approximate scrollbar size
const SCROLLBAR_APPROX_WIDTH = 17;
const MIN_IMAGE_HEIGHT = 300;
const MIN_IMAGE_WIDTH = 700;
// some devices have hard limit of 4096x4096 canvas, however in practice smaller canvases seem to perform slightly faster
const MAX_CANVAS_AREA = 8000000;

@Component({
    selector: "draw-canvas",
    template: `
        <draw-toolbar *ngIf="toolBar" [(drawSettings)]="drawOptions"
              [undo]="undo" optionsStorage="drawCanvasOptions"></draw-toolbar>
        <div style="position: relative;">
            <div style="position: absolute; top: 50%; left:-5%; display: flex; flex-flow: column; gap: 1em;
                         -ms-transform: translateY(-50%); transform: translateY(-50%); z-index: 4;">
                <div *ngIf="bgSources.length > 1" style="display: flex; flex-flow: column;">
                    <button title="Previous image" i18n-title class="btn btn-primary" (click)="scrollBgImage(false)">
                        &uarr;
                    </button>
                    <button title="Next image" i18n-title class="btn btn-primary" (click)="scrollBgImage(true)">&darr;
                    </button>
                </div>
                <div style="display: flex; flex-flow: column;">
                    <button title="Zoom in" i18n-title class="btn btn-primary" (click)="zoom(0.1)">
                        <i class="glyphicon glyphicon-zoom-in"></i>
                    </button>
                    <!-- TODO icons-->
                    <button title="Reset zoom" i18n-title class="btn btn-primary" (click)="zoom(0)">R
                    </button>
                    <button title="Zoom out" i18n-title class="btn btn-primary" (click)="zoom(-0.1)">
                        <i class="glyphicon glyphicon-zoom-out"></i>
                    </button>
                </div>
            </div>
            <div i18n *ngIf="loading" style="position: absolute; z-index: 1;">Loading images ({{loadedImages}}
                                                                              /{{bgSources.length}})
                <tim-loading></tim-loading>
            </div>
            <div #wrapper style="overflow: auto; position: relative; resize: both;"
                 [style.opacity]="loading ? 0.3 : 1"
                 [style.height.px]="getWrapperHeight(true)">
                <div class="zoomer" style="-webkit-transform-origin: 0 0;" [style.transform]="getZoomLevel()">
                    <div #backGround style="position: absolute; display:flex; flex-direction: column;">
                        <img alt="review image" *ngFor="let item of bgImages; let i = index"
                             style="max-width: none; display: unset;"
                             [src]="bgImages[i]" (load)="onImgLoad($event, i)">
                    </div>
                </div>
                <div #objectContainer class="canvasObjectContainer"
                     style="overflow: visible; position: absolute; height: 0; width: 0;">
                </div>
                <div class="zoomer" #canvasWrapper style="-webkit-transform-origin: 0 0;"
                     [style.transform]="getZoomLevel()">
                    <canvas #drawbases *ngFor="let item of canvasHeights; let i = index" class="drawbase"
                            style="border-left:1px solid #000000; margin:-1px; border-right:1px solid #000000; margin:-1px; position: absolute;">
                    </canvas>
                </div>
            </div>
        </div>
    `,
    styleUrls: ["./draw-canvas.component.scss"],
})
export class DrawCanvasComponent
    implements OnInit, OnChanges, OnDestroy, IUnsavedComponent
{
    @Input() public bgSources: string[] = [];
    bgSourceSizes: IImageSizes[] = []; // dimensions of loaded background images, sorted
    bgOffsets: number[] = []; // top starting coordinates of each background image, sorted
    bgImages: SafeResourceUrl[] = [];
    @ViewChildren("drawbases") canvases!: QueryList<
        ElementRef<HTMLCanvasElement>
    >;
    @ViewChild("canvasWrapper") canvasWrapper!: ElementRef<HTMLDivElement>;
    @ViewChild("wrapper") wrapper!: ElementRef<HTMLDivElement>;
    @ViewChild("backGround") bgElement!: ElementRef<HTMLDivElement>;
    @ViewChild("objectContainer") objectContainer!: ElementRef<HTMLDivElement>;
    ctx!: CanvasRenderingContext2D;
    imgHeight = 0; // total height of all background images
    imgWidth = 0;
    loadedImages = 0;
    zoomLevel = 1;
    defaultZoomLevel = 1; // adjusted to show full image width after images are loaded
    eventsAdded = false;
    loading = false;
    canvasHeights: {h: number; top: boolean; bottom: boolean}[] = [];
    drawHandler?: Drawing;
    // track DrawItem arrays to enable deleting a piece of drawing
    drawItems: Map<number, DrawItem[]> = new Map();

    // optional function to call when image is loaded to let external users know the canvas is ready for use
    @Input() imgLoadCallback?: (arg0: this) => void;

    @Input() setExternalBg?: (arg0: string) => void;

    eraserOrNormalOptions: IFillAndWidth = {
        w: 20,
        fill: true,
    };

    // TODO: for now there's no option to draw for e.g filled rectangle with borders, but save format should support it
    drawOptions: IDrawOptions = {
        enabled: true,
        drawType: DrawType.Freehand,
        color: "red",
        opacity: 1,
        eraser: false,
        w: 3,
        fill: false,
    };

    eraserState = false;

    // initial draw options
    @Input() options?: Partial<IDrawOptions>;

    @Input() toolBar = true;

    // optional function to call on certain drawing events and clicks
    updateCallback?: (arg0: this, arg1: IDrawUpdate) => void;

    // identifier e.g for associating specific canvas with specific answer review
    public id: number = 0;

    constructor(
        el: ElementRef<HTMLElement>,
        private domSanitizer: DomSanitizer
    ) {}

    ngOnInit() {
        this.drawOptions = {...this.drawOptions, ...this.options};
        if (!this.toolBar) {
            this.drawOptions.enabled = false;
        }
        this.setBg();
    }

    /**
     * Sets up the background image
     */
    setBg() {
        if (!this.bgSources) {
            return;
        }
        this.loading = true;
        this.loadedImages = 0;
        // This goes to src of img tag, so there should be no XSS danger because imgs cannot execute scripts.
        this.bgImages = this.bgSources.map((src) =>
            this.domSanitizer.bypassSecurityTrustResourceUrl(src)
        );
    }

    ngOnChanges(changes: SimpleChanges) {
        if (changes.bgSources) {
            this.bgSourceSizes = new Array<IImageSizes>(this.bgSources.length);
            this.setBg();
        }
    }

    ngOnDestroy() {
        this.clearObjectContainer();
        if (this.updateCallback) {
            this.updateCallback(this, {drawingUpdated: false, deleted: true});
        }
    }

    /**
     * Deletes all HTML elements within canvasObjectContainer
     */
    clearObjectContainer(): void {
        $(this.objectContainer.nativeElement.children).remove();
    }

    /**
     * Create a new Drawing and set up mouse events
     */
    prepareDrawing(): void {
        this.drawHandler = new Drawing(
            this.drawOptions,
            this.canvases.map((c) => c.nativeElement)
        );
        if (!this.eventsAdded) {
            this.canvasWrapper.nativeElement.addEventListener(
                "mousedown",
                (event) => {
                    this.downEvent(event, event);
                }
            );
            this.canvasWrapper.nativeElement.addEventListener(
                "touchstart",
                (event) => {
                    if (event.touches.length > 1) {
                        return;
                    }
                    this.downEvent(event, touchEventToTouch(event));
                }
            );
            this.canvasWrapper.nativeElement.addEventListener(
                "mousemove",
                (event) => {
                    this.moveEvent(event, event);
                }
            );
            this.canvasWrapper.nativeElement.addEventListener(
                "touchmove",
                (event) => {
                    if (event.touches.length > 1) {
                        return;
                    }
                    this.moveEvent(event, touchEventToTouch(event));
                }
            );
            this.canvasWrapper.nativeElement.addEventListener(
                "mouseup",
                (event) => {
                    this.upEvent(event, event);
                }
            );
            this.canvasWrapper.nativeElement.addEventListener(
                "touchend",
                (event) => {
                    if (event.touches.length > 1) {
                        return;
                    }
                    this.upEvent(event, touchEventToTouch(event));
                }
            );
            this.eventsAdded = true;
        }
    }

    /**
     * Store dimensions of loaded background images
     */
    onImgLoad(e: Event, index: number): void {
        // TODO: This event is called twice when re-opening image review
        const img = e.target as HTMLImageElement;
        const height = img.height;
        const width = img.width;
        this.bgSourceSizes[index] = {height: height, width: width};
        this.loadedImages += 1;
        if (this.loadedImages < this.bgSources.length) {
            return;
        }
        this.allImagesLoaded();
    }

    /**
     * Resizes canvas, fits image zoom to show full width, and calls the image load callback
     * function after every background image is loaded
     */
    allImagesLoaded(): void {
        // TODO: Show clear error if any image fails loading
        this.loading = false;
        let bgOffset = 0;
        this.bgOffsets = [bgOffset];
        this.imgHeight = this.bgElement.nativeElement.clientHeight;
        this.imgWidth = this.bgElement.nativeElement.clientWidth;
        // width will be same for all images/canvases
        const newWidth = Math.max(this.imgWidth, MIN_IMAGE_WIDTH);
        this.canvasHeights = [];
        const maxCanvasHeight = Math.ceil(MAX_CANVAS_AREA / newWidth);
        for (const size of this.bgSourceSizes) {
            let canvasesForImage = 1;
            if (size.height > maxCanvasHeight) {
                canvasesForImage = Math.ceil(size.height / maxCanvasHeight);
            }
            const canvasHeight = Math.ceil(size.height / canvasesForImage);
            for (let j = 0; j < canvasesForImage; j++) {
                this.canvasHeights.push({
                    h: canvasHeight,
                    top: j == 0,
                    bottom: j == canvasesForImage - 1,
                });
            }
            this.bgOffsets.push(size.height + bgOffset);
            bgOffset += size.height;
        }
        setTimeout(() => {
            this.prepareDrawing();
            let canvasOffset = 0;
            for (let i = 0; i < this.canvasHeights.length; i++) {
                const canvas = this.canvases.get(i)?.nativeElement;
                if (!canvas) {
                    throw Error(`Missing canvas ${i}`);
                }
                const canvasInfo = this.canvasHeights[i];
                let newHeight = canvasInfo.h;
                if (i == this.canvasHeights.length - 1) {
                    // if total canvas size is less than minimum, stretch last canvas
                    newHeight = Math.max(
                        newHeight,
                        MIN_IMAGE_HEIGHT - canvasOffset
                    );
                }
                canvas.height = newHeight;
                canvas.width = newWidth;
                canvas.style.top = canvasOffset + "px";
                if (canvasInfo.top) {
                    canvas.style.borderTop = "1px solid #000000";
                }
                if (canvasInfo.bottom) {
                    canvas.style.borderBottom = "1px solid #000000";
                }
                this.drawHandler!.setOffSet(
                    i,
                    {w: canvas.width, h: newHeight},
                    -canvasOffset
                );
                canvasOffset += newHeight;
            }
            if (this.imgWidth > this.wrapper.nativeElement.clientWidth) {
                this.defaultZoomLevel =
                    (this.wrapper.nativeElement.clientWidth -
                        SCROLLBAR_APPROX_WIDTH) /
                    this.imgWidth;
                this.zoomLevel = this.defaultZoomLevel;
            }
            if (this.imgLoadCallback) {
                this.imgLoadCallback(this);
            }
        }, 0);
    }

    /**
     * Returns the resizable div that contains the canvas, background images and objects within objectContainer
     */
    getWrapper(): HTMLDivElement {
        return this.wrapper.nativeElement;
    }

    /**
     * Gets the outer wrapper div's height based on background image and min/max settings
     */
    getWrapperHeight(includeScrollbar: boolean = false): number {
        const min = 300;
        const max = window.innerHeight - 155; // screen - answerbrowser - drawtoolbar (eyeballed)

        // +100 => reduce screen jump when opening velps near bottom of canvas
        return (
            Math.min(Math.max(min, this.imgHeight) + 100, max) +
            (includeScrollbar ? SCROLLBAR_APPROX_WIDTH : 0)
        );
        // TODO: Use case related min/max settings via attrs
    }

    /**
     * Sets the optional function to call on mouse or drawing update events
     * @param cb function to execute
     */
    public setUpdateCallback(
        cb: (arg0: DrawCanvasComponent, arg1: IDrawUpdate) => void
    ) {
        this.updateCallback = cb;
    }

    /**
     * Checks if down event originates from middle or right mouse button
     * @param e MouseOrTouch event to inspect
     */
    middleOrRightClick(e: MouseOrTouch): boolean {
        return e instanceof MouseEvent && (e.button == 1 || e.button == 2);
    }

    /**
     * Starts the drawing event
     */
    downEvent(event: Event, e: MouseOrTouch): void {
        const middleOrRightClick = this.middleOrRightClick(e);
        if (
            !middleOrRightClick &&
            !(isTouchEvent(event) && !this.drawOptions.enabled)
        ) {
            // allow inspect element and scrolling
            event.preventDefault();
        }
        if (!middleOrRightClick) {
            this.drawHandler!.downEvent(
                this.normalizeCoordinate(
                    posToRelative(this.canvasWrapper.nativeElement, e)
                )
            );
        }
    }

    /**
     * Handles drawing new image when mouse is moved
     * TODO: check if double-layered canvas is needed (for now we re-draw everything every time mouse moves during draw)
     */
    moveEvent(event: Event, e: MouseOrTouch): void {
        if (!(isTouchEvent(event) && !this.drawOptions.enabled)) {
            event.preventDefault();
        }
        this.drawHandler!.moveEvent(
            this.normalizeCoordinate(
                posToRelative(this.canvasWrapper.nativeElement, e)
            )
        );
    }

    /**
     * Finishes the draw event
     */
    upEvent(event: Event, e: MouseOrTouch): void {
        const pxy = this.normalizeCoordinate(
            posToRelative(this.canvasWrapper.nativeElement, e)
        );
        this.drawHandler!.upEvent(pxy);
        if (this.updateCallback) {
            this.updateCallback(this, {
                x: pxy.x,
                y: pxy.y,
                drawingUpdated:
                    this.drawHandler!.drawMoved && this.drawOptions.enabled,
            });
        }
    }

    /**
     * Removes the latest piece of (non-permanent) drawing data
     */
    undo = (e?: Event) => {
        if (e) {
            e.preventDefault();
        }
        if (!this.drawHandler) {
            return;
        }
        this.drawHandler.undo();
        if (this.updateCallback) {
            this.updateCallback(this, {drawingUpdated: true});
        }
    };

    /**
     * Returns current drawing progress in an array
     */
    getDrawing(): DrawItem[] {
        if (!this.drawHandler) {
            return [];
        }
        return this.drawHandler.getDrawing();
    }

    /**
     * Returns dimensions (start coordinate, max width/height) on current drawing progress
     * @param minSize extra padding to add if width/height is below this
     */
    getCurrentDrawingDimensions(minSize = 0): {
        x: number;
        y: number;
        w: number;
        h: number;
    } {
        return getDrawingDimensions(this.getDrawing(), minSize);
    }

    /**
     * Moves current drawing progress to permanent storage (e.g makes it immune to undo)
     */
    saveAndStoreDrawing(id: number) {
        if (!this.drawHandler) {
            return;
        }
        this.drawItems.set(id, this.drawHandler.getDrawing());
        this.drawHandler.storeDrawing();
    }

    /**
     * Sets and draws the given permanent drawings on canvas
     * @param data IDrawingWithID array of drawings to draw
     */
    setPersistentDrawData(data: IDrawingWithID[]): void {
        const drawings: DrawItem[] = [];
        this.drawItems.clear();
        data.forEach((item) => {
            this.drawItems.set(item.id, item.drawData);
            drawings.push(...item.drawData);
        });
        this.drawHandler?.setPersistentDrawData(drawings);
    }

    /**
     * Delete a piece of drawing
     * @param removableId drawing to delete
     */
    deleteDrawItem(removableId: number): void {
        this.drawItems.delete(removableId);
        const drawings = Array.from(this.drawItems.values()).flat();
        this.drawHandler?.setPersistentDrawData(drawings);
    }

    /**
     * Scroll to the top coordinate of the next background image
     * @param down direction, going down => next image / increased y coord
     */
    scrollBgImage(down: boolean) {
        const currPos = this.wrapper.nativeElement.scrollTop;
        const sizes = this.bgOffsets.map((os) => os * this.zoomLevel);
        let pos;
        if (down) {
            if (
                currPos <=
                this.wrapper.nativeElement.scrollHeight -
                    this.wrapper.nativeElement.offsetHeight
            ) {
                pos = sizes.find((s) => Math.floor(s) > currPos);
            }
        } else {
            for (let i = sizes.length - 1; i >= 0; i--) {
                if (Math.floor(sizes[i]) < currPos) {
                    pos = sizes[i];
                    break;
                }
            }
        }
        if (pos != undefined) {
            this.wrapper.nativeElement.scrollTo({top: pos});
        } else {
            this.wrapper.nativeElement.scrollTo({
                top: down ? 0 : sizes[this.bgSourceSizes.length - 1],
            });
        }
    }

    /**
     * Change zoom level of canvas and background image
     * objects within canvasObjectContainer are unaffected, they should be updated manually by listening
     * to updateCallback on external controller
     * @param delta change in zoom level, 0 to reset to initial
     */
    zoom(delta: number) {
        if (delta === 0) {
            this.zoomLevel = this.defaultZoomLevel;
        } else {
            this.zoomLevel += delta;
            if (this.zoomLevel < 0.1) {
                this.zoomLevel = 0.1;
            }
        }
        if (this.updateCallback) {
            this.updateCallback(this, {
                drawingUpdated: false,
                scaleChange: this.zoomLevel,
            });
        }
    }

    getZoomLevel() {
        return `scale(${this.zoomLevel})`;
    }

    /**
     * Adjust coordinate according to current zoom level
     * @param coord {x, y}
     */
    normalizeCoordinate(coord: {x: number; y: number}) {
        return {
            x: coord.x / this.zoomLevel,
            y: coord.y / this.zoomLevel,
        };
    }

    isUnSaved() {
        return this.getDrawing().length > 0;
    }
}

@NgModule({
    declarations: [DrawCanvasComponent],
    imports: [BrowserModule, DrawToolbarModule, FormsModule, TimUtilityModule],
    exports: [DrawCanvasComponent],
})
export class DrawCanvasModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
