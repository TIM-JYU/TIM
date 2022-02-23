import {
    ApplicationRef,
    Component,
    DoBootstrap,
    ElementRef,
    Input,
    NgModule,
    OnChanges,
    OnDestroy,
    OnInit,
    SimpleChanges,
    ViewChild,
} from "@angular/core";
import {
    BrowserModule,
    DomSanitizer,
    SafeResourceUrl,
} from "@angular/platform-browser";
import {
    DrawOptions,
    DrawToolbarModule,
    DrawType,
    IDrawOptions,
} from "tim/plugin/drawToolbar";
import {
    ILine,
    ILineSegment,
    IPoint,
    IRectangleOrEllipse,
    TuplePoint,
} from "tim/plugin/imagextypes";
import {
    isTouchEvent,
    MouseOrTouch,
    numOrStringToNumber,
    posToRelative,
    TimStorage,
    touchEventToTouch,
} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {IUnsavedComponent} from "tim/document/viewctrl";

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

/**
 * Gets dimensions (start coordinates, width, height) from given drawing
 * @param drawing DrawItem[] to check
 * @param minSize if width/height is less than this, then add extra padding (at both ends)
 */
export function getDrawingDimensions(
    drawing: DrawItem[],
    minSize = 0
): {x: number; y: number; w: number; h: number} {
    let x = Number.MAX_SAFE_INTEGER;
    let y = Number.MAX_SAFE_INTEGER;
    let w = 0;
    let h = 0;
    for (const obj of drawing) {
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
    if (w - x < minSize) {
        x = Math.max(x - minSize, 0);
        w = w + minSize;
    }
    if (h - y < minSize) {
        y = Math.max(y - minSize, 0);
        h = h + minSize;
    }
    return {x: x, y: y, w: w - x, h: h - y};
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
    ctx.globalAlpha = obj.opacity ?? options.opacity;
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
    ctx.globalAlpha = line.opacity ?? options.opacity;
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
    if (rectangle.fillColor) {
        ctx.fillRect(rectangle.x, rectangle.y, rectangle.w, rectangle.h);
    } else {
        ctx.strokeRect(rectangle.x, rectangle.y, rectangle.w, rectangle.h);
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
    const ratio = ellipse.w / ellipse.h;
    ctx.save();
    ctx.beginPath();
    ctx.scale(ratio, 1);
    const r = Math.min(ellipse.w / ratio, ellipse.h) / 2;
    ctx.arc(
        (ellipse.x + ellipse.w / 2) / ratio,
        ellipse.y + ellipse.h / 2,
        r,
        0,
        2 * Math.PI
    );
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
    ctx.globalAlpha = options.opacity;
    ctx.strokeStyle = options.color;
    ctx.lineWidth = options.w;
    ctx.fillStyle = options.color;
}

function applyStyleAndWidth(ctx: CanvasRenderingContext2D, seg: ILineSegment) {
    ctx.strokeStyle = seg.color ?? ctx.strokeStyle;
    ctx.lineWidth = numOrStringToNumber(seg.w ?? ctx.lineWidth);
    ctx.globalAlpha = seg.opacity ?? 1;
}

interface IImageSizes {
    height: number;
    width: number;
}

/**
 * Draw basic shapes on canvas via manually called movement events
 */
export class Drawing {
    drawOptions: IDrawOptions;
    ctx: CanvasRenderingContext2D;
    canvas: HTMLCanvasElement;
    externalClear = false;

    /**
     * @param drawOptions settings used when drawing new items
     * @param canvas CanvasElement to draw on
     * @param externalClear if true, don't automatically clear canvas before redraws
     */
    constructor(
        drawOptions: IDrawOptions,
        canvas: HTMLCanvasElement,
        externalClear?: boolean
    ) {
        this.drawOptions = drawOptions;
        this.canvas = canvas;
        this.ctx = this.canvas.getContext("2d")!;
        this.ctx.lineCap = "round";
        if (externalClear) {
            this.externalClear = true;
        }
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

    // dimension used when drawing shapes
    private itemX: number = 0;
    private itemY: number = 0;
    private itemW: number = 0;
    private itemH: number = 0;

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
            this.startSegmentDraw(coords);
        }
    }

    /**
     * Draw preview object based on mouse movement and current drawing settings
     * @param coords current location
     */
    public moveEvent(coords: {x: number; y: number}) {
        if (!this.drawStarted) {
            return;
        }
        this.drawMoved = true;
        const {x, y} = coords;
        if (
            this.drawOptions.drawType == DrawType.Ellipse ||
            this.drawOptions.drawType == DrawType.Rectangle
        ) {
            this.redrawAll();
            setContextSettingsFromOptions(this.drawOptions, this.ctx);
            this.itemX = Math.min(x, this.startX);
            this.itemY = Math.min(y, this.startY);
            this.itemW = Math.abs(x - this.startX);
            this.itemH = Math.abs(y - this.startY);
            if (this.drawOptions.drawType == DrawType.Ellipse) {
                this.drawPreviewEllipse();
            } else {
                this.drawPreviewRectangle();
            }
        } else if (this.drawOptions.drawType == DrawType.Arrow) {
            this.redrawAll();
            setContextSettingsFromOptions(this.drawOptions, this.ctx);
            drawArrow(
                {
                    start: {x: this.startX, y: this.startY},
                    end: coords,
                    w: this.drawOptions.w,
                },
                this.ctx
            );
        } else if (this.prevPos) {
            if (this.drawOptions.drawType == DrawType.Line) {
                this.popPoint(1);
            }
            setContextSettingsFromOptions(this.drawOptions, this.ctx);
            this.addPoint(coords);
            this.redrawAll();
        }
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
            if (this.drawOptions.drawType == DrawType.Ellipse) {
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
     * Create ILine based on draw coordinates and current settings
     */
    makeLine(): ILine {
        return {
            start: {x: this.startX, y: this.startY},
            end: {x: this.endX, y: this.endY},
            w: this.drawOptions.w,
            opacity: this.drawOptions.opacity,
            color: this.drawOptions.color,
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
        return {
            x: this.itemX,
            y: this.itemY,
            w: this.itemW,
            h: this.itemH,
            opacity: this.drawOptions.opacity,
            color: this.drawOptions.color,
            ...fillOrBorder,
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
        drawRectangle(this.makeShape(), this.ctx);
    }

    /**
     * Draw an ellipse during mouse move
     */
    drawPreviewEllipse() {
        drawEllipse(this.makeShape(), this.ctx);
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
        if (!this.externalClear) {
            this.clearCanvas();
        }
    }

    /**
     * Clears the entire canvas
     */
    clearCanvas(): void {
        this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
    }

    /**
     * Redraws everything (current drawings, permanent drawings, possible freehand drawing)
     * Canvas will be cleared first unless this.externalClear is true
     */
    redrawAll(): void {
        if (!this.externalClear) {
            this.clearCanvas();
        }
        drawFromArray(this.persistentDrawData, this.drawOptions, this.ctx);
        drawFromArray(this.drawData, this.drawOptions, this.ctx);
        if (this.freeDrawing) {
            drawFreeHand(this.ctx, [this.freeDrawing]);
        }
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
     * Draws a line between two points
     */
    line(p1: TuplePoint, p2: IPoint) {
        this.ctx.beginPath();
        this.ctx.moveTo(p1[0], p1[1]);
        this.ctx.lineTo(p2.x, p2.y);
        this.ctx.stroke();
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
        this.ctx.lineCap = "round";
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

@Component({
    selector: "draw-canvas",
    template: `

        <div style="position: relative;">
            <div style="position: absolute; top: 50%; left:-5%; display: flex; flex-flow: column; gap: 1em;
                         -ms-transform: translateY(-50%); transform: translateY(-50%); z-index: 4;">
                <div *ngIf="bgSources.length > 1" style="display: flex; flex-flow: column;">
                    <button title="Previous image" class="btn btn-primary" (click)="scrollBgImage(false)">&uarr;
                    </button>
                    <button title="Next image" class="btn btn-primary" (click)="scrollBgImage(true)">&darr;
                    </button>
                </div>
                <div style="display: flex; flex-flow: column;">
                   <button title="Zoom in" class="btn btn-primary" (click)="zoom(0.1)">
                       <i class="glyphicon glyphicon-zoom-in"></i>
                    </button>
                    <!-- TODO icons-->
                    <button title="Reset zoom" class="btn btn-primary" (click)="zoom(0)">R
                    </button>
                    <button title="Zoom out" class="btn btn-primary" (click)="zoom(-0.1)">
                       <i class="glyphicon glyphicon-zoom-out"></i>
                    </button>
                </div>

            </div>
            <div #wrapper style="overflow: auto; position: relative; resize: both;"
                 [style.height.px]="getWrapperHeight(true)"
                 >
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
                <div class="zoomer" style="-webkit-transform-origin: 0 0;" [style.transform]="getZoomLevel()">
                    <canvas #drawbase class="drawbase" style="border:1px solid #000000; position: absolute;">
                    </canvas>
                </div>
            </div>
        </div>
        <draw-toolbar *ngIf="toolBar" [drawSettings]="drawOptions" (drawSettingsChange)="saveSettings()"
                      [undo]="undo"></draw-toolbar>
    `,
})
export class DrawCanvasComponent
    implements OnInit, OnChanges, OnDestroy, IUnsavedComponent
{
    @Input() public bgSources: string[] = [];
    bgSourceSizes: IImageSizes[] = []; // dimensions of loaded background images, sorted
    bgOffsets: number[] = []; // top starting coordinates of each background image, sorted
    bgImages: SafeResourceUrl[] = [];
    @ViewChild("drawbase") canvas!: ElementRef<HTMLCanvasElement>;
    @ViewChild("wrapper") wrapper!: ElementRef<HTMLDivElement>;
    @ViewChild("backGround") bgElement!: ElementRef<HTMLDivElement>;
    @ViewChild("objectContainer") objectContainer!: ElementRef<HTMLDivElement>;
    ctx!: CanvasRenderingContext2D;
    imgHeight = 0; // total height of all background images
    imgWidth = 0;
    loadedImages = 0;
    zoomLevel = 1;
    defaultZoomLevel = 1;

    drawHandler!: Drawing;

    // optional function to call when image is loaded to let external users know the canvas is ready for use
    @Input() imgLoadCallback?: (arg0: this) => void;

    @Input() setExternalBg?: (arg0: string) => void;

    // TODO: for now there's no option to draw for e.g filled rectangle with borders, but save format should support it
    drawOptions: IDrawOptions = {
        enabled: true,
        drawType: DrawType.Freehand,
        color: "red",
        w: 3,
        opacity: 1,
        fill: false,
    };

    // initial draw options
    @Input() options?: Partial<IDrawOptions>;

    @Input() toolBar = true;

    // optional function to call on certain drawing events and clicks
    updateCallback?: (arg0: this, arg1: IDrawUpdate) => void;

    // identifier e.g for associating specific canvas with specific answer review
    public id: number = 0;

    private optionsStorage = new TimStorage("drawCanvasOptions", DrawOptions);

    constructor(
        el: ElementRef<HTMLElement>,
        private domSanitizer: DomSanitizer
    ) {}

    ngOnInit() {
        const prevSettings = this.optionsStorage.get();
        if (prevSettings) {
            this.drawOptions = prevSettings;
        } else {
            this.drawOptions = {...this.drawOptions, ...this.options};
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
        this.loadedImages = 0;
        // This goes to src of img tag, so there should be no XSS danger because imgs cannot execute scripts.
        this.bgImages = this.bgSources.map((src) =>
            this.domSanitizer.bypassSecurityTrustResourceUrl(src)
        );
    }

    ngAfterViewInit() {
        this.drawHandler = new Drawing(
            this.drawOptions,
            this.canvas.nativeElement
        );
        this.canvas.nativeElement.addEventListener("mousedown", (event) => {
            this.downEvent(event, event);
        });
        this.canvas.nativeElement.addEventListener("touchstart", (event) => {
            if (event.touches.length > 1) {
                return;
            }
            this.downEvent(event, touchEventToTouch(event));
        });
        this.canvas.nativeElement.addEventListener("mousemove", (event) => {
            this.moveEvent(event, event);
        });
        this.canvas.nativeElement.addEventListener("touchmove", (event) => {
            if (event.touches.length > 1) {
                return;
            }
            this.moveEvent(event, touchEventToTouch(event));
        });
        this.canvas.nativeElement.addEventListener("mouseup", (event) => {
            this.upEvent(event, event);
        });
        this.canvas.nativeElement.addEventListener("touchend", (event) => {
            if (event.touches.length > 1) {
                return;
            }
            this.upEvent(event, touchEventToTouch(event));
        });
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

    saveSettings() {
        this.optionsStorage.set(this.drawOptions);
    }

    /**
     * Deletes all HTML elements within canvasObjectContainer
     */
    clearObjectContainer(): void {
        $(this.objectContainer.nativeElement.children).remove();
    }

    /**
     * Store dimensions of loaded background images
     */
    onImgLoad(e: Event, index: number): void {
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
     * Resizes canvas and calls the image load callback function after every background image is loaded
     */
    allImagesLoaded(): void {
        let offset = 0;
        this.bgOffsets = [offset];
        for (let i = 0; i < this.bgSourceSizes.length - 1; i++) {
            this.bgOffsets.push(this.bgSourceSizes[i].height + offset);
            offset += this.bgSourceSizes[i].height;
        }
        this.imgHeight = this.bgElement.nativeElement.clientHeight;
        this.imgWidth = this.bgElement.nativeElement.clientWidth;
        const newWidth = Math.max(
            this.bgElement.nativeElement.clientWidth,
            this.wrapper.nativeElement.clientWidth - 50
        );
        const newHeight = Math.max(
            this.bgElement.nativeElement.clientHeight,
            this.getWrapperHeight() - 5
        );
        this.canvas.nativeElement.width = newWidth;
        this.canvas.nativeElement.height = newHeight;
        if (this.imgWidth > this.wrapper.nativeElement.clientWidth) {
            this.defaultZoomLevel =
                this.wrapper.nativeElement.clientWidth / this.imgWidth;
            this.zoomLevel = this.defaultZoomLevel;
        }
        if (this.imgLoadCallback) {
            this.imgLoadCallback(this);
        }
    }

    getWrapper(): HTMLDivElement {
        return this.wrapper.nativeElement;
    }

    getWrapperWidth(includeScrollbar: boolean = false): number {
        const min = 400;
        const max = window.innerWidth;
        return (
            Math.min(Math.max(min, this.imgWidth), max) +
            (includeScrollbar ? SCROLLBAR_APPROX_WIDTH : 0)
        );
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
            this.drawHandler.downEvent(
                this.normalizeCoordinate(
                    posToRelative(this.canvas.nativeElement, e)
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
        this.drawHandler.moveEvent(
            this.normalizeCoordinate(
                posToRelative(this.canvas.nativeElement, e)
            )
        );
    }

    /**
     * Finishes the draw event
     */
    upEvent(event: Event, e: MouseOrTouch): void {
        const pxy = this.normalizeCoordinate(
            posToRelative(this.canvas.nativeElement, e)
        );
        this.drawHandler.upEvent(pxy);
        if (this.updateCallback) {
            this.updateCallback(this, {
                x: pxy.x,
                y: pxy.y,
                drawingUpdated:
                    this.drawHandler.drawMoved && this.drawOptions.enabled,
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
        this.drawHandler.undo();
        if (this.updateCallback) {
            this.updateCallback(this, {drawingUpdated: true});
        }
    };

    /**
     * Returns current drawing progress in an array
     */
    getDrawing(): DrawItem[] {
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
    storeDrawing() {
        this.drawHandler.storeDrawing();
    }

    /**
     * Sets and draws the given permanent drawing on canvas
     * @param data Drawing to draw
     */
    setPersistentDrawData(data: DrawItem[]): void {
        this.drawHandler.setPersistentDrawData(data);
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
    imports: [BrowserModule, CommonModule, DrawToolbarModule, FormsModule],
    exports: [DrawCanvasComponent],
})
export class DrawCanvasModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
