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

type IRectangle = {
    type: "rectangle";
    drawData: IRectangleOrEllipse;
};

type IEllipse = {
    type: "ellipse";
    drawData: IRectangleOrEllipse;
};

type IFreeHand = {
    type: "freehand";
    drawData: ILineSegment;
};

// TODO: 4th variable for reporting MouseDown, TouchMove etc could be useful for fine-tuning the desired
//  update behaviour - for now this is called on certain MouseUp/TouchEnd events, and the behaviour
//  is not optimal for mobile (e.g contained objects register clicks when user just wanted to scroll the page)
export interface IDrawUpdate {
    x?: number;
    y?: number;
    drawingUpdated: boolean;
}

// TODO: Name overlap with imagex DrawObject
export type DrawObject = IRectangle | IEllipse | IFreeHand;

/**
 * Gets dimensions (start coordinates, width, height) from given drawing
 * @param drawing DrawObject[] to check
 * @param minSize if width/height is less than this, then add extra padding (at both ends)
 */
export function getDrawingDimensions(
    drawing: DrawObject[],
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
        } else {
            // TODO: for now shapes don't start in pixel-perfect locations so lineWidth is currently ignored
            const shape = obj.drawData;
            if (x > shape.x) {
                x = shape.x;
            }
            if (y > shape.y) {
                y = shape.y;
            }
            if (w < shape.x + shape.w) {
                w = shape.x + shape.w;
            }
            if (h < shape.y + shape.h) {
                h = shape.y + shape.h;
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
 * @param drawing DrawObject[] to check
 * @param x start coordinate
 * @param y start coordinate
 * @param minSize if width/height is less than this, then add extra padding (at both ends)
 */
export function isCoordWithinDrawing(
    drawing: DrawObject[],
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

// TODO: Repeated from imagex, move
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

function applyStyleAndWidth(ctx: CanvasRenderingContext2D, seg: ILineSegment) {
    ctx.strokeStyle = seg.color ?? ctx.strokeStyle;
    ctx.lineWidth = numOrStringToNumber(seg.w ?? ctx.lineWidth);
    ctx.globalAlpha = seg.opacity ?? 1;
}

@Component({
    selector: "draw-canvas",
    template: `
        <div #wrapper style="overflow: auto; position: relative;" [style.height.px]="getWrapperHeight()">
            <img style="max-width: none; position: absolute; display: unset;" #backGround *ngIf="bgImage"
                 [src]="bgImage" (load)="onImgLoad()">
            <!-- div for positioning custom objects behind the canvas-->
            <div #objectContainer class="canvasObjectContainer"
                 style="overflow: visible; position: absolute; height: 100%; width: 100%;">

            </div>
            <canvas #drawbase class="drawbase" style="border:1px solid #000000; position: absolute;">
            </canvas>
        </div>
        <draw-toolbar *ngIf="toolBar" [drawSettings]="drawOptions" (drawSettingsChange)="saveSettings()" [undo]="undo"></draw-toolbar>
    `,
})
export class DrawCanvasComponent implements OnInit, OnChanges, OnDestroy {
    @Input() public bgSource = "";
    bgImage: SafeResourceUrl = "";
    @ViewChild("drawbase") canvas!: ElementRef<HTMLCanvasElement>;
    @ViewChild("wrapper") wrapper!: ElementRef<HTMLDivElement>;
    @ViewChild("backGround") bgElement!: ElementRef<HTMLImageElement>;
    @ViewChild("objectContainer") objectContainer!: ElementRef<HTMLDivElement>;
    ctx!: CanvasRenderingContext2D;
    imgHeight = 0;

    // optional function to call when image is loaded to let external users know the canvas is ready for use
    @Input() imgLoadCallback?: (arg0: this) => void;

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

    // keep track of mousedown while drawing enabled
    drawStarted = false;
    drawMoved = false;
    // drawings that can altered with undo (TODO: Redo, erase...?)
    drawData: DrawObject[] = [];
    // drawings that cannot be altered via undo, e.g background or permanent drawings
    persistentDrawData: DrawObject[] = [];

    // freehand drawing that is built while mouse is pressed
    freeDrawing?: ILineSegment;
    // previous mouse position when drawing freehand
    private prevPos?: TuplePoint;

    // click start position
    private startX: number = 0;
    private startY: number = 0;

    // dimension used when drawing shapes
    private objX: number = 0;
    private objY: number = 0;
    private objW: number = 0;
    private objH: number = 0;

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
        // This goes to src of img tag, so there should be no XSS danger because imgs cannot execute scripts.
        this.bgImage = this.domSanitizer.bypassSecurityTrustResourceUrl(
            this.bgSource
        );
    }

    ngAfterViewInit() {
        this.ctx = this.canvas.nativeElement.getContext("2d")!;
        this.ctx.lineCap = "round";
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
        if (changes.bgSource) {
            this.setBg();
        }
    }

    ngOnDestroy() {
        this.clearObjectContainer();
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
     * Resizes canvas and calls the image load callback function after image is loaded
     */
    onImgLoad(): void {
        this.imgHeight = this.bgElement.nativeElement.clientHeight;
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
        this.objectContainer.nativeElement.style.width = newWidth + "px";
        this.objectContainer.nativeElement.style.height = newHeight + "px";
        if (this.imgLoadCallback) {
            this.imgLoadCallback(this);
        }
    }

    /**
     * Gets the outer wrapper div's height based on background image and min/max settings
     * For now we set it to image + 100px to reduce scroll jumping when inner objects (e.g velps) are
     * near bottom of the canvas
     * // TODO: if canvas is used without inner objects then this 100px is a waste of screen space
     */
    getWrapperHeight(): number {
        const min = 300;
        const max = 1024;

        return Math.min(Math.max(min, this.imgHeight) + 100, max);
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
        const {x, y} = posToRelative(this.canvas.nativeElement, e);
        if (this.drawOptions.enabled && !middleOrRightClick) {
            this.drawStarted = true;
            this.drawMoved = false;
            this.startX = x;
            this.startY = y;
            this.startSegmentDraw(posToRelative(this.canvas.nativeElement, e));
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
        if (!this.drawStarted) {
            return;
        }
        this.drawMoved = true;
        const pxy = posToRelative(this.canvas.nativeElement, e);
        const {x, y} = pxy;

        if (
            this.drawOptions.drawType == DrawType.Ellipse ||
            this.drawOptions.drawType == DrawType.Rectangle
        ) {
            this.redrawAll();
            this.objX = Math.min(x, this.startX);
            this.objY = Math.min(y, this.startY);
            this.objW = Math.abs(x - this.startX);
            this.objH = Math.abs(y - this.startY);
            this.setContextSettingsFromOptions();
            if (this.drawOptions.drawType == DrawType.Ellipse) {
                this.drawPreviewEllipse();
            } else {
                this.drawPreviewRectangle();
            }
        } else if (this.prevPos) {
            if (this.drawOptions.drawType == DrawType.Line) {
                this.popPoint(1);
            }
            this.line(this.prevPos, pxy);
            this.addPoint(pxy);
            this.redrawAll();
        }
    }

    /**
     * Finishes the draw event
     */
    upEvent(event: Event, e: MouseOrTouch): void {
        const pxy = posToRelative(this.canvas.nativeElement, e);
        const {x, y} = pxy;
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
            } else if (this.freeDrawing) {
                const freeDrawing: IFreeHand = {
                    type: "freehand",
                    drawData: this.freeDrawing,
                };
                this.drawData.push(freeDrawing);
                this.freeDrawing = undefined;
            }
        }

        if (this.updateCallback) {
            this.updateCallback(this, {
                x: x,
                y: y,
                drawingUpdated: this.drawMoved && this.drawOptions.enabled,
            });
        }
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
            x: this.objX,
            y: this.objY,
            w: this.objW,
            h: this.objH,
            opacity: this.drawOptions.opacity,
            color: this.drawOptions.color,
            ...fillOrBorder,
        };
    }

    /**
     * Removes the latest piece of (non-permanent) drawing data
     */
    undo = (e?: Event) => {
        if (e) {
            e.preventDefault();
        }
        this.drawData.pop();
        this.redrawAll();
        if (this.updateCallback) {
            this.updateCallback(this, {drawingUpdated: true});
        }
    };

    /**
     * Draw a rectangle during mouse move
     */
    drawPreviewRectangle() {
        this.drawRectangle(this.makeObj());
    }

    /**
     * Draw an ellipse during mouse move
     */
    drawPreviewEllipse() {
        this.drawEllipse(this.makeObj());
    }

    /**
     * Return dimension for current shape dimensions based on last draw event
     */
    makeObj(): IRectangleOrEllipse {
        return {
            x: this.objX,
            y: this.objY,
            w: this.objW,
            h: this.objH,
            fillColor: this.drawOptions.fill
                ? this.drawOptions.color
                : undefined,
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
     * Clears the entire canvas
     */
    clear(): void {
        this.ctx.clearRect(
            0,
            0,
            this.canvas.nativeElement.width,
            this.canvas.nativeElement.height
        );
    }

    /**
     * Clears the canvas and redraws everything (current drawings, permanent drawings, possible freehand drawing)
     */
    redrawAll(): void {
        this.clear();
        this.drawFromArray(this.persistentDrawData);
        this.drawFromArray(this.drawData);
        if (this.freeDrawing) {
            drawFreeHand(this.ctx, [this.freeDrawing]);
        }
    }

    /**
     * Draws given input on canvas
     * @param data array of freehand drawings, ellipses and rectangles
     */
    drawFromArray(data: DrawObject[]) {
        for (const object of data) {
            if (object.type == "ellipse") {
                this.setContextSettingsFromObject(object.drawData);
                this.drawEllipse(object.drawData);
            } else if (object.type == "rectangle") {
                this.setContextSettingsFromObject(object.drawData);
                this.drawRectangle(object.drawData);
            } else {
                drawFreeHand(this.ctx, [object.drawData]);
            }
        }
    }

    /**
     * Sets canvas options (line widths, colors, opacity) to suit given object
     * If any is missing use current canvas options
     * @param obj IRectangleOrEllipse object with options
     */
    setContextSettingsFromObject(obj: IRectangleOrEllipse) {
        this.ctx.strokeStyle = obj.color ?? this.drawOptions.color;
        this.ctx.lineWidth = obj.lineWidth ?? this.drawOptions.w;
        this.ctx.fillStyle = obj.fillColor ?? "transparent";
        this.ctx.globalAlpha = obj.opacity ?? this.drawOptions.opacity;
    }

    /**
     * Sets canvas options (line widths, colors, opacity) based on current toolbar settings
     */
    setContextSettingsFromOptions() {
        this.ctx.globalAlpha = this.drawOptions.opacity;
        this.ctx.strokeStyle = this.drawOptions.color;
        this.ctx.lineWidth = this.drawOptions.w;
        this.ctx.fillStyle = this.drawOptions.color;
    }

    /**
     * Draws an ellipse
     * @param ellipse in IRectangleOrEllipse format
     */
    drawEllipse(ellipse: IRectangleOrEllipse) {
        const ratio = ellipse.w / ellipse.h;
        this.ctx.save();
        this.ctx.beginPath();
        this.ctx.scale(ratio, 1);
        const r = Math.min(ellipse.w / ratio, ellipse.h) / 2;
        this.ctx.arc(
            (ellipse.x + ellipse.w / 2) / ratio,
            ellipse.y + ellipse.h / 2,
            r,
            0,
            2 * Math.PI
        );
        this.ctx.restore();
        if (ellipse.fillColor) {
            this.ctx.fill();
        } else {
            this.ctx.stroke();
        }
    }

    /**
     * Draws a rectangle
     * @param rectangle in IRectangleOrEllipse format
     */
    drawRectangle(rectangle: IRectangleOrEllipse) {
        // TODO: Draw border with own settings but custom fill color
        this.ctx.lineJoin = "miter";
        if (rectangle.fillColor) {
            this.ctx.fillRect(
                rectangle.x,
                rectangle.y,
                rectangle.w,
                rectangle.h
            );
        } else {
            this.ctx.strokeRect(
                rectangle.x,
                rectangle.y,
                rectangle.w,
                rectangle.h
            );
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
        this.ctx.strokeStyle = this.drawOptions.color;
        this.ctx.lineWidth = this.drawOptions.w;
        this.ctx.globalAlpha = this.drawOptions.opacity;
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
     * Returns current drawing progress in an array
     */
    getDrawing(): DrawObject[] {
        return this.drawData;
    }

    /**
     * Returns dimensions (start coordinate, max width/height) on current drawing progress
     * @param minSize extra padding to add if width/height is below this
     */
    getCurrentDrawingDimensions(
        minSize = 0
    ): {x: number; y: number; w: number; h: number} {
        return getDrawingDimensions(this.getDrawing(), minSize);
    }

    /**
     * Moves current drawing progress to permanent storage (e.g makes it immune to undo)
     */
    storeDrawing() {
        this.persistentDrawData = this.persistentDrawData.concat(this.drawData);
        this.drawData = [];
    }

    /**
     * Sets and draws the given permanent drawing on canvas
     * @param data Drawing to draw
     */
    setPersistentDrawData(data: DrawObject[]): void {
        this.ctx.lineCap = "round";
        this.persistentDrawData = data;
        this.redrawAll();
    }
}

@NgModule({
    declarations: [DrawCanvasComponent],
    imports: [BrowserModule, DrawToolbarModule, FormsModule],
    exports: [DrawCanvasComponent],
})
export class DrawCanvasModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
