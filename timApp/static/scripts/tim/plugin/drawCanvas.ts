import {
    ApplicationRef,
    Component,
    DoBootstrap,
    ElementRef,
    Input,
    NgModule,
    OnChanges,
    OnInit,
    SimpleChanges,
    StaticProvider,
    ViewChild,
} from "@angular/core";
import {BrowserModule, DomSanitizer, SafeResourceUrl} from "@angular/platform-browser";
import {DrawToolbarModule, DrawType} from "tim/plugin/drawToolbar";
import {ILineSegment, IPoint, IRectangleOrCircle, TuplePoint} from "tim/plugin/imagextypes";
import {MouseOrTouch, numOrStringToNumber, posToRelative, touchEventToTouch} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";

interface IRectangle {
    type: "rectangle";
    drawData: IRectangleOrCircle;
}

interface ICircle {
    type: "circle";
    drawData: IRectangleOrCircle;
}

interface IFreeHand {
    type: "freehand";
    drawData: ILineSegment
}

// TODO: Name overlap with imagex DrawObject
export type DrawObject = IRectangle | ICircle | IFreeHand;

/**
 * Gets dimensions (start coordinates, width, height) from given drawing
 * @param drawing DrawObject[] to check
 */
export function getDrawingDimensions(drawing: DrawObject[]): { x: number, y: number, w: number, h: number } {
    let x = Number.MAX_SAFE_INTEGER;
    let y = Number.MAX_SAFE_INTEGER;
    let w = 0;
    let h = 0;
    for (const obj of drawing) {
        if (obj.type == "freehand") {
            for (const line of obj.drawData.lines) {
                if (x == null || x > line[0]) {
                    x = line[0];
                }
                if (y == null || y > line[1]) {
                    y = line[1];
                }
                if (w < line[0]) {
                    w = line[0];
                }
                if (h < line[1]) {
                    h = line[1];
                }
            }
        } else {
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
    return {x: x, y: y, w: w - x, h: h - y};
}

/**
 * Checks if a coordinate exists within given drawing
 * @param drawing DrawObject[] to check
 * @param x start coordinate
 * @param y start coordinate
 */
export function isCoordWithinDrawing(drawing: DrawObject[], x: number, y: number): boolean {
    const dimensions = getDrawingDimensions(drawing);
    return (x > dimensions.x && x < dimensions.x + dimensions.w && y > dimensions.y && y < dimensions.y + dimensions.h);
}

// TODO: Repeated from imagex, move
export function drawFreeHand(ctx: CanvasRenderingContext2D, dr: ILineSegment[]): void {
    // console.log(dr);
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
            <img style="max-width: none; position: absolute; display: unset;" #backGround *ngIf="bypassedImage"
                 [src]="bypassedImage" (load)="onImgLoad()">
            <!-- 0-sized div for positioning custom objects on canvas-->
            <div #objectContainer class="canvasObjectContainer"
                 style="width: 0px; height: 0px; overflow: visible; position: relative">

            </div>
            <canvas #drawbase class="drawbase" style="border:1px solid #000000; position: absolute;">
            </canvas>
<!--            </div>-->
        </div>
        <draw-toolbar [(enabled)]="drawingEnabled" [(drawType)]="drawType"
                      [(color)]="color" [(fill)]="drawFill" [undo]="undo"
                      [(w)]="w" [(opacity)]="opacity"></draw-toolbar>
    `,
})
export class DrawCanvasComponent implements OnInit, OnChanges {
    @Input() public bgSource = "";
    bypassedImage: SafeResourceUrl = "";
    @ViewChild("drawbase") canvas!: ElementRef<HTMLCanvasElement>;
    @ViewChild("wrapper") wrapper!: ElementRef<HTMLDivElement>;
    @ViewChild("backGround") bgImage!: ElementRef<HTMLImageElement>;
    @ViewChild("objectContainer") objectContainer!: ElementRef<HTMLDivElement>;
    ctx!: CanvasRenderingContext2D;
    imgHeight = 0;

    // optional function to call when image is loaded to let external users know the canvas is ready for use
    @Input() imgLoadCallback?: (arg0: this) => void;

    // draw-related attributes
    @Input() drawingEnabled = true;
    drawType = DrawType.Freehand;
    color = "red";
    w = 3;
    opacity = 1;
    // TODO: for now there's no option to draw for e.g filled rectangle with borders, but save format should support it
    drawFill = false;

    // optional function to call whenever mouse is pressed (whether drawing enabled or not)
    clickCallback?: (arg0: this, arg1: number, arg2: number) => void;

    // keep track of mousedown while drawing enabled
    drawStarted = false;
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

    constructor(el: ElementRef<HTMLElement>, private domSanitizer: DomSanitizer) {
    }


    ngOnInit() {
        this.setBg();
    }

    setBg() {
        this.bypassedImage = this.domSanitizer.bypassSecurityTrustResourceUrl(this.bgSource);
    }

    ngAfterViewInit() {
        this.ctx = this.canvas.nativeElement.getContext("2d")!;
        // this.canvas.nativeElement.addEventListener("mousedown", (event) => {
        //     this.downEvent(event);
        // });
        this.canvas.nativeElement.addEventListener("mousedown", (event) => {
            event.preventDefault();
            this.downEvent(event, event);
        });
        this.canvas.nativeElement.addEventListener("touchstart", (event) => {
            event.preventDefault();
            this.downEvent(event, touchEventToTouch(event));
        });
        this.canvas.nativeElement.addEventListener("mousemove", (event) => {
            this.moveEvent(event, event);
        });
        this.canvas.nativeElement.addEventListener("touchmove", (event) => {
            event.preventDefault();
            this.moveEvent(event, touchEventToTouch(event));
        });
        this.canvas.nativeElement.addEventListener("mouseup", (event) => {
            this.upEvent(event, event);
        });
        this.canvas.nativeElement.addEventListener("touchend", (event) => {
            event.preventDefault();
            this.upEvent(event, touchEventToTouch(event));
        });
    }

    ngOnChanges(changes: SimpleChanges) {
        if (changes.bgSource) {
            this.setBg();
        }
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
        // this.wrapper.nativeElement.style = "height: 300px";
        this.imgHeight = this.bgImage.nativeElement.clientHeight;
        this.canvas.nativeElement.width = Math.max(this.bgImage.nativeElement.clientWidth, this.wrapper.nativeElement.clientWidth - 50);
        this.canvas.nativeElement.height = Math.max(this.bgImage.nativeElement.clientHeight, this.getWrapperHeight() - 20);
        if (this.imgLoadCallback) {
            this.imgLoadCallback(this);
        }
    }

    getWrapperHeight(): number {
        const min = 300;
        const max = 1024;

        return Math.min(Math.max(min, this.imgHeight) + 100, max);
    }

    /**
     * Sets the optional function to call whenever mouse is pressed (whether drawing enabled or not)
     * @param cb function to execute on click
     */
    public setClickCallback(cb: (arg0: DrawCanvasComponent, arg1: number, arg2: number) => void) {
        this.clickCallback = cb;
    }

    /**
     * Starts the drawing and calls the callback function for clicks
     */
    downEvent(event: Event, e: MouseOrTouch): void {
        if (!(e instanceof MouseEvent && (e.button == 1 || e.button == 2))) {  // allow inspect element and scrolling
            event.preventDefault();
        }
        const {x, y} = posToRelative(this.canvas.nativeElement, e);
        if (this.drawingEnabled && e instanceof MouseEvent && !(e.button == 1 || e.button == 2)) {
            this.drawStarted = true;
            this.startX = x;
            this.startY = y;
            this.startSegmentDraw(posToRelative(this.canvas.nativeElement, e));
        }
        if (this.clickCallback) {
            this.clickCallback(this, x, y);
        }
    }

    /**
     * Handles drawing new image when mouse is moved
     * TODO: check if double-layered canvas is needed (for now we re-draw everything every time mouse moves during draw)
     */
    moveEvent(event: Event, e: MouseOrTouch): void {
        event.preventDefault();
        if (!this.drawStarted) {
            return;
        }
        const pxy = posToRelative(this.canvas.nativeElement, e);
        const {x, y} = pxy;

        if (this.drawType == DrawType.Circle || this.drawType == DrawType.Rectangle) {
            this.redrawAll();
            this.objX = Math.min(x, this.startX);
            this.objY = Math.min(y, this.startY);
            this.objW = Math.abs(x - this.startX);
            this.objH = Math.abs(y - this.startY);
            this.setContextSettingsFromOptions();
            if (this.drawType == DrawType.Circle) {
                this.drawPreviewCircle();
            } else {
                this.drawPreviewRectangle();
            }
        } else {
            if (this.drawType == DrawType.Line) {
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
    upEvent(event: Event, p: MouseOrTouch): void {
        if (!this.drawStarted) {
            return;
        }
        this.drawStarted = false;
        if (this.drawType == DrawType.Circle) {
            const circle: ICircle = {
                    type: "circle",
                    drawData: this.makeFullRectangleOrCircle(),
            };
            this.drawData.push(circle);
        } else if (this.drawType == DrawType.Rectangle) {
            const rect: IRectangle = {
                type: "rectangle",
                drawData: this.makeFullRectangleOrCircle(),

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

    /**
     * Returns full shape information for ellipse or rectangle based on current settings
     */
    makeFullRectangleOrCircle(): IRectangleOrCircle {
        let fillOrBorder = {};
        if (this.drawFill) {
            fillOrBorder = {fillColor: this.color};
        } else {
            fillOrBorder = {lineWidth: this.w};
        }
        return {
            x: this.objX,
            y: this.objY,
            w: this.objW,
            h: this.objH,
            opacity: this.opacity,
            color: this.color,
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
    };

    /**
     * Draw rectangle during mouse move
     */
    drawPreviewRectangle() {
        this.drawRectangle(this.makeObj());
    }

    /**
     * Draw circle during mouse move
     */
    drawPreviewCircle() {
        this.drawCircle(this.makeObj());
    }

    /**
     * Return dimension for current shape dimensions based on last draw event
     */
    makeObj(): IRectangleOrCircle {
        return {
            x: this.objX,
            y: this.objY,
            w: this.objW,
            h: this.objH,
            fillColor: this.drawFill ? this.color : undefined,
        };
    }

    /**
     * Start drawing freehand
     */
    startSegmentDraw(pxy: IPoint) {
        if (!pxy) {
            return;
        }
        const p: TuplePoint = [Math.round(pxy.x), Math.round(pxy.y)];
        const ns: ILineSegment = {lines: [p]};
        ns.color = this.color;
        ns.w = this.w;
        if (this.opacity < 1) {
            ns.opacity = this.opacity;
        }
        this.freeDrawing = ns;
        this.prevPos = p;
    }

    /**
     * Clears the entire canvas
     */
    clear(): void {
        this.ctx.clearRect(0, 0, this.canvas.nativeElement.width, this.canvas.nativeElement.height);
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
     * @param data array of freehand drawings, circles and rectangles
     */
    drawFromArray(data: DrawObject[]) {
        for (const object of data) {
            if (object.type == "circle") {
                this.setContextSettingsFromObject(object.drawData);
                this.drawCircle(object.drawData);
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
     * @param obj IRectangleOrCircle object with options
     */
    setContextSettingsFromObject(obj: IRectangleOrCircle) {
        this.ctx.strokeStyle = obj.color ?? this.color;
        this.ctx.lineWidth = obj.lineWidth ?? this.w;
        this.ctx.fillStyle = obj.fillColor ?? "transparent";
        this.ctx.globalAlpha = obj.opacity ?? this.opacity;
    }

    /**
     * Sets canvas options (line widths, colors, opacity) based on current toolbar settings
     */
    setContextSettingsFromOptions() {
        this.ctx.globalAlpha = this.opacity;
        this.ctx.strokeStyle = this.color;
        this.ctx.lineWidth = this.w;
        this.ctx.fillStyle = this.color;
    }

    /**
     * Draws a circle (or ellipse)
     * @param circle in IRectangleOrCircle format
     */
    drawCircle(circle: IRectangleOrCircle) {
        const ratio = circle.w / circle.h;
        this.ctx.save();
        this.ctx.beginPath();
        this.ctx.scale(ratio, 1);
        const r = Math.min(circle.w / ratio, circle.h) / 2;
        this.ctx.arc((circle.x + circle.w / 2) / ratio, circle.y + circle.h / 2, r, 0, 2 * Math.PI);
        this.ctx.restore();
        circle.fillColor ? this.ctx.fill() : this.ctx.stroke();
    }

    /**
     * Draws a rectangle
     * @param rectangle in IRectangleOrCircle format
     */
    drawRectangle(rectangle: IRectangleOrCircle) {
            // TODO: Draw border with own settings but custom fill color
            rectangle.fillColor ?
                this.ctx.fillRect(rectangle.x, rectangle.y, rectangle.w, rectangle.h) :
                this.ctx.strokeRect(rectangle.x, rectangle.y, rectangle.w, rectangle.h);
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
    line(p1: TuplePoint | undefined, p2: IPoint) {
        if (!p1 || !p2) {
            return;
        }
        this.ctx.beginPath();
        this.ctx.strokeStyle = this.color;
        this.ctx.lineWidth = this.w;
        this.ctx.globalAlpha = this.opacity;
        this.ctx.moveTo(p1[0], p1[1]);
        this.ctx.lineTo(p2.x, p2.y);
        this.ctx.stroke();
    }

    /**
     * Adds and draws a point to current freehand drawing
     * @param pxy
     */
    addPoint(pxy: IPoint) {
        if (!pxy || !this.freeDrawing) {
            return;
        }

        const p: TuplePoint = [Math.round(pxy.x), Math.round(pxy.y)];
        this.freeDrawing.lines.push(p);
        if (this.drawType != DrawType.Line) {
            this.prevPos = p;
        }
    }

    // TODO Check if redundant
    startSegment(pxy: IPoint) {
        const p: TuplePoint = [Math.round(pxy.x), Math.round(pxy.y)];
        this.freeDrawing = {lines: [p]};
        this.prevPos = p;
    }

    /**
     * Returns current drawing progress in an array
     */
    getDrawing(): DrawObject[] {
        return this.drawData;
    }

    /**
     * Returns dimensions (start coordinate, max width/height) on current drawing progress
     */
    getCurrentDrawingDimensions(): { x: number, y: number, w: number, h: number } {
        return getDrawingDimensions(this.getDrawing());
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
        this.persistentDrawData = data;
        this.redrawAll();
    }

}

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        DrawCanvasComponent,
    ], imports: [
        BrowserModule,
        DrawToolbarModule,
        FormsModule,
    ],
    exports: [DrawCanvasComponent],
})
export class DrawCanvasModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
    }
}

const bootstrapFn = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(DrawCanvasModule);
};

const angularJsModule = createDowngradedModule(bootstrapFn);
doDowngrade(angularJsModule, "drawCanvas", DrawCanvasComponent);
export const moduleDefs = [angularJsModule];
