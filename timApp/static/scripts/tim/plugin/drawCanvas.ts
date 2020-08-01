import {
    ApplicationRef,
    Component,
    DoBootstrap,
    ElementRef,
    Input,
    NgModule,
    OnInit,
    StaticProvider,
    ViewChild,
} from "@angular/core";
import {BrowserModule, DomSanitizer, SafeResourceUrl} from "@angular/platform-browser";
import {DrawToolbarModule, DrawType, IDrawVisibleOptions} from "tim/plugin/drawToolbar";
import {ILineSegment, IPoint, IRectangleOrCircle, TuplePoint} from "tim/plugin/imagextypes";
import {posToRelative} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {drawFreeHand} from "tim/plugin/imagex";

export class FreeDrawing {
    public drawData: ILineSegment;

    constructor(freeDrawing: ILineSegment) {
        this.drawData = freeDrawing;
    }
}

export class Circle {
    public drawData: IRectangleOrCircle;

    constructor(circle: IRectangleOrCircle) {
        this.drawData = circle;
    }
}

export class Rectangle {
    public drawData: IRectangleOrCircle;

    constructor(rectangle: IRectangleOrCircle) {
        this.drawData = rectangle;
    }
}

@Component({
    selector: "draw-canvas",
    template: `
        <div #wrapper>
            <div class="canvascontainer" style="width: 0px; height: 0px; overflow: visible">
                <canvas #drawbase class="drawbase" style="border:1px solid #000000; position: relative;">
                </canvas>
            </div>
            <img #backGround *ngIf="bypassedImage" [src]="bypassedImage" (load)="onImgLoad()">
        </div>
        <draw-toolbar [(enabled)]="drawingAnything" [(drawType)]="drawType"
                      [(color)]="color" [(fill)]="drawFill" [undo]="undo"
                      [(w)]="w"></draw-toolbar>
    `,
})
export class DrawCanvasComponent implements OnInit {
    @Input() public bgSource = "";
    bypassedImage: SafeResourceUrl = "";
    @ViewChild("drawbase") canvas!: ElementRef<HTMLCanvasElement>;
    @ViewChild("wrapper") wrapper!: ElementRef<HTMLElement>;
    @Input() imgLoadCallback?: (arg0: this) => void;

    drawingAnything = true;
    drawType = DrawType.Freehand;
    color = "red";
    w = 3;
    opacity = 1;
    drawFill = false;
    ctx!: CanvasRenderingContext2D;

    clickCallback?: (arg0: this) => void;

    drawStarted = false;
    drawData: Array<FreeDrawing | Circle | Rectangle> = [];
    freeDrawing?: ILineSegment;
    private prevPos?: TuplePoint;
    private startX: number = 0;
    private startY: number = 0;
    private objX: number = 0;
    private objY: number = 0;
    private objW: number = 0;
    private objH: number = 0;

    constructor(el: ElementRef<HTMLElement>, private domSanitizer: DomSanitizer) {
    }


    ngOnInit() {
        this.bypassedImage = this.domSanitizer.bypassSecurityTrustResourceUrl(this.bgSource);
    }

    ngAfterViewInit() {
        this.ctx = this.canvas.nativeElement.getContext("2d")!;
        this.canvas.nativeElement.addEventListener("mousedown", (event) => {
            this.clickStart(event);
        });
        this.canvas.nativeElement.addEventListener("mousemove", (event) => {
            this.clickMove(event);
        });
        this.canvas.nativeElement.addEventListener("mouseup", (event) => {
            this.clickFinish(event);
        });
    }


    onImgLoad(): void {
        this.canvas.nativeElement.width = this.wrapper.nativeElement.clientWidth;
        this.canvas.nativeElement.height = this.wrapper.nativeElement.clientHeight;
        if (this.imgLoadCallback) {
            this.imgLoadCallback(this);
        }
    }

    public setClickCallback(cb: (arg0: DrawCanvasComponent) => void) {
        this.clickCallback = cb;
    }

    clickStart(e: MouseEvent): void {
        if (this.drawingAnything) {
            this.drawStarted = true;
            this.startX = e.offsetX;
            this.startY = e.offsetY;
            this.startSegmentDraw(posToRelative(this.canvas.nativeElement, e));
        }
        if (this.clickCallback) {
            this.clickCallback(this);
        }
    }

    clickMove(e: MouseEvent): void {
        if (!this.drawStarted) {
            return;
        }
        this.clear();
        this.redrawAll();
        if (this.drawType == DrawType.Circle || this.drawType == DrawType.Rectangle) {
            this.objX = Math.min(e.offsetX, this.startX);
            this.objY = Math.min(e.offsetY, this.startY);
            this.objW = Math.abs(e.offsetX - this.startX);
            this.objH = Math.abs(e.offsetY - this.startY);
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
            const pxy = posToRelative(this.canvas.nativeElement, e); // HANDLE MOUSEORTOUCH HERE
            this.line(this.prevPos, pxy);
            this.addPoint(pxy);
        }

    }

    clickFinish(e: MouseEvent): void {
        if (!this.drawStarted) {
            return;
        }
        this.drawStarted = false;
        if (this.drawType == DrawType.Circle) {
            const circle = new Circle({
                x: this.objX,
                y: this.objY,
                w: this.objW,
                h: this.objH,
                opacity: this.opacity,
                color: this.color,
                fillColor: this.drawFill ? this.color : undefined,
                lineWidth: this.w,
            });
            this.drawData.push(circle);
        } else if (this.drawType == DrawType.Rectangle) {
            const rect = new Rectangle({
                x: this.objX,
                y: this.objY,
                w: this.objW,
                h: this.objH,
                opacity: this.opacity,
                color: this.color,
                fillColor: this.drawFill ? this.color : undefined,
                lineWidth: this.w,
            });
            this.drawData.push(rect);
        } else if (this.freeDrawing) {
            this.drawData.push(new FreeDrawing(this.freeDrawing));
        }
    }

    undo = (e?: Event) => {
        if (e) {
            e.preventDefault();
        }
        this.drawData.pop();
        this.clear();
        this.redrawAll();
    };

    drawPreviewRectangle() {
        this.drawRectangle(this.makeObj());
    }

    drawPreviewCircle() {
        this.drawCircle(this.makeObj());
    }

    makeObj(): IRectangleOrCircle {
        return {x: this.objX, y: this.objY, w: this.objW, h: this.objH, fillColor: this.drawFill ? this.color : undefined};
    }

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

    clear(): void {
        this.ctx.clearRect(0, 0, this.canvas.nativeElement.width, this.canvas.nativeElement.height);
    }

    redrawAll(): void {
        for (const object of this.drawData) {
            if (object instanceof Circle) {
                this.setContextSettingsFromObject(object.drawData);
                this.drawCircle(object.drawData);
            } else if (object instanceof Rectangle) {
                this.setContextSettingsFromObject(object.drawData);
                this.drawRectangle(object.drawData);
            } else {
                drawFreeHand(this.ctx, [object.drawData]);
            }
        }
        if (this.freeDrawing) {
            drawFreeHand(this.ctx, [this.freeDrawing]);
        }
    }

    setContextSettingsFromObject(obj: IRectangleOrCircle) {
        this.ctx.strokeStyle = obj.color ?? this.color;
        this.ctx.lineWidth = obj.lineWidth ?? this.w;
        this.ctx.fillStyle = obj.fillColor ?? "transparent";
        this.ctx.globalAlpha = obj.opacity ?? this.opacity;
    }

    setContextSettingsFromOptions() {
        this.ctx.globalAlpha = this.opacity;
        this.ctx.strokeStyle = this.color;
        this.ctx.lineWidth = this.w;
        this.ctx.fillStyle = this.color;
    }

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

    drawRectangle(rectangle: IRectangleOrCircle) {
            // TODO: Draw border with own settings but custom fill color
            rectangle.fillColor ?
                this.ctx.fillRect(rectangle.x, rectangle.y, rectangle.w, rectangle.h) :
                this.ctx.strokeRect(rectangle.x, rectangle.y, rectangle.w, rectangle.h);
    }

    popPoint(minlen: number) {
        if (!this.freeDrawing) {
            return;
        }
        if (this.freeDrawing.lines.length > minlen) {
            this.freeDrawing.lines.pop();
        }
    }

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

    startSegment(pxy: IPoint) {
        const p: TuplePoint = [Math.round(pxy.x), Math.round(pxy.y)];
        this.freeDrawing = {lines: [p]};
        this.prevPos = p;
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
