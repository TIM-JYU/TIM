import deepmerge from "deepmerge";
import * as t from "io-ts";
import {ApplicationRef, Component, DoBootstrap, ElementRef, NgModule, OnInit, StaticProvider} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {BrowserModule, DomSanitizer} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {DrawType, IDrawVisibleOptions} from "tim/plugin/drawToolbar";
import {$http, $sce, $timeout} from "../util/ngimport";
import {TimDefer} from "../util/timdefer";
import {defaultTimeout, MouseOrTouch, numOrStringToNumber, posToRelative, Require, to, valueOr} from "../util/utils";
import {editorChangeValue} from "../editor/editorScope";
import {ViewCtrl} from "../document/viewctrl";
import {
    CommonPropsT,
    DefaultPropsT,
    DragObjectPropsT,
    FixedObjectPropsT,
    IFakeVideo,
    ILineSegment,
    ImageXAll,
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
    SingleSize,
    SizeT,
    TargetPropsT,
    TextboxPropsT,
    TuplePoint,
    ValidCoord,
    VideoPlayer,
} from "./imagextypes";
import {AngularPluginBase} from "./angular-plugin-base.directive";

let globalPreviewColor = "#fff";

function isFakePlayer(p: VideoPlayer): p is IFakeVideo {
    return "fakeVideo" in p && p.fakeVideo;
}

export function tupleToCoords(p: TuplePoint) {
    return {x: p[0], y: p[1]};
}

function tupleToSizedOrDef(p: SizeT | undefined | null, def: ISizedPartial | undefined) {
    return p ? {width: p[0], height: p[1]} : def;
}

export class FreeHand {
    public freeDrawing: ILineSegment[];
    public redraw?: () => void;
    private videoPlayer: VideoPlayer;
    private emotion: boolean;
    private imgx: ImageXComponent;
    private prevPos?: TuplePoint;
    private lastDrawnSeg?: number;
    private lastVT?: number;
    private ctx: CanvasRenderingContext2D;

    constructor(imgx: ImageXComponent, drawData: ILineSegment[], player: HTMLVideoElement | undefined) {
        this.freeDrawing = drawData;
        this.ctx = imgx.getCanvasCtx();
        this.emotion = imgx.emotion;
        this.imgx = imgx;
        this.videoPlayer = player ?? {currentTime: 1e60, fakeVideo: true};
        if (this.emotion && this.imgx.teacherMode) {
            if (!isFakePlayer(this.videoPlayer)) {
                if (this.imgx.markup.analyzeDot) {
                    this.videoPlayer.ontimeupdate = () => this.drawCirclesVideoDot(this.ctx, this.freeDrawing, this.videoPlayer);
                } else {
                    this.videoPlayer.ontimeupdate = () => this.drawCirclesVideo(this.ctx, this.freeDrawing, this.videoPlayer);
                }
            }
        }
    }

    draw(ctx: CanvasRenderingContext2D) {
        if (this.emotion) {
            if (this.imgx.teacherMode) {
                if (isFakePlayer(this.videoPlayer)) {
                    this.drawCirclesVideo(ctx, this.freeDrawing, this.videoPlayer);
                }
            }
        } else {
            drawFreeHand(ctx, this.freeDrawing);
        }
    }

    startSegment(pxy: IPoint) {
        const p: TuplePoint = [Math.round(pxy.x), Math.round(pxy.y)];
        const ns: ILineSegment = {lines: [p]};
        if (!this.emotion) {
            ns.color = this.imgx.color;
            ns.w = this.imgx.w;
        }
        this.freeDrawing.push(ns);
        this.prevPos = p;
    }

    endSegment() {
        // this.drawingSurfaceImageData = null; // not used anywhere
    }

    startSegmentDraw(pxy: IPoint) {
        if (!pxy) {
            return;
        }
        let p: TuplePoint;
        if (this.emotion) {
            if (this.freeDrawing.length != 0) {
                return;
            }
            p = [Math.round(pxy.x), Math.round(pxy.y),
                Date.now() / 1000, this.videoPlayer.currentTime];
        } else {
            p = [Math.round(pxy.x), Math.round(pxy.y)];
        }
        const ns: ILineSegment = {lines: [p]};
        if (!this.emotion) {
            ns.color = this.imgx.color;
            ns.w = this.imgx.w;
        }
        this.freeDrawing.push(ns);
        this.prevPos = p;
    }

    addPoint(pxy: IPoint) {
        if (!pxy) {
            return;
        }
        let p: TuplePoint;
        if (this.emotion) {
            p = [Math.round(pxy.x), Math.round(pxy.y),
                Date.now() / 1000, this.videoPlayer.currentTime];
        } else {
            p = [Math.round(pxy.x), Math.round(pxy.y)];
        }
        const n = this.freeDrawing.length;
        if (n == 0) {
            this.startSegment(tupleToCoords(p));
        } else {
            const ns = this.freeDrawing[n - 1];
            //    ns.lines = [p];
            ns.lines.push(p);
        }
        if (!this.imgx.lineMode() || this.emotion) {
            this.prevPos = p;
        }
    }

    popPoint(minlen: number) {
        const n = this.freeDrawing.length;
        if (n == 0) {
            return;
        }
        const ns = this.freeDrawing[n - 1];
        if (ns.lines.length > minlen) {
            ns.lines.pop();
        }
    }

    popSegment(minlen: number) {
        const n = this.freeDrawing.length;
        if (n <= minlen) {
            return;
        }
        this.freeDrawing.pop();
        if (this.redraw) {
            this.redraw();
        }
    }

    addPointDraw(ctx: CanvasRenderingContext2D, pxy: IPoint) {
        if (this.imgx.lineMode()) {
            this.popPoint(1);
            if (this.redraw) {
                this.redraw();
            }
        }
        if (!this.emotion) {
            this.line(ctx, this.prevPos, pxy);
        }
        this.addPoint(pxy);
    }

    clear() {
        this.freeDrawing = [];
        if (this.redraw) {
            this.redraw();
        }
    }

    setColor(newColor: string) {
        this.imgx.color = newColor;
        if (this.prevPos) {
            this.startSegment(tupleToCoords(this.prevPos));
        }
    }

    setWidth(newWidth: number) {
        this.imgx.w = newWidth;
        if (this.imgx.w < 1) {
            this.imgx.w = 1;
        }
        if (this.prevPos) {
            this.startSegment(tupleToCoords(this.prevPos));
        }
    }

    incWidth(dw: number) {
        this.setWidth(this.imgx.w + dw);
    }

    // setLineMode(newMode: boolean) {
    //     this.imgx.lineMode = newMode;
    // }
    //
    // flipLineMode() {
    //     this.setLineMode(!this.imgx.lineMode);
    // }

    line(ctx: CanvasRenderingContext2D, p1: TuplePoint | undefined, p2: IPoint) {
        if (!p1 || !p2) {
            return;
        }
        ctx.beginPath();
        ctx.strokeStyle = this.imgx.color;
        ctx.lineWidth = this.imgx.w;
        ctx.moveTo(p1[0], p1[1]);
        ctx.lineTo(p2.x, p2.y);
        ctx.stroke();
    }

    // TODO get rid of type assertions ("as number")
    drawCirclesVideoDot(ctx: CanvasRenderingContext2D, dr: ILineSegment[], videoPlayer: VideoPlayer) {
        for (const seg of dr) {
            const vt = videoPlayer.currentTime;
            // console.log("vt: " + vt + " " + this.lastDrawnSeg );
            let seg1 = -1;
            let seg2 = 0;
            let s = "";
            for (let lni = 0; lni < seg.lines.length; lni++) {
                if (vt < (seg.lines[lni][3] as number)) {
                    break;
                }
                seg1 = seg2;
                seg2 = lni;
            }

            // console.log("" + seg1 + "-" + seg2 + ": " + seg.lines[seg2][3]);
            let drawDone = false;
            if (this.imgx.markup.dotVisibleTime && this.lastVT && (vt - this.lastVT) > this.imgx.markup.dotVisibleTime) { // remove old dot if needed
                this.imgx.draw();
                drawDone = true;
            }
            if (this.imgx.markup.showVideoTime) {
                showTime(ctx, vt, 0, 0, "13px Arial");
            }
            if (this.lastDrawnSeg == seg2) {
                return;
            }
            this.lastDrawnSeg = -1;
            if (!drawDone) {
                this.imgx.draw();
            }
            if (this.imgx.markup.showVideoTime) {
                showTime(ctx, vt, 0, 0, "13px Arial");
            }
            if (seg1 < 0 || vt == 0 || seg.lines[seg2][3] == 0) {
                return;
            }

            // console.log("" + seg1 + "-" + seg2 + ": " + seg.lines[seg2][3]);

            ctx.beginPath();
            this.lastVT = vt;
            applyStyleAndWidth(ctx, seg);
            ctx.moveTo(seg.lines[seg1][0], seg.lines[seg1][1]);
            if (this.imgx.markup.drawLast) {
                ctx.lineTo(seg.lines[seg2][0], seg.lines[seg2][1]);
            }
            ctx.stroke();
            drawFillCircle(ctx, 30, seg.lines[seg2][0], seg.lines[seg2][1], "black", 0.5);
            if (this.imgx.markup.showTimes) {
                s = dateToString(seg.lines[seg2][3] as number, false);
                drawText(ctx, s, seg.lines[seg2][0], seg.lines[seg2][1], "13px Arial");
            }

            this.lastDrawnSeg = seg2;
        }
    }

    // TODO get rid of type assertions ("as number")
    drawCirclesVideo(ctx: CanvasRenderingContext2D, dr: ILineSegment[], videoPlayer: VideoPlayer) {
        if (!isFakePlayer(videoPlayer)) {
            this.imgx.draw();
        }
        const vt = videoPlayer.currentTime;
        for (const seg of dr) {
            if (seg.lines.length < 1) {
                continue;
            }
            let s = "";
            ctx.beginPath();
            applyStyleAndWidth(ctx, seg);
            ctx.moveTo(seg.lines[0][0], seg.lines[0][1]);
            if (isFakePlayer(videoPlayer)) {
                s = dateToString(seg.lines[0][2] as number, true);
                drawText(ctx, s, seg.lines[0][0], seg.lines[0][1]);
            } else {
                if (vt >= (seg.lines[0][3] as number)) {
                    drawFillCircle(ctx, 5, seg.lines[0][0], seg.lines[0][1], "black", 0.5);
                }
            }
            for (let lni = 1; lni < seg.lines.length; lni++) {
                if (vt < (seg.lines[lni][3] as number)) {
                    break;
                }
                ctx.lineTo(seg.lines[lni][0], seg.lines[lni][1]);
                if (isFakePlayer(videoPlayer)) {
                    s = dateToString(seg.lines[lni][2] as number, true);
                    drawText(ctx, s, seg.lines[lni][0], seg.lines[lni][1]);
                }
            }
            ctx.stroke();
        }
    }
}

function applyStyleAndWidth(ctx: CanvasRenderingContext2D, seg: ILineSegment) {
    ctx.strokeStyle = seg.color ?? ctx.strokeStyle;
    ctx.lineWidth = numOrStringToNumber(seg.w ?? ctx.lineWidth);
    ctx.globalAlpha = seg.opacity ?? 1;
}

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

function pad(num: number, size: number) {
    const s = "000000000" + num;
    return s.substr(s.length - size);
}

function dateToString(d: number, h: boolean) {
    const dt = new Date(d * 1000);
    let hs = "";
    if (h) {
        hs = pad(dt.getHours(), 2) + ":";
    }
    return hs + pad(dt.getMinutes(), 2) + ":" + pad(dt.getSeconds(), 2) + "." + pad(dt.getMilliseconds(), 3);
}

function drawText(ctx: CanvasRenderingContext2D, s: string, x: number, y: number, font = "10px Arial") {
    ctx.save();
    ctx.textBaseline = "top";
    ctx.font = font;
    const width = ctx.measureText(s).width;
    ctx.fillStyle = "#ffff00";
    ctx.fillRect(x, y, width + 1, parseInt(ctx.font, 10));
    ctx.fillStyle = "#000000";
    ctx.fillText(s, x, y);
    ctx.restore();
}

function showTime(ctx: CanvasRenderingContext2D, vt: number, x: number, y: number, font: string) {
    ctx.beginPath();
    const s = dateToString(vt, false);
    drawText(ctx, s, x, y, font);
    ctx.stroke();
}

const directiveTemplate = `
<div class="csRunDiv no-popup-menu">
    <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
    <div class="pluginError" *ngIf="imageLoadError" [textContent]="imageLoadError"></div>
    <h4 *ngIf="header" [innerHtml]="header"></h4>
    <p *ngIf="stem" class="stem" [innerHtml]="stem"></p>
    <div>
        <canvas class="canvas no-popup-menu"
                tabindex="1"
                width={{canvaswidth}}
                height={{canvasheight}}></canvas>
        <div class="content"></div>
    </div>
    <p class="csRunMenu">&nbsp;<button *ngIf="button"
                                       class="timButton"
                                       [disabled]="isRunning"
                                       (click)="save()">{{button}}
    </button>
        &nbsp;&nbsp;
        <button *ngIf="buttonPlay"
                [disabled]="isRunning"
                (click)="videoPlay()">{{buttonPlay}}
        </button>
        &nbsp;&nbsp;
        <button *ngIf="buttonRevert"
                [disabled]="isRunning"
                (click)="videoBeginning()">{{buttonRevert}}
        </button>
        &nbsp;&nbsp;
        <button [hidden]="!(finalanswer && userHasAnswered)"
                [disabled]="isRunning"
                (click)="showAnswer()">
            Show correct answer
        </button>
        &nbsp;&nbsp;<a *ngIf="button"
        (click)="resetExercise()">{{resetText}}</a>&nbsp;&nbsp;<a
                href="" *ngIf="muokattu" (click)="initCode()">{{resetText}}</a>
                <draw-toolbar [(enabled)]="freeHand" [(drawType)]="drawType" [undo]="passUndo"
                    [setFColor]="passSetFColor" [drawVisibleOptions]="drawVisibleOptions" [(color)]="color" [(w)]="w"></draw-toolbar>
    </p>
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
`;

function drawFillCircle(ctx: CanvasRenderingContext2D, r: number, p1: number, p2: number, c: string, tr: number) {
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

function isObjectOnTopOf(position: IPoint, object: DrawObject, grabOffset: number) {
    return object.isPointInside(position, grabOffset);
}

function areObjectsOnTopOf<T extends DrawObject>(
    position: IPoint,
    objects: T[],
    grabOffset: number | undefined,
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
    public ctx: CanvasRenderingContext2D;
    private mousePosition: IPoint;
    private freeHand: FreeHand;
    private mouseDown = false;
    private activeDragObject?: { obj: DragObject, xoffset: number, yoffset: number };
    public drawObjects: DrawObject[];
    public lines?: Line[];

    constructor(private canvas: HTMLCanvasElement, private imgx: ImageXComponent) {
        this.ctx = canvas.getContext("2d")!;
        this.drawObjects = [];
        this.mousePosition = {x: 0, y: 0};
        this.freeHand = imgx.freeHandDrawing;

        this.canvas.style.touchAction = "double-tap-zoom"; // To get IE and EDGE touch to work
        this.freeHand.redraw = () => this.draw();

        this.canvas.addEventListener("mousemove", (event) => {
            event.preventDefault();
            this.moveEvent(event, event);
        });
        this.canvas.addEventListener("touchmove", (event) => {
            event.preventDefault();
            this.moveEvent(event, this.te(event));
        });
        this.canvas.addEventListener("mousedown", (event) => {
            event.preventDefault();
            this.downEvent(event, event);
        });
        this.canvas.addEventListener("touchstart", (event) => {
            event.preventDefault();
            // this.imgx.getScope().$evalAsync(() => {
            //     this.downEvent(event, this.te(event));
            // });
            this.downEvent(event, this.te(event));
        });
        this.canvas.addEventListener("mouseup", (event) => {
            event.preventDefault();
            this.upEvent(event, event);
        });
        this.canvas.addEventListener("touchend", (event) => {
            event.preventDefault();
            this.upEvent(event, this.te(event));
        });

        if (imgx.markup.freeHandShortCuts) {
            this.canvas.addEventListener("keypress", (event) => {
                const c = String.fromCharCode(event.keyCode);
                if (event.keyCode === 26) {
                    this.freeHand.popSegment(0);
                }
                if (c === "c") {
                    this.freeHand.clear();
                }
                if (c === "r") {
                    this.freeHand.setColor("#f00");
                }
                if (c === "b") {
                    this.freeHand.setColor("#00f");
                }
                if (c === "y") {
                    this.freeHand.setColor("#ff0");
                }
                if (c === "g") {
                    this.freeHand.setColor("#0f0");
                }
                if (c === "+") {
                    this.freeHand.incWidth(+1);
                }
                if (c === "-") {
                    this.freeHand.incWidth(-1);
                }
                if (c === "1") {
                    this.freeHand.setWidth(1);
                }
                if (c === "2") {
                    this.freeHand.setWidth(2);
                }
                if (c === "3") {
                    this.freeHand.setWidth(3);
                }
                if (c === "4") {
                    this.freeHand.setWidth(4);
                }
                // if (c === "l") {
                //     this.freeHand.flipLineMode();
                // }
                if (c === "f" && imgx.freeHandShortCut) {
                    imgx.freeHand = !imgx.freeHand;
                }
            }, false);
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

    draw() {
        const canvas = this.canvas;
        setIdentityTransform(this.ctx);
        this.ctx.fillStyle = "white"; // TODO get from markup?
        this.ctx.fillRect(0, 0, canvas.width, canvas.height);

        if (this.activeDragObject) {
            const dobj = this.activeDragObject;
            const xlimits: [number, number] = dobj.obj.xlimits ?? [0, 1e100];
            const ylimits: [number, number] = dobj.obj.ylimits ?? [0, 1e100];
            if (dobj.obj.lock !== "x") {
                dobj.obj.x = toRange(xlimits, this.mousePosition.x - dobj.xoffset);
            }
            if (dobj.obj.lock !== "y") {
                dobj.obj.y = toRange(ylimits, this.mousePosition.y - dobj.yoffset);
            }
        }
        const targets = this.targets;
        for (const o of this.drawObjects) {
            o.draw();
            if (o.name === "dragobject") {
                if (this.activeDragObject) {
                    const onTopOf = areObjectsOnTopOf(this.activeDragObject.obj,
                        targets, this.imgx.grabOffset);
                    if (onTopOf && onTopOf.objectCount < onTopOf.maxObjects) {
                        onTopOf.state = TargetState.Snap;
                    }
                    const onTopOfB = areObjectsOnTopOf(o,
                        targets, this.imgx.grabOffset);
                    if (onTopOfB && o !== this.activeDragObject.obj) {
                        onTopOfB.state = TargetState.Drop;
                    }
                } else {
                    const onTopOfA = areObjectsOnTopOf(o,
                        targets, this.imgx.grabOffset);
                    if (onTopOfA) {
                        onTopOfA.state = TargetState.Drop;
                    }
                }
            } else if (o.name === "target") {
                o.state = TargetState.Normal;
            }
        }
        setIdentityTransform(this.ctx);
        if (this.lines) {
            for (const l of this.lines) {
                l.draw(this.ctx);
            }
        }
        this.freeHand.draw(this.ctx);
    }

    downEvent(event: Event, p: MouseOrTouch) {
        event.preventDefault();
        this.mousePosition = posToRelative(this.canvas, p);
        let active = areObjectsOnTopOf(this.mousePosition, this.dragobjects, 0);
        if (!active) {
            active = areObjectsOnTopOf(this.mousePosition, this.dragobjects, this.imgx.grabOffset);
        }

        if (this.imgx.emotion) {
            drawFillCircle(this.ctx, 30, this.mousePosition.x, this.mousePosition.y, "black", 0.5);
            this.freeHand.startSegmentDraw(this.mousePosition);
            this.freeHand.addPointDraw(this.ctx, this.mousePosition);
            if (this.imgx.markup.autosave) {
                this.imgx.save();
            }
        }

        if (active) {
            const array = this.drawObjects;
            array.push(array.splice(array.indexOf(active), 1)[0]); // put active last so that it gets drawn on top of others
            this.canvas.style.cursor = "pointer";
            this.activeDragObject = {
                obj: active,
                xoffset: this.mousePosition.x - active.x,
                yoffset: this.mousePosition.y - active.y,
            };
            this.draw();
        } else if (this.imgx.freeHand) {
            this.canvas.style.cursor = "pointer";
            this.mouseDown = true;
            if (!this.imgx.emotion) {
                this.freeHand.startSegmentDraw(this.mousePosition);
            }
        }

        if (this.imgx.attrsall.preview) {
            this.imgx.coords = `[${Math.round(this.mousePosition.x)}, ${Math.round(this.mousePosition.y)}]`;
            editorChangeValue(["position:"], this.imgx.coords);
        }
    }

    moveEvent(event: Event, p: MouseOrTouch) {
        event.preventDefault();
        if (this.activeDragObject) {
            if (event != p) {
                event.preventDefault();
            }
            this.mousePosition = posToRelative(this.canvas, p);
            if (!this.imgx.emotion) {
                this.draw();
            }
        } else if (this.mouseDown) {
            if (event != p) {
                event.preventDefault();
            }
            if (!this.imgx.emotion) {
                this.freeHand.addPointDraw(this.ctx, posToRelative(this.canvas, p));
            }
        }
    }

    upEvent(event: Event, p: MouseOrTouch) {
        if (this.activeDragObject) {
            this.canvas.style.cursor = "default";
            if (event != p) {
                event.preventDefault();
            }
            this.mousePosition = posToRelative(this.canvas, p);

            const isTarget = areObjectsOnTopOf(this.activeDragObject.obj,
                this.targets, undefined);

            if (isTarget) {
                if (isTarget.objectCount < isTarget.maxObjects) {
                    if (isTarget.snap) {
                        this.activeDragObject.obj.x = isTarget.x + isTarget.snapOffset.x;
                        this.activeDragObject.obj.y = isTarget.y + isTarget.snapOffset.y;
                    }
                }
            }

            this.activeDragObject = undefined;
            if (!this.imgx.emotion) {
                this.draw();
            }

        } else if (this.mouseDown) {
            this.canvas.style.cursor = "default";
            this.mouseDown = false;
            this.freeHand.endSegment();
        }
    }

    te(event: TouchEvent) {
        event.preventDefault();
        return event.touches[0] || event.changedTouches[0];
    }

    addRightAnswers(answers: RightAnswerT[]) {
        this.lines = [];
        const objects = this.drawObjects;
        for (const d of answers) {
            for (const o of objects) {
                if (o.id === d.id) {
                    const line = new Line("green", 2, o, tupleToCoords(d.position));
                    this.lines.push(line);
                }
            }
        }
        this.draw();
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
        default:
            throw new Error(`Unexpected alignment: ${a}`);
    }
    return alignOffset;
}

class Pin {
    private pos: RequiredNonNull<IPinPosition>;

    constructor(
        private values: RequiredNonNull<PinPropsT>,
        defaultAlign: PinAlign,
    ) {
        const a = values.position.align ?? defaultAlign;
        const {x, y} = alignToDir(a, 1 / Math.sqrt(2));
        const lngth = values.length;
        this.pos = {
            align: a,
            coord: tupleToCoords(values.position.coord ?? [x * lngth, y * lngth]),
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
            ctx.lineTo(-(this.pos.coord.x - this.pos.start.x), -(this.pos.coord.y - this.pos.start.y));
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

abstract class ObjBase<T extends RequireExcept<CommonPropsT, OptionalCommonPropNames>> {
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

    protected constructor(protected ctx: CanvasRenderingContext2D,
                          protected values: T,
                          public did: string) {
        const z = values.position;
        this.x = z[0];
        this.y = z[1];
        this.a = -(valueOr(this.values.a, 0));
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
                    [this.pendingImage, img] = loadImage(this.values.imgproperties.src);
                    if (this.values.imgproperties.textbox) {
                        this.childShapes.push({
                            p: this.values.textboxproperties ? tupleToCoords(this.values.textboxproperties.position ?? [0, 0]) : {
                                x: 0,
                                y: 0,
                            },
                            s: textboxFromProps(this.values, s, () => this.overrideColor, this.id),
                        });
                    }
                } else {
                    [this.pendingImage, img] = loadImage("/static/images/Imagex.png");
                }
                this.mainShape = new DImage(img, s);
                break;
            case "ellipse":
                this.mainShape = new Ellipse(() => (this.overrideColor ?? this.values.color) ?? "black", 2, s);
                break;
            case "textbox":
                this.mainShape = textboxFromProps(this.values, s, () => this.overrideColor, this.id);
                break;
            case "vector":
                const vprops = this.values.vectorproperties ?? {};
                this.mainShape = new Vector(
                    () => (this.overrideColor ?? vprops.color) ?? this.values.color ?? "black",
                    2,
                    s,
                    vprops.arrowheadwidth,
                    vprops.arrowheadlength,
                );
                break;
            default: // rectangle
                this.mainShape = new Rectangle(() => (this.overrideColor ?? this.values.color) ?? "black",
                    "transparent",
                    this.values.borderWidth ?? 2,
                    0,
                    s);
                break;
        }
    }

    isPointInside(p: IPoint, offset?: number) {
        // TODO check child shapes too
        return this.mainShape.isNormalizedPointInsideShape(this.normalizePoint(p), offset);
    }

    normalizePoint(p: IPoint) {
        const {x, y} = this.center;
        const xdiff = (p.x - x);
        const ydiff = (p.y - y);
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
        this.ctx.translate(-this.mainShape.size.width / 2, -this.mainShape.size.height / 2);
        for (const s of this.childShapes) {
            this.ctx.save();
            this.ctx.translate(s.p.x + s.s.size.width / 2, s.p.y + s.s.size.height / 2);
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

function textboxFromProps(values: { textboxproperties?: TextboxPropsT | null, color?: string | null },
                          s: ISizedPartial | undefined,
                          overrideColorFn: () => string | undefined,
                          defaultText: string) {
    const props = values.textboxproperties ?? {};
    return new Textbox(
        () => (overrideColorFn() ?? props.borderColor) ?? values.color ?? "black",
        props.textColor ?? "black",
        props.fillColor ?? "white",
        valueOr(props.borderWidth, 2),
        valueOr(props.cornerradius, 2),
        tupleToSizedOrDef(props.size, s),
        props.text ?? defaultText,
        props.font ?? "14px Arial",
    );
}

class DragObject extends ObjBase<RequireExcept<DragObjectPropsT, OptionalDragObjectPropNames>> {

    public name: "dragobject" = "dragobject";
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

    constructor(ctx: CanvasRenderingContext2D,
                values: RequireExcept<DragObjectPropsT, OptionalDragObjectPropNames>,
                defId: string) {
        super(ctx, values, defId);
        this.pin = new Pin({
            color: values.pin.color ?? "blue",
            dotRadius: valueOr(values.pin.dotRadius, 3),
            length: valueOr(values.pin.length, 15),
            linewidth: valueOr(values.pin.linewidth, 2),
            position: values.pin.position ?? {},
            visible: valueOr(values.pin.visible, true),
        }, this.values.type === "vector" ? "west" : "northwest");
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

class Target extends ObjBase<RequireExcept<TargetPropsT, OptionalTargetPropNames>> {
    public name: "target" = "target";
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

    constructor(ctx: CanvasRenderingContext2D,
                values: RequireExcept<TargetPropsT, OptionalTargetPropNames>,
                defId: string) {
        super(ctx, values, defId);
    }
}

class FixedObject extends ObjBase<RequireExcept<FixedObjectPropsT, OptionalFixedObjPropNames>> {
    public name: "fixedobject" = "fixedobject";

    constructor(ctx: CanvasRenderingContext2D,
                values: RequireExcept<FixedObjectPropsT, OptionalFixedObjPropNames>,
                defId: string) {
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
    protected constructor(protected explicitSize: ISizedPartial | undefined) {

    }

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
                return {width: this.explicitSize.width, height: this.explicitSize.width * p.height / p.width};
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
        return p.x >= -r1 / 2 && p.x <= r1 / 2 &&
            p.y >= -r2 / 2 && p.y <= r2 / 2;
    }

    abstract draw(ctx: CanvasRenderingContext2D): void;
}

class Line {
    constructor(
        private readonly color: string,
        private readonly lineWidth: number,
        private readonly beg: IPoint,
        private readonly end: IPoint,
    ) {

    }

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
        size: ISizedPartial | undefined,
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
        return (Math.pow(p.x, 2) / Math.pow(r1 / 2, 2)) +
            (Math.pow(p.y, 2) / Math.pow(r2 / 2, 2)) <= 1;
    }
}

class Rectangle extends Shape {
    constructor(
        protected readonly color: () => string,
        protected readonly fillColor: string,
        protected readonly lineWidth: number,
        protected readonly cornerRadius: number,
        size: ISizedPartial | undefined,
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
        ctx.arc(width / 2 - this.cornerRadius, -height / 2 + this.cornerRadius,
            this.cornerRadius, 1.5 * Math.PI, 0);
        ctx.lineTo(width / 2, height / 2 - this.cornerRadius);
        ctx.arc(width / 2 - this.cornerRadius, height / 2 - this.cornerRadius,
            this.cornerRadius, 0, 0.5 * Math.PI);
        ctx.lineTo(-width / 2 + this.cornerRadius, height / 2);
        ctx.arc(-width / 2 + this.cornerRadius, height / 2 - this.cornerRadius,
            this.cornerRadius, 0.5 * Math.PI, Math.PI);
        ctx.lineTo(-width / 2, -height / 2 + this.cornerRadius);
        ctx.arc(-width / 2 + this.cornerRadius, -height / 2 +
            this.cornerRadius, this.cornerRadius, Math.PI, 1.5 * Math.PI);
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
        protected readonly font: string,
    ) {
        super(size);
        this.lines = text.split("\n");
        auxctx.font = this.font;
        const lineWidths = this.lines.map((line) => auxctx.measureText(line).width);
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
        this.rect = new Rectangle(borderColor, fillColor, lineWidth, cornerRadius, this.size);
    }

    get preferredSize() {
        return {
            width: this.textwidth + this.leftMargin + this.rightMargin,
            height: (this.textHeight * this.lines.length)
                + this.topMargin + this.bottomMargin,
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
        arrowHeadLength: number | undefined,
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
        ctx.lineTo(width - this.arrowHeadLength, height / 2 +
            this.arrowHeadWidth / 2);
        ctx.lineTo(width, height / 2);
        ctx.lineTo(width - this.arrowHeadLength,
            height / 2 - this.arrowHeadWidth / 2);
        ctx.lineTo(width - this.arrowHeadLength, 0);
        ctx.lineTo(0, 0);
        ctx.fill();
    }
}

class DImage extends Shape {
    constructor(
        private readonly image: HTMLImageElement,
        size: ISizedPartial | undefined,
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

function baseDefs(objType: ObjectTypeT, color: string): RequireExcept<DefaultPropsT, OptionalPropNames> {
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
    template: directiveTemplate,
})
class ImageXComponent extends AngularPluginBase<t.TypeOf<typeof ImageXMarkup>,

    t.TypeOf<typeof ImageXAll>,
    typeof ImageXAll> implements OnInit {
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
        return this.markup.buttonPlay ?? (this.videoPlayer ? "Aloita/pysäytä" : null);
    }

    get buttonRevert() {
        return this.markup.buttonRevert ?? (this.videoPlayer ? "Video alkuun" : null);
    }

    get teacherMode() {
        return this.vctrl.teacherMode;
    }

    get grabOffset() {
        return isTouchDevice() ? this.markup.extraGrabAreaHeight : 0;
    }

    public w = 0;
    public freeHand = false; // Drawing enabled
    public drawType: DrawType = DrawType.Freehand;
    public drawVisibleOptions: IDrawVisibleOptions = {
        enabled: true,
        freeHand: true,
        lineMode: true,
        color: true,
        w: true,
    };
    public color = "";
    public freeHandDrawing!: FreeHand;
    public coords = "";
    public drags: DragObject[] = [];
    public userHasAnswered = false;

    public muokattu: boolean;
    public result: string;
    public error?: string;
    private cursor: string;
    private tries = 0;
    public previewColor = "";
    public isRunning: boolean = false;
    private vctrl!: Require<ViewCtrl>;
    public replyImage?: string;
    public replyHTML?: string;
    private canvas!: HTMLCanvasElement;
    private dt!: DragTask;
    private answer?: IAnswerResponse;
    private embed: boolean = false;

    draw() {
        this.dt.draw();
    }

    constructor(el: ElementRef, http: HttpClient, domSanitizer: DomSanitizer) {
        super(el, http, domSanitizer);
        this.muokattu = false;
        this.result = "";
        this.cursor = "\u0383"; // "\u0347"; // "\u02FD";
    }

    getCanvasCtx() {
        return this.canvas.getContext("2d")!;
    }

    async ngOnInit() {
        super.ngOnInit();
        // timeout required; otherwise the canvas element will be overwritten with another by Angular
        await $timeout();
        this.canvas = this.element.find(".canvas")[0] as HTMLCanvasElement;
        this.element[0].addEventListener("touchstart", (event) => {
            // event.preventDefault();
        });

        this.element[0].addEventListener("touchmove", (event) => {
            // event.preventDefault();
        });

        this.tries = this.attrsall.info?.earlier_answers ?? 0;
        this.freeHandDrawing = new FreeHand(this,
            this.attrsall.state?.freeHandData ?? [],
            this.videoPlayer);
        if (this.isFreeHandInUse) {
            this.freeHand = true;
        }

        this.w = this.markup.freeHandWidth;
        this.color = this.markup.freeHandColor;
        // this.lineMode = this.markup.freeHandLine;
        if (this.markup.freeHandLine) {
            this.drawType = DrawType.Line;
        }

        const dt = new DragTask(this.canvas, this);
        this.dt = dt;

        const userObjects = this.markup.objects;

        const userTargets = this.markup.targets;
        const userFixedObjects = this.markup.fixedobjects;
        const fixedobjects = [];
        const targets = [];
        const objects = [];
        const ctx = this.getCanvasCtx();

        if (this.markup.background) {
            const background = new FixedObject(
                ctx, {
                    a: this.markup.background.a ?? 0,
                    imgproperties: {src: this.markup.background.src, textbox: false},
                    position: [0, 0],
                    size: this.markup.background.size,
                    type: "img",
                },
                "background");
            dt.drawObjects.push(background);
        }

        this.error = "";

        // We don't ever want to concatenate arrays when merging.
        const opts: deepmerge.Options = {arrayMerge: (destinationArray, sourceArray, options) => sourceArray};

        let fixedDef = deepmerge<RequireExcept<FixedObjectPropsT, OptionalFixedObjPropNames>>(
            baseDefs("rectangle", "blue"),
            this.markup.defaults ?? {},
            opts,
        );

        if (userFixedObjects) {
            for (let i = 0; i < userFixedObjects.length; i++) {
                fixedDef = deepmerge(fixedDef, userFixedObjects[i], opts);
                fixedobjects.push(new FixedObject(ctx, fixedDef, "fix" + (i + 1)));
            }
        }

        let targetDef = deepmerge<RequireExcept<TargetPropsT, OptionalTargetPropNames>>(
            baseDefs("rectangle", "blue"),
            this.markup.defaults ?? {},
            opts,
        );
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

        let dragDef = deepmerge<RequireExcept<DragObjectPropsT, OptionalDragObjectPropNames>>(
            baseDefs("textbox", "black"),
            this.markup.defaults ?? {},
            opts,
        );
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
                    for (const ud of userDrags) {
                        if (o.did === ud.did) {
                            o.x = ud.position[0];
                            o.y = ud.position[1];
                        }
                    }
                }
            }
        }

        dt.drawObjects = [...dt.drawObjects, ...fixedobjects, ...targets, ...objects];
        for (const d of dt.drawObjects) {
            if (d.pendingImage) {
                const r = await d.pendingImage;
                if (r instanceof Event) {
                    this.imageLoadError = `Failed to load image ${(r.target as Element).getAttribute("src")}`;
                    break;
                }
            }
        }
        if (!this.imageLoadError) {
            this.imageLoadError = null;
        }
        // console.log(dt.drawObjects);
        dt.draw();

        this.previewColor = globalPreviewColor;
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
        this.freeHandDrawing.popSegment(0);
    }

    lineMode(): boolean {
        return this.drawType == DrawType.Line;
    }

    passSetFColor = (color: string) => {
        console.log("Setting color", color);
        this.setFColor(color);
    };

    setFColor(color: string) {
        this.freeHandDrawing.setColor(color);
    }

    getPColor() {
        if (this.attrsall.preview) {
            globalPreviewColor = this.previewColor;
            editorChangeValue(["[a-zA-Z]*[cC]olor[a-zA-Z]*:"], `"${globalPreviewColor}"`);
        }
    }

    save() {
        this.doSave(false);
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
                freeHandData: this.freeHandDrawing.freeDrawing,
            },
        };
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to($http<{
            web: {
                error?: string, result: string, tries: number, answer: IAnswerResponse,
            },
        }>({method: "PUT", url, data: params, timeout: defaultTimeout},
        ));
        this.isRunning = false;
        if (r.ok) {
            const data = r.result.data;
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
        this.result = "";
        this.freeHandDrawing.clear();

        for (const obj of this.drags) {
            obj.resetPosition();
        }

        this.dt.lines = undefined;

        // Draw the exercise so that reset appears instantly.
        this.dt.draw();
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
                freeHandData: this.freeHandDrawing.freeDrawing,
                nosave: false,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const url = this.pluginMeta.getAnswerUrl();

        const r = await to($http<{
            web: {
                error?: string,
                result: string,
                tries: number,
                "-replyImage": string,
                "-replyHTML": string,
            },
        }>({method: "PUT", url, data: params, timeout: defaultTimeout},
        ));
        this.isRunning = false;
        if (r.ok) {
            const data = r.result.data;
            this.error = data.web.error;
            this.result = data.web.result;
            this.tries = data.web.tries;
            this.userHasAnswered = true;
            this.replyImage = data.web["-replyImage"];
            this.replyHTML = data.web["-replyHTML"];
        } else {
            this.error = "Ikuinen silmukka tai jokin muu vika?";
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
        return this.max_tries && this.tries >= this.max_tries;
    }
}

// imagexApp.component("imagexRunner", {
//     bindings: pluginBindings,
//     controller: ImageXController,
//     require: {
//         vctrl: "^timView",
//     },
//     template: directiveTemplate,
// });
// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        ImageXComponent,
    ],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
    ],
})
export class ImagexModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
    }
}

const bootstrapFn = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(ImagexModule);
};

const angularJsModule = createDowngradedModule(bootstrapFn);
doDowngrade(angularJsModule, "imagexRunner", ImageXComponent);
export const moduleDefs = [angularJsModule];
