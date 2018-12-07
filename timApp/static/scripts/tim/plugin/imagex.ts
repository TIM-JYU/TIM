import angular, {IRootElementService, IScope} from "angular";
import ngSanitize from "angular-sanitize";
import * as t from "io-ts";
import {ViewCtrl} from "../document/viewctrl";
import {editorChangeValue} from "../editor/editorScope";
import {$http, $q, $sce, $window} from "../util/ngimport";
import {markAsUsed, Require, to} from "../util/utils";
import {GenericPluginMarkup, PluginBase, withDefault} from "./util";

markAsUsed(ngSanitize);

const imagexApp = angular.module("imagexApp", ["ngSanitize"]);

let globalPreviewColor = "#fff";

interface IPoint {
    x: number;
    y: number;
}

const TuplePointR = t.union([t.tuple([t.number, t.number]), t.tuple([t.number, t.number, t.number, t.number])]);

const LineSegment = t.intersection([
    t.type({lines: t.array(TuplePointR)}),
    t.partial({
        color: t.string,
        w: t.number,
    }),
]);

type TuplePoint = t.TypeOf<typeof TuplePointR>;

interface ILineSegment extends t.TypeOf<typeof LineSegment> {
}

interface IFakeVideo {
    currentTime: number;
    fakeVideo: true;
}

type VideoPlayer = IFakeVideo | HTMLVideoElement;

function isFakePlayer(p: VideoPlayer): p is IFakeVideo {
    return "fakeVideo" in p && p.fakeVideo;
}

function tupleToCoords(p: TuplePoint) {
    return {x: p[0], y: p[1]};
}

class FreeHand {
    public freeDrawing: ILineSegment[];
    public redraw?: () => void;
    private videoPlayer: VideoPlayer;
    private emotion: boolean;
    private imgx: ImageXController;
    private prevPos?: TuplePoint;
    private lastDrawnSeg?: number;
    private lastVT?: number;
    private ctx: CanvasRenderingContext2D;

    constructor(imgx: ImageXController, drawData: ILineSegment[], player: HTMLVideoElement | undefined) {
        this.freeDrawing = drawData;
        this.ctx = imgx.getCanvasCtx();
        this.emotion = imgx.emotion;
        this.imgx = imgx;
        this.videoPlayer = player || {currentTime: 1e60, fakeVideo: true};
        if (!isFakePlayer(this.videoPlayer)) {
            if (this.imgx.attrs.analyzeDot) {
                this.videoPlayer.ontimeupdate = () => this.drawCirclesVideoDot(this.ctx, this.freeDrawing, this.videoPlayer);
            } else {
                this.videoPlayer.ontimeupdate = () => this.drawCirclesVideo(this.ctx, this.freeDrawing, this.videoPlayer);
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
        if (!this.imgx.lineMode || this.emotion) {
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
        if (this.imgx.lineMode) {
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
        if (this.prevPos)
            this.startSegment(tupleToCoords(this.prevPos));
    }

    setWidth(newWidth: number) {
        this.imgx.w = newWidth;
        if (this.imgx.w < 1) {
            this.imgx.w = 1;
        }
        if (this.prevPos)
            this.startSegment(tupleToCoords(this.prevPos));
    }

    incWidth(dw: number) {
        this.setWidth(this.imgx.w + dw);
    }

    setLineMode(newMode: boolean) {
        this.imgx.lineMode = newMode;
    }

    flipLineMode() {
        this.setLineMode(!this.imgx.lineMode);
    }

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
        for (let dri = 0; dri < dr.length; dri++) {
            const seg = dr[dri];
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
            if (this.imgx.attrs.dotVisibleTime && this.lastVT && (vt - this.lastVT) > this.imgx.attrs.dotVisibleTime) { // remove old dot if needed
                this.imgx.draw();
                drawDone = true;
            }
            if (this.imgx.attrs.showVideoTime) {
                showTime(ctx, vt, 0, 0, "13px Arial");
            }
            if (this.lastDrawnSeg == seg2) {
                return;
            }
            this.lastDrawnSeg = -1;
            if (!drawDone) {
                this.imgx.draw();
            }
            if (this.imgx.attrs.showVideoTime) {
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
            if (this.imgx.attrs.drawLast) {
                ctx.lineTo(seg.lines[seg2][0], seg.lines[seg2][1]);
            }
            ctx.stroke();
            drawFillCircle(ctx, 30, seg.lines[seg2][0], seg.lines[seg2][1], "black", 0.5);
            if (this.imgx.attrs.showTimes) {
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
        for (let dri = 0; dri < dr.length; dri++) {
            const seg = dr[dri];
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
    ctx.strokeStyle = seg.color || ctx.strokeStyle;
    ctx.lineWidth = seg.w || ctx.lineWidth;
}

function drawFreeHand(ctx: CanvasRenderingContext2D, dr: ILineSegment[]) {
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
    <h4 ng-if="$ctrl.header" ng-bind-html="$ctrl.header"></h4>
    <p ng-if="$ctrl.stem" class="stem" ng-bind-html="$ctrl.stem"></p>
    <div>
        <canvas class="canvas"
                tabindex="1"
                width={{$ctrl.canvaswidth}}
                height={{$ctrl.canvasheight}}
                no-popup-menu></canvas>
        <div class="content"></div>
    </div>
    <p class="csRunMenu">&nbsp;<button ng-if="$ctrl.button"
                                       class="timButton"
                                       ng-disabled="$ctrl.isRunning"
                                       ng-click="$ctrl.save()">{{$ctrl.button}}
    </button>
        &nbsp&nbsp
        <button ng-if="$ctrl.buttonPlay"
                ng-disabled="$ctrl.isRunning"
                ng-click="$ctrl.videoPlay()">{{$ctrl.buttonPlay}}
        </button>
        &nbsp&nbsp
        <button ng-if="$ctrl.buttonRevert"
                ng-disabled="$ctrl.isRunning"
                ng-click="$ctrl.videoBeginning()">{{$ctrl.buttonRevert}}
        </button>
        &nbsp&nbsp
        <button ng-show="$ctrl.finalanswer && $ctrl.userHasAnswered"
                ng-disabled="$ctrl.isRunning"
                ng-click="$ctrl.showAnswer()">
            Showanswer
        </button>
        &nbsp&nbsp<a ng-if="$ctrl.button" ng-disabled="$ctrl.isRunning" ng-click="$ctrl.resetExercise()">
            {{resetText}}</a>&nbsp&nbsp<a
                href="" ng-if="$ctrl.muokattu" ng-click="$ctrl.initCode()">{{$ctrl.resetText}}</a><label
                ng-show="$ctrl.freeHandVisible">FreeHand <input type="checkbox" name="freeHand" value="true"
                                                                ng-model="$ctrl.freeHand"></label> <span><span
                ng-show="$ctrl.freeHand"><label ng-show="$ctrl.freeHandLineVisible">Line
            <input type="checkbox"
                   name="freeHandLine"
                   value="true"
                   ng-model="$ctrl.lineMode"></label> <span
                ng-show="$ctrl.freeHandToolbar"><input ng-show="true" id="freeWidth" size="1" style="width: 1.7em"
                                                       ng-model="$ctrl.w"/>
            <input colorpicker="hex"
                   type="text"
                   ng-style="{'background-color': $ctrl.color}"
                   ng-model="$ctrl.color" size="4"/>&nbsp; <span
                    style="background-color: red; display: table-cell; text-align: center; width: 30px;"
                    ng-click="$ctrl.setFColor('#f00')">R</span><span
                    style="background-color: blue; display: table-cell; text-align: center; width: 30px;"
                    ng-click="$ctrl.setFColor('#00f')">B</span><span
                    style="background-color: yellow; display: table-cell; text-align: center; width: 30px;"
                    ng-click="$ctrl.setFColor('#ff0')">Y</span><span
                    style="background-color: #0f0; display: table-cell; text-align: center; width: 30px;"
                    ng-click="$ctrl.setFColor('#0f0')">G</span>&nbsp;<a href="" ng-click="$ctrl.undo()">Undo</a>
        </span></span></span>
    </p>
    <div ng-show="$ctrl.preview"><span><span ng-style="{'background-color': $ctrl.previewColor}"
                                             style="display: table-cell; text-align: center; width: 30px;"
                                             ng-click="$ctrl.getPColor()">&lt;-</span>
        <input ng-model="$ctrl.previewColor"
               colorpicker="hex"
               type="text"
               ng-click="$ctrl.getPColor()"
               size="10"/> <label> Coord:
            <input ng-model="$ctrl.coords" ng-click="$ctrl.getPColor()" size="10"/></label></span></div>
    <span class="tries" ng-if="$ctrl.max_tries"> Tries: {{$ctrl.tries}}/{{$ctrl.max_tries}}</span>
    <pre class="" ng-if="$ctrl.error && $ctrl.preview">{{$ctrl.error}}</pre>
    <pre class="" ng-show="$ctrl.result">{{$ctrl.result}}</pre>
    <div class="replyHTML" ng-if="$ctrl.replyHTML"><span ng-bind-html="$ctrl.svgImageSnippet()"></span></div>
    <img ng-if="$ctrl.replyImage" class="grconsole" ng-src="{{$ctrl.replyImage}}" alt=""/>
    <p class="plgfooter" ng-if="$ctrl.footer" ng-bind-html="$ctrl.footer"></p>
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

function getPos(canvas: Element, p: MouseOrTouch) {
    const rect = canvas.getBoundingClientRect();
    const posX = p.clientX;
    const posY = p.clientY;
    return {
        x: posX - rect.left,
        y: posY - rect.top,
    };
}

type ObjectType = "target" | "fixedobject" | "dragobject";

function isObjectOnTopOf(position: IPoint, object: DrawObject, grabOffset: number) {
    return object.isPointInside(position, grabOffset);
}

function areObjectsOnTopOf<T extends DrawObject>(
    position: IPoint,
    objects: T[],
    grabOffset: number | undefined,
): T | undefined {
    for (const o of objects) {
        const collision = isObjectOnTopOf(position, o, grabOffset || 0);
        if (collision) {
            return o;
        }
    }
    return undefined;
}

type MouseOrTouch = MouseEvent | Touch;

type DrawObject = Target | FixedObject | DragObject;

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
    private activeDragObject?: {obj: DragObject, xoffset: number, yoffset: number};
    public drawObjects: DrawObject[];

    constructor(private canvas: HTMLCanvasElement, private imgx: ImageXController) {
        this.ctx = canvas.getContext("2d")!;
        this.drawObjects = [];
        this.mousePosition = {x: 0, y: 0};
        this.freeHand = imgx.freeHandDrawing;

        this.canvas.style.touchAction = "double-tap-zoom"; // To get IE and EDGE touch to work
        this.freeHand.redraw = () => this.draw();

        this.canvas.style.msTouchAction = "none";

        this.canvas.addEventListener("mousemove", (event) => {
            this.imgx.getScope().$evalAsync(() => {
                this.moveEvent(event, event);
            });
        });
        this.canvas.addEventListener("touchmove", (event) => {
            this.imgx.getScope().$evalAsync(() => {
                this.moveEvent(event, this.te(event));
            });
        });
        this.canvas.addEventListener("mousedown", (event) => {
            this.imgx.getScope().$evalAsync(() => {
                this.downEvent(event, event);
            });
        });
        this.canvas.addEventListener("touchstart", (event) => {
            this.imgx.getScope().$evalAsync(() => {
                this.downEvent(event, this.te(event));
            });
        });
        this.canvas.addEventListener("mouseup", (event) => {
            this.imgx.getScope().$evalAsync(() => {
                this.upEvent(event, event);
            });
        });
        this.canvas.addEventListener("touchend", (event) => {
            this.imgx.getScope().$evalAsync(() => {
                this.upEvent(event, this.te(event));
            });
        });

        if (imgx.attrs.freeHandShortCuts) {
            this.canvas.addEventListener("keypress", (event) => {
                this.imgx.getScope().$evalAsync(() => {
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
                    if (c === "l") {
                        this.freeHand.flipLineMode();
                    }
                    if (c === "f" && imgx.freeHandShortCut) {
                        imgx.freeHand = !imgx.freeHand;
                    }
                });
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
        this.ctx.fillStyle = "white"; // TODO get from markup?
        this.ctx.fillRect(0, 0, canvas.width, canvas.height);

        if (this.activeDragObject) {
            const dobj = this.activeDragObject;
            if (dobj.obj.lock !== "x" && dobj.obj.xlimits) {
                dobj.obj.x = toRange(dobj.obj.xlimits, this.mousePosition.x - dobj.xoffset);
            }
            if (dobj.obj.lock !== "y" && dobj.obj.ylimits) {
                dobj.obj.y = toRange(dobj.obj.ylimits, this.mousePosition.y - dobj.yoffset);
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
        this.freeHand.draw(this.ctx);
    }

    downEvent(event: Event, p: MouseOrTouch) {
        this.mousePosition = getPos(this.canvas, p);
        let active = areObjectsOnTopOf(this.mousePosition, this.dragobjects, 0);
        if (!active) {
            active = areObjectsOnTopOf(this.mousePosition, this.dragobjects, this.imgx.grabOffset);
        }

        if (this.imgx.emotion) {
            drawFillCircle(this.ctx, 30, this.mousePosition.x, this.mousePosition.y, "black", 0.5);
            this.freeHand.startSegmentDraw(this.mousePosition);
            this.freeHand.addPointDraw(this.ctx, this.mousePosition);
            if (this.imgx.attrs.autosave) {
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
        if (this.activeDragObject) {
            if (event != p) {
                event.preventDefault();
            }
            this.mousePosition = getPos(this.canvas, p);
            if (!this.imgx.emotion) {
                this.draw();
            }
        } else if (this.mouseDown) {
            if (event != p) {
                event.preventDefault();
            }
            if (!this.imgx.emotion) {
                this.freeHand.addPointDraw(this.ctx, getPos(this.canvas, p));
            }
        }
    }

    upEvent(event: Event, p: MouseOrTouch) {
        if (this.activeDragObject) {
            this.canvas.style.cursor = "default";
            if (event != p) {
                event.preventDefault();
            }
            this.mousePosition = getPos(this.canvas, p);

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
        return event.touches[0] || event.changedTouches[0];
    }

    addRightAnswers() {
        // have to add handler for drawing finalanswer here. no way around it.
        if (this.imgx.rightAnswersSet) {
            this.draw();
            return;
        }
        if (!this.imgx.answer || !this.imgx.answer.rightanswers) {
            return;
        }

        const objects = this.dragobjects; // TODO or all objects?
        const rightdrags = this.imgx.answer.rightanswers;
        for (let j = 0; j < rightdrags.length; j++) {
            let p = 0;
            for (p = 0; p < objects.length; p++) {
                if (objects[p].id === rightdrags[j].id) {
                    const values = {beg: objects[p], end: rightdrags[j]};
                    rightdrags[j].x = rightdrags[j].position[0];
                    rightdrags[j].y = rightdrags[j].position[1];
                    // line.color = getValueDef(values, "answerproperties.color", "green");
                    // line.lineWidth = getValueDef(values, "answerproperties.lineWidth", 2);
                    const line = new Line(dt, values);
                    line.did = "-";
                    this.drawObjects.push(line);
                }
            }
        }
        this.imgx.rightAnswersSet = true;

        this.draw();
    }
}

// pin begin (non-dot) position = align place + start
// pin end (dot) position = begin + coord
// in YAML, "position" means pin end, so everything else should be computed based on that
interface IPinPosition {
    align?: PinAlign;
    start?: IPoint;
    coord?: IPoint;
}

interface ISized {
    width: number;
    height: number;
}

class Pin {
    private pos: Required<IPinPosition>;

    constructor(
        private ctx: CanvasRenderingContext2D,
        private values: Required<PinPropsT>,
    ) {
        this.pos = {
            align: values.position.align || "northwest",
            coord: tupleToCoords(values.position.coord || [0, 0]), // TODO better defaults for coord
            start: tupleToCoords(values.position.start || [0, 0]),
        };
    }

    /**
     * Returns the offset for pinned object. The returned offset refers to the center of the object.
     * @param o
     */
    getOffsetForObject(o: ISized) {
        const wp2 = o.width / 2;
        const hp2 = o.height / 2;
        let alignOffset;
        switch (this.pos.align) {
            case "north":
                alignOffset = {x: 0, y: hp2};
                break;
            case "northeast":
                alignOffset = {x: wp2, y: hp2};
                break;
            case "northwest":
                alignOffset = {x: -wp2, y: hp2};
                break;
            case "south":
                alignOffset = {x: 0, y: -hp2};
                break;
            case "southeast":
                alignOffset = {x: wp2, y: -hp2};
                break;
            case "southwest":
                alignOffset = {x: -wp2, y: -hp2};
                break;
            case "east":
                alignOffset = {x: wp2, y: 0};
                break;
            case "west":
                alignOffset = {x: -wp2, y: 0};
                break;
            case "center":
                alignOffset = {x: 0, y: 0};
                break;
            default:
                throw new Error(`Unexpected alignment: ${this.pos.align}`);
        }
        return {
            x: -(this.pos.coord.x + this.pos.start.x + alignOffset.x),
            y: -(this.pos.coord.y + this.pos.start.y + alignOffset.y),
        };
    }

    draw() {
        if (this.values.visible) {
            this.ctx.strokeStyle = this.values.color;
            this.ctx.fillStyle = this.values.color;
            this.ctx.beginPath();
            this.ctx.arc(0, 0, this.values.dotRadius, 0, 2 * Math.PI);
            this.ctx.fill();
            this.ctx.beginPath();
            this.ctx.moveTo(0, 0);
            this.ctx.lineWidth = this.values.linewidth;
            this.ctx.lineTo(-this.pos.coord.x, -this.pos.coord.y);
            this.ctx.stroke();
        }
    }
}

function point(x: number, y: number) {
    return {x, y};
}

// These two are from type-zoo: https://github.com/pelotom/type-zoo
export type Omit<T, K extends keyof any> = T extends any ? Pick<T, Exclude<keyof T, K>> : never;
export type Overwrite<T, U> = Omit<T, keyof T & keyof U> & U;

type MakeOptional<T, U extends keyof T> = Overwrite<T, { [P in U]?: T[P] }>;
type RequireExcept<T, U extends keyof T> = MakeOptional<Required<T>, U>;

/**
 * Makes all properties required except size.
 */
type RequireExceptSize<T extends CommonPropsT> = RequireExcept<T, "size" | "id">;

type Id<T> = T;
type C<T> = Readonly<RequireExceptSize<T>>;

abstract class ObjBase<T extends C<TargetPropsT | DragObjectPropsT | FixedObjectPropsT>> {
    public x: number;
    public y: number;
    public a: number;

    abstract get mainShape(): Shape;

    // Center point of object ignoring possible pin; used in drag & drop checks.
    abstract get center(): IPoint;

    protected constructor(protected ctx: CanvasRenderingContext2D,
                          protected values: T,
                          public did: string) {
        const z = values.position;
        this.x = z[0];
        this.y = z[1];
        this.a = this.values.a;
    }

    isPointInside(p: IPoint, offset?: number) {
        // TODO check child shapes too
        return this.mainShape.isNormalizedPointInsideShape(this.normalizePoint(p), offset);
    }

    normalizePoint(p: IPoint) {
        const to_radians = Math.PI / 180;
        const {x, y} = this.center;
        const sina = Math.sin(this.a * to_radians);
        const cosa = Math.cos(this.a * to_radians);
        const rotatedX = cosa * (p.x - x) - sina * (p.y - y);
        const rotatedY = cosa * (p.y - y) + sina * (p.x - x);
        return point(rotatedX, rotatedY);
    }

    get id(): string {
        return this.values.id || this.did;
    }
}

class DragObject extends ObjBase<C<DragObjectPropsT>> {

    public name: "dragobject" = "dragobject";
    private pin: Pin;
    public mainShape: Shape;

    get lock() {
        return this.values.lock;
    }

    get xlimits() {
        return this.values.xlimits;
    }

    get ylimits() {
        return this.values.xlimits;
    }

    constructor(ctx: CanvasRenderingContext2D,
                values: C<DragObjectPropsT>,
                defId: string) {
        super(ctx, values, defId);
        this.pin = new Pin(ctx, {
            color: values.pin.color || "black",
            dotRadius: values.pin.dotRadius || 5,
            length: values.pin.length || 10,
            linewidth: values.pin.linewidth || 1,
            position: values.pin.position || {},
            visible: values.pin.visible || true,
        });

    }

    draw() {
        // TODO
    }

    get center(): IPoint {
        const off = this.pin.getOffsetForObject(this.mainShape.size);
        return point(this.x - off.x, this.y - off.y);
    }

    resetPosition() {
        this.x = this.values.position[0];
        this.y = this.values.position[1];
    }
}

class Target extends ObjBase<C<TargetPropsT>> {
    public name: "target" = "target";
    objectCount = 0;
    maxObjects = 1000;
    state = TargetState.Normal;
    public mainShape: Shape;

    get snapOffset() {
        return this.values.snapOffset ? tupleToCoords(this.values.snapOffset) : point(0, 0);
    }

    get snap() {
        return this.values.snap;
    }

    get color() {
        switch (this.state) {
            case TargetState.Normal:
                return this.values.color;
            case TargetState.Snap:
                return this.values.snapColor;
            case TargetState.Drop:
                return this.values.dropColor;
        }
    }

    constructor(ctx: CanvasRenderingContext2D,
                values: C<TargetPropsT>,
                defId: string) {
        super(ctx, values, defId);
        // If default values of type, size, a or position are changed, check also imagex.py
        this.position = getValueDef(values, "position", [0, 0]);
        this.snap = getValueDef(values, "snap", true);
        this.snapOffset = getValueDef(values, "snapOffset", [0, 0]);
        this.type = getValueDef(values, "type", "rectangle", true);
        this.color = getValueDef(values, "color", "Blue");
        this.origColor = getValueDef(values, "color", "Blue");
        this.snapColor = getValueDef(values, "snapColor", "Cyan");
        this.dropColor = getValueDef(values, "dropColor", this.snapColor);
    }

    draw() {
        // TODO
    }

    get center(): IPoint {
        return tupleToCoords(this.values.position); // position means center for Target
    }
}

class FixedObject extends ObjBase<C<FixedObjectPropsT>> {
    public name: "fixedobject" = "fixedobject";
    public mainShape: Shape;

    constructor(ctx: CanvasRenderingContext2D,
                values: C<FixedObjectPropsT>,
                defId: string) {
        super(ctx, values, defId);
        // If default values of type, size, a or position are changed, check also imagex.py
        this.color = getValueDef(values, "color", "Blue");
        this.size = getValueDef(values, "size", [10, 10]);
        this.r1 = getValue(this.size[0], 10);
        this.r2 = getValue(this.size[1], this.r1);
    }

    draw() {
        // TODO
    }

    get center(): IPoint {
        const s = this.mainShape.size;
        return {x: this.x + s.width / 2, y: this.y + s.height / 2}; // for FixedObject, position is top-left corner
    }
}

class Shape {
    constructor(protected explicitSize: ISized | undefined) {

    }

    /**
     * Size of the object if no size has been explicitly set.
     */
    get preferredSize() {
        return {width: 10, height: 10};
    }

    get size(): ISized {
        if (this.explicitSize) {
            return this.explicitSize;
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
        const r1 = s.width + (offset || 0);
        const r2 = s.height + (offset || 0);
        return p.x >= -r1 / 2 && p.y <= r1 / 2 &&
            p.y >= -r2 / 2 && p.y <= r2 / 2;
    }
}

class Line {
    constructor(
        private readonly ctx: CanvasRenderingContext2D,
        private readonly color: string,
        private readonly lineWidth: number,
        private readonly beg: IPoint,
        private readonly end: IPoint,
    ) {

    }

    draw() {
        this.ctx.beginPath();
        this.ctx.moveTo(this.beg.x, this.beg.y);
        this.ctx.lineTo(this.end.x, this.end.y);
        this.ctx.lineWidth = this.lineWidth;
        this.ctx.strokeStyle = this.color;
        this.ctx.stroke();
    }
}

class Ellipse extends Shape {
    constructor(
        private readonly ctx: CanvasRenderingContext2D,
        private readonly color: string,
        private readonly lineWidth: number = 2,
        size: ISized | undefined,
    ) {
        super(size);
    }

    draw() {
        const {width, height} = this.size;
        this.ctx.strokeStyle = this.color;
        this.ctx.lineWidth = this.lineWidth;
        this.ctx.save();
        this.ctx.beginPath();
        this.ctx.scale(width / 2, height / 2);
        this.ctx.arc(0, 0, 1, 0, 2 * Math.PI, false);
        this.ctx.restore();
        this.ctx.stroke();
    }

    isNormalizedPointInsideShape(p: IPoint, offset?: number) {
        const {width, height} = this.size;
        const r1 = width + (offset || 0);
        const r2 = height + (offset || 0);
        return (Math.pow(p.x, 2) / Math.pow(r1 / 2, 2)) +
            (Math.pow(p.y, 2) / Math.pow(r2 / 2, 2)) <= 1;
    }
}

class Rectangle extends Shape {
    constructor(
        protected readonly ctx: CanvasRenderingContext2D,
        protected readonly color: string,
        protected readonly lineWidth: number = 2,
        protected readonly cornerRadius: number = 0,
        size: ISized | undefined,
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

    draw() {
        const {width, height} = this.size;
        this.ctx.strokeStyle = this.color;
        this.ctx.lineWidth = this.lineWidth;
        this.ctx.save();
        this.ctx.beginPath();
        this.ctx.moveTo(-width / 2 - 1 + this.cornerRadius, -height / 2); // TODO why -1?
        this.ctx.lineTo(width / 2 - this.cornerRadius, -height / 2);
        this.ctx.arc(width / 2 - this.cornerRadius, -height / 2 + this.cornerRadius,
            this.cornerRadius, 1.5 * Math.PI, 0);
        this.ctx.lineTo(width / 2, height / 2 - this.cornerRadius);
        this.ctx.arc(width / 2 - this.cornerRadius, height / 2 - this.cornerRadius,
            this.cornerRadius, 0, 0.5 * Math.PI);
        this.ctx.lineTo(-width / 2 + this.cornerRadius, height / 2);
        this.ctx.arc(-width / 2 + this.cornerRadius, height / 2 - this.cornerRadius,
            this.cornerRadius, 0.5 * Math.PI, Math.PI);
        this.ctx.lineTo(-width / 2, -height / 2 + this.cornerRadius);
        this.ctx.arc(-width / 2 + this.cornerRadius, -height / 2 +
            this.cornerRadius, this.cornerRadius, Math.PI, 1.5 * Math.PI);
        this.ctx.restore();
        this.ctx.stroke();
    }
}

const auxctx = document.createElement("canvas").getContext("2d")!;

class Textbox extends Shape {
    private lines: string[];
    private textwidth: number;
    private textHeight: number;
    private rect: Rectangle;
    private leftMargin = 0;
    private rightMargin = 0;
    private topMargin = 0;
    private bottomMargin = 0;

    constructor(
        protected readonly ctx: CanvasRenderingContext2D,
        protected readonly color: string,
        protected readonly borderColor: string,
        protected readonly lineWidth: number = 2,
        protected readonly cornerRadius: number = 0,
        size: ISized | undefined,
        text: string,
        protected readonly font: string,
    ) {
        super(size);
        this.lines = text.split("\n");
        auxctx.font = this.font;
        const lineWidths = this.lines.map((line) => auxctx.measureText(line).width);
        this.textwidth = Math.max(...lineWidths);
        this.textHeight = parseInt(auxctx.font, 10) * 1.1;

        const {width, height} = this.size;
        if (this.cornerRadius > width / 2 || this.cornerRadius > height / 2) {
            if (this.cornerRadius > width / 2) {
                this.cornerRadius = width / 2;
            }
            if (this.cornerRadius > height / 2) {
                this.cornerRadius = height / 2;
            }
        }
        this.rect = new Rectangle(ctx, color, lineWidth, cornerRadius, this.size);
    }

    get preferredSize() {
        return {
            width: this.textwidth + this.leftMargin + this.rightMargin,
            height: (this.textHeight * this.lines.length)
                + this.topMargin + this.bottomMargin,
        };
    }

    draw() {
        this.rect.draw();
        let textStart = this.topMargin;
        for (const line of this.lines) {
            this.ctx.fillText(line, this.leftMargin, textStart);
            textStart += this.textHeight;
        }
        this.ctx.lineWidth = this.lineWidth;
        this.ctx.strokeStyle = this.borderColor;
        this.ctx.stroke();
    }
}

class Vector extends Shape {
    private arrowHeadWidth: number;
    private arrowHeadLength: number;

    constructor(
        private readonly ctx: CanvasRenderingContext2D,
        private readonly color: string,
        private readonly lineWidth: number = 2,
        size: ISized | undefined,
        arrowHeadWidth: number | undefined,
        arrowHeadLength: number | undefined,
    ) {
        super(size);
        this.arrowHeadWidth = arrowHeadWidth || this.size.height * 3;
        this.arrowHeadLength = arrowHeadLength || this.size.height * 5;
    }

    get preferredSize() {
        return {
            width: 50,
            height: 4,
        };
    }

    draw() {
        const {width, height} = this.size;
        this.ctx.strokeStyle = this.color;
        this.ctx.fillStyle = this.ctx.strokeStyle;
        this.ctx.save();
        this.ctx.beginPath();
        this.ctx.lineTo(0, height);
        this.ctx.lineTo(width - this.arrowHeadLength, height);
        this.ctx.lineTo(width - this.arrowHeadLength, height / 2 +
            this.arrowHeadWidth / 2);
        this.ctx.lineTo(width, height / 2);
        this.ctx.lineTo(width - this.arrowHeadLength,
            height / 2 - this.arrowHeadWidth / 2);
        this.ctx.lineTo(width - this.arrowHeadLength, 0);
        this.ctx.lineTo(0, 0);
        this.ctx.fill();
        this.ctx.restore();
    }
}

class DImage extends Shape {
    constructor(
        private readonly ctx: CanvasRenderingContext2D,
        private readonly image: HTMLImageElement,
        size: ISized | undefined,
    ) {
        super(size);
    }

    get preferredSize() {
        return {
            width: this.image.width,
            height: this.image.height,
        };
    }

    draw() {
        const {width, height} = this.size;
        this.ctx.drawImage(this.image, 0, 0, width, height);
    }
}

function isTouchDevice() {
    return typeof window.ontouchstart !== "undefined";
}

function loadImage(src: string) {
    const deferred = $q.defer<HTMLImageElement>();
    const sprite = new Image();
    sprite.onload = () => {
        deferred.resolve(sprite);
    };
    sprite.src = src;
    return deferred.promise;
}

const videos = {};

const ObjectType = t.keyof({
    ellipse: null,
    img: null,
    rectangle: null,
    textbox: null,
    vector: null,
});

const PinAlignment = t.keyof({
    center: null,
    east: null,
    north: null,
    northeast: null,
    northwest: null,
    south: null,
    southeast: null,
    southwest: null,
    west: null,
});

type PinAlign = t.TypeOf<typeof PinAlignment>;

const ImgProps = t.type({
    src: t.string,
    textbox: withDefault(t.boolean, true),
});

const EmptyCoord = t.readonlyArray(t.number); // TODO should get rid of this by fixing imagex markups

const ValidCoord = t.tuple([t.number, t.number]);

const CoordProp = t.union([ValidCoord, EmptyCoord]);

const CommonProps = t.partial({
    a: t.number,
    id: t.string,
    position: ValidCoord,
    size: ValidCoord,
});

const TextboxProps = t.intersection([
    CommonProps,
    t.partial({
        borderColor: t.string,
        borderWidth: t.number,
        cornerradius: t.number,
        fillColor: t.string,
        font: t.string,
        text: t.string,
        textColor: t.string,
    })]);

const VectorProps = t.partial({
    arrowheadlength: t.number,
    arrowheadwidth: t.number,
    color: t.string,
});

const PinProps = t.partial({
    color: t.string,
    dotRadius: t.number,
    length: t.number,
    linewidth: t.number,
    position: t.partial({
        align: PinAlignment,
        coord: ValidCoord,
        start: ValidCoord,
    }),
    visible: t.boolean,
});

type PinPropsT = t.TypeOf<typeof PinProps>;

const FixedObjectProps = t.intersection([
    CommonProps,
    t.partial({
        imgproperties: ImgProps,
        textboxproperties: TextboxProps,
        type: ObjectType,
        vectorproperties: VectorProps,
    })]);

const TargetProps = t.intersection([
    CommonProps,
    t.partial({
        color: t.string,
        dropColor: t.string,
        points: t.dictionary(t.string, t.number),
        snap: t.boolean,
        snapColor: t.string,
        snapOffset: ValidCoord,
        type: ObjectType,
    })]);

const Lock = t.keyof({
    x: null,
    y: null,
});

const DragObjectProps = t.intersection([
    t.partial({
        lock: Lock,
        pin: PinProps,
        xlimits: ValidCoord,
        ylimits: ValidCoord,
    }),
    FixedObjectProps,
]);

type CommonPropsT = t.TypeOf<typeof CommonProps>;
type DragObjectPropsT = t.TypeOf<typeof DragObjectProps>;
type TargetPropsT = t.TypeOf<typeof TargetProps>;
type FixedObjectPropsT = t.TypeOf<typeof FixedObjectProps>;

const BackgroundProps = t.intersection([
    t.partial({
        a: t.number,
    }),
    t.type({
        src: t.string,
    }),
]);

const DefaultProps = t.intersection([DragObjectProps, TargetProps]);

const ImageXMarkup = t.intersection([
    t.partial({
        background: BackgroundProps,
        buttonPlay: t.string,
        buttonRevert: t.string,
        fixedobjects: t.readonlyArray(FixedObjectProps),
        followid: t.string,
        freeHand: t.union([t.boolean, t.literal("use")]),
        freeHandColor: t.string,
        freeHandShortCut: t.boolean,
        freeHandVisible: t.boolean,
        freeHandWidth: t.number,
        max_tries: t.number,
        objects: t.readonlyArray(DragObjectProps),
        targets: t.readonlyArray(TargetProps),
    }),
    GenericPluginMarkup,
    t.type({
        analyzeDot: withDefault(t.boolean, false),
        autosave: withDefault(t.boolean, false),
        autoupdate: withDefault(t.number, 500),
        canvasheight: withDefault(t.number, 600),
        canvaswidth: withDefault(t.number, 800),
        cols: withDefault(t.number, 20),
        dotVisibleTime: withDefault(t.number, 1),
        drawLast: withDefault(t.boolean, false),
        emotion: withDefault(t.boolean, false),
        extraGrabAreaHeight: withDefault(t.number, 30),
        finalanswer: withDefault(t.boolean, false),
        freeHandColor: withDefault(t.string, "red"),
        freeHandLine: withDefault(t.boolean, false),
        freeHandLineVisible: withDefault(t.boolean, true),
        freeHandShortCuts: withDefault(t.boolean, true),
        freeHandToolbar: withDefault(t.boolean, true),
        freeHandWidth: withDefault(t.number, 2),
        showTimes: withDefault(t.boolean, false),
        showVideoTime: withDefault(t.boolean, true),
    }),
]);

const ImageXAll = t.type({
    freeHandData: t.array(LineSegment),
    markup: ImageXMarkup,
    preview: t.boolean,
    tries: t.number,
});

class ImageXController extends PluginBase<t.TypeOf<typeof ImageXMarkup>,
    t.TypeOf<typeof ImageXAll>,
    typeof ImageXAll> {

    get emotion() {
        return this.attrs.emotion;
    }

    get max_tries() {
        return this.attrs.max_tries;
    }

    get freeHandVisible() {
        const r = this.attrs.freeHandVisible;
        if (r != null) {
            return r;
        }
        return this.isFreeHandInUse;
    }

    get freeHandShortCut() {
        const r = this.attrs.freeHandShortCut;
        if (r != null) {
            return r;
        }
        return this.isFreeHandInUse;
    }

    get isFreeHandInUse() {
        return this.attrs.freeHand === "use" || this.attrs.freeHand === true;
    }

    get videoPlayer(): HTMLVideoElement | undefined {
        return $window.videoApp && this.attrs.followid && $window.videoApp.videos[this.attrs.followid];
    }

    get buttonPlay() {
        return this.attrs.buttonPlay || (this.videoPlayer ? "Aloita/pysäytä" : undefined);
    }

    get buttonRevert() {
        return this.attrs.buttonRevert || (this.videoPlayer ? "Video alkuun" : undefined);
    }

    get teacherMode() {
        return this.vctrl.teacherMode;
    }

    get grabOffset() {
        return isTouchDevice() ? this.attrs.extraGrabAreaHeight : 0;
    }

    getScope() {
        return this.scope;
    }

    private static $inject = ["$scope", "$element"];
    public w = 0;
    public freeHand = false;
    public lineMode = false;
    public color = "";
    public freeHandDrawing!: FreeHand;
    public coords = "";
    public drags: DragObject[] = [];
    public userHasAnswered = false;
    public rightAnswersSet: boolean = false;

    private muokattu: boolean;
    private result: string;
    private error?: string;
    private cursor: string;
    private tries = 0;
    private previewColor = "";
    private isRunning: boolean = false;
    private vctrl!: Require<ViewCtrl>;
    private replyImage?: string;
    private replyHTML?: string;
    private canvas!: HTMLCanvasElement;
    private dt!: DragTask;

    draw() {
        this.dt.draw();
    }

    constructor(scope: IScope, element: IRootElementService) {
        super(scope, element);
        this.muokattu = false;
        this.result = "";
        this.cursor = "\u0383"; // "\u0347"; // "\u02FD";
    }

    getCanvasCtx() {
        return this.canvas.getContext("2d")!;
    }

    $onInit() {
        super.$onInit();
        this.tries = this.attrsall.tries;
        this.freeHandDrawing = new FreeHand(this,
            this.attrsall.freeHandData,
            this.videoPlayer);
        if (this.attrs.freeHand === "use") {
            this.freeHand = false;
        }

        this.w = this.attrs.freeHandWidth;
        this.color = this.attrs.freeHandColor;
        this.lineMode = this.attrs.freeHandLine;

        this.canvas = this.element.find(".canvas")[0] as HTMLCanvasElement;

        const dt = new DragTask(this.canvas, this);
        this.dt = dt;

        const userObjects = this.attrs.objects;

        const userTargets = this.attrs.targets;
        const userFixedObjects = this.attrs.fixedobjects;
        const fixedobjects = [];
        const targets = [];
        const objects = [];
        const ctx = this.getCanvasCtx();

        if (this.attrs.background) {
            const background = new FixedObject(dt, this.attrs.background);
            dt.drawObjects.push(background);
        }

        this.error = "";

        if (userFixedObjects) {
            for (let i = 0; i < userFixedObjects.length; i++) {
                fixedobjects.push(new FixedObject(ctx, userFixedObjects[i], "fix" + (i + 1)));
            }
        }

        if (userTargets) {
            for (let i = 0; i < userTargets.length; i++) {
                targets.push(new Target(ctx, userTargets[i], "trg" + (i + 1)));
            }
        }

        if (userObjects) {
            for (let i = 0; i < userObjects.length; i++) {
                const newObject = new DragObject(ctx, userObjects[i], "obj" + (i + 1));
                objects.push(newObject);
                this.drags.push(newObject);
            }
        }

        if (this.attrs.state && this.attrs.state.userAnswer) {
            this.userHasAnswered = true;
            const userDrags = this.attrs.state.userAnswer.drags;
            if (objects && userDrags && userDrags.length > 0) {
                for (let i = 0; i < objects.length; i++) {
                    for (let j = 0; j < userDrags.length; j++) {
                        if (objects[i].did === userDrags[j].did) {
                            objects[i].position[0] = userDrags[j].position[0];
                            objects[i].position[1] = userDrags[j].position[1];
                            objects[i].x = objects[i].position[0];
                            objects[i].y = objects[i].position[1];
                        }
                    }
                }
            }
        }

        dt.drawObjects = dt.drawObjects.concat(fixedobjects, targets, objects);
        dt.draw();

        this.previewColor = globalPreviewColor;
    }

    initCode() {
        this.error = "";
        this.result = "";
    }

    undo() {
        this.freeHandDrawing.popSegment(0);
    }

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
        if (this.answer && this.answer.rightanswers) {
            this.dt.addRightAnswers();
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
        let url = "/imagex/answer";
        const plugin = this.getPlugin();
        const taskId = this.getTaskId();
        if (plugin) {
            url = plugin;
            const i = url.lastIndexOf("/");
            if (i > 0) {
                url = url.substring(i);
            }
            url += "/" + taskId + "/answer/";
        }

        const r = await to($http<{
            web: {
                error?: string, result: string, tries: number, answer: string,
            },
        }>({method: "PUT", url, data: params, timeout: 20000},
        ));
        this.isRunning = false;
        if (r.ok) {
            const data = r.result.data;
            this.error = data.web.error;
            this.result = data.web.result;
            this.tries = data.web.tries;
            // for showing right answers.
            this.answer = data.web.answer;
            this.dt.addRightAnswers();
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


        const dobjs = this.dt.drawObjects;

        // this is for hiding the lines of right answers
        for (let i = dobjs.length - 1; i--;) {
            if (dobjs[i].did === "-") {
                dobjs.splice(i, 1);
            }
        }
        this.rightAnswersSet = false;

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
        let url = "/imagex/answer";
        const plugin = this.getPlugin();
        const taskId = this.getTaskId();
        if (plugin) {
            url = plugin;
            const i = url.lastIndexOf("/");
            if (i > 0) {
                url = url.substring(i);
            }
            url += "/" + taskId + "/answer/";
        }

        const r = await to($http<{
            web: {
                error?: string,
                result: string,
                tries: number,
                "-replyImage": string,
                "-replyHTML": string,
            },
        }>({method: "PUT", url, data: params, timeout: 20000},
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

    protected getAttributeType() {
        return ImageXAll;
    }
}

imagexApp.component("imagexRunner", {
    bindings: {
        json: "@",
    },
    controller: ImageXController,
    require: {
        vctrl: "^timView",
    },
    template: directiveTemplate,
});
