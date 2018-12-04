import angular, {IRootElementService, IScope} from "angular";
import ngSanitize from "angular-sanitize";
import * as t from "io-ts";
import {ViewCtrl} from "../document/viewctrl";
import {editorChangeValue} from "../editor/editorScope";
import {$http, $q, $sce, $window} from "../util/ngimport";
import {markAsUsed, to} from "../util/utils";
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

function tupleToCoords(p: TuplePoint | undefined) {
    if (!p) {
        return undefined;
    }
    return {x: p[0], y: p[1]};
}

class FreeHand {
    public freeDrawing: ILineSegment[];
    public redraw?: () => void;
    private videoPlayer: VideoPlayer;
    private emotion: boolean;
    private scope: ImageXController;
    private prevPos?: TuplePoint;
    private lastDrawnSeg?: number;
    private lastVT?: number;
    private ctx: CanvasRenderingContext2D;

    constructor(scope: ImageXController, drawData: ILineSegment[], player: HTMLVideoElement | undefined) {
        this.freeDrawing = drawData;
        this.ctx = scope.getCanvasCtx();
        this.emotion = scope.emotion;
        this.scope = scope;
        this.videoPlayer = player || {currentTime: 1e60, fakeVideo: true};
        if (!isFakePlayer(this.videoPlayer)) {
            if (this.scope.attrs.analyzeDot) {
                this.videoPlayer.ontimeupdate = () => this.drawCirclesVideoDot(this.ctx, this.freeDrawing, this.videoPlayer);
            } else {
                this.videoPlayer.ontimeupdate = () => this.drawCirclesVideo(this.ctx, this.freeDrawing, this.videoPlayer);
            }
        }
    }

    draw(ctx: CanvasRenderingContext2D) {
        if (this.emotion) {
            if (this.scope.teacherMode) {
                if (isFakePlayer(this.videoPlayer)) {
                    this.drawCirclesVideo(ctx, this.freeDrawing, this.videoPlayer);
                }
            }
        } else {
            drawFreeHand(ctx, this.freeDrawing);
        }
    }

    startSegment(pxy?: IPoint) {
        if (!pxy) {
            return;
        }
        const p: TuplePoint = [Math.round(pxy.x), Math.round(pxy.y)];
        const ns: ILineSegment = {lines: [p]};
        if (!this.emotion) {
            ns.color = this.scope.color;
            ns.w = this.scope.w;
        }
        this.freeDrawing.push(ns);
        this.prevPos = p;
    }

    endSegment() {
        // this.drawingSurfaceImageData = null; // not used anywhere
    }

    startSegmentDraw(redraw: () => void, pxy: IPoint) {
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
        this.redraw = redraw;
        const ns: any = {};
        if (!this.emotion) {
            ns.color = this.scope.color;
            ns.w = this.scope.w;
        }
        ns.lines = [p];
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
        if (!this.scope.lineMode || this.emotion) {
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

    addPointDraw(ctx: CanvasRenderingContext2D, pxy?: IPoint) {
        if (!pxy) {
            return;
        }
        if (this.scope.lineMode) {
            this.popPoint(1);
            if (this.redraw) {
                this.redraw();
            }
        }
        if (!this.emotion) {
            this.line(ctx, this.prevPos, [pxy.x, pxy.y]);
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
        this.scope.color = newColor;
        this.startSegment(tupleToCoords(this.prevPos));
    }

    setWidth(newWidth: number) {
        this.scope.w = newWidth;
        if (this.scope.w < 1) {
            this.scope.w = 1;
        }
        this.startSegment(tupleToCoords(this.prevPos));
    }

    incWidth(dw: number) {
        this.setWidth(this.scope.w + dw);
    }

    setLineMode(newMode: boolean) {
        this.scope.lineMode = newMode;
    }

    flipLineMode() {
        this.setLineMode(!this.scope.lineMode);
    }

    line(ctx: CanvasRenderingContext2D, p1: TuplePoint | undefined, p2: TuplePoint) {
        if (!p1 || !p2) {
            return;
        }
        ctx.beginPath();
        ctx.strokeStyle = this.scope.color;
        ctx.lineWidth = this.scope.w;
        ctx.moveTo(p1[0], p1[1]);
        ctx.lineTo(p2[0], p2[1]);
        ctx.stroke();
    }

    drawCirclesVideoDot(ctx: CanvasRenderingContext2D, dr: ILineSegment[], videoPlayer: VideoPlayer) {
        for (let dri = 0; dri < dr.length; dri++) {
            const seg = dr[dri];
            const vt = videoPlayer.currentTime;
            // console.log("vt: " + vt + " " + this.lastDrawnSeg );
            let seg1 = -1;
            let seg2 = 0;
            let s = "";
            for (let lni = 0; lni < seg.lines.length; lni++) {
                if (vt < seg.lines[lni][3]) {
                    break;
                }
                seg1 = seg2;
                seg2 = lni;
            }

            // console.log("" + seg1 + "-" + seg2 + ": " + seg.lines[seg2][3]);
            let drawDone = false;
            if (this.scope.attrs.dotVisibleTime && this.lastVT && (vt - this.lastVT) > this.scope.attrs.dotVisibleTime) { // remove old dot if needed
                this.scope.draw();
                drawDone = true;
            }
            if (this.scope.attrs.showVideoTime) {
                showTime(ctx, vt, 0, 0, "13px Arial");
            }
            if (this.lastDrawnSeg == seg2) {
                return;
            }
            this.lastDrawnSeg = -1;
            if (!drawDone) {
                this.scope.draw();
            }
            if (this.scope.attrs.showVideoTime) {
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
            if (this.scope.attrs.drawLast) {
                ctx.lineTo(seg.lines[seg2][0], seg.lines[seg2][1]);
            }
            ctx.stroke();
            drawFillCircle(ctx, 30, seg.lines[seg2][0], seg.lines[seg2][1], "black", 0.5);
            if (this.scope.attrs.showTimes) {
                s = dateToString(seg.lines[seg2][3], false);
                drawText(ctx, s, seg.lines[seg2][0], seg.lines[seg2][1], "13px Arial");
            }

            this.lastDrawnSeg = seg2;
        }
    }

    drawCirclesVideo(ctx: CanvasRenderingContext2D, dr: ILineSegment[], videoPlayer: VideoPlayer) {
        if (!isFakePlayer(videoPlayer)) {
            this.scope.draw();
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
                s = dateToString(seg.lines[0][2], true);
                drawText(ctx, s, seg.lines[0][0], seg.lines[0][1]);
            } else {
                if (vt >= seg.lines[0][3]) {
                    drawFillCircle(ctx, 5, seg.lines[0][0], seg.lines[0][1], "black", 0.5);
                }
            }
            for (let lni = 1; lni < seg.lines.length; lni++) {
                if (vt < seg.lines[lni][3]) {
                    break;
                }
                ctx.lineTo(seg.lines[lni][0], seg.lines[lni][1]);
                if (isFakePlayer(videoPlayer)) {
                    s = dateToString(seg.lines[lni][2], true);
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
    for (let dri = 0; dri < dr.length; dri++) {
        const seg = dr[dri];
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

const directiveTemplate = `<div class="csRunDiv no-popup-menu">
    <p>Header comes here</p>
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
    <p class="plgfooter">Here comes footer</p>
</div>
`;

function drawFillCircle(ctx: CanvasRenderingContext2D, r: number, p1: number, p2: number, c: string, tr: number) {
    if (!p1 || !p2) {
        return;
    }
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

function isObjectOnTopOf(position: IPoint, object: DrawObject, name: ObjectType, grabOffset: number) {
    if (object.name !== name) {
        return false;
    }
    return object.isPointInside(position, grabOffset);
}

function areObjectsOnTopOf<T extends DrawObject>(
    position: IPoint,
    objects: T[],
    name: ObjectType,
    grabOffset: number | undefined,
): T | undefined {
    for (const o of objects) {
        const collision = isObjectOnTopOf(position, o, name, 0);
        if (collision) {
            return o;
        }
    }
    if (grabOffset) {
        for (const o of objects) {
            const collision = isObjectOnTopOf(position, o, name, grabOffset);
            if (collision && o.name === "dragobject") {
                return o;
            }
        }
    }
    return undefined;
}

type MouseOrTouch = MouseEvent | Touch;

type DrawObject = Target | FixedObject | DragObject;

class DragTask {
    public ctx: CanvasRenderingContext2D;
    private mousePosition: IPoint;
    private freeHand: FreeHand;
    private mouseDown = false;
    private activeDragObject?: DragObject;
    private drawObjects: DrawObject[];

    constructor(private canvas: HTMLCanvasElement, private imgx: ImageXController) {
        this.ctx = canvas.getContext("2d")!;
        this.drawObjects = [];
        this.mousePosition = {x: 0, y: 0};
        this.freeHand = imgx.freeHandDrawing;

        this.canvas.style.touchAction = "double-tap-zoom"; // To get IE and EDGE touch to work
        this.freeHand.redraw = () => this.redraw();

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
                        this.draw();
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

    draw() {
        const canvas = this.canvas;
        this.ctx.fillStyle = getValue1(scope.background, "color", "white");
        this.ctx.fillRect(0, 0, canvas.width, canvas.height);

        for (let i = 0; i < this.drawObjects.length; i++) {
            if (this.activeDragObject) {
                const dobj = this.activeDragObject;
                if (dobj.lock != "x") {
                    dobj.x = toRange(dobj.xlimits, this.mousePosition.x - dobj.xoffset);
                }
                if (dobj.lock != "y") {
                    dobj.y = toRange(dobj.ylimits, this.mousePosition.y - dobj.yoffset);
                }
            }
            this.drawObjects[i].draw();
            if (this.drawObjects[i].name == "dragobject") {
                if (this.activeDragObject) {
                    const onTopOf = areObjectsOnTopOf(this.activeDragObject,
                        this.targets, "target", this.imgx.grabOffset);
                    if (onTopOf && onTopOf.objectCount < onTopOf.maxObjects) {
                        onTopOf.color = onTopOf.snapColor;
                    }
                    const onTopOfB = areObjectsOnTopOf(this.drawObjects[i],
                        this.drawObjects, "target", this.imgx.grabOffset);
                    if (onTopOfB && this.drawObjects[i] !== this.activeDragObject) {
                        onTopOfB.color = onTopOfB.dropColor;
                    }
                } else {
                    const onTopOfA = areObjectsOnTopOf(this.drawObjects[i],
                        this.drawObjects, "target", this.imgx.grabOffset);
                    if (onTopOfA) {
                        onTopOfA.color = onTopOfA.dropColor;
                    }
                }
            } else if (this.drawObjects[i].name === "target") {
                this.drawObjects[i].color = this.drawObjects[i].origColor;

            }
        }
        this.freeHand.draw(this.ctx);
    }

    downEvent(event: Event, p: MouseOrTouch) {
        this.mousePosition = getPos(this.canvas, p);
        this.activeDragObject =
            areObjectsOnTopOf(this.mousePosition, this.drawObjects, "dragobject", this.imgx.grabOffset);

        if (scope.emotion) {
            drawFillCircle(this.ctx, 30, this.mousePosition.x, this.mousePosition.y, "black", 0.5);
            this.freeHand.startSegmentDraw(this.ctx, this.mousePosition); // TODO this is wrong in original
            this.freeHand.addPointDraw(this.ctx, this.mousePosition);
            if (scope.autosave) {
                scope.imagexScope.save();
            }
        }

        if (this.activeDragObject) {
            const array = this.drawObjects;
            array.push(array.splice(array.indexOf(this.activeDragObject), 1)[0]); // put active last so that it gets drawn on top of others
            this.canvas.style.cursor = "pointer";
            this.activeDragObject.xoffset = this.mousePosition.x - this.activeDragObject.x;
            this.activeDragObject.yoffset = this.mousePosition.y - this.activeDragObject.y;
            // event.preventDefault();
            this.draw();
        } else if (scope.freeHand) {
            this.canvas.style.cursor = "pointer";
            this.mouseDown = true;
            if (!this.imgx.emotion) {
                this.freeHand.startSegmentDraw(this.redraw, this.mousePosition);
            }
        }

        if (scope.preview) {
            this.imgx.coords = `[${Math.round(this.mousePosition.x)}, ${Math.round(this.mousePosition.y)}]`;
            editorChangeValue(["position:"], this.imgx.coords);
        }
    }

    redraw() {
        this.draw();
    }

    moveEvent(event: Event, p: MouseOrTouch) {
        if (this.activeDragObject) {
            // if (this.activeDragObject)
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

            const isTarget = areObjectsOnTopOf(this.activeDragObject,
                this.drawObjects, "target", undefined);

            if (isTarget) {
                if (isTarget.objectCount < isTarget.maxObjects) {
                    if (isTarget.snap) {
                        this.activeDragObject.x = isTarget.x + isTarget.snapOffset[0];
                        this.activeDragObject.y = isTarget.y + isTarget.snapOffset[1];
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
        setTimeout(this.draw, 1000);
        // TODO: Miksi tama on taalla?
        if (!this.imgx.emotion) {
            this.draw();
        }
    }

    te(event: TouchEvent) {
        return event.touches[0] || event.changedTouches[0];
    }

    addRightAnswers() {
        // have to add handler for drawing finalanswer here. no way around it.
        if (this.imgx.rightAnswersSet) {
            dt.draw();
            return;
        }
        if (!scope.answer || !scope.answer.rightanswers) {
            return;
        }

        const rightdrags = scope.answer.rightanswers;
        let j = 0;
        for (j = 0; j < rightdrags.length; j++) {
            let p = 0;
            for (p = 0; p < objects.length; p++) {
                if (objects[p].id === rightdrags[j].id) {
                    let values: any = {beg: objects[p], end: rightdrags[j]};
                    rightdrags[j].x = rightdrags[j].position[0];
                    rightdrags[j].y = rightdrags[j].position[1];
                    // line.color = getValueDef(values, "answerproperties.color", "green");
                    // line.lineWidth = getValueDef(values, "answerproperties.lineWidth", 2);
                    const line = new Line(dt, values);
                    line.did = "-";
                    this.drawObjects.push(line);
                    // line.draw();
                    values = {};
                }
            }
        }
        scope.rightAnswersSet = true;

        dt.draw();
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
        pos: IPinPosition,
        private color: string,
        private visible: boolean,
        private radius: number,
        private lineWidth: number,
    ) {
        this.pos = {
            align: pos.align || "northwest",
            coord: pos.coord || {x: 0, y: 0}, // TODO better defaults for coord
            start: pos.start || {x: 0, y: 0},
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
        if (this.visible) {
            this.ctx.strokeStyle = this.color;
            this.ctx.fillStyle = this.color;
            this.ctx.beginPath();
            this.ctx.arc(0, 0, this.radius, 0, 2 * Math.PI);
            this.ctx.fill();
            this.ctx.beginPath();
            this.ctx.moveTo(0, 0);
            this.ctx.lineWidth = this.lineWidth;
            this.ctx.lineTo(-this.pos.coord.x, -this.pos.coord.y);
            this.ctx.stroke();
        }
    }
}

function point(x: number, y: number) {
    return {x, y};
}

abstract class ObjBase {
    public x: number;
    public y: number;
    public a: number;
    public mainShape: Shape;

    // Center point of object ignoring possible pin; used in drag & drop checks.
    abstract get center(): IPoint;

    isPointInside(p: IPoint, offset?: number) {
        // TODO check child shapes too
        return this.mainShape.isNormalizedPointInsideShape(this.normalizePoint(p), offset);
    }

    normalizePoint(p: IPoint) {
        const {x, y} = this.center;
        const sina = Math.sin(-this.a * to_radians);
        const cosa = Math.cos(-this.a * to_radians);
        const rotatedX = cosa * (p.x - x) - sina * (p.y - y);
        const rotatedY = cosa * (p.y - y) + sina * (p.x - x);
        return point(rotatedX, rotatedY);
    }
}

class DragObject extends ObjBase {
    public did: string;
    public id: string;

    public name: "dragobject" = "dragobject";

    constructor(private ctx: CanvasRenderingContext2D, values, defId) {
        super();
        this.did = defId;
        this.id = getValue(values.id, defId);
        values.id = this.id;

        this.draw = Empty;
        this.draggableObject = {};
        this.draggablePin = getValueDef(values, "pin.draggable", false); // TODO: not used according to search
        if (this.draggablePin) {
            this.type = "pin";
            this.draggableObject.type = getValueDef(values, "type", "textbox", true);
        } else {
            this.type = getValueDef(values, "type", "textbox", true);
        }
        if (values.state === "state") {
            this.position = values.position;
            this.x = this.position[0] - values.pinpointoffsetx; // TODO: these are not used according to search
            this.y = this.position[1] - values.pinpointoffsety;
        } else {
            this.position = getValueDef(values, "position", [0, 0]);
            this.x = this.position[0];
            this.y = this.position[1];
        }
        this.draggableObject.x = this.x;
        this.draggableObject.y = this.y;

        this.origPos = {};
        this.origPos.x = this.x;
        this.origPos.y = this.y;
        this.origPos.pos = this.position;
        this.lock = getValueDef(values, "lock", "");
        this.xlimits = getValueDef(values, "xlimits", null);
        this.ylimits = getValueDef(values, "ylimits", null);

        // If default values of type, size, a or position are changed, check also imagex.py
        if (this.type === "vector") {
            this.size = getValueDef(values, "size", [50, 4]);
        } else {
            this.size = getValueDef(values, "size", [10, 10]);
        }
        this.r1 = getValue(this.size[0], 10);
        this.r2 = getValue(this.size[1], this.r1);
        this.target = null;
        this.currentTarget = null;
        this.ispin = isKeyDef(values, "pin", false);
        this.a = -getValueDef(values, "a", 0);
        this.imgproperties = {};
        this.textboxproperties = {};
        this.vectorproperties = {};

        this.init = shapeFunctions[this.type].init;
        this.pinInit = shapeFunctions.pin.init;
        this.pinInit2 = shapeFunctions.pin.init2;
        this.textbox = {};
        this.textbox.init = shapeFunctions.textbox.init;
        this.textbox.init(values);

        if (this.type === "vector" || this.draggableObject.type === "vector") {
            this.vectorInit = shapeFunctions.vector.init;
            this.vectorInit(values);
            this.vectorDraw = shapeFunctions.vector.draw;
        }

        if (this.type === "img" || this.draggableObject.type === "img") {
            this.init2 = shapeFunctions.img.init2;
            this.imageDraw = shapeFunctions.img.draw;
        }

        this.pinInit(values);
        // this.pinDraw = shapeFunctions['pin'].draw;
        this.textbox.draw = shapeFunctions.textbox.draw;

        this.init(values);
        this.draw = shapeFunctions.pin.draw;
        // this.draw = shapeFunctions[this.type].draw;
    }

    draw() {
        // TODO
    }

    get center(): IPoint {
        return point(this.x, this.y); // TODO
    }
}

class Target extends ObjBase {
    public name: "target" = "target";

    constructor(dt: DragTask, values, defId) {
        super();
        this.did = defId;
        this.id = getValue(values.id, defId);
        values.id = this.id;
        this.ctx = dt.ctx;
        this.maxObjects = getValue(values.maxObjects, 1000);
        this.objectCount = 0;
        // If default values of type, size, a or position are changed, check also imagex.py
        this.position = getValueDef(values, "position", [0, 0]);
        this.x = this.position[0];
        this.y = this.position[1];
        this.a = -getValueDef(values, "a", 0);
        this.snap = getValueDef(values, "snap", true);
        this.snapOffset = getValueDef(values, "snapOffset", [0, 0]);
        this.type = getValueDef(values, "type", "rectangle", true);
        this.size = getValueDef(values, "size", [10, 10]);
        this.imgproperties = {};
        this.textboxproperties = {};
        this.vectorproperties = {};
        this.r1 = getValue(this.size[0], 10);
        this.r2 = getValue(this.size[1], this.r1);
        this.color = getValueDef(values, "color", "Blue");
        this.origColor = getValueDef(values, "color", "Blue");
        this.snapColor = getValueDef(values, "snapColor", "Cyan");
        this.dropColor = getValueDef(values, "dropColor", this.snapColor);
        this.init = shapeFunctions[this.type].init;
        this.init(values);
        // this.textboxInit = shapeFunctions['textbox'].init;
        // this.textBoxDraw = shapeFunctions['textbox'].draw;
        this.draw = shapeFunctions[this.type].draw;
    }

    draw() {
        // TODO
    }

    get center(): IPoint {
        return undefined;
    }
}

class FixedObject extends ObjBase {
    public name: "fixedobject" = "fixedobject";

    constructor(dt: DragTask, values, defId?) {
        super();
        this.did = defId;
        this.id = getValue(values.id, defId);
        values.id = this.id;
        if (values.name === "background") {
            this.type = "img";
        } else {
            this.type = getValueDef(values, "type", "rectangle", true);
        }
        this.ctx = dt.ctx;
        // If default values of type, size, a or position are changed, check also imagex.py
        this.position = getValueDef(values, "position", [0, 0]);
        this.x = this.position[0];
        this.y = this.position[1];
        this.a = -getValueDef(values, "a", 0);
        this.color = getValueDef(values, "color", "Blue");
        this.size = getValueDef(values, "size", [10, 10]);
        this.r1 = getValue(this.size[0], 10);
        this.r2 = getValue(this.size[1], this.r1);
        this.imgproperties = {};
        this.textboxproperties = {};
        this.vectorproperties = {};

        this.init = shapeFunctions[this.type].init;
        if (this.name === "fixedobject") {
            this.textbox = {};
            this.textbox.init = shapeFunctions.textbox.init;
            this.textbox.init(values);
            this.textbox.draw = shapeFunctions.textbox.draw;
        }
        if (this.type === "img") {
            this.init2 = shapeFunctions.img.init2;
        }
        this.imageDraw = shapeFunctions.img.draw;
        this.vectorDraw = shapeFunctions.vector.draw;

        this.init(values);
        this.draw = shapeFunctions[this.type].draw;
    }

    draw() {
        // TODO
    }

    get center(): IPoint {
        return undefined;
    }
}

class Shape {
    // All shapes are rectangular or elliptical for now, so we can have these here.
    constructor(protected r1: number, protected r2: number) {

    }

    /**
     * Checks if a normalized point is inside this shape.
     * Default implementation assumes a rectangular shape.
     * @param p point to check
     * @param offset how near the shape the point should be to be considered inside the shape. Default 0.
     */
    isNormalizedPointInsideShape(p: IPoint, offset?: number) {
        const r1 = this.r1 + (offset || 0);
        const r2 = this.r2 + (offset || 0);
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
        r1: number,
        r2: number,
    ) {
        super(r1, r2);
    }

    draw() {
        this.ctx.strokeStyle = this.color;
        this.ctx.lineWidth = this.lineWidth;
        this.ctx.save();
        this.ctx.beginPath();
        this.ctx.scale(this.r1 / 2, this.r2 / 2);
        this.ctx.arc(0, 0, 1, 0, 2 * Math.PI, false);
        this.ctx.restore();
        this.ctx.stroke();
    }

    isNormalizedPointInsideShape(p: IPoint, offset?: number) {
        const r1 = this.r1 + (offset || 0);
        const r2 = this.r2 + (offset || 0);
        return (Math.pow(p.x, 2) / Math.pow(r1 / 2, 2)) +
            (Math.pow(p.y, 2) / Math.pow(r2 / 2, 2)) <= 1;
    }
}

class Rectangle {
    constructor(
        protected readonly ctx: CanvasRenderingContext2D,
        protected readonly color: string,
        protected readonly lineWidth: number = 2,
        protected readonly cornerRadius: number = 0,
        protected readonly r1: number,
        protected readonly r2: number,
    ) {
        if (this.cornerRadius > this.r1 / 2 || this.cornerRadius > this.r2 / 2) {
            if (this.cornerRadius > this.r1 / 2) {
                this.cornerRadius = this.r1 / 2;
            }
            if (this.cornerRadius > this.r2 / 2) {
                this.cornerRadius = this.r2 / 2;
            }
        }
    }

    draw() {
        this.ctx.strokeStyle = this.color;
        this.ctx.lineWidth = this.lineWidth;
        this.ctx.save();
        this.ctx.beginPath();
        this.ctx.moveTo(-this.r1 / 2 - 1 + this.cornerRadius, -this.r2 / 2);
        this.ctx.lineTo(this.r1 / 2 - this.cornerRadius, -this.r2 / 2);
        this.ctx.arc(this.r1 / 2 - this.cornerRadius, -this.r2 / 2 + this.cornerRadius,
            this.cornerRadius, 1.5 * Math.PI, 0);
        this.ctx.lineTo(this.r1 / 2, this.r2 / 2 - this.cornerRadius);
        this.ctx.arc(this.r1 / 2 - this.cornerRadius, this.r2 / 2 - this.cornerRadius,
            this.cornerRadius, 0, 0.5 * Math.PI);
        this.ctx.lineTo(-this.r1 / 2 + this.cornerRadius, this.r2 / 2);
        this.ctx.arc(-this.r1 / 2 + this.cornerRadius, this.r2 / 2 - this.cornerRadius,
            this.cornerRadius, 0.5 * Math.PI, Math.PI);
        this.ctx.lineTo(-this.r1 / 2, -this.r2 / 2 + this.cornerRadius);
        this.ctx.arc(-this.r1 / 2 + this.cornerRadius, -this.r2 / 2 +
            this.cornerRadius, this.cornerRadius, Math.PI, 1.5 * Math.PI);
        this.ctx.restore();
        this.ctx.stroke();
    }
}

const auxctx = document.createElement("canvas").getContext("2d")!;

class Textbox {
    private lines: string[];
    private textwidth: number;
    private textHeight: number;
    private rect: Rectangle;
    private r1: number;
    private r2: number;
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
        size: IPoint | undefined,
        text: string,
        protected readonly font: string,
    ) {
        this.lines = text.split("\n");
        auxctx.font = this.font;
        const lineWidths = this.lines.map((line) => auxctx.measureText(line).width);
        this.textwidth = Math.max(...lineWidths);
        this.textHeight = parseInt(auxctx.font, 10) * 1.1;

        if (size) {
            this.r1 = size.x;
            this.r2 = size.y;
        } else {
            this.r1 = this.textwidth + this.leftMargin + this.rightMargin;
            this.r2 = (this.textHeight * this.lines.length)
                + this.topMargin + this.bottomMargin;
        }
        if (this.cornerRadius > this.r1 / 2 || this.cornerRadius > this.r2 / 2) {
            if (this.cornerRadius > this.r1 / 2) {
                this.cornerRadius = this.r1 / 2;
            }
            if (this.cornerRadius > this.r2 / 2) {
                this.cornerRadius = this.r2 / 2;
            }
        }
        this.rect = new Rectangle(ctx, color, lineWidth, cornerRadius, this.r1, this.r2);
    }

    draw() {
        this.rect.draw();
        let textStart = this.topMargin;
        for (let i = 0; i < this.lines.length; i++) {
            this.ctx.fillText(this.lines[i], this.leftMargin, textStart);
            textStart += this.textHeight;
        }
        this.ctx.lineWidth = this.lineWidth;
        this.ctx.strokeStyle = this.borderColor;
        this.ctx.stroke();
    }
}

class Vector {
    constructor(
        private readonly ctx: CanvasRenderingContext2D,
        private readonly color: string,
        private readonly lineWidth: number = 2,
        private readonly r1: number,
        private readonly r2: number,
        private readonly arrowHeadWidth: number = r2 * 3,
        private readonly arrowHeadLength: number = r2 * 5,
    ) {
    }

    draw() {
        this.ctx.strokeStyle = this.color;
        this.ctx.fillStyle = this.ctx.strokeStyle;
        this.ctx.save();
        this.ctx.beginPath();
        this.ctx.lineTo(0, this.r2);
        this.ctx.lineTo(this.r1 - this.arrowHeadLength, this.r2);
        this.ctx.lineTo(this.r1 - this.arrowHeadLength, this.r2 / 2 +
            this.arrowHeadWidth / 2);
        this.ctx.lineTo(this.r1, this.r2 / 2);
        this.ctx.lineTo(this.r1 - this.arrowHeadLength,
            this.r2 / 2 - this.arrowHeadWidth / 2);
        this.ctx.lineTo(this.r1 - this.arrowHeadLength, 0);
        this.ctx.lineTo(0, 0);
        this.ctx.fill();
        this.ctx.restore();
    }
}

class DImage {
    constructor(
        private readonly ctx: CanvasRenderingContext2D,
        private readonly image: HTMLImageElement,
        private readonly r1: number = image.width,
        private readonly r2: number = image.height,
    ) {
    }

    draw() {
        this.ctx.drawImage(this.image, 0, 0, this.r1, this.r2);
    }
}

function isTouchDevice() {
    return typeof window.ontouchstart !== "undefined";
}

const to_radians = Math.PI / 180;

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

const ObjectSpec = t.union([]);

const ObjectType = t.union([
    t.literal("textbox"),
    t.literal("vector"),
    t.literal("ellipse"),
    t.literal("img"),
    t.literal("rectangle"),
]);

const PinAlignment = t.union([
    t.literal("north"),
    t.literal("south"),
    t.literal("west"),
    t.literal("east"),
    t.literal("northeast"),
    t.literal("northwest"),
    t.literal("southwest"),
    t.literal("southeast"),
    t.literal("center"),
]);

type PinAlign = t.TypeOf<typeof PinAlignment>;

const ImgProps = t.type({
    src: t.string,
    textbox: withDefault(t.boolean, true),
});

const EmptyCoord = t.readonlyArray(t.number); // TODO should get rid of this by fixing imagex markups

const ValidCoord = t.tuple([t.number, t.number]);

const CoordProp = t.union([ValidCoord, EmptyCoord]);

const TextboxProps = t.partial({
    borderColor: t.string,
    borderWidth: t.number,
    cornerradius: t.number,
    fillColor: t.string,
    font: t.string,
    position: CoordProp,
    size: CoordProp,
    text: t.string,
    textColor: t.string,
});

const VectorProps = t.partial({
    arrowheadlength: t.number,
    arrowheadwidth: t.number,
    color: t.string,
});

const PinProps = t.partial({
    position: t.partial({
        align: PinAlignment,
    }),
    visible: t.boolean,
});

const FixedObjectProps = t.partial({
    id: t.string,
    imgproperties: ImgProps,
    position: ValidCoord,
    textboxproperties: TextboxProps,
    type: ObjectType,
});

const TargetProps = t.partial({
    color: t.string,
    dropColor: t.string,
    id: t.string,
    points: t.dictionary(t.string, t.number),
    position: ValidCoord,
    size: ValidCoord,
    snapColor: t.string,
    snapOffset: ValidCoord,
    type: ObjectType,
});

const DragObjectProps = t.intersection([
    t.partial({
        lock: t.union([t.literal("x"), t.literal("y")]),
        pin: PinProps,
        xlimits: ValidCoord,
        ylimits: ValidCoord,
    }),
    FixedObjectProps,
]);

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
        return false; // TODO
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
    private vctrl!: ViewCtrl;
    private replyImage?: string;
    private replyHTML?: string;
    private canvas!: HTMLCanvasElement;
    private dt!: DragTask;

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
        this.freeHandDrawing.update = () => {
            const phase = this.$root.$$phase;
            if (phase == "$apply" || phase == "$digest") {
                return;
            }
            this.$apply();
        };

        this.canvas = this.element.find(".canvas")[0] as HTMLCanvasElement;

        const dt = new DragTask(this.canvas, this);
        this.dt = dt;

        const userObjects = this.attrs.markup.objects;

        const userTargets = this.attrs.markup.targets;
        const userFixedObjects = this.attrs.markup.fixedobjects;
        const fixedobjects = [];
        const targets = [];
        const objects = [];

        this.defaults = this.attrs.markup.defaults;

        if (this.attrs.markup.background) {
            this.attrs.markup.background.name = "background";
            const background = new FixedObject(dt, this.attrs.markup.background);
            dt.drawObjects.push(background);
        }

        this.error = "";

        if (userFixedObjects) {
            for (let i = 0; i < userFixedObjects.length; i++) {
                if (userFixedObjects[i]) {
                    fixedobjects.push(new FixedObject(dt, userFixedObjects[i], "fix" + (i + 1)));
                }
            }
        }

        if (userTargets) {
            for (let i = 0; i < userTargets.length; i++) {
                if (userTargets[i]) {
                    targets.push(new Target(dt, userTargets[i], "trg" + (i + 1)));
                }
            }
        }

        if (userObjects) {
            for (let i = 0; i < userObjects.length; i++) {
                if (userObjects[i]) {
                    const newObject = new DragObject(dt, userObjects[i], "obj" + (i + 1));
                }
                objects.push(newObject);
                if (!this.drags) {
                    this.drags = [];
                }
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

        for (let i = 0; i < fixedobjects.length; i++) {
            dt.drawObjects.push(fixedobjects[i]);
        }

        for (let i = 0; i < targets.length; i++) {
            dt.drawObjects.push(targets[i]);
        }

        for (let i = 0; i < objects.length; i++) {
            dt.drawObjects.push(objects[i]);
        }

        dt.draw();

        this.objects = objects;

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
        if (this.preview) {
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

// Reset the positions of dragobjects.
    resetExercise() {
        this.error = "";
        this.result = "";
        this.freeHandDrawing.clear();
        // Objects dragged by user.
        const objects = this.drags;

        if (objects) {
            for (let i = 0; i < objects.length; i++) {
                const obj = objects[i];
                obj.x = obj.origPos.x;
                obj.y = obj.origPos.y;
                obj.position = obj.origPos.pos;
            }
        }
        // Draw the excercise so that reset appears instantly.

        const dobjs = this.dt.drawObjects;

        for (let i = dobjs.length - 1; i--;) {
            if (dobjs[i].did === "-") {
                dobjs.splice(i, 1);
            }
        }
        this.rightAnswersSet = false;

        this.dt.draw();
    }

    svgImageSnippet() {
        return $sce.trustAsHtml(this.replyHTML);
    }

    videoPlay() {
        const video = this.videoPlayer;
        if (video.fakeVideo) {
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
        if (video.fakeVideo) {
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
