import angular, {IRootElementService, IScope} from "angular";
import ngSanitize from "angular-sanitize";
import * as t from "io-ts";
import {editorChangeValue} from "../editor/editorScope";
import {$http, $q, $sce, $window} from "../util/ngimport";
import {markAsUsed, to} from "../util/utils";
import {GenericPluginMarkup, PluginBase, withDefault} from "./util";
import {ViewCtrl} from "../document/viewctrl";

markAsUsed(ngSanitize);

const imagexApp = angular.module("imagexApp", ["ngSanitize"]);

let globalPreviewColor = "#fff";

interface IVideoPlayer {
    currentTime: number;
    fakeVideo: boolean;
}

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

class FreeHand {
    private videoPlayer: IVideoPlayer;
    private emotion: boolean;
    public freeDrawing: ILineSegment[];
    private scope: ImageXController;
    private redraw?: () => void;
    private update?: () => void;
    private prevPos?: TuplePoint;
    private lastDrawnSeg?: number;
    private lastVT?: number;

    constructor(scope: ImageXController, drawData: ILineSegment[]) {
        this.freeDrawing = [];
        this.emotion = scope.emotion;
        this.scope = scope;
        this.videoPlayer = {currentTime: 1e60, fakeVideo: true};  // fake
    }

    draw(ctx: CanvasRenderingContext2D) {
        if (this.emotion) {
            if (this.scope.teacherMode)
                if (this.videoPlayer.fakeVideo)
                    this.drawCirclesVideo(ctx, this.freeDrawing, this.videoPlayer);
                else if (this.scope.analyzeDot)
                //Siirrä johonkin missä kutsutaan vain kerran.
                    this.videoPlayer.ontimeupdate = () => this.drawCirclesVideoDot(ctx, this.freeDrawing, this.videoPlayer);
                else
                //Siirrä johonkin missä kutsutaan vain kerran.
                    this.videoPlayer.ontimeupdate = () => this.drawCirclesVideo(ctx, this.freeDrawing, this.videoPlayer);
        } else drawFreeHand(ctx, this.freeDrawing);
    }

    startSegment(pxy?: IPoint) {
        if (!pxy) return;
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
        if (!pxy) return;
        let p: TuplePoint;
        if (this.emotion) {
            if (this.freeDrawing.length != 0) return;
            p = [Math.round(pxy.x), Math.round(pxy.y),
                Date.now() / 1000, this.videoPlayer.currentTime];
        } else p = [Math.round(pxy.x), Math.round(pxy.y)];
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
        if (!pxy) return;
        let p: TuplePoint;
        if (this.emotion) {
            p = [Math.round(pxy.x), Math.round(pxy.y),
                Date.now() / 1000, this.videoPlayer.currentTime];
        } else p = [Math.round(pxy.x), Math.round(pxy.y)];
        const n = this.freeDrawing.length;
        if (n == 0) this.startSegment(p); // TODO this seems broken
        else {
            const ns = this.freeDrawing[n - 1];
            //    ns.lines = [p];
            ns.lines.push(p);
        }
        if (!this.scope.lineMode || this.emotion)
            this.prevPos = p;
    }

    popPoint(minlen: number) {
        const n = this.freeDrawing.length;
        if (n == 0) return;
        const ns = this.freeDrawing[n - 1];
        if (ns.lines.length > minlen) ns.lines.pop();
    }

    popSegment(minlen: number) {
        const n = this.freeDrawing.length;
        if (n <= minlen) return;
        this.freeDrawing.pop();
        if (this.redraw) this.redraw();
    }

    addPointDraw(ctx: CanvasRenderingContext2D, pxy?: IPoint) {
        if (!pxy) return;
        if (this.scope.lineMode) {
            this.popPoint(1);
            if (this.redraw) this.redraw();
        }
        if (!this.emotion)
            this.line(ctx, this.prevPos, [pxy.x, pxy.y]);
        this.addPoint(pxy);
    }

    clear() {
        this.freeDrawing = [];
        if (this.redraw) this.redraw();
    }

    setColor(newColor: string) {
        this.scope.color = newColor;
        if (this.update) this.update();
        this.startSegment(this.prevPos);
    }

    setWidth(newWidth: number) {
        this.scope.w = newWidth;
        if (this.scope.w < 1) this.scope.w = 1;
        if (this.update) this.update();
        this.startSegment(this.prevPos);
    }

    incWidth(dw: number) {
        this.setWidth(this.scope.w + dw);
    }

    setLineMode(newMode: boolean) {
        this.scope.lineMode = newMode;
        if (this.update) this.update();
    }

    flipLineMode() {
        this.setLineMode(!this.scope.lineMode);
    }

    line(ctx: CanvasRenderingContext2D, p1: TuplePoint | undefined, p2: TuplePoint) {
        if (!p1 || !p2) return;
        ctx.beginPath();
        ctx.strokeStyle = this.scope.color;
        ctx.lineWidth = this.scope.w;
        ctx.moveTo(p1[0], p1[1]);
        ctx.lineTo(p2[0], p2[1]);
        ctx.stroke();
    }

    drawCirclesVideoDot(ctx: CanvasRenderingContext2D, dr: ILineSegment[], videoPlayer: IVideoPlayer) {
        for (let dri = 0; dri < dr.length; dri++) {
            const seg = dr[dri];
            var vt = videoPlayer.currentTime;
            // console.log("vt: " + vt + " " + this.lastDrawnSeg );
            var seg1 = -1;
            var seg2 = 0;
            var s = "";
            for (let lni = 0; lni < seg.lines.length; lni++) {
                if (vt < seg.lines[lni][3])
                    break;
                seg1 = seg2;
                seg2 = lni;
            }


            // console.log("" + seg1 + "-" + seg2 + ": " + seg.lines[seg2][3]);
            var drawDone = false;
            if (this.scope.dotVisibleTime && this.lastVT && (vt - this.lastVT) > this.scope.dotVisibleTime) { // remove old dot if needed
                this.scope.draw();
                drawDone = true;
            }
            if (this.scope.showVideoTime) showTime(ctx, vt, 0, 0, "13px Arial");
            if (this.lastDrawnSeg == seg2) return;
            this.lastDrawnSeg = -1;
            if (!drawDone) this.scope.draw();
            if (this.scope.showVideoTime) showTime(ctx, vt, 0, 0, "13px Arial");
            if (seg1 < 0 || vt == 0 || seg.lines[seg2][3] == 0) return;

            // console.log("" + seg1 + "-" + seg2 + ": " + seg.lines[seg2][3]);


            ctx.beginPath();
            this.lastVT = vt;
            applyStyleAndWidth(ctx, seg);
            ctx.moveTo(seg.lines[seg1][0], seg.lines[seg1][1]);
            if (this.scope.drawLast)
                ctx.lineTo(seg.lines[seg2][0], seg.lines[seg2][1]);
            ctx.stroke();
            drawFillCircle(ctx, 30, seg.lines[seg2][0], seg.lines[seg2][1], "black", 0.5);
            if (this.scope.showTimes) {
                s = dateToString(seg.lines[seg2][3], false);
                drawText(ctx, s, seg.lines[seg2][0], seg.lines[seg2][1], "13px Arial");
            }

            this.lastDrawnSeg = seg2;
        }
    }

    drawCirclesVideo(ctx: CanvasRenderingContext2D, dr: ILineSegment[], videoPlayer: IVideoPlayer) {
        if (!videoPlayer.fakeVideo) this.scope.draw();
        const vt = videoPlayer.currentTime;
        for (let dri = 0; dri < dr.length; dri++) {
            const seg = dr[dri];
            if (seg.lines.length < 1) continue;
            var s = "";
            ctx.beginPath();
            applyStyleAndWidth(ctx, seg);
            ctx.moveTo(seg.lines[0][0], seg.lines[0][1]);
            if (videoPlayer.fakeVideo) {
                s = dateToString(seg.lines[0][2], true);
                drawText(ctx, s, seg.lines[0][0], seg.lines[0][1]);
            } else {
                if (vt >= seg.lines[0][3])
                    drawFillCircle(ctx, 5, seg.lines[0][0], seg.lines[0][1], "black", 0.5);
            }
            for (let lni = 1; lni < seg.lines.length; lni++) {
                if (vt < seg.lines[lni][3])
                    break;
                ctx.lineTo(seg.lines[lni][0], seg.lines[lni][1]);
                if (videoPlayer.fakeVideo) {
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
        if (seg.lines.length < 2) continue;
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
    //return dt.toLocaleTimeString('fi-FI');
    //return dt.format("HH:mm:ss.f");
    var hs = "";
    if (h) hs = pad(dt.getHours(), 2) + ":";
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
        <canvas id="canvas" tabindex="1" width={{$ctrl.canvaswidth}} height={{$ctrl.canvasheight}}
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
    if (!p1 || !p2) return;
    const p = ctx;
    p.globalAlpha = tr;
    p.beginPath();
    p.fillStyle = c;
    p.arc(p1, p2, r, 0, 2 * Math.PI);
    p.fill();
    p.globalAlpha = 1;
}

function toRange(range: TuplePoint, p: number) {
    if (!range) return p;
    // if (p.length < 2) return p; TODO this seems wrong
    if (p < range[0]) return range[0];
    if (p > range[1]) return range[1];
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

interface IObject extends IPoint {
    a: number;
    r1: number;
    r2: number;
    name: string;
    type: "rectangle" | "ellipse" | "img" | "textbox" | "vector";
}

function isObjectOnTopOf(position: IPoint, object: IObject, name: string | undefined, grabOffset: number) {
    if (!position)
        return false;
    if (!object)
        return false;
    const sina = Math.sin(-object.a * to_radians);
    const cosa = Math.cos(-object.a * to_radians);
    const rotatedX = cosa * (position.x - object.x) - sina * (position.y - object.y);
    const rotatedY = cosa * (position.y - object.y) + sina * (position.x - object.x);

    if (object.name === "target") {
        if (object.type === "rectangle") {
            if (rotatedX >= -object.r1 / 2 && rotatedX <= object.r1 / 2 &&
                rotatedY >= -object.r2 / 2 && rotatedY <= object.r2 / 2) {
                if (name && object.name === name) return true;
                else if (!name) return true;
            }
        } else if (object.type === "ellipse") {
            if ((Math.pow(rotatedX, 2) / Math.pow(object.r1 / 2, 2)) +
                (Math.pow(rotatedY, 2) / Math.pow(object.r2 / 2, 2)) <= 1) {
                if (name && object.name === name) return true;
                else if (!name) return true;
            }
        }
    } else if (object.name === "dragobject") {
        if (object.type === "img" || object.type === "textbox") {
            if (rotatedX >= -object.pinPosition.off.x - object.pinPosition.x &&
                rotatedX <= object.r1 - object.pinPosition.off.x - object.pinPosition.x &&
                rotatedY >= -object.pinPosition.off.y - object.pinPosition.y &&
                rotatedY <= object.r2 - object.pinPosition.off.y - object.pinPosition.y
                + grabOffset) {
                if (name && object.name === name) return true;
                else if (!name) return true;
            }
        }
        if (object.type === "vector") {
            if (rotatedX >= -object.pinPosition.off.x - object.pinPosition.x &&
                rotatedX <= object.r1 - object.pinPosition.off.x - object.pinPosition.x &&
                rotatedY >= -object.r2 / 2 - object.arrowHeadWidth / 2
                - object.pinPosition.off.y - object.pinPosition.y &&
                rotatedY <= object.r2 / 2 + object.arrowHeadWidth / 2
                - object.pinPosition.off.y - object.pinPosition.y + grabOffset) {
                if (name && object.name === name) return true;
                else if (!name) return true;
            }
        }
    }
}

function areObjectsOnTopOf(position: IPoint, objects: IObject[], name: string, grabOffset: number | undefined) {
    for (let i = objects.length - 1; i >= 0; i--) {
        const collision = isObjectOnTopOf(position, objects[i], name, 0);
        if (collision) {
            return objects[i];
        }
    }
    if (grabOffset) {
        for (let i = objects.length - 1; i >= 0; i--) {
            const collision = isObjectOnTopOf(position, objects[i], name, grabOffset);
            if (collision && objects[i].name === "dragobject") {
                return objects[i];
            }
        }
    }
    return null;
}

type MouseOrTouch = MouseEvent | Touch;

class DragTask {
    public ctx: CanvasRenderingContext2D;
    private mousePosition: IPoint;

    constructor(private canvas: HTMLCanvasElement, private scope: ImageXController) {
        const th = this;
        this.ctx = canvas.getContext("2d")!;
        this.drawObjects = [];
        this.activeDragObject = null;
        this.mousePosition = {x: 0, y: 0};
        this.freeHand = scope.freeHandDrawing;
        this.interval = setTimeout(this.draw, 20);

        let mouseDown = false;

        this.canvas.style.touchAction = "double-tap-zoom"; // To get IE and EDGE touch to work
        this.freeHand.redraw = this.redraw;

        this.canvas.style.msTouchAction = "none";
        this.canvas.addEventListener("mousemove", event => {
            th.moveEvent(event, event);
        });
        this.canvas.addEventListener("touchmove", event => {
            th.moveEvent(event, this.te(event));
        });
        this.canvas.addEventListener("mousedown", event => {
            th.downEvent(event, event);
        });
        this.canvas.addEventListener("touchstart", event => {
            th.downEvent(event, this.te(event));
        });
        this.canvas.addEventListener("mouseup", event => {
            th.upEvent(event, event);
        });
        this.canvas.addEventListener("touchend", event => {
            th.upEvent(event, this.te(event));
        });

        if (scope.freeHandShortCuts)
        // this.canvas.parentElement.parentElement.addEventListener( "keypress", function(event) {
        /*
        this.canvas.addEventListener("keydown", function(event) {
            const c = String.fromCharCode(event.keyCode);

        });
        */
            this.canvas.addEventListener("keypress", event => {
                const c = String.fromCharCode(event.keyCode);
                if (event.keyCode == 26) {
                    th.freeHand.popSegment(0);
                }
                if (c == "c") {
                    th.freeHand.clear();
                    th.draw();
                }
                if (c == "r") th.freeHand.setColor("#f00");
                if (c == "b") th.freeHand.setColor("#00f");
                if (c == "y") th.freeHand.setColor("#ff0");
                if (c == "g") th.freeHand.setColor("#0f0");
                if (c == "+") th.freeHand.incWidth(+1);
                if (c == "-") th.freeHand.incWidth(-1);
                if (c == "1") th.freeHand.setWidth(1);
                if (c == "2") th.freeHand.setWidth(2);
                if (c == "3") th.freeHand.setWidth(3);
                if (c == "4") th.freeHand.setWidth(4);
                if (c == "l") th.freeHand.flipLineMode();
                if (c == "f" && scope.freeHandShortCut) {
                    scope.freeHand = !scope.freeHand;
                    scope.$apply();
                }
            }, false);

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

    draw() {
        const canvas = this.canvas;
        this.ctx.fillStyle = getValue1(scope.background, "color", "white");
        this.ctx.fillRect(0, 0, canvas.width, canvas.height);

        for (let i = 0; i < this.drawObjects.length; i++) {
            try {
                this.drawObjects[i].ctx = this.ctx;
                if (this.activeDragObject) {
                    const dobj = this.activeDragObject;
                    let p;
                    if (dobj.lock != "x")
                        dobj.x = toRange(dobj.xlimits, this.mousePosition.x - dobj.xoffset);
                    if (dobj.lock != "y")
                        dobj.y = toRange(dobj.ylimits, this.mousePosition.y - dobj.yoffset);
                    if (this.drawObjects[i] == dobj) {
                        topmostIndex = i;
                        topmostElement = this.drawObjects[i];
                    }
                }
                if (this.activeDragObject != this.drawObjects[i]) {
                    // kutsutaan objektin omaa piirtoa
                    this.drawObjects[i].draw(this.drawObjects[i]);
                }
                if (this.drawObjects[i].name == "dragobject") {
                    if (this.activeDragObject) {
                        const onTopOf = areObjectsOnTopOf(this.activeDragObject,
                            this.drawObjects, "target");
                        if (onTopOf && onTopOf.objectCount < onTopOf.maxObjects) {
                            onTopOf.color = onTopOf.snapColor;
                        }
                        const onTopOfB = areObjectsOnTopOf(this.drawObjects[i],
                            this.drawObjects, "target");
                        if (onTopOfB && this.drawObjects[i] !== this.activeDragObject)
                            onTopOfB.color = onTopOfB.dropColor;
                    } else {
                        const onTopOfA = areObjectsOnTopOf(this.drawObjects[i],
                            this.drawObjects, "target");
                        if (onTopOfA) {
                            onTopOfA.color = onTopOfA.dropColor;
                            //onTopOfA.objectCount++;
                        }
                    }
                } else if (this.drawObjects[i].name === "target") {
                    this.drawObjects[i].color = this.drawObjects[i].origColor;

                }
                //$log.info(onTopOf);
            } catch (err) {
                scope.error += "draw " + this.drawObjects[i].id + ": " + err + "\n";
            }
        }
        this.freeHand.draw(this.ctx);
        if (this.activeDragObject) this.activeDragObject.draw(this.activeDragObject);
    }

    downEvent(event: Event, p: MouseOrTouch) {
        this.mousePosition = getPos(this.canvas, p);
        this.activeDragObject =
            areObjectsOnTopOf(this.mousePosition, this.drawObjects, "dragobject");

        if (scope.emotion) {
            drawFillCircle(this.canvas, 30, this.mousePosition.x, this.mousePosition.y, "black", 0.5);
            this.freeHand.startSegmentDraw(this.canvas, this.mousePosition);
            this.freeHand.addPointDraw(this.canvas, this.mousePosition);
            if (scope.autosave) scope.imagexScope.save();
        }

        if (this.activeDragObject) {
            this.canvas.style.cursor = "pointer";
            this.activeDragObject.xoffset = this.mousePosition.x - this.activeDragObject.x;
            this.activeDragObject.yoffset = this.mousePosition.y - this.activeDragObject.y;
            // event.preventDefault();
            this.draw();
        } else if (scope.freeHand) {
            this.canvas.style.cursor = "pointer";
            mouseDown = true;
            if (!scope.emotion) this.freeHand.startSegmentDraw(this.redraw, this.mousePosition);
        }

        if (scope.preview) {
            this.scope.coords = `[${Math.round(this.mousePosition.x)}, ${Math.round(this.mousePosition.y)}]`;
            editorChangeValue(["position:"], this.scope.coords);
        }
    }

    redraw() {
        th.draw();
    }

    moveEvent(event: Event, p: MouseOrTouch) {
        if (this.activeDragObject) {
            // if (this.activeDragObject)
            if (event != p) event.preventDefault();
            this.mousePosition = getPos(this.canvas, p);
            if (!scope.emotion) this.draw();
        } else if (mouseDown) {
            if (event != p) event.preventDefault();
            if (!scope.emotion) this.freeHand.addPointDraw(this.ctx, getPos(this.canvas, p));
        }
    }

    upEvent(event: Event, p: MouseOrTouch) {

        if (this.activeDragObject) {
            this.canvas.style.cursor = "default";
            if (event != p) event.preventDefault();
            this.mousePosition = getPos(this.canvas, p);

            const isTarget = areObjectsOnTopOf(this.activeDragObject,
                this.drawObjects, "target");
            this.drawObjects.splice(topmostIndex, 1);
            this.drawObjects.splice(this.drawObjects.length, 0, topmostElement);

            if (isTarget) {
                if (isTarget.objectCount < isTarget.maxObjects) {
                    if (isTarget.snap) {
                        this.activeDragObject.x = isTarget.x + isTarget.snapOffset[0];
                        this.activeDragObject.y = isTarget.y + isTarget.snapOffset[1];
                    }
                }
            }

            this.activeDragObject = null;
            if (!scope.emotion) this.draw();

        } else if (mouseDown) {
            this.canvas.style.cursor = "default";
            mouseDown = false;
            this.freeHand.endSegment();
        }
        setTimeout(this.draw, 1000);
        if (!scope.emotion) this.draw(); // TODO: Miksi tama on taalla?
    }

    te(event: TouchEvent) {
        return event.touches[0] || event.changedTouches[0];
    }

    addRightAnswers() {
        // have to add handler for drawing finalanswer here. no way around it.
        if (scope.rightAnswersSet) {
            dt.draw();
            return;
        }
        if (!scope.answer || !scope.answer.rightanswers) return;

        const rightdrags = scope.answer.rightanswers;
        const dragtable = scope.objects;
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
                    //line.draw();
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

class DragObject {
    public did: string;
    public id: string;
    public x: number;
    public y: number;

    constructor(private ctx: CanvasRenderingContext2D, values, defId) {
        this.did = defId;
        this.id = getValue(values.id, defId);
        values.id = this.id;

        this.draw = Empty;
        this.draggableObject = {};
        this.draggablePin = getValueDef(values, "pin.draggable", false); // TODO: not used according to search
        if (this.draggablePin) {
            this.type = "pin";
            this.draggableObject.type = getValueDef(values, "type", "textbox", true);
        } else
            this.type = getValueDef(values, "type", "textbox", true);
        this.name = "dragobject";
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
        if (this.type === "vector") this.size = getValueDef(values, "size", [50, 4]);
        else this.size = getValueDef(values, "size", [10, 10]);
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
}

class Target {
    constructor(dt: DragTask, values, defId) {
        this.did = defId;
        this.id = getValue(values.id, defId);
        values.id = this.id;
        this.name = "target";
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
        //this.textboxInit = shapeFunctions['textbox'].init;
        //this.textBoxDraw = shapeFunctions['textbox'].draw;
        this.draw = shapeFunctions[this.type].draw;
    }
}

class FixedObject {
    constructor(dt: DragTask, values, defId?) {
        this.did = defId;
        this.id = getValue(values.id, defId);
        values.id = this.id;
        if (values.name === "background") {
            this.type = "img";
            this.name = "background";
        } else {
            this.type = getValueDef(values, "type", "rectangle", true);
            this.name = "fixedobject";
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

class Ellipse {
    constructor(
        private readonly ctx: CanvasRenderingContext2D,
        private readonly color: string,
        private readonly lineWidth: number = 2,
        private readonly r1: number,
        private readonly r2: number,
    ) {
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
            if (this.cornerRadius > this.r1 / 2) this.cornerRadius = this.r1 / 2;
            if (this.cornerRadius > this.r2 / 2) this.cornerRadius = this.r2 / 2;
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
            if (this.cornerRadius > this.r1 / 2) this.cornerRadius = this.r1 / 2;
            if (this.cornerRadius > this.r2 / 2) this.cornerRadius = this.r2 / 2;
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
    id: t.string,
    points: t.dictionary(t.string, t.number),
    position: ValidCoord,
    size: ValidCoord,
    snapColor: t.string,
    type: ObjectType,
});

const DragObjectProps = t.intersection([
    t.partial({
        pin: PinProps,
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
    private static $inject = ["$scope", "$element"];

    private muokattu: boolean;
    private result: string;
    private error?: string;
    private cursor: string;
    private tries = 0;
    public w = 0;
    public freeHand = false;
    public lineMode = false;
    public color = "";
    private previewColor = "";
    public freeHandDrawing!: FreeHand;
    private isRunning: boolean = false;
    public coords = "";
    public drags: DragObject[] = [];
    public userHasAnswered = false;
    private vctrl!: ViewCtrl;

    get emotion() {
        return false; // TODO
    }

    get max_tries() {
        return this.attrs.max_tries;
    }

    constructor(scope: IScope, element: IRootElementService) {
        super(scope, element);
        this.muokattu = false;
        this.result = "";
        this.cursor = "\u0383"; //"\u0347"; // "\u02FD";
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

    get videoPlayer() {
        return $window.videoApp && videoApp.videos[this.attrs.followid];
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

    $onInit() {
        super.$onInit();
        this.tries = this.attrsall.tries;

        // Free hand drawing things:
        this.freeHandDrawing = new FreeHand(this, this.attrsall.freeHandData);

        const vp = this.videoPlayer;
        if (vp) {
            this.freeHandDrawing.videoPlayer = vp;
        }
        if (this.attrs.freeHand === "use") {
            this.freeHand = false;
        }

        this.w = this.attrs.freeHandWidth;
        this.color = this.attrs.freeHandColor;
        this.lineMode = this.attrs.freeHandLine;
        this.freeHandDrawing.update = () => {
            const phase = this.$root.$$phase;
            if (phase == "$apply" || phase == "$digest") return;
            this.$apply();
        };

        this.canvas = this.element.find("#canvas")[0];

        const isTouch = isTouchDevice();
        let grabOffset: number;
        if (isTouch) grabOffset = this.extraGrabAreaHeight;
        else grabOffset = 0;

        const dt = new DragTask(this.canvas, this);
        this.dt = dt;

        // Use these to reset exercise if there is no state.
        // This esoteric marking works as a deep copy.

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
                if (userFixedObjects[i])
                    try {
                        fixedobjects.push(new FixedObject(dt, userFixedObjects[i], "fix" + (i + 1)));
                    } catch (err) {
                        this.error += "init fix" + (i + 1) + ": " + err + "\n";
                    }
            }
        }

        if (userTargets) {
            for (let i = 0; i < userTargets.length; i++) {
                if (userTargets[i])
                    try {
                        targets.push(new Target(dt, userTargets[i], "trg" + (i + 1)));
                    } catch (err) {
                        this.error += "init trg" + (i + 1) + ": " + err + "\n";
                    }
            }
        }

        if (userObjects) {
            for (let i = 0; i < userObjects.length; i++) {
                if (userObjects[i])
                    try {
                        const newObject = new DragObject(dt, userObjects[i], "obj" + (i + 1));
                        objects.push(newObject);
                        if (!this.drags) {
                            this.drags = [];
                        }
                        this.drags.push(newObject);
                    } catch (err) {
                        this.error += "init obj" + (i + 1) + ": " + err + "\n";
                    }
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
                freeHandData: this.freeHandDrawing.freeDrawing,
                finalanswerquery: true,
            },
        };
        let url = "/imagex/answer";
        const plugin = this.getPlugin();
        const taskId = this.getTaskId();
        if (plugin) {
            url = plugin;
            const i = url.lastIndexOf("/");
            if (i > 0) url = url.substring(i);
            url += "/" + taskId + "/answer/";  // Hack piti vahan muuttaa, jotta kone haviaa.
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
            if (dobjs[i].did === "-") dobjs.splice(i, 1);
        }
        this.rightAnswersSet = false;

        this.dt.draw();
    }

    svgImageSnippet() {
        return $sce.trustAsHtml(this.replyHTML);
    }

    videoPlay() {
        const video = this.videoPlayer;
        if (video.fakeVideo) return;
        if (video.paused)
            video.play();
        else
            video.pause();
    }

    videoBeginning() {
        const video = this.videoPlayer;
        if (video.fakeVideo) return;
        if (video.paused)
            video.currentTime = 0;
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

        if (nosave) params.input.nosave = true;
        let url = "/imagex/answer";
        const plugin = this.getPlugin();
        const taskId = this.getTaskId();
        if (plugin) {
            url = plugin;
            const i = url.lastIndexOf("/");
            if (i > 0) url = url.substring(i);
            url += "/" + taskId + "/answer/";  // Hack piti vahan muuttaa, jotta kone haviaa.
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

    protected getAttributeType() {
        return ImageXAll;
    }

    getDefaultMarkup() {
        return {};
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
