import {IScope, IRootElementService, IAttributes} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {timLogTime} from "tim/timTiming";
import {$document, $parse} from "../ngimport";

function setStorage(key, value) {
    value = JSON.stringify(value);
    window.localStorage.setItem(key, value);
}

function getStorage(key: string) {
    let s = window.localStorage.getItem(key);
    if (!s) {
        return s;
    }
    s = JSON.parse(s);
    return s;
}

function getPixels(s) {
    const s2 = s.replace(/px$/, "");
    return Number(s2) || 0;
}

function wmax(value, min, max) {
    if (!value) {
        return min + "px";
    }
    if (value == "auto") {
        return value;
    }
    const v = getPixels(value);

    if (v <= min) {
        return min + "px";
    }
    if (v >= max) {
        return max + "px";
    }
    return value;
}

timApp.directive("timDraggableFixed", [function() {
    return {
        controller: DraggableController,
        replace: false,
        restrict: "A",
    };
}]);

type Pos = {X: number, Y: number};

class DraggableController {
    private static $inject = ["$scope", "$attrs", "$element"];

    private posKey: string;
    private clickFn = null;
    private areaMinimized: boolean = false;
    private areaHeight: number = 0;
    private setLeft: boolean = true;
    private setRight: boolean = false;
    private setBottom: boolean = false;
    private setTop: boolean = true;
    private prevTop: number;
    private prevLeft: number;
    private prevBottom: number;
    private prevRight: number;
    private prevHeight: number;
    private prevWidth: number;
    private upResize: boolean;
    private rightResize: boolean;
    private downResize: boolean;
    private leftResize: boolean;
    private lastPos: Pos;
    private pos: Pos;
    private delta: Pos;
    private element: IRootElementService;
    private scope: IScope;
    private lastPageXYPos = {X: 0, Y: 0};
    private handle = $("<div>", {class: "draghandle"});
    private broadcastMsg: string;

    constructor(scope: IScope, attr: IAttributes, element: IRootElementService) {
        this.element = element;
        if (attr.save) {
            const pageId = window.location.pathname.split("/")[1];  // /velp/???
            this.posKey = attr.save.replace("%%PAGEID%%", pageId);
        }

        this.broadcastMsg = attr.broadcastmsg;

        if (attr.click) {
            if (attr.click === "true") {
                this.clickFn = () => this.minimize();
            } else {
                this.clickFn = $parse(attr.click);
            }
        }

        let closeFn = null;
        if (attr.close) {
            closeFn = $parse(attr.close);
        }

        if (attr.click) {
            const minimizeElem = $("<img>", {
                src: "/static/images/minimize-window-16.png",
                class: "titlebutton minimize",
            });
            minimizeElem.on("click", () => {
                this.clickFn(scope);
            });
            this.handle.append(minimizeElem);
        }
        if (attr.close) {
            const close = $("<img>", {src: "/static/images/close-window-16.png", class: "titlebutton close"});
            close.on("click", () => {
                closeFn(scope);
            });
            this.handle.append(close);
        }
        this.element.prepend(this.handle); // TODO.  Laita elementin ympärille se mitä raahataan.

        attr.$observe("resize", () => {
            if (attr.resize === "true") {
                this.createResizeHandles();
            } else {
                this.removeResizeHandles();
            }
        });

        attr.$observe("caption", () => {
            const handle = $(this.element).find(".draghandle");
            handle.find("p").remove();
            handle.append("<p>" + attr.caption + "</p>");
        });

        this.updateHandle(this.element, this.handle);

        this.handle.on("mousedown pointerdown touchstart", (e) => {
            this.upResize = false;
            this.rightResize = false;
            this.downResize = false;
            this.leftResize = false;
            $document.off("mouseup pointerup touchend", this.release);
            $document.off("mousemove pointermove touchmove", this.move);
            this.lastPos = this.getPageXY(e);
            // Rules for what we should set in CSS
            // to keep the element dimensions (X).
            // Prefer left over right.
            this.getSetDirs();
            //prevTop = this.element.position().top;
            //prevLeft = this.element.position().left;

            $document.on("mouseup pointerup touchend", this.release);
            $document.on("mousemove pointermove touchmove", this.move);
        });

        // TODO context is deprecated
        (this.element.context as HTMLElement).style.msTouchAction =
            "none";

        if (attr.save) {
            this.getSetDirs();
            const oldSize: any = getStorage(this.posKey +
                "Size");
            if (oldSize) {
                if (oldSize.width) {
                    this.element.css("width",
                        oldSize.width);
                }
                if (oldSize.height) {
                    this.element.css("height",
                        oldSize.height);
                }
            }
            const oldPos: any = getStorage(this.posKey);
            const w = window.innerWidth;
            const h = window.innerHeight;
            const ew = this.element.width();
            const eh = this.element.height();
            if (oldPos) {
                if (oldPos.top && this.setTop) {
                    this.element.css("top", wmax(oldPos.top, 0, h - 20));
                }
                if (oldPos.left && this.setLeft) {
                    this.element.css("left", wmax(oldPos.left, 0 - ew + 20, w - 20));
                }
                if (oldPos.right && this.setRight) {
                    this.element.css("right", wmax(oldPos.right, 20, w));
                }
                if (oldPos.bottom && this.setBottom) {
                    this.element.css("bottom", wmax(oldPos.bottom, 20, h));
                }
                // this.element.css("background", "red");
                timLogTime("oldpos:" + oldPos.left + ", " + oldPos.top, "drag");
            }
            if (getStorage(this.posKey + "min")) {
                scope.minimize();
            }
        }
    }

    minimize() {
        this.areaMinimized = !this.areaMinimized;
        let handles;
        const base = this.element.find(".draggable-content");
        if (this.areaMinimized) {
            this.areaHeight = this.element.height();
            this.element.height(15);
            base.css("display", "none");
            this.element.css("min-height", "0");
            setStorage(this.posKey + "min", true);
            handles = this.element.find(".resizehandle");
            if (handles.length) {
                handles.css("display", "none");
            }

        } else {
            base.css("display", "");
            this.element.css("min-height", "");
            this.element.height(this.areaHeight);
            setStorage(this.posKey + "min", false);
            this.element.find(".resizehandle").css("display", "");
        }
        if (handles && handles.length) {
            this.scope.$apply();
        }
    }

    getSetDirs() {
        const leftSet = this.element.css("left") != "auto";
        const rightSet = this.element.css("right") != "auto";
        this.setLeft = (!leftSet && !rightSet) || leftSet;
        this.setRight = rightSet;
        // setLeft = true; // because for some reason in iPad it was right???

        // Rules for what we should set in CSS
        // to keep the element dimensions (Y).
        // Prefer top over bottom.

        const topSet = this.element.css("top") != "auto";
        const botSet = this.element.css("bottom") != "auto";
        this.setTop = (!topSet && !botSet) || topSet;
        this.setBottom = botSet;
        this.prevHeight = this.element.height();
        this.prevWidth = this.element.width();

        this.prevTop = getPixels(this.element.css("top"));
        this.prevLeft = getPixels(this.element.css("left"));
        this.prevBottom = getPixels(this.element.css("bottom"));
        this.prevRight = getPixels(this.element.css("right"));
        timLogTime("set:" + this.setLeft + "," + this.setTop, "drag");
    }

    resizeElement(e, up, right, down, left) {
        this.upResize = up;
        this.rightResize = right;
        this.downResize = down;
        this.leftResize = left;
        $document.off("mouseup pointerup touchend", this.release);
        $document.off("mousemove pointermove touchmove", this.moveResize);
        this.lastPos = this.getPageXY(e);

        this.getSetDirs();

        //prevTop = this.element.position().top;
        //prevLeft = this.element.position().left;

        $document.on("mouseup pointerup touchend", this.release);
        $document.on("mousemove pointermove touchmove", this.moveResize);
    }

    /*
     this.element.css('top', this.element.position().top);
     this.element.css('left', this.element.position().left); */

    createResizeHandles() {
        const handleRight = $("<div>", {class: "resizehandle-r resizehandle"});
        handleRight.on("mousedown pointerdown touchstart", (e) => {
            this.resizeElement(e, false, true, false, false);
        });
        this.element.append(handleRight);
        const handleDown = $("<div>", {class: "resizehandle-d resizehandle"});
        handleDown.on("mousedown pointerdown touchstart", (e) => {
            this.resizeElement(e, false, false, true, false);
        });
        this.element.append(handleDown);
        const handleRightDown = $("<div>", {class: "resizehandle-rd resizehandle"});
        handleRightDown.on("mousedown pointerdown touchstart", (e) => {
            this.resizeElement(e, false, true, true, false);
        });
        this.element.append(handleRightDown);

        if (this.areaMinimized) {
            const handles = this.element.find(".resizehandle");
            if (handles.length) {
                handles.css("display", "none");
            }
        }
    }

    removeResizeHandles() {
        this.element.find(".resizehandle").remove();
    }

    /* Prevent document scrolling, when element inside draggable is scrolled. Currently doesn't work on touch
     $(this.element).find('.scrollable').on('scroll wheel DOMMouseScroll mousewheel', function (e) {
     var e0 = e.originalEvent,
     delta = e0.wheelDelta || -e0.detail;
     $log.info(delta);
     e.preventDefault();
     if (isNaN(delta)) {
     return;
     }
     if ($(e.target).hasClass('scrollable')) {
     $log.info('target ', e.target);
     e.target.scrollTop += ( delta < 0 ? 1 : -1 ) * 30;
     } else {
     e.target.closest('.scrollable').scrollTop += ( delta < 0 ? 1 : -1 ) * 30;
     $log.info('closest ', e.target.closest('.scrollable'));
     }
     });*/

    updateHandle(e, h) {
        // TODO: find an efficient way to call this whenever
        // position is changed between static and absolute
        const position = e.css("position");
        const movable = position != "static";
        h.css("visibility", movable ? "visible" : "hidden");
    }

    getPageXYnull(e) {
        if (!(
            "pageX" in e) || (
            e.pageX == 0 && e.pageY == 0)) {
            if (e.originalEvent.touches.length) {
                return {
                    X: e.originalEvent.touches[0].pageX,
                    Y: e.originalEvent.touches[0].pageY,
                };
            }
            if (e.originalEvent.changedTouches.length) {
                return {
                    X: e.originalEvent.changedTouches[0].pageX,
                    Y: e.originalEvent.changedTouches[0].pageY,
                };
            }
            return null;
        }

        return {X: e.pageX, Y: e.pageY};
    }

    getPageXY(e) {
        const pos = this.getPageXYnull(e);
        if (pos) {
            this.lastPageXYPos = pos;
        }
        return this.lastPageXYPos;
    }

    // The methods release, move and moveResize are required to be instance
    // functions because they are used as callbacks for $document.on/off.
    release = (e) => {
        $document.off("mouseup pointerup touchend", this.release);
        $document.off("mousemove pointermove touchmove", this.move);
        $document.off("mousemove pointermove touchmove", this.moveResize);
        // this.element.css("background", "red");
        this.pos = this.getPageXY(e);
        // this.element.css("background", "blue");
        if (this.posKey) {
            // this.element.css("background", "yellow");
            const css = this.element.css(["top", "bottom", "left",
                "right"]);
            setStorage(this.posKey, css);
            // this.element.css("background", "green");

            timLogTime("pos:" + css.left + "," + css.top, "drag");
        }
        /*
         if (!(upResize || rightResize || downResize || leftResize) && clickFn && e.which === 1) {
         delta = {X: pos.X - lastPos.X, Y: pos.Y - lastPos.Y};
         if (Math.abs(delta.Y) < 3 && Math.abs(delta.X) < 3) {
         clickFn(scope);
         }
         }*/
    }

    move = (e) => {
        this.pos = this.getPageXY(e);
        this.delta = {
            X: this.pos.X -

            this.lastPos.X, Y: this.pos.Y - this.lastPos.Y,
        };

        if (this.setTop) {
            this.element.css("top", this.prevTop + this.delta.Y);
        }
        if (this.setLeft) {
            this.element.css("left", this.prevLeft + this.delta.X);
        }
        if (this.setBottom) {
            this.element.css("bottom",
                this.prevBottom - this.delta.Y);
        }
        if (this.setRight) {
            this.element.css("right"
                , this.prevRight - this.delta.X);
        }

        e.preventDefault();
        e.stopPropagation();
    }

    moveResize = (e) => {
        this.pos = this.getPageXY(e);
        this.delta = {
            X: this.pos.X - this.lastPos.X, Y: this.pos.Y -
            this.lastPos.Y,
        };

        if (this.upResize) {
            this.element.css("height",
                this.prevHeight - this.delta.Y);
            if (this.setTop) {
                this.element.css("top", this.prevTop + this.delta.Y);
            }
        }
        if (this.leftResize) {
            this.element.css(
                "width", this.prevWidth - this.delta.X);
            if (this.setLeft) {
                this.element.css("left", this.prevLeft + this.delta.X);
            }
        }
        if (this.downResize) {
            this.element.css("height", this.prevHeight + this.delta.Y);
            if (this.setBottom) {
                this.element.css("bottom", this.prevBottom - this.delta.Y);
            }
        }
        if (this.rightResize) {
            this.element.css("width", this.prevWidth + this.delta.X);

            if (this.setRight && this.delta.X >= 0) {
                this.element.css(
                    "right", this.prevRight - this.delta.X);
            }
        }

        e.preventDefault();
        e.stopPropagation();

        const size = this.element.css(["width", "height"]);
        if (this.posKey) {
            setStorage(this.posKey + "Size", size);
        }
        if (this.broadcastMsg) {
            this.scope.$broadcast(
                this.broadcastMsg, {size});
            // $rootScope.$broadcast(broadcastMsg, {size: size});
        }
    }
}
