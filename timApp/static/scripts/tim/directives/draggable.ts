import {IAttributes, IChangesObject, IController, IOnChangesObject, IRootElementService, IScope} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {timLogTime} from "tim/timTiming";
import {$compile, $document} from "../ngimport";

function setStorage(key: string, value: any) {
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

function getPixels(s: string) {
    const s2 = s.replace(/px$/, "");
    return Number(s2) || 0;
}

function wmax(value: string, min: number, max: number) {
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

const draggableTemplate = `
<div class="draghandle">
    <p ng-show="d.caption" ng-bind="d.caption"></p>
    <i ng-show="d.closeFn"
       title="Close dialog"
       ng-click="d.closeFn()"
       class="glyphicon glyphicon-remove pull-right"></i>
    <i ng-show="d.click"
       title="Minimize dialog"
       ng-click="d.minimize()"
       class="glyphicon glyphicon-minus pull-right"></i>
</div>
    `;

timApp.directive("timDraggableFixed", [() => {
    return {
        bindToController: {
            caption: "@?",
            click: "<?",
            resize: "<?",
            save: "@?",
        },
        controller: DraggableController,
        controllerAs: "d", // default $ctrl does not work, possibly because of some ng-init
        restrict: "A",
        scope: {},
        // Using template + transclude here does not work for some reason with uib-modal, so we compile the
        // template manually in $postLink.
    };
}]);

type Pos = {X: number, Y: number};

export class DraggableController implements IController {
    private static $inject = ["$scope", "$attrs", "$element"];

    private posKey: string;
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
    private handle: JQuery;
    private closeFn?: () => void;
    private caption?: string;
    private click?: boolean;
    private resize?: boolean;
    private save?: string;

    constructor(scope: IScope, attr: IAttributes, element: IRootElementService) {
        this.scope = scope;
        this.element = element;
    }

    $onInit() {
        if (this.save) {
            const pageId = window.location.pathname.split("/")[1];  // /velp/???
            this.posKey = this.save.replace("%%PAGEID%%", pageId);
        }
    }

    setCloseFn(fn: () => void) {
        this.closeFn = fn;
    }

    $onChanges(onChangesObj?: IOnChangesObject) {
        if (!onChangesObj) {
            return;
        }
        const resize = onChangesObj.resize as IChangesObject<boolean> | undefined;
        if (resize) {
            if (resize.currentValue) {
                this.createResizeHandles();
            } else {
                this.removeResizeHandles();
            }
        }
    }

    $postLink() {
        this.element.prepend($compile(draggableTemplate)(this.scope));
        this.handle = this.element.children(".draghandle");

        this.updateHandle(this.element, this.handle);

        this.handle.on("mousedown pointerdown touchstart", (e: JQueryEventObject) => {
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

        this.element[0].style.msTouchAction = "none";

        if (this.save) {
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
            const ew = this.element.width() || 500;
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
                this.minimize();
            }
        }
    }

    minimize() {
        this.areaMinimized = !this.areaMinimized;
        let handles;
        const base = this.element.find(".draggable-content");
        if (this.areaMinimized) {
            this.areaHeight = this.element.height() || 200;
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
        this.prevHeight = this.element.height() || 200;
        this.prevWidth = this.element.width() || 500;

        this.prevTop = getPixels(this.element.css("top"));
        this.prevLeft = getPixels(this.element.css("left"));
        this.prevBottom = getPixels(this.element.css("bottom"));
        this.prevRight = getPixels(this.element.css("right"));
        timLogTime("set:" + this.setLeft + "," + this.setTop, "drag");
    }

    resizeElement(e: JQueryEventObject, up: boolean, right: boolean, down: boolean, left: boolean) {
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
        handleRight.on("mousedown pointerdown touchstart", (e: JQueryEventObject) => {
            this.resizeElement(e, false, true, false, false);
        });
        this.element.append(handleRight);
        const handleDown = $("<div>", {class: "resizehandle-d resizehandle"});
        handleDown.on("mousedown pointerdown touchstart", (e: JQueryEventObject) => {
            this.resizeElement(e, false, false, true, false);
        });
        this.element.append(handleDown);
        const handleRightDown = $("<div>", {class: "resizehandle-rd resizehandle"});
        handleRightDown.on("mousedown pointerdown touchstart", (e: JQueryEventObject) => {
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

    updateHandle(e: JQuery, h: JQuery) {
        // TODO: find an efficient way to call this whenever
        // position is changed between static and absolute
        const position = e.css("position");
        const movable = position != "static";
        h.css("display", movable ? "block" : "none");
    }

    getPageXYnull(e: JQueryEventObject) {
        if (!(
                "pageX" in e) || (
                e.pageX == 0 && e.pageY == 0)) {
            const originalEvent = e.originalEvent as any;
            if (originalEvent.touches.length) {
                return {
                    X: originalEvent.touches[0].pageX,
                    Y: originalEvent.touches[0].pageY,
                };
            }
            if (originalEvent.changedTouches.length) {
                return {
                    X: originalEvent.changedTouches[0].pageX,
                    Y: originalEvent.changedTouches[0].pageY,
                };
            }
            return null;
        }

        return {X: e.pageX, Y: e.pageY};
    }

    getPageXY(e: JQueryEventObject) {
        const pos = this.getPageXYnull(e);
        if (pos) {
            this.lastPageXYPos = pos;
        }
        return this.lastPageXYPos;
    }

    // The methods release, move and moveResize are required to be instance
    // functions because they are used as callbacks for $document.on/off.
    release = (e: JQueryEventObject) => {
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

    move = (e: JQueryEventObject) => {
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

    moveResize = (e: JQueryEventObject) => {
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
    }

    $destroy() {
        this.element.remove();
    }
}
