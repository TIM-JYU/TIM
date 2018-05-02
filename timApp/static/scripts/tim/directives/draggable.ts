import {IController, IRootElementService, IScope} from "angular";
import {timApp} from "tim/app";
import {timLogTime} from "tim/timTiming";
import {$compile, $document, $timeout} from "../ngimport";
import {getOutOffsetFully, getOutOffsetVisible, getPageXYnull, IBounds, isMobileDevice} from "../utils";

function setStorage(key: string, value: any) {
    value = JSON.stringify(value);
    window.localStorage.setItem(key, value);
}

function getStorage(key: string) {
    const s = window.localStorage.getItem(key);
    if (!s) {
        return s;
    }
    return JSON.parse(s);
}

function getPixels(s: string) {
    const s2 = s.replace(/px$/, "");
    return Number(s2) || 0;
}

const draggableTemplate = `
<div class="draghandle" ng-mousedown="d.dragClick()" ng-show="d.canDrag()">
    <p ng-show="d.caption" ng-bind="d.caption"></p>
    <i ng-show="d.closeFn"
       title="Close dialog"
       ng-click="d.closeFn()"
       class="glyphicon glyphicon-remove pull-right"></i>
    <i ng-show="d.click"
       title="Minimize dialog"
       ng-click="d.toggleMinimize()"
       class="glyphicon glyphicon-minus pull-right"></i>
</div>
<button class="timButton pull-right" ng-show="d.detachable" ng-click="d.toggleDetach()">
    <i class="glyphicon glyphicon-arrow-{{ d.canDrag() ? 'left' : 'right' }}"></i>
</button>
<div ng-show="d.canResize() && !d.autoWidth"
     class="resizehandle-r resizehandle"></div>
<div ng-show="d.canResize() && !d.autoHeight"
     class="resizehandle-d resizehandle"></div>
<div ng-show="d.canResize() && !d.autoWidth && !d.autoHeight"
     class="resizehandle-rd resizehandle"></div>
    `;

timApp.directive("timDraggableFixed", [() => {
    return {
        bindToController: {
            absolute: "<?",
            caption: "@?",
            click: "<?",
            detachable: "<?",
            forceMaximized: "<?",
            resize: "<?",
            save: "@?",
        },
        controller: DraggableController,
        controllerAs: "d", // default $ctrl does not work, possibly because of some ng-init
        require: {
            parentDraggable: "?^^timDraggableFixed", // for checking whether we are inside some other draggable
        },
        restrict: "A",
        scope: {},
        // Using template + transclude here does not work for some reason with uib-modal, so we compile the
        // template manually in $postLink.
    };
}]);

type Pos = {X: number, Y: number};

export class DraggableController implements IController {
    private static $inject = ["$scope", "$element"];

    private posKey?: string;
    private areaMinimized: boolean = false;
    private areaHeight: number = 0;
    private areaWidth: number = 0;
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
    private lastPageXYPos = {X: 0, Y: 0};
    private handle: JQuery;
    private closeFn?: () => void;
    private caption?: string;
    private click?: boolean;
    private resize?: boolean;
    private save?: string;
    private dragClick?: () => void;
    private autoHeight: boolean;
    private absolute?: boolean;
    private parentDraggable?: DraggableController;
    private forceMaximized?: boolean;

    constructor(private scope: IScope, private element: IRootElementService) {
    }

    $onInit() {
        if (this.save) {
            const pageId = window.location.pathname.split("/")[1];  // /velp/???
            this.posKey = this.save.replace("%%PAGEID%%", pageId);
        }
    }

    setCaption(caption: string) {
        this.caption = caption;
    }

    makeHeightAutomatic() {
        this.element.css("height", "auto");
        this.autoHeight = true;
    }

    private toggleDetach() {
        if (this.canDrag()) {
            this.element.css("position", "static");
            for (const prop of ["width", "height"]) {
                this.element.css(prop, "");
            }
        } else {
            this.element.css("position", "absolute");
            this.restoreSizeAndPosition();
        }
        setStorage(this.posKey + "detach", this.canDrag());
    }

    private makeModalPositionAbsolute() {
        this.getModal().css("position", "absolute");
    }

    private getModal() {
        return this.element.parents(".modal");
    }

    public isMinimized() {
        return this.areaMinimized;
    }

    private modalHasAbsolutePosition() {
        return this.getModal().css("position") === "absolute" && this.parentDraggable == null;
    }

    private elementHasAbsoluteOrRelativePosition() {
        const s = this.element.css("position");
        return s === "absolute" || s === "relative";
    }

    setDragClickFn(fn: () => void) {
        this.dragClick = fn;
    }

    setCloseFn(fn: () => void) {
        this.closeFn = fn;
    }

    async $postLink() {
        this.element.prepend($compile(draggableTemplate)(this.scope));
        this.createResizeHandlers();
        if (this.absolute) {
            this.makeModalPositionAbsolute();
        }
        if (isMobileDevice()) {
            this.getModal().css("overflow", "visible");
        }
        this.handle = this.element.children(".draghandle");

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
        await $timeout(); // the restored position will be slightly off without this
        if (this.posKey) {
            this.getSetDirs();
            this.restoreSizeAndPosition();
            if (getStorage(this.posKey + "min") && !this.forceMaximized) {
                this.toggleMinimize();
            }
            if (getStorage(this.posKey + "detach")) {
                this.toggleDetach();
            }
        }
        this.ensureVisibleInViewport();
    }

    private restoreSizeAndPosition() {
        if (!this.posKey) {
            return;
        }
        const oldSize: any = getStorage(this.posKey +
            "Size");
        if (oldSize && this.elementHasAbsoluteOrRelativePosition()) {
            if (oldSize.width) {
                this.element.css("width",
                    oldSize.width);
            }
            if (oldSize.height && !this.autoHeight) {
                this.element.css("height",
                    oldSize.height);
            }
        }
        const oldPos: {left: string, right: string, top: string, bottom: string} | null = getStorage(this.posKey);
        // it doesn't make sense to restore Y position if the dialog has absolute position (instead of fixed)
        if (this.modalHasAbsolutePosition()) {
            this.element.css("top", window.pageYOffset + "px");
        }
        if (oldPos) {
            if (oldPos.left && this.setLeft) {
                this.element.css("left", oldPos.left);
            }
            if (oldPos.right && this.setRight) {
                this.element.css("right", oldPos.right);
            }
            if (!this.modalHasAbsolutePosition()) {
                if (oldPos.top && this.setTop) {
                    this.element.css("top", oldPos.top);
                }
                if (oldPos.bottom && this.setBottom) {
                    this.element.css("bottom", oldPos.bottom);
                }
            }
            timLogTime("oldpos:" + oldPos.left + ", " + oldPos.top, "drag");
        }
    }

    getWidth(): number {
        const w = this.element.width();
        if (w == null) {
            // should never happen because element is not empty set
            throw new Error("this.element.width() returned null");
        }
        return w;
    }

    getHeight(): number {
        const w = this.element.height();
        if (w == null) {
            // should never happen because element is not empty set
            throw new Error("this.element.height() returned null");
        }
        return w;
    }

    toggleMinimize() {
        this.areaMinimized = !this.areaMinimized;
        const base = this.element.find(".draggable-content");
        if (this.areaMinimized) {
            this.areaHeight = this.getHeight();
            this.areaWidth = this.getWidth();
            this.element.height(15);
            this.element.width(200);
            this.element.css("left", this.getCss("left") + (this.areaWidth - this.getWidth()));

            base.css("display", "none");
            this.element.css("min-height", "0");
            setStorage(this.posKey + "min", true);
        } else {
            base.css("display", "");
            this.element.css("min-height", "");
            this.element.css("left", this.getCss("left") - (this.areaWidth - this.getWidth()));
            if (this.autoHeight) {
                this.element.height("auto");
            } else {
                this.element.height(this.areaHeight);
            }
            this.element.width(this.areaWidth);
            setStorage(this.posKey + "min", false);
        }
    }

    private getSetDirs() {
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
        this.prevHeight = this.getHeight();
        this.prevWidth = this.getWidth();

        this.prevTop = getPixels(this.element.css("top"));
        this.prevLeft = getPixels(this.element.css("left"));
        this.prevBottom = getPixels(this.element.css("bottom"));
        this.prevRight = getPixels(this.element.css("right"));
        timLogTime("set:" + this.setLeft + "," + this.setTop, "drag");
    }

    private resizeElement(e: JQueryEventObject, up: boolean, right: boolean, down: boolean, left: boolean) {
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

    private createResizeHandlers() {
        const handleRight = this.element.children(".resizehandle-r");
        handleRight.on("mousedown pointerdown touchstart", (e: JQueryEventObject) => {
            this.resizeElement(e, false, true, false, false);
        });
        const handleDown = this.element.children(".resizehandle-d");
        handleDown.on("mousedown pointerdown touchstart", (e: JQueryEventObject) => {
            this.resizeElement(e, false, false, true, false);
        });
        const handleRightDown = this.element.children(".resizehandle-rd");
        handleRightDown.on("mousedown pointerdown touchstart", (e: JQueryEventObject) => {
            this.resizeElement(e, false, true, true, false);
        });
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

    private canDrag() {
        return this.element.css("position") !== "static";
    }

    private canResize() {
        return !this.areaMinimized && this.resize && this.canDrag();
    }

    private getPageXY(e: JQueryEventObject) {
        const pos = getPageXYnull(e);
        if (pos) {
            this.lastPageXYPos = pos;
        }
        return this.lastPageXYPos;
    }

    private getCss(key: "left" | "right" | "bottom" | "top") {
        return getPixels(this.element.css(key));
    }

    // The methods release, move and moveResize are required to be instance
    // functions because they are used as callbacks for $document.on/off.
    release = (e: JQueryEventObject) => {
        $document.off("mouseup pointerup touchend", this.release);
        $document.off("mousemove pointermove touchmove", this.move);
        $document.off("mousemove pointermove touchmove", this.moveResize);
        // this.element.css("background", "red");
        this.pos = this.getPageXY(e);
        this.ensureVisibleInViewport();
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

    private ensureFullyInViewport() {
        const bound = getOutOffsetFully(this.element[0]);
        this.setCssFromBound(bound);
    }

    private ensureVisibleInViewport() {
        const bound = getOutOffsetVisible(this.element[0]);
        this.setCssFromBound(bound);
    }

    private setCssFromBound(bound: IBounds) {
        if (this.setTop) {
            this.element.css("top", this.getCss("top") - bound.top);
            this.element.css("top", this.getCss("top") + bound.bottom);
        }
        if (this.setBottom) {
            this.element.css("bottom", this.getCss("bottom") - bound.bottom);
            this.element.css("bottom", this.getCss("bottom") + bound.top);
        }
        if (this.setLeft) {
            this.element.css("left", this.getCss("left") - bound.left);
            this.element.css("left", this.getCss("left") + bound.right);
        }
        if (this.setRight) {
            this.element.css("right", this.getCss("right") - bound.right);
            this.element.css("right", this.getCss("right") + bound.left);
        }
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
