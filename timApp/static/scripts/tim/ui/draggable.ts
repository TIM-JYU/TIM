import {IController, IScope} from "angular";
import {IModalInstanceService} from "angular-ui-bootstrap";
import * as t from "io-ts";
import {timApp} from "tim/app";
import {timLogTime} from "tim/util/timTiming";
import {$compile} from "../util/ngimport";
import {
    Binding,
    getOutOffsetFully,
    getOutOffsetVisible,
    getPageXY,
    getStorage,
    getViewPortSize,
    IBounds,
    ISize,
    isMobileDevice,
    setStorage,
    TimDefer,
} from "../util/utils";

function getPixels(s: string) {
    const s2 = s.replace(/px$/, "");
    return Number(s2) || 0;
}

const draggableTemplate = `
<div class="draghandle drag"
     ng-class="{'attached': !d.canDrag()}" ng-mousedown="d.dragClick(); $event.preventDefault()">
    <p class="drag" ng-show="d.getCaption()" ng-bind="d.getCaption()"></p>
    <i ng-show="d.detachable"
       ng-click="d.toggleDetach()"
       title="{{ d.canDrag() ? 'Attach' : 'Detach' }}"
       class="glyphicon glyphicon-arrow-{{ d.canDrag() ? 'left' : 'right' }}"></i>
    <i ng-show="d.click"
       title="{{ d.areaMinimized ? 'Maximize' : 'Minimize' }} dialog"
       ng-click="d.toggleMinimize()"
       class="glyphicon glyphicon-{{ d.areaMinimized ? 'unchecked' : 'minus' }}"></i>
    <i ng-show="d.closeFn"
       title="Close dialog"
       ng-click="d.closeFn()"
       class="glyphicon glyphicon-remove"></i>
</div>
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
        // template: draggableTemplate,
        // transclude: true,
        // Using template + transclude here does not work for some reason with uib-modal, so we compile the
        // template manually in $postLink.
    };
}]);

export interface Pos {
    X: number;
    Y: number;
}

interface IResizeStates {
    up: boolean;
    down: boolean;
    left: boolean;
    right: boolean;
}

export enum VisibilityFix {
    Partial,
    Full,
}

type CssDir = "left" | "right" | "bottom" | "top";

export interface IResizeCallbackParams {
    w: number | null;
    h: number | null;
    state: IResizeStates;
}

export type ResizeCallback = (p: IResizeCallbackParams) => void;

export class DraggableController implements IController {
    static $inject = ["$scope", "$element"];

    private posKey?: string;
    private areaMinimized: boolean = false;
    private areaHeight: number = 0;
    private areaWidth: number = 0;
    private setLeft: boolean = true;
    private setRight: boolean = false;
    private setBottom: boolean = false;
    private setTop: boolean = true;
    private prev: IBounds = {left: 0, right: 0, bottom: 0, top: 0};
    private prevSize: ISize = {width: 0, height: 0};
    private resizeStates: IResizeStates = {up: false, down: false, right: false, left: false};
    private lastPos: Pos = {X: 0, Y: 0};
    private lastPageXYPos = {X: 0, Y: 0};
    private handle?: JQuery;
    private closeFn?: () => void;
    private caption?: Binding<string, "<">;
    private click?: Binding<boolean, "<">;
    private detachable?: Binding<boolean, "<">;
    private resize?: Binding<boolean, "<">;
    private save?: Binding<string, "<">;
    private dragClick?: () => void;
    private autoHeight?: boolean;
    private absolute?: Binding<boolean, "<">;
    private parentDraggable?: DraggableController;
    private forceMaximized?: Binding<boolean, "<">;
    private modal?: IModalInstanceService;
    private layoutReady = new TimDefer();
    private resizeCallback?: ResizeCallback;
    private captionCb?: () => string;

    constructor(private scope: IScope, private element: JQLite) {
    }

    isModal() {
        return this.element.parent(".modal").length === 1;
    }

    $onInit() {
        // hide element temporarily to avoid flashing the dialog in wrong position briefly
        this.setVisibility("hidden");
        if (this.save) {
            const pageId = window.location.pathname.split("/")[1];
            this.posKey = this.save.replace("%%PAGEID%%", pageId);
        }
    }

    private setVisibility(v: "visible" | "hidden" | "inherit") {
        this.element.css("visibility", v);
    }

    public getLayoutPromise() {
        return this.layoutReady.promise;
    }

    getCaption() {
        return this.caption || (this.captionCb ? this.captionCb() : "Dialog");
    }

    setCaptionCb(cb: () => string) {
        this.captionCb = cb;
    }

    async makeHeightAutomatic() {
        // workaround for Chrome issue where the browser jumps to the start of page when opening a dialog that
        // calls this
        await this.getModal();

        this.element.css("height", "auto");
        this.autoHeight = true;
    }

    private toggleDetach() {
        if (this.canDrag()) {
            this.element.css("position", "static");
            this.element.css("visibility", "inherit");
            for (const prop of ["width", "height"]) {
                this.element.css(prop, "");
            }
        } else {
            this.element.css("position", "absolute");
            this.element.css("visibility", "visible");
            this.restoreSizeAndPosition(VisibilityFix.Partial);
        }
        setStorage(this.posKey + "detach", this.canDrag());
    }

    private async makeModalPositionAbsolute() {
        const modal = await this.getModal();
        if (!modal) {
            return;
        }
        modal.css("position", "absolute");
    }

    private async getModal() {
        if (!this.modal) {
            return;
        }

        // We don't want "this.modal.opened" here because then the saved position gets set incorrectly
        // (20px too much left). On the other hand, when using "rendered", we have to manually hide the modal while
        // loading its old position to avoid it being flashed briefly in wrong position.
        // This is done in $onInit and setInitialLayout.
        await this.modal.rendered;
        return this.element.parents(".modal");
    }

    public isMinimized() {
        return this.areaMinimized;
    }

    private async modalHasAbsolutePosition() {
        const m = await this.getModal();
        if (!m) {
            return false;
        }
        return m.css("position") === "absolute" && this.parentDraggable == null;
    }

    setDragClickFn(fn: () => void) {
        this.dragClick = fn;
    }

    setCloseFn(fn: (() => void) | undefined) {
        this.closeFn = fn;
    }

    async $postLink() {
        this.element.prepend($compile(draggableTemplate)(this.scope));
        this.createResizeHandlers();
        this.handle = this.element.children(".draghandle");

        const handleResizeBegin = (e: MouseEvent | TouchEvent) => {
            if (!this.canDrag()) {
                return;
            }
            if ($(e.target as HTMLElement).hasClass("drag")) {
                e.preventDefault();
            }

            this.resizeStates = {
                up: false,
                down: false,
                left: false,
                right: false,
            };
            this.refreshEventListeners(e, this.move);
        };
        this.handle[0].addEventListener("mousedown", handleResizeBegin);
        this.handle[0].addEventListener("pointerdown", handleResizeBegin);
        this.handle[0].addEventListener("touchstart", handleResizeBegin);

        // DialogController will call setInitialLayout in case draggable is inside modal
        if (!this.isModal()) {
            await this.setInitialLayout(VisibilityFix.Partial);
        }
    }

    public async setInitialLayout(vf: VisibilityFix) {
        if (this.absolute) {
            await this.makeModalPositionAbsolute();
        }
        if (isMobileDevice()) {
            const modal = await this.getModal();
            if (modal) {
                modal.css("overflow", "visible");
            }
        }
        if (this.posKey) {
            this.getSetDirs();
            await this.restoreSizeAndPosition(vf);
            if (getStorage(this.posKey + "min") && !this.forceMaximized) {
                this.toggleMinimize();
            }
            if (getStorage(this.posKey + "detach")) {
                this.toggleDetach();
            }
        }
        // restore visibility (see $onInit)
        if (this.canDrag()) {
            this.setVisibility("visible");
        } else {
            this.setVisibility("inherit");
        }
        this.layoutReady.resolve();
    }

    private async restoreSizeAndPosition(vf: VisibilityFix) {
        if (!this.posKey) {
            return;
        }
        const SizeType = t.partial({width: t.string, height: t.string});
        const oldSize = getStorage(this.posKey +
            "Size");
        if (SizeType.is(oldSize) && this.canDrag()) {
            const vps = getViewPortSize();
            if (oldSize.width) {
                this.element.css("width",
                    Math.min(getPixels(oldSize.width), vps.width * 0.97));
            }
            if (oldSize.height && !this.autoHeight) {
                this.element.css("height",
                    Math.min(getPixels(oldSize.height), vps.height * 0.97));
            }
        }
        const PosType = t.partial({left: t.string, right: t.string, top: t.string, bottom: t.string});
        const oldPos = getStorage(this.posKey);
        // it doesn't make sense to restore Y position if the dialog has absolute position (instead of fixed)
        if (await this.modalHasAbsolutePosition()) {
            const off = this.element.offset() || {left: 20};
            this.element.offset({top: window.pageYOffset, left: off.left});
        }
        if (PosType.is(oldPos)) {
            if (oldPos.left && this.setLeft) {
                this.element.css("left", oldPos.left);
            }
            if (oldPos.right && this.setRight) {
                this.element.css("right", oldPos.right);
            }
            if (!await this.modalHasAbsolutePosition()) {
                if (oldPos.top && this.setTop) {
                    this.element.css("top", oldPos.top);
                }
                if (oldPos.bottom && this.setBottom) {
                    this.element.css("bottom", oldPos.bottom);
                }
            }
            // debugTextToHeader(JSON.stringify(oldPos));
            timLogTime("oldpos:" + oldPos.left + ", " + oldPos.top, "drag");
        }
        switch (vf) {
            case VisibilityFix.Partial:
                this.ensureVisibleInViewport();
                break;
            case VisibilityFix.Full:
                this.ensureFullyInViewport();
                break;
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
        const base = this.element.children(".draggable-content");
        if (this.areaMinimized) {
            this.areaHeight = this.getHeight();
            this.areaWidth = this.getWidth();
            this.element.height(15);
            this.element.width(200);
            // Do not set left here - it will make dialogs appear hidden in some scenarios.
            // this.element.css("left", this.getCss("left") + (this.areaWidth - this.getWidth()));

            base.css("visibility", "hidden");
            this.element.css("min-height", "0");
            setStorage(this.posKey + "min", true);
        } else {
            base.css("visibility", "");
            this.element.css("min-height", "");
            // Do not set left here - it will make dialogs appear hidden in some scenarios.
            // this.element.css("left", this.getCss("left") - (this.areaWidth - this.getWidth()));
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
        this.prevSize = {
            height: this.getHeight(),
            width: this.getWidth(),
        };

        this.prev = {
            top: getPixels(this.element.css("top")),
            left: getPixels(this.element.css("left")),
            bottom: getPixels(this.element.css("bottom")),
            right: getPixels(this.element.css("right")),
        };
        timLogTime("set:" + [this.setLeft, this.setTop, this.setBottom, this.setRight].join(", "), "drag");
    }

    private resizeElement(e: MouseEvent | TouchEvent, up: boolean, right: boolean, down: boolean, left: boolean) {
        e.preventDefault();
        this.resizeStates = {up, down, left, right};
        this.refreshEventListeners(e, this.moveResize);
    }

    private refreshEventListeners(e: MouseEvent | TouchEvent, handler: (e: MouseEvent | TouchEvent) => unknown) {
        this.removeMouseUp();
        this.removeMouseMove(handler);
        this.lastPos = this.getPageXY(e);
        this.getSetDirs();
        document.addEventListener("mouseup", this.release);
        document.addEventListener("pointerup", this.release);
        document.addEventListener("touchend", this.release);
        document.addEventListener("mousemove", handler);
        document.addEventListener("pointermove", handler);
        document.addEventListener("touchmove", handler);
    }

    private removeMouseUp() {
        document.removeEventListener("mouseup", this.release);
        document.removeEventListener("pointerup", this.release);
        document.removeEventListener("touchend", this.release);
    }

    private removeMouseMove(handler: (e: (MouseEvent | TouchEvent)) => unknown) {
        document.removeEventListener("mousemove", handler);
        document.removeEventListener("pointermove", handler);
        document.removeEventListener("touchmove", handler);
    }

    private createResizeHandlers() {
        const handleRight = this.element.children(".resizehandle-r")[0];
        const listenerRight = (e: MouseEvent | TouchEvent) => {
            this.resizeElement(e, false, true, false, false);
        };
        handleRight.addEventListener("mousedown", listenerRight);
        handleRight.addEventListener("pointerdown", listenerRight);
        handleRight.addEventListener("touchstart", listenerRight);

        const handleDown = this.element.children(".resizehandle-d")[0];
        const listenerDown = (e: MouseEvent | TouchEvent) => {
            this.resizeElement(e, false, false, true, false);
        };
        handleDown.addEventListener("mousedown", listenerDown);
        handleDown.addEventListener("pointerdown", listenerDown);
        handleDown.addEventListener("touchstart", listenerDown);

        const handleRightDown = this.element.children(".resizehandle-rd")[0];
        const listenerRightDown = (e: MouseEvent | TouchEvent) => {
            this.resizeElement(e, false, true, true, false);
        };
        handleRightDown.addEventListener("mousedown", listenerRightDown);
        handleRightDown.addEventListener("pointerdown", listenerRightDown);
        handleRightDown.addEventListener("touchstart", listenerRightDown);
    }

    private canDrag() {
        return this.element.css("position") !== "static";
    }

    private canResize() {
        return !this.areaMinimized && this.resize && this.canDrag();
    }

    private getPageXY(e: MouseEvent | TouchEvent) {
        const pos = getPageXY(e);
        if (pos) {
            this.lastPageXYPos = pos;
        }
        return this.lastPageXYPos;
    }

    private getCss(key: CssDir) {
        return getPixels(this.element.css(key));
    }

    // The methods release, move and moveResize are required to be instance
    // functions because they are used as callbacks for $document.on/off.
    release = (e: Event) => {
        this.removeMouseUp();
        this.removeMouseMove(this.move);
        this.removeMouseMove(this.moveResize);
        this.ensureVisibleInViewport();
        if (this.posKey) {
            // e.preventDefault();
            const css = this.element.css(["top", "bottom", "left",
                "right"]);
            setStorage(this.posKey, css);

            timLogTime("pos:" + css.left + "," + css.top, "drag");
        }
        this.scope.$evalAsync();
    };

    public ensureFullyInViewport() {
        const bound = getOutOffsetFully(this.element[0]);
        this.setCssFromBound(bound);
    }

    private ensureVisibleInViewport() {
        const bound = getOutOffsetVisible(this.element[0]);
        this.setCssFromBound(bound);
    }

    private addToCss(prop: CssDir, amount: number) {
        this.element.css(prop, this.getCss(prop) + amount);
    }

    private setCssFromBound(bound: IBounds) {
        if (bound.bottom === 0 && bound.top === 0 && bound.right === 0 && bound.left === 0) {
            return;
        }
        if (this.setTop) {
            this.addToCss("top", -bound.top + bound.bottom);
        }
        if (this.setBottom) {
            this.addToCss("bottom", -bound.bottom + bound.top);
        }
        if (this.setLeft) {
            this.addToCss("left", -bound.left + bound.right);
        }
        if (this.setRight) {
            this.addToCss("right", -bound.right + bound.left);
        }
    }

    move = (e: MouseEvent | TouchEvent) => {
        // e.preventDefault();
        const pos = this.getPageXY(e);
        this.doMove(pos);
        e.preventDefault();
        e.stopPropagation();
    };

    private doMove(pos: Pos) {
        const delta = {
            X: pos.X -

                this.lastPos.X, Y: pos.Y - this.lastPos.Y,
        };

        if (this.setTop) {
            this.element.css("top", this.prev.top + delta.Y);
        }
        if (this.setLeft) {
            this.element.css("left", this.prev.left + delta.X);
        }
        if (this.setBottom) {
            this.element.css("bottom",
                this.prev.bottom - delta.Y);
        }
        if (this.setRight) {
            this.element.css("right"
                , this.prev.right - delta.X);
        }
    }

    moveResize = async (e: MouseEvent | TouchEvent) => {
        // e.preventDefault();
        const pos = this.getPageXY(e);
        const delta = {
            X: pos.X - this.lastPos.X, Y: pos.Y -
                this.lastPos.Y,
        };

        if (this.resizeStates.up) {
            this.element.css("height",
                this.prevSize.height - delta.Y);
            if (this.setTop) {
                this.element.css("top", this.prev.top + delta.Y);
            }
        }
        if (this.resizeStates.left) {
            this.element.css(
                "width", this.prevSize.width - delta.X);
            if (this.setLeft) {
                this.element.css("left", this.prev.left + delta.X);
            }
        }
        if (this.resizeStates.down) {
            this.element.css("height", this.prevSize.height + delta.Y);
            if (this.setBottom) {
                this.element.css("bottom", this.prev.bottom - delta.Y);
            }
        }
        if (this.resizeStates.right) {
            this.element.css("width", this.prevSize.width + delta.X);

            if (this.setRight && delta.X >= 0) {
                this.element.css(
                    "right", this.prev.right - delta.X);
            }
        }

        e.preventDefault();
        e.stopPropagation();

        const size = await this.getSize();
        if (this.posKey) {
            setStorage(this.posKey + "Size", size);
        }
        if (this.resizeCallback) {
            this.resizeCallback({
                ...await this.getSizeAsNum(),
                state: this.resizeStates,
            });
        }
    };

    async getSize() {
        await this.layoutReady.promise;
        return {width: this.element[0].style.width || null, height: this.element[0].style.height || null};
    }

    async getSizeAsNum() {
        const s = await this.getSize();
        return {
            w: s.width != null ? getPixels(s.width) : null,
            h: s.height != null ? getPixels(s.height) : null,
        };
    }

    setResizeCallback(f: ResizeCallback) {
        this.resizeCallback = f;
    }

    async moveTo(p: Pos) {
        await this.layoutReady.promise;
        this.element.css("margin", "inherit");
        this.doMove(p);
    }

    $destroy() {
        this.element.remove();
    }

    setModal(modalInstance: IModalInstanceService) {
        this.modal = modalInstance;
    }
}
