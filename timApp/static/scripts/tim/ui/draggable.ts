import * as t from "io-ts";
import {timLogTime} from "tim/util/timTiming";
import {TimDefer} from "tim/util/timdefer";
import type {Pos} from "tim/ui/pos";
import {nullable} from "tim/plugin/attributes";
import type {IBounds, ISize} from "tim/util/utils";
import {
    getOutOffsetFully,
    getOutOffsetVisible,
    getPageXY,
    getViewPortSize,
    isMobileDevice,
    TimStorage,
} from "tim/util/utils";
import {vctrlInstance} from "tim/document/viewctrlinstance";

function getPixels(s: string) {
    const s2 = s.replace(/px$/, "");
    return Number(s2) || 0;
}

const draggableTemplate = `
<div class="draghandle drag">
    <p class="drag"></p>
    <i class="drag-detach glyphicon"></i>
    <i class="drag-minimize glyphicon"></i>
    <i class="drag-close glyphicon glyphicon-remove" title="Close" style="cursor: pointer; float: right"></i>
</div>
<div class="resizehandle-r resizehandle"></div>
<div class="resizehandle-d resizehandle"></div>
<div class="resizehandle-rd resizehandle"></div>
    `;

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

/**
 * The part of a modal that the draggable needs: a promise that resolves when the modal has been rendered.
 */
export interface IDraggableModal {
    rendered: PromiseLike<unknown>;
}

export type DraggableSize = t.TypeOf<typeof SizeType>;

/**
 * The options of a draggable. In templates, these are given as attributes of the element.
 */
export interface IDraggableOptions {
    canMinimize?: boolean;
    absolute?: boolean;
    anchor?: string;
    caption?: string;
    click?: boolean;
    detachable?: boolean;
    forceMaximized?: boolean;
    resize?: boolean;
    draggable?: boolean;
    save?: string;
    initialSize?: DraggableSize;
    slidePars?: boolean;
}

const PosType = t.partial({
    left: t.string,
    right: t.string,
    top: t.string,
    bottom: t.string,
});
const SizeType = t.type({
    width: nullable(t.string),
    height: nullable(t.string),
});

/**
 * Makes an element draggable, resizable, minimizable and optionally detachable.
 * Does not depend on any UI framework; the wrappers (DraggableDirective for Angular templates and
 * attachDraggables for HTML that comes from the server) create and drive this.
 */
export class DraggableController implements IDraggableOptions {
    private posKey = "draggable";
    private areaMinimized: boolean = false;
    private areaHeight: number = 0;
    private areaWidth: number = 0;
    private setLeft: boolean = true;
    private setRight: boolean = false;
    private setBottom: boolean = false;
    private setTop: boolean = true;
    private prev: IBounds = {left: 0, right: 0, bottom: 0, top: 0};
    private prevSize: ISize = {width: 0, height: 0};
    private resizeStates: IResizeStates = {
        up: false,
        down: false,
        right: false,
        left: false,
    };
    private lastPos: Pos = {x: 0, y: 0};
    private lastPageXYPos: Pos = {x: 0, y: 0};
    private handle?: HTMLElement;
    private closeFn?: () => void;
    caption?: string;
    click?: boolean;
    detachable?: boolean;
    slidePars?: boolean;
    resize?: boolean;
    save?: string;
    private dragClick?: () => void;
    private autoHeight?: boolean;
    absolute?: boolean;
    anchor: string = "absolute";
    initialSize?: DraggableSize;
    parentDraggable?: DraggableController;
    forceMaximized?: boolean;
    private modal?: IDraggableModal;
    private layoutReady = new TimDefer<void>();
    private resizeCallback?: ResizeCallback;
    private captionCb?: () => string;
    private dragEnabled = true;
    draggable?: boolean;
    private posStorage!: TimStorage<t.TypeOf<typeof PosType>>;
    private sizeStorage!: TimStorage<t.TypeOf<typeof SizeType>>;
    private minStorage!: TimStorage<boolean>;
    private detachStorage!: TimStorage<boolean>;
    canMinimize?: boolean;
    private initialized = false;
    private destroyed = false;
    protected element: JQuery;

    constructor(element: HTMLElement) {
        this.element = $(element);
    }

    getElement() {
        return this.element[0];
    }

    private onWindowResize = () => {
        void this.restoreSizeAndPosition(VisibilityFix.Full);
    };

    isModal() {
        return this.element.parent(".modal").length === 1;
    }

    init() {
        this.dragEnabled = this.draggable ?? true;
        // hide element temporarily to avoid flashing the dialog in wrong position briefly
        this.setVisibility("hidden");
        if (this.save) {
            const pageId = window.location.pathname.split("/")[1];
            this.posKey = this.save.replace("%%PAGEID%%", pageId);
        }
        this.posStorage = new TimStorage(this.posKey, PosType);
        this.sizeStorage = new TimStorage(this.posKey + "Size", SizeType);
        this.minStorage = new TimStorage(this.posKey + "min", t.boolean);
        this.detachStorage = new TimStorage(this.posKey + "detach", t.boolean);
        // Move element towards an anchor point selected by the user.
        window.addEventListener("resize", this.onWindowResize);
        // User may have refreshed the document after resizing the window.
        void this.restoreSizeAndPosition(VisibilityFix.Full);
    }

    private setVisibility(v: "visible" | "hidden" | "inherit") {
        this.element.css("visibility", v);
    }

    public getLayoutPromise() {
        return this.layoutReady.promise;
    }

    getCaption() {
        return this.caption ?? (this.captionCb ? this.captionCb() : "Dialog");
    }

    setCaptionCb(cb: () => string) {
        this.captionCb = cb;
        this.update();
    }

    /**
     * Called after the user has finished dragging or resizing the element.
     */
    afterRelease?: () => void;

    private autoWidth?: boolean;

    async makeHeightAutomatic() {
        // workaround for Chrome issue where the browser jumps to the start of page when opening a dialog that
        // calls this
        await this.getModal();

        this.element.css("height", "auto");
        this.autoHeight = true;
    }

    toggleDetach() {
        const canDrag = this.canDrag();
        if (canDrag) {
            if (this.areaMinimized) {
                this.toggleMinimize();
            }
            this.element.css("position", "static");
            this.element.css("visibility", "inherit");
            for (const prop of ["width", "height"]) {
                this.element.css(prop, "");
            }
            this.element.removeClass("draggable-detached");
            this.element.addClass("draggable-attached");
            // console.log("attach", this.slidePars);
            if (this.slidePars) {
                vctrlInstance?.removeSlideParsState();
            }
        } else {
            this.element.css("position", this.anchor);
            this.element.css("visibility", "visible");
            if (this.slidePars) {
                this.initSlidePars();
            }
            void this.restoreSizeAndPosition(VisibilityFix.Full);
            this.element.removeClass("draggable-attached");
            this.element.addClass("draggable-detached");
        }
        this.element.css("z-index", this.getVisibleLayer());

        this.detachStorage.set(!canDrag);
        this.update();
    }

    private initSlidePars() {
        // Move the window to top-left corner unless it was moved previously.
        if (!this.posStorage.get()) {
            if (this.setLeft) {
                this.element.css("left", 0);
            }
            if (this.setTop) {
                this.element.css("top", 0);
            }
        }
        vctrlInstance?.addSlideParsState();
    }

    /**
     * The layering of pareditor's detachable windows acts weird (Source Block Preview's arrow and contents show above
     * detached windows) so this makes use of z-indexes to work around it.
     * @returns the layer/z-index for this window.
     */
    getVisibleLayer() {
        const diff = document.getElementById("diff");
        const preview = document.getElementById("currpreview");
        const orig = document.getElementById("origprev");
        let index = 1;
        if (!this.canDrag()) {
            return 0;
        }
        if (diff?.style.zIndex && diff?.style.zIndex > "0") {
            index++;
        }
        if (preview?.style.zIndex && preview?.style.zIndex > "0") {
            index++;
        }
        if (orig?.style.zIndex && orig?.style.zIndex > "0") {
            index++;
        }
        return index;
    }

    private async makeModalPositionAbsolute() {
        const modal = await this.getModal();
        if (!modal) {
            return;
        }
        modal.css("position", this.anchor);
    }

    private async getModal() {
        if (!this.modal) {
            return;
        }

        // We don't want "this.modal.opened" here because then the saved position gets set incorrectly
        // (50px too much left). On the other hand, when using "rendered", we have to manually hide the modal while
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
        return (
            m.css("position") === this.anchor && this.parentDraggable == null
        );
    }

    setDragClickFn(fn: () => void) {
        this.dragClick = fn;
    }

    setCloseFn(fn: (() => void) | undefined) {
        this.closeFn = fn;
        this.update();
    }

    /**
     * Refreshes the drag handle and the resize handles to match the current state.
     * The wrappers call this whenever the options or the element's position mode may have changed.
     */
    update() {
        if (!this.handle || this.destroyed) {
            return;
        }
        const canDrag = this.canDrag();
        const handle = $(this.handle);
        handle.toggleClass("attached", !canDrag);
        handle.toggleClass("minimized", this.areaMinimized);
        handle.toggleClass("hidden", !this.dragEnabled);
        const caption = this.getCaption();
        const p = handle.children("p");
        if (p.text() !== caption) {
            p.text(caption);
        }
        p.toggleClass("hidden", !caption);
        handle
            .children(".drag-detach")
            .toggleClass("hidden", !this.detachable)
            .toggleClass("glyphicon-arrow-left", canDrag)
            .toggleClass("glyphicon-arrow-right", !canDrag)
            .attr("title", canDrag ? "Attach" : "Detach");
        handle
            .children(".drag-minimize")
            .toggleClass(
                "hidden",
                !((this.click && canDrag) || this.canMinimize)
            )
            .toggleClass("glyphicon-unchecked", this.areaMinimized)
            .toggleClass("glyphicon-minus", !this.areaMinimized)
            .attr(
                "title",
                (this.areaMinimized ? "Maximize" : "Minimize") + " dialog"
            );
        handle.children(".drag-close").toggleClass("hidden", !this.closeFn);
        const canResize = this.canResize();
        this.element
            .children(".resizehandle-r")
            .toggleClass("hidden", !(canResize && !this.autoWidth));
        this.element
            .children(".resizehandle-d")
            .toggleClass("hidden", !(canResize && !this.autoHeight));
        this.element
            .children(".resizehandle-rd")
            .toggleClass(
                "hidden",
                !(canResize && !this.autoWidth && !this.autoHeight)
            );
    }

    private buildHandle() {
        this.element.prepend($(draggableTemplate));
        this.handle = this.element.children(".draghandle")[0];
        const handle = $(this.handle);
        handle.on("mousedown", (e) => {
            this.dragClick?.();
            e.preventDefault();
        });
        handle.children(".drag-detach").on("click", () => this.toggleDetach());
        handle
            .children(".drag-minimize")
            .on("click", () => this.toggleMinimize());
        handle.children(".drag-close").on("click", () => this.closeFn?.());
    }

    async postLink() {
        this.dragEnabled = this.draggable ?? true;
        this.buildHandle();
        this.update();
        this.createResizeHandlers();

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
        if (this.handle) {
            this.handle.addEventListener("mousedown", handleResizeBegin);
            this.handle.addEventListener("pointerdown", handleResizeBegin);
            this.handle.addEventListener("touchstart", handleResizeBegin);
        }

        // DialogController will call setInitialLayout in case draggable is inside modal
        if (!this.isModal()) {
            await this.setInitialLayout(VisibilityFix.Partial);
        }

        const canDrag = this.detachStorage.get();
        if (canDrag) {
            this.element.addClass("draggable-detached");
        } else {
            this.element.addClass("draggable-attached");
        }
        this.initialized = true;
        this.update();
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
            if (this.minStorage.get() && !this.forceMaximized) {
                this.toggleMinimize();
                this.areaHeight = 0;
            }
            if (this.detachStorage.get()) {
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
        this.update();
    }

    private async restoreSizeAndPosition(vf: VisibilityFix) {
        if (!this.posKey) {
            return;
        }
        // Don't try to restore size and position if the element is not draggable.
        if (!this.canDrag()) {
            return;
        }
        const oldSize = this.sizeStorage.get() ?? this.initialSize;
        if (oldSize && this.canDrag() && !this.isMinimized()) {
            const vps = getViewPortSize();
            if (oldSize.width) {
                this.element.css(
                    "width",
                    Math.min(getPixels(oldSize.width), vps.width * 0.97)
                );
            }
            if (oldSize.height && !this.autoHeight) {
                this.element.css(
                    "height",
                    Math.min(getPixels(oldSize.height), vps.height * 0.97)
                );
            }
        }
        const oldPos = this.posStorage.get();
        // it doesn't make sense to restore Y position if the dialog has absolute position (instead of fixed)
        if (await this.modalHasAbsolutePosition()) {
            // 50px was kept as a constant because it would be tiresome to look for
            // return value of the (utils.ts) file's (getOutOffsetVisible) function.
            const off = this.element.offset() ?? {left: 50};
            this.element.offset({top: window.pageYOffset, left: off.left});
        }
        if (oldPos) {
            if (oldPos.left && this.setLeft) {
                this.element.css("left", oldPos.left);
            }
            if (oldPos.right && this.setRight) {
                this.element.css("right", oldPos.right);
            }
            if (!(await this.modalHasAbsolutePosition())) {
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
            this.minStorage.set(true);
        } else {
            base.css("visibility", "");
            this.element.css("min-height", "");
            // Do not set left here - it will make dialogs appear hidden in some scenarios.
            // this.element.css("left", this.getCss("left") - (this.areaWidth - this.getWidth()));
            if (this.autoHeight) {
                this.element.height("auto");
            } else {
                this.element.height(
                    this.areaHeight != 0 ? this.areaHeight : "auto"
                );
            }
            this.element.width(this.areaWidth);
            this.minStorage.set(false);
        }

        // This should probably be moved elsewhere
        const previews = document.getElementById("previews");
        if (previews) {
            this.movePreviewsProperly(previews);
        }
        this.update();
    }

    /**
     * Handles moving previews around when at least one is minimized in editor.
     * @param previews The HTML element that holds the previews
     */
    movePreviewsProperly(previews: HTMLElement) {
        // The last child should have the class ".draggable-content".
        // If it's not, change pareditor's previews' structuring.
        const currPreview = document.getElementById("currpreview")
            ?.lastElementChild as HTMLElement;
        const origPreview = document.getElementById("origpreview")
            ?.lastElementChild as HTMLElement;
        const diff = document.getElementById("diff");
        if (
            this.caption != diff?.getAttribute("caption") &&
            (currPreview || origPreview)
        ) {
            if (
                currPreview?.style.visibility ||
                origPreview?.style.visibility
            ) {
                previews.classList.remove("sidebyside");
                previews.classList.add("stacked");
                if (currPreview) {
                    document
                        .getElementById("currpreview")!
                        .classList.add("ForceFullSize");
                }
                if (origPreview) {
                    document
                        .getElementById("origpreview")!
                        .classList.add("ForceFullSize");
                }
            } else {
                previews.classList.remove("stacked");
                previews.classList.add("sidebyside");
                if (currPreview) {
                    document
                        .getElementById("currpreview")!
                        .classList.remove("ForceFullSize");
                }
                if (origPreview) {
                    document
                        .getElementById("origpreview")!
                        .classList.remove("ForceFullSize");
                }
            }
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
        timLogTime(
            "set:" +
                [this.setLeft, this.setTop, this.setBottom, this.setRight].join(
                    ", "
                ),
            "drag"
        );
    }

    private resizeElement(
        e: MouseEvent | TouchEvent,
        up: boolean,
        right: boolean,
        down: boolean,
        left: boolean
    ) {
        e.preventDefault();
        this.resizeStates = {up, down, left, right};
        this.refreshEventListeners(e, this.moveResize);
    }

    private refreshEventListeners(
        e: MouseEvent | TouchEvent,
        handler: (e: MouseEvent | TouchEvent) => unknown
    ) {
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

    private removeMouseMove(handler: (e: MouseEvent | TouchEvent) => unknown) {
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

    canDrag() {
        return this.element.css("position") !== "static";
    }

    private canResize() {
        if (this.resize === false || this.areaMinimized) {
            return false;
        }
        if (this.resize === true) {
            return true;
        }
        // If resize is not specified, allow resizing only when the element is draggable.
        return this.canDrag();
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
            const css = this.element.css(["top", "bottom", "left", "right"]);
            this.posStorage.set(css);

            timLogTime("pos:" + css.left + "," + css.top, "drag");
        }
        this.update();
        this.afterRelease?.();
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
        if (
            bound.bottom === 0 &&
            bound.top === 0 &&
            bound.right === 0 &&
            bound.left === 0
        ) {
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
        const pos = this.getPageXY(e);
        this.doMove(pos);
        e.preventDefault();
        e.stopPropagation();
    };

    private doMove(pos: Pos) {
        const delta = {
            X: pos.x - this.lastPos.x,
            Y: pos.y - this.lastPos.y,
        };

        if (this.setTop) {
            this.element.css("top", this.prev.top + delta.Y);
        }
        if (this.setLeft) {
            this.element.css("left", this.prev.left + delta.X);
        }
        if (this.setBottom) {
            this.element.css("bottom", this.prev.bottom - delta.Y);
        }
        if (this.setRight) {
            this.element.css("right", this.prev.right - delta.X);
        }
    }

    moveResize = async (e: MouseEvent | TouchEvent) => {
        const pos = this.getPageXY(e);
        const delta = {
            X: pos.x - this.lastPos.x,
            Y: pos.y - this.lastPos.y,
        };

        // If the top and left styles have not been set, resizing behaves illogically.
        if (this.element[0].style.top == "") {
            this.element.css("top", "0");
        }
        if (this.element[0].style.left == "") {
            this.element.css("left", "0");
        }

        if (this.resizeStates.up) {
            this.element.css("height", this.prevSize.height - delta.Y);
            if (this.setTop) {
                this.element.css("top", this.prev.top + delta.Y);
            }
        }
        if (this.resizeStates.left) {
            this.element.css("width", this.prevSize.width - delta.X);
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
                this.element.css("right", this.prev.right - delta.X);
            }
        }

        e.preventDefault();
        e.stopPropagation();

        const size = await this.getSize();
        if (this.posKey) {
            this.sizeStorage.set(size);
        }
        if (this.resizeCallback) {
            this.resizeCallback({
                ...(await this.getSizeAsNum()),
                state: this.resizeStates,
            });
        }
    };

    async getSize() {
        await this.layoutReady.promise;
        return {
            width: this.element[0].style.width || null,
            height: this.element[0].style.height || null,
        };
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

    destroy() {
        this.destroyed = true;
        window.removeEventListener("resize", this.onWindowResize);
        this.removeMouseUp();
        this.removeMouseMove(this.move);
        this.removeMouseMove(this.moveResize);
    }

    setModal(modalInstance: IDraggableModal) {
        this.modal = modalInstance;
    }
}

const attached = new WeakMap<Element, DraggableController>();

function boolAttr(el: Element, name: string): boolean | undefined {
    const v = el.getAttribute(name);
    if (v == null) {
        return undefined;
    }
    return v !== "false";
}

/**
 * Makes the elements that have the attribute tim-draggable-fixed draggable.
 * This is for HTML that does not come from an Angular template, such as document paragraphs
 * with the float attribute. The options are read from the attributes of the element.
 * Elements that are already draggable are skipped.
 *
 * @param root The element whose descendants (and the element itself) are checked.
 */
export function attachDraggables(root: Element) {
    const elements = [
        ...(root.matches("[tim-draggable-fixed]") ? [root] : []),
        ...root.querySelectorAll("[tim-draggable-fixed]"),
    ];
    for (const el of elements) {
        if (attached.has(el) || !(el instanceof HTMLElement)) {
            continue;
        }
        const ctrl = new DraggableController(el);
        attached.set(el, ctrl);
        const parent = el.parentElement?.closest("[tim-draggable-fixed]");
        ctrl.parentDraggable = parent ? attached.get(parent) : undefined;
        ctrl.canMinimize = boolAttr(el, "can-minimize");
        ctrl.absolute = boolAttr(el, "absolute");
        ctrl.anchor = el.getAttribute("anchor") ?? ctrl.anchor;
        ctrl.caption = el.getAttribute("caption") ?? undefined;
        ctrl.click = boolAttr(el, "click");
        ctrl.detachable = boolAttr(el, "detachable");
        ctrl.forceMaximized = boolAttr(el, "force-maximized");
        ctrl.resize = boolAttr(el, "resize");
        ctrl.draggable = boolAttr(el, "draggable");
        ctrl.save = el.getAttribute("save") ?? undefined;
        ctrl.slidePars = boolAttr(el, "slide-pars");
        ctrl.init();
        void ctrl.postLink();
    }
}

/**
 * Tells attachDraggables that the element already has a draggable. Used by DraggableDirective.
 */
export function markDraggableAttached(el: Element, ctrl: DraggableController) {
    attached.set(el, ctrl);
}

/**
 * Returns the draggable of the element, if any.
 */
export function getAttachedDraggable(el: Element) {
    return attached.get(el);
}
