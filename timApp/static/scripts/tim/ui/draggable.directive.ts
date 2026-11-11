import type {AfterViewInit, DoCheck, OnDestroy, OnInit} from "@angular/core";
import {
    Directive,
    ElementRef,
    Input,
    NgModule,
    Optional,
    SkipSelf,
} from "@angular/core";
import type {IDraggableOptions} from "tim/ui/draggable";
import {
    DraggableController,
    DraggableSize,
    markDraggableAttached,
} from "tim/ui/draggable";

type BoolInput = boolean | string | undefined;

function toBool(v: BoolInput): boolean | undefined {
    if (typeof v === "string") {
        return v !== "false";
    }
    return v;
}

/**
 * Makes the element a draggable window with a caption bar. The content must be in a child element
 * that has the class "draggable-content".
 */
@Directive({
    selector: "[tim-draggable-fixed]",
    exportAs: "timDraggable",
    standalone: false,
})
export class DraggableDirective
    implements OnInit, AfterViewInit, DoCheck, OnDestroy
{
    @Input() canMinimize?: BoolInput;
    @Input() absolute?: BoolInput;
    @Input() anchor?: string;
    @Input() caption?: string;
    @Input() click?: BoolInput;
    @Input() detachable?: BoolInput;
    @Input() forceMaximized?: BoolInput;
    @Input() resize?: BoolInput;
    @Input() dragEnabled?: BoolInput;
    @Input() save?: string;
    @Input() initialSize?: DraggableSize;
    @Input() slidePars?: BoolInput;

    readonly ctrl: DraggableController;

    constructor(
        element: ElementRef<HTMLElement>,
        @Optional() @SkipSelf() parent?: DraggableDirective
    ) {
        this.ctrl = new DraggableController(element.nativeElement);
        markDraggableAttached(element.nativeElement, this.ctrl);
        this.ctrl.parentDraggable = parent?.ctrl;
    }

    private copyOptions() {
        const o: IDraggableOptions = this.ctrl;
        o.canMinimize = toBool(this.canMinimize);
        o.absolute = toBool(this.absolute);
        if (this.anchor) {
            o.anchor = this.anchor;
        }
        o.caption = this.caption;
        o.click = toBool(this.click);
        o.detachable = toBool(this.detachable);
        o.forceMaximized = toBool(this.forceMaximized);
        o.resize = toBool(this.resize);
        o.draggable = toBool(this.dragEnabled);
        o.save = this.save;
        o.initialSize = this.initialSize;
        o.slidePars = toBool(this.slidePars);
    }

    ngOnInit() {
        this.copyOptions();
        this.ctrl.init();
    }

    ngAfterViewInit() {
        void this.ctrl.postLink();
    }

    ngDoCheck() {
        this.copyOptions();
        this.ctrl.update();
    }

    ngOnDestroy() {
        this.ctrl.destroy();
    }
}

@NgModule({
    declarations: [DraggableDirective],
    exports: [DraggableDirective],
})
export class DraggableModule {}
