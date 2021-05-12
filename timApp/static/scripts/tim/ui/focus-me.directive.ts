import {Directive, ElementRef, Input, OnChanges, OnInit} from "@angular/core";
import {Changes} from "tim/util/angularchanges";

@Directive({
    selector: "[focusMe]",
})
export class FocusMeDirective implements OnInit, OnChanges {
    @Input() enable = true;

    constructor(private elementRef: ElementRef<HTMLElement>) {}

    ngOnInit() {
        if (this.enable) {
            this.doFocus();
        }
    }

    ngOnChanges(changes: Changes<this, "enable">): void {
        if (changes.enable?.currentValue) {
            this.doFocus();
        }
    }

    private doFocus() {
        // setTimeout required to avoid ExpressionChangedAfterItHasBeenCheckedError,
        // see https://stackoverflow.com/questions/41873893/angular2-autofocus-input-element
        setTimeout(() => {
            this.elementRef.nativeElement.focus();
        }, 0);
    }
}
