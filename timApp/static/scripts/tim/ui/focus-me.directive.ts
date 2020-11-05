import {Directive, OnInit, ElementRef, Input} from "@angular/core";

@Directive({
    selector: "[focusMe]",
})
export class FocusMeDirective implements OnInit {
    @Input() enable = true;

    constructor(private elementRef: ElementRef<HTMLElement>) {}

    ngOnInit() {
        // setTimeout required to avoid ExpressionChangedAfterItHasBeenCheckedError,
        // see https://stackoverflow.com/questions/41873893/angular2-autofocus-input-element
        if (!this.enable) {
            return;
        }
        setTimeout(() => {
            this.elementRef.nativeElement.focus();
        }, 0);
    }
}
