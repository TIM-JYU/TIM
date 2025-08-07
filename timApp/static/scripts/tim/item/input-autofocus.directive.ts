import {
    Directive,
    ElementRef,
    Input,
    type OnChanges,
    type SimpleChanges,
} from "@angular/core";

@Directive({
    selector: "[inputAutofocus]",
})
export class InputAutofocusDirective implements OnChanges {
    @Input() inputAutofocus = false;

    constructor(private elementRef: ElementRef<HTMLInputElement>) {}

    ngOnChanges(changes: SimpleChanges) {
        if (this.inputAutofocus && changes.inputAutofocus.currentValue) {
            setTimeout(() => this.elementRef.nativeElement.focus(), 0);
        }
    }
}
