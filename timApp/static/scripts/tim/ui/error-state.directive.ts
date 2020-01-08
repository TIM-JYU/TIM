import {AfterContentInit, ContentChild, Directive, ElementRef, OnDestroy} from "@angular/core";
import {NgModel} from "@angular/forms";
import {Subscription} from "rxjs";
import {InputService} from "./input.service";

@Directive({
    selector: "[timErrorState]",
    providers: [InputService],
})
export class ErrorStateDirective implements AfterContentInit, OnDestroy {
    @ContentChild(NgModel, {static: false}) c?: NgModel;
    private sub?: Subscription;

    constructor(private i: InputService, private e: ElementRef<HTMLElement>) {
    }

    ngAfterContentInit() {
        if (this.c && this.c.statusChanges) {
            this.sub = this.c.statusChanges.subscribe((status) => {
                if (status === "VALID") {
                    this.e.nativeElement.classList.remove("has-error");
                } else if (this.c!.dirty) {
                    this.e.nativeElement.classList.add("has-error");
                }
            });
            this.i.defer.resolve(this.c);
        } else {
            this.i.defer.reject();
        }
    }

    ngOnDestroy() {
        if (this.sub) {
            this.sub.unsubscribe();
        }
    }
}
