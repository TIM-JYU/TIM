import type {OnChanges} from "@angular/core";
import {Directive, Input} from "@angular/core";
import {TooltipDirective} from "ngx-bootstrap/tooltip";

/**
 * Opens and closes a ngx-bootstrap tooltip according to a bound value:
 *
 * `<input [tooltip]="error" [isOpen]="error !== undefined">`
 *
 * ngx-bootstrap had this binding itself until version 21.2, where `isOpen` stopped being an input
 * (it is only a property of the tooltip directive there). Import this directive together with `TooltipModule`.
 */
@Directive({
    selector: "[tooltip][isOpen]",
    standalone: true,
})
export class TooltipIsOpenDirective implements OnChanges {
    @Input() isOpen?: boolean;

    constructor(private tooltip: TooltipDirective) {}

    ngOnChanges() {
        this.tooltip.isOpen = this.isOpen ?? false;
    }
}
