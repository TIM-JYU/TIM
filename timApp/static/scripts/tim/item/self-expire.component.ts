import {Component, Input} from "@angular/core";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {HttpClient} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
import {documentglobals} from "tim/util/globals";

@Component({
    selector: "tim-self-expire",
    template: `
        <button class="timButton" (click)="clicked()" [disabled]="itemId === undefined">{{ buttonText }}</button>
    `,
})
export class SelfExpireComponent {
    @Input() confirm?: string;
    @Input() buttonText?: string;
    @Input() itemId?: number;
    @Input() redirectHref?: string;
    @Input() setField?: string;

    constructor(private http: HttpClient) {}

    ngOnInit() {
        // Legacy support for AngularJS item-id being a string
        if (typeof this.itemId !== "number") {
            this.itemId = documentglobals()?.curr_item?.id;
        }
        this.itemId = this.itemId ?? documentglobals()?.curr_item?.id;
        if (!this.buttonText) {
            this.buttonText = "Remove your rights";
        }
    }

    async clicked() {
        if (!this.confirm || window.confirm(this.confirm)) {
            const r = await toPromise(
                this.http.post("/permissions/selfExpire", {
                    id: this.itemId,
                    set_field: this.setField,
                })
            );
            if (r.ok) {
                if (this.redirectHref) {
                    window.location.href = this.redirectHref;
                } else {
                    location.reload();
                }
            } else {
                await showMessageDialog(
                    `Error expiring right: ${r.result.error.error}`
                );
            }
        }
    }
}
