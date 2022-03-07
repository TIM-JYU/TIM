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
    private confirm?: string;
    buttonText?: string;
    @Input() itemId?: number;

    constructor(private http: HttpClient) {}

    ngOnInit() {
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
                })
            );
            if (r.ok) {
                location.reload();
            } else {
                await showMessageDialog(
                    `Error expiring right: ${r.result.error.error}`
                );
            }
        }
    }
}
