import {Component, Input} from "@angular/core";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {HttpClient} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
import {documentglobals} from "tim/util/globals";
import {getActiveDocument} from "../document/activedocument";

@Component({
    selector: "tim-mark-all-as-read",
    template: `
        <button class="timButton" (click)="clicked()">{{ buttonText }}</button>
    `,
    styles: [
        `
            button {
                font-size: x-small;
                margin-right: 0px;
                display: block;
                margin-left: auto;
            }
        `,
    ],
})
export class MarkAllAsReadComponent {
    @Input() buttonText?: string;
    @Input() itemId?: number;

    constructor(private http: HttpClient) {}

    ngOnInit() {
        this.itemId = this.itemId ?? documentglobals()?.curr_item?.id;
        if (!this.buttonText) {
            this.buttonText = $localize`Mark all as read`;
        }
    }

    async clicked() {
        if (this.itemId) {
            const r = await toPromise(
                this.http.put(`/read/${this.itemId}`, {})
            );
            if (!r.ok) {
                await showMessageDialog(
                    $localize`Could not mark the document as read.`
                );
                return;
            }
            document
                .querySelectorAll(".readline")
                .forEach((e) => e.setAttribute("class", "readline read"));
            getActiveDocument().refreshSectionReadMarks();
        }
    }
}
