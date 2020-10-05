/**
 * Dialog showing relevance editing component.
 */

import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {Component} from "@angular/core";
import {IItem} from "./IItem";

@Component({
    selector: "tim-relevance-edit-dialog",
    template: `
        <tim-dialog-frame class="overflow-visible">
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <tim-relevance-edit [item]="data"></tim-relevance-edit>
            </ng-container>
            <ng-container footer>
                <button class="timButton" (click)="dismiss()">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class RelevanceEditDialogComponent extends AngularDialogComponent<
    IItem,
    void
> {
    protected dialogName = "relevanceEdit";

    getTitle() {
        return "Edit relevance";
    }
}

export async function showRelevanceEditDialog(d: IItem) {
    return await (await angularDialog.open(RelevanceEditDialogComponent, d))
        .result;
}
