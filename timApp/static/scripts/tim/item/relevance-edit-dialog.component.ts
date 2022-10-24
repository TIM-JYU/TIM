/**
 * Dialog showing relevance editing component.
 */

import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {FormsModule} from "@angular/forms";
import {RelevanceEditComponent} from "tim/item/relevance-edit.component";
import {TypeaheadModule} from "ngx-bootstrap/typeahead";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {IItem} from "tim/item/IItem";
import {CommonModule} from "@angular/common";

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

@NgModule({
    declarations: [RelevanceEditDialogComponent, RelevanceEditComponent],
    imports: [
        CommonModule,
        DialogModule,
        FormsModule,
        TypeaheadModule.forRoot(),
        TimUtilityModule,
    ],
})
export class ViewRangeEditDialogModule {}
