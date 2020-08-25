import {Component, OnInit} from "@angular/core";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {to} from "tim/util/utils";

export interface ICopyTableWidthsParams {
}

@Component({
    selector: "tim-copy-table-width-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>Copy table widths</ng-container>
            <ng-container body>wew</ng-container>
            <ng-container footer>wew2</ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["./copy-table-width-dialog.component.scss"],
})
export class CopyTableWidthDialogComponent extends AngularDialogComponent<ICopyTableWidthsParams, void> implements OnInit {
    protected dialogName: string = "copy-table-width";

    ngOnInit(): void {
    }
}

let instance: AngularDialogComponent<ICopyTableWidthsParams, void> | undefined;
export async function showCopyWidthsDialog(params: ICopyTableWidthsParams) {
    if (instance) {
        return;
    }
    const dialog = angularDialog.open(CopyTableWidthDialogComponent, params);
    instance = await dialog;
    await to(instance.result);
    instance = undefined;
    return;
}
