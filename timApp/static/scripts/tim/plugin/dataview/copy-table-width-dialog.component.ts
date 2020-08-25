import {Component, OnInit} from "@angular/core";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {copyToClipboard, to} from "tim/util/utils";

export interface ICopyTableWidthsParams {
    columnWidths: number[];
}

@Component({
    selector: "tim-copy-table-width-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>Copy table widths</ng-container>
            <ng-container body>
                <p>Loading times can be improved by setting static column widths.</p>
                <p>Copy the below setting under <code>dataView</code> configuration:</p>
                <pre>{{settingYaml}}</pre>
            </ng-container>
            <ng-container footer>
                <span *ngIf="showCopiedMessage" >Copied!</span>
                <button class="timButton" type="button" (click)="copyText()">Copy text</button>
                <button class="btn btn-default" type="button" (click)="close()">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["./copy-table-width-dialog.component.scss"],
})
export class CopyTableWidthDialogComponent extends AngularDialogComponent<ICopyTableWidthsParams, void> implements OnInit {
    protected dialogName: string = "copy-table-width";
    settingYaml: string = "";
    showCopiedMessage = false;

    ngOnInit(): void {
    }

    copyText() {
        copyToClipboard(this.settingYaml);
        this.showCopiedMessage = true;
        setTimeout(() => this.showCopiedMessage = false, 1000);
    }

    close() {
        this.dismiss();
    }
}

let instance: AngularDialogComponent<ICopyTableWidthsParams, void> | undefined;
export async function showCopyWidthsDialog(params: ICopyTableWidthsParams) {
    if (instance) {
        return;
    }
    const dialog = angularDialog.open(CopyTableWidthDialogComponent, params, { resetSize: true });
    instance = await dialog;
    await to(instance.result);
    instance = undefined;
    return;
}
