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
            <ng-container header i18n>Copy table widths</ng-container>
            <ng-container body>
                <ng-container i18n>
                    <p>Loading times can be improved by setting static column widths.</p>
                    <p>Copy the below settings under <code>dataView</code> configuration:</p>
                </ng-container>
                <pre>{{settingYaml}}</pre>
            </ng-container>
            <ng-container footer>
                <span *ngIf="showCopiedMessage" i18n>Copied!</span>
                <button class="timButton" type="button" (click)="copyText()" i18n>Copy text</button>
                <button class="btn btn-default" type="button" (click)="close()" i18n>Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["./copy-table-width-dialog.component.scss"],
})
export class CopyTableWidthDialogComponent
    extends AngularDialogComponent<ICopyTableWidthsParams, void>
    implements OnInit {
    protected dialogName: string = "copy-table-width";
    settingYaml: string = "";
    showCopiedMessage = false;

    ngOnInit(): void {
        this.settingYaml = "  columnWidths:\n";
        this.settingYaml += this.data.columnWidths
            .map((val, i) => `    '${i}': ${val}`)
            .filter((v) => v !== undefined)
            .join("\n");
    }

    copyText() {
        copyToClipboard(this.settingYaml);
        this.showCopiedMessage = true;
        setTimeout(() => (this.showCopiedMessage = false), 1000);
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
    const dialog = angularDialog.open(CopyTableWidthDialogComponent, params, {
        resetSize: true,
    });
    instance = await dialog;
    await to(instance.result);
    instance = undefined;
    return;
}
