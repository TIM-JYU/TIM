import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showKorppiExportDialog() {
    const {KorppiExportDialogComponent} = await import(
        "./korppi-export-dialog.component"
    );
    return (await angularDialog.open(KorppiExportDialogComponent, undefined))
        .result;
}
