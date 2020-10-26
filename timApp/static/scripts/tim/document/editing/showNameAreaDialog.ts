import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showNameAreaDialog() {
    const {NameAreaDialogComponent} = await import(
        "./name-area-dialog.component"
    );
    return await (await angularDialog.open(NameAreaDialogComponent, undefined))
        .result;
}
