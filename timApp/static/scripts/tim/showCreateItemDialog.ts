import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showCreateItemDialog() {
    const {CreateItemDialogComponent} = await import(
        "./create-item-dialog.component"
    );
    return (await angularDialog.open(CreateItemDialogComponent, undefined))
        .result;
}
