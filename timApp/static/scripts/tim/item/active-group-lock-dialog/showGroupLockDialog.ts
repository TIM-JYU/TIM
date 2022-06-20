import {angularDialog} from "../../ui/angulardialog/dialog.service";

export async function showGroupLockDialog() {
    const {ActiveGroupLockDialogComponent} = await import(
        "./active-group-lock-dialog.component"
    );
    return await (
        await angularDialog.open(ActiveGroupLockDialogComponent, {})
    ).result;
}
