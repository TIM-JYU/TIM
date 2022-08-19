import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {IRestTaskLockDialogParams} from "./reset-task-lock-dialog-component";

export async function showResetTaskLock(p: IRestTaskLockDialogParams) {
    const {ResetTaskLockDialogComponent} = await import(
        "./reset-task-lock-dialog-component"
    );
    return (await angularDialog.open(ResetTaskLockDialogComponent, p)).result;
}
