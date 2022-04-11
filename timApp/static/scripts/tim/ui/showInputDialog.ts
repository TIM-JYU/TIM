import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {
    InputDialogComponent as IDC,
    InputDialogParams,
} from "tim/ui/input-dialog.component";

export async function showInputDialog<T>(p: InputDialogParams<T>) {
    const {InputDialogComponent} = await import("./input-dialog.component");
    return (
        await angularDialog.open<InputDialogParams<T>, T, IDC<T>>(
            InputDialogComponent,
            p
        )
    ).result;
}
