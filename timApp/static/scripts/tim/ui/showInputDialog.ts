import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {
    InputDialogComponent as IDC,
    InputDialogParams,
} from "tim/ui/input-dialog.component";
import {IDialogOptions} from "tim/ui/angulardialog/angular-dialog-component.directive";

export async function showInputDialog<T>(
    p: InputDialogParams<T>,
    dialogOptions?: IDialogOptions
) {
    const {InputDialogComponent} = await import("./input-dialog.component");
    return (
        await angularDialog.open<InputDialogParams<T>, T, IDC<T>>(
            InputDialogComponent,
            p,
            dialogOptions
        )
    ).result;
}
