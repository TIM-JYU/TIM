import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {
    DiffDialogComponent as DiffDialogComponentType,
    IDiffParams,
} from "tim/document/diff-dialog.component";

export let diffDialog: DiffDialogComponentType | undefined;

export function setDiffDialog(d: DiffDialogComponentType | undefined) {
    diffDialog = d;
}

export async function showDiffDialog(p: IDiffParams) {
    const {DiffDialogComponent} = await import("./diff-dialog.component");
    return await angularDialog.open<IDiffParams, void, DiffDialogComponentType>(
        DiffDialogComponent,
        p
    );
}
