import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {IMergeParams} from "tim/document/minutes/merge-pdf-dialog.component";

export async function showMergePdfDialog(p: IMergeParams) {
    const {MergePdfDialogComponent} = await import(
        "./merge-pdf-dialog.component"
    );
    return await (await angularDialog.open(MergePdfDialogComponent, p)).result;
}
