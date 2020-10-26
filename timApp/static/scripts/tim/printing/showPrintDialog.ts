import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {IPrintParams} from "tim/printing/print-dialog.component";

export async function showPrintDialog(p: IPrintParams) {
    const {PrintDialogComponent} = await import("./print-dialog.component");
    return await (await angularDialog.open(PrintDialogComponent, p)).result;
}
