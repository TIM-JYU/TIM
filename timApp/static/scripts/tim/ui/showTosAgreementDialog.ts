import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showTosAgreementDialog() {
    const {TosDialogComponent} = await import("./tos-dialog.component");
    return await (
        await angularDialog.open(TosDialogComponent, null)
    ).result;
}
