import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showMessageDialog(message: string) {
    const {MessageDialogComponent} = await import("./message-dialog.component");
    return (await angularDialog.open(MessageDialogComponent, message)).result;
}
