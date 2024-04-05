import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {to2} from "tim/util/utils";

export async function showMessageDialog(
    message: string,
    modal: boolean = false
) {
    const {MessageDialogComponent} = await import("./message-dialog.component");
    return to2(
        (
            await angularDialog.open(MessageDialogComponent, {
                message,
                modal,
            })
        ).result
    );
}
