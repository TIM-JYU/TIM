import {to2} from "tim/util/utils";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function openSessionCheckDialog() {
    const {SessionCheckDialogComponent} = await import(
        "./session-check-dialog.component"
    );
    return to2(
        (await angularDialog.open(SessionCheckDialogComponent, undefined))
            .result
    );
}
