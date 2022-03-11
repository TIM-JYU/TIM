import {angularDialog} from "../ui/angulardialog/dialog.service";
import {to2} from "../util/utils";

export async function showVelpDialog() {
    void (await import("./velps.module"));
    const {VelpSelectionDialog} = await import(
        "./velp-selection-dialog.component"
    );
    return await to2(angularDialog.open(VelpSelectionDialog, undefined));
}
