import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {
    IPopupParams,
    PopupMenuDialogComponent as PopupMenuDialogComponentType,
} from "tim/document/popup-menu-dialog.component";
import {Pos} from "tim/ui/pos";

export async function showPopupMenu(p: IPopupParams, pos: Pos) {
    const {PopupMenuDialogComponent} = await import(
        "./popup-menu-dialog.component"
    );
    return await angularDialog.open<
        IPopupParams,
        void,
        PopupMenuDialogComponentType
    >(PopupMenuDialogComponent, p, {pos});
}
