import {IItem} from "tim/item/IItem";
import {to2} from "tim/util/utils";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showViewRangeEditDialog(d: IItem) {
    const {ViewRangeEditDialogComponent} = await import(
        "./view-range-edit-dialog.component"
    );
    return to2(
        (await angularDialog.open(ViewRangeEditDialogComponent, d)).result
    );
}
