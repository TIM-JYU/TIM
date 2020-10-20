import {IItem} from "tim/item/IItem";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showTagDialog(d: IItem) {
    const {EditTagsDialogComponent} = await import(
        "./edit-tags-dialog.component"
    );
    return await (await angularDialog.open(EditTagsDialogComponent, d)).result;
}
