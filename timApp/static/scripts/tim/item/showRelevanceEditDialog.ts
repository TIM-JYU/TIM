import {IItem} from "tim/item/IItem";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showRelevanceEditDialog(d: IItem) {
    const {RelevanceEditDialogComponent} = await import(
        "./relevance-edit-dialog.component"
    );
    return await (await angularDialog.open(RelevanceEditDialogComponent, d))
        .result;
}
