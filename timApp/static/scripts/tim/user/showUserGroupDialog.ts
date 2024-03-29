import type {IDocument} from "tim/item/IItem";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showUserGroupDialog(): Promise<IDocument> {
    const {UserGroupDialogComponent} = await import(
        "./user-group-dialog.component"
    );
    return (await angularDialog.open(UserGroupDialogComponent, undefined))
        .result;
}
