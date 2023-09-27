import type {IDocument} from "tim/item/IItem";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {UserGroupDialogParams} from "tim/user/user-group-dialog.component";

export async function showUserGroupDialog(
    params?: UserGroupDialogParams
): Promise<IDocument> {
    const {UserGroupDialogComponent} = await import(
        "./user-group-dialog.component"
    );
    return (await angularDialog.open(UserGroupDialogComponent, params)).result;
}
