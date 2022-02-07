import {IDocument} from "../item/IItem";
import {angularDialog} from "../ui/angulardialog/dialog.service";

export async function showUserGroupDialog(): Promise<IDocument> {
    const {UserGroupDialogComponent} = await import(
        "./user-group-dialog.component"
    );
    return (await angularDialog.open(UserGroupDialogComponent, undefined))
        .result;
}
