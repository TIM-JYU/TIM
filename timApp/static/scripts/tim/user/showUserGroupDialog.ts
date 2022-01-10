import {IDocument} from "../item/IItem";
import {angularDialog} from "../ui/angulardialog/dialog.service";
import {UserGroupDialogComponent} from "./user-group-dialog.component";

export async function showUserGroupDialog(): Promise<IDocument> {
    return (await angularDialog.open(UserGroupDialogComponent, undefined))
        .result;
}
