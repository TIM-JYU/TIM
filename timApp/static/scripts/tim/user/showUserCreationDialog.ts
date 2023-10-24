import type {IDocument} from "../item/IItem";
import {angularDialog} from "../ui/angulardialog/dialog.service";
import type {UserCreationDialogParams} from "./user-creation-dialog.component";
import {GroupMember} from "tim/ui/group-management.component";

export async function showUserCreationDialog(
    params?: UserCreationDialogParams
): Promise<GroupMember> {
    const {UserCreationDialogComponent} = await import(
        "./user-creation-dialog.component"
    );
    return (await angularDialog.open(UserCreationDialogComponent, params))
        .result;
}
