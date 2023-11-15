import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {UserCreationDialogParams} from "tim/user/user-creation-dialog.component";
import type {GroupMember} from "tim/ui/group-management.component";

export async function showUserCreationDialog(
    params?: UserCreationDialogParams
): Promise<GroupMember> {
    const {UserCreationDialogComponent} = await import(
        "tim/user/user-creation-dialog.component"
    );
    return (await angularDialog.open(UserCreationDialogComponent, params))
        .result;
}
