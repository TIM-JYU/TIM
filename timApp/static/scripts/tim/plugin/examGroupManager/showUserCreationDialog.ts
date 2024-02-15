import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {UserCreationDialogParams} from "tim/plugin/examGroupManager/user-creation-dialog.component";
import type {GroupMember} from "tim/plugin/examGroupManager/exam-group-manager.component";

export async function showUserCreationDialog(
    params?: UserCreationDialogParams
): Promise<GroupMember> {
    const {UserCreationDialogComponent} = await import(
        "./user-creation-dialog.component"
    );
    return (await angularDialog.open(UserCreationDialogComponent, params))
        .result;
}
