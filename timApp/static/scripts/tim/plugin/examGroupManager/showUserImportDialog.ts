import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {UserImportDialogParams} from "tim/plugin/examGroupManager/user-import-dialog.component";
import type {GroupMember} from "tim/plugin/examGroupManager/exam-group-manager.component";

export async function showUserImportDialog(
    params?: UserImportDialogParams
): Promise<GroupMember[]> {
    const {UserImportDialogComponent} = await import(
        "./user-import-dialog.component"
    );
    return (await angularDialog.open(UserImportDialogComponent, params)).result;
}
