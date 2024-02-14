import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {GroupMember} from "tim/ui/group-management.component";
import type {UserImportDialogParams} from "tim/user/user-import-dialog.component";

export async function showUserImportDialog(
    params?: UserImportDialogParams
): Promise<GroupMember[]> {
    const {UserImportDialogComponent} = await import(
        "tim/user/user-import-dialog.component"
    );
    return (await angularDialog.open(UserImportDialogComponent, params)).result;
}
