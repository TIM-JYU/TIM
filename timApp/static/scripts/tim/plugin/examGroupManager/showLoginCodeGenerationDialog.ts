import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {LoginCodeGenerationDialogParams} from "tim/plugin/examGroupManager/login-code-generation-dialog.component";

export async function showLoginCodeGenerationDialog(
    params?: LoginCodeGenerationDialogParams
): Promise<undefined> {
    const {LoginCodeGenerationDialogComponent} = await import(
        "./login-code-generation-dialog.component"
    );
    return (
        await angularDialog.open(LoginCodeGenerationDialogComponent, params)
    ).result;
}