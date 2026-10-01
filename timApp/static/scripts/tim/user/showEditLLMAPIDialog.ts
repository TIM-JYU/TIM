import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {IUserLLMApiKey} from "tim/user/IUser";

export async function showEditLLMAPIKeyDialog(
    key: IUserLLMApiKey,
    onEdit: (key: IUserLLMApiKey) => void
) {
    const {EditLLMAPIKeyDialogComponent} = await import(
        "./edit-llm-api-key-dialog.component"
    );

    return (
        await angularDialog.open(
            EditLLMAPIKeyDialogComponent,
            {key, onEdit},
            {resetSize: true}
        )
    ).result;
}
