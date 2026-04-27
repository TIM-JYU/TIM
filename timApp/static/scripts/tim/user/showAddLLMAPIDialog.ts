import type {IUserLLMApiKey} from "tim/user/IUser";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showAddLLMAPIKeyDialog(
    onAdd: (key: IUserLLMApiKey) => void
) {
    const {AddLLMAPIKeyDialogComponent} = await import(
        "./add-llm-api-key-dialog.component"
    );

    return (
        await angularDialog.open(
            AddLLMAPIKeyDialogComponent,
            {onAdd},
            {
                resetSize: true,
            }
        )
    ).result;
}
