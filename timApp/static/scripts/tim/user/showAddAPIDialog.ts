/**
 * Brings up the dialog component for adding translator API keys to user settings
 * @author Noora Jokela
 * @date 21.3.2022
 * @licence MIT
 * @copyright 2022 TIMTra project authors
 */

import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {IUserApiKey} from "tim/user/IUser";

/*
Code source: showAddContactDialog.ts
 */
export async function showAddAPIKeyDialog(onAdd: (key: IUserApiKey) => void) {
    const {AddAPIKeyDialogComponent} = await import(
        "./add-api-key-dialog.component"
    );

    return (
        await angularDialog.open(
            AddAPIKeyDialogComponent,
            {onAdd},
            {
                resetSize: true,
            }
        )
    ).result;
}
