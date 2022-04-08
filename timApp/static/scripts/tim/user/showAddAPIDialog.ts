/**
 * Created by noemjoke on 21.3.2022
 * Brings up the dialog component for adding translator API keys to user settings
 * @author Noora Jokela
 * @licence MIT
 * @copyright 2022 TIMTra project authors
 */

import {angularDialog} from "../ui/angulardialog/dialog.service";
import {IUserApiKey} from "./IUser";

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
