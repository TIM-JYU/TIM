import {angularDialog} from "../ui/angulardialog/dialog.service";
import {IUserAPIKey} from "./IUser";

/*
Code source: showAddContactDialog.ts
 */
export async function showAddAPIKeyDialog(onAdd: (key: IUserAPIKey) => void) {
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
