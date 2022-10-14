import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {IUserContact} from "tim/user/IUser";

export async function showAddContactDialog(
    onAdd: (contact: IUserContact) => void
) {
    const {AddContactDialogComponent} = await import(
        "./add-contact-dialog.component"
    );

    return (
        await angularDialog.open(
            AddContactDialogComponent,
            {onAdd},
            {
                resetSize: true,
            }
        )
    ).result;
}
