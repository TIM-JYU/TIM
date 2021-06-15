import {unknown} from "io-ts";
import {angularDialog} from "../ui/angulardialog/dialog.service";

export async function showAddContactDialog() {
    const {AddContactDialogComponent} = await import(
        "./add-contact-dialog.component"
    );

    return (
        await angularDialog.open(AddContactDialogComponent, unknown, {
            resetSize: true,
        })
    ).result;
}
