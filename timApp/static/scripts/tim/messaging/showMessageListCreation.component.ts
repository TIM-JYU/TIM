import {unknown} from "io-ts";
import {angularDialog} from "../ui/angulardialog/dialog.service";

export async function showMessageListCreation() {
    const {MessageListCreateDialogComponent} = await import(
        "./message-list-create-dialog.component"
    );
    return (
        await angularDialog.open(MessageListCreateDialogComponent, unknown, {
            resetSize: true,
        })
    ).result;
}
