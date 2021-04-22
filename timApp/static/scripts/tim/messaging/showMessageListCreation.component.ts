import {unknown} from "io-ts";
import {angularDialog} from "../ui/angulardialog/dialog.service";

export async function showMessageListCreation() {
    const {MessageListComponent} = await import(
        "./message-list-creation.component"
    );
    return (
        await angularDialog.open(MessageListComponent, unknown, {
            resetSize: true,
        })
    ).result;
}
