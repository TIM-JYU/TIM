import {angularDialog} from "../ui/angulardialog/dialog.service";

export async function showMessageListCreation(message: string) {
    const {MessageListComponent} = await import(
        "./message-list-creation.component"
    );
    return (await angularDialog.open(MessageListComponent, message)).result;
}
