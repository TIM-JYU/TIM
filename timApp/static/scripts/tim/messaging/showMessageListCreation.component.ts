import {angularDialog} from "../ui/angulardialog/dialog.service";

export async function showMessageListCreation(message: string) {
    const {MessageListCreation} = await import(
        "./message-list-creation.component"
    );
    return (await angularDialog.open(MessageListCreation, message)).result;
}
