import {to2} from "../../util/utils";
import {angularDialog} from "../../ui/angulardialog/dialog.service";
import {IEditJsonEventOptions} from "./edit-json-event-dialog.component";

export async function showEditJsonEventDialog(opts: IEditJsonEventOptions) {
    const {EditJsonEventDialogComponent} = await import(
        "./edit-json-event-dialog.component"
    );
    return to2(
        (await angularDialog.open(EditJsonEventDialogComponent, opts)).result
    );
}
