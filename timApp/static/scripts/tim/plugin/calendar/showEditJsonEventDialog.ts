import {to2} from "tim/util/utils";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {IEditJsonEventOptions} from "tim/plugin/calendar/edit-json-event-dialog.component";

export async function showEditJsonEventDialog(opts: IEditJsonEventOptions) {
    const {EditJsonEventDialogComponent} = await import(
        "./edit-json-event-dialog.component"
    );
    return to2(
        (await angularDialog.open(EditJsonEventDialogComponent, opts)).result
    );
}
