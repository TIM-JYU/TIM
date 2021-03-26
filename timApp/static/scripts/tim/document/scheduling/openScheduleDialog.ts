import {IItem} from "tim/item/IItem";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function openScheduleDialog(d: IItem) {
    const {ScheduleDialogComponent} = await import(
        "./schedule-dialog.component"
    );
    return await (await angularDialog.open(ScheduleDialogComponent, d)).result;
}
