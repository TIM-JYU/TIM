import {angularDialog} from "../../ui/angulardialog/dialog.service";
import {TIMCalendarEvent} from "./calendar.component";

export async function showCalendarEventDialog(
    event: TIMCalendarEvent
): Promise<TIMCalendarEvent> {
    const {CalendarEventDialogComponent} = await import(
        "./calendar-event-dialog.component"
    );
    return (await angularDialog.open(CalendarEventDialogComponent, event))
        .result;
}
