import {CalendarEvent} from "angular-calendar";
import {angularDialog} from "../../ui/angulardialog/dialog.service";

export async function showCalendarEventDialog(
    event: CalendarEvent
): Promise<unknown> {
    const {CalendarEventDialogComponent} = await import(
        "./calendar-event-dialog.component"
    );
    return (await angularDialog.open(CalendarEventDialogComponent, event))
        .result;
}
