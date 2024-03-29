/**
 * Component that controls the dialog component
 *
 * @author Miika Immonen
 * @author Terhi Kamula
 * @author Anssi Lepikko
 * @author Touko Miettinen
 * @author Joose Tikkanen
 * @license MIT
 * @date 24.5.2022
 */
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {TIMCalendarEvent} from "tim/plugin/calendar/calendar.component";

export async function showCalendarEventDialog(
    event: TIMCalendarEvent
): Promise<TIMCalendarEvent> {
    const {CalendarEventDialogComponent} = await import(
        "./calendar-event-dialog.component"
    );
    return (await angularDialog.open(CalendarEventDialogComponent, event))
        .result;
}
