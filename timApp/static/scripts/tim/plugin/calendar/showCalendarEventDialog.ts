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
