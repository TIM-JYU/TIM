/**
 * Formats the tool tip texts and event texts for the calendar plugin
 *
 * @author Miika Immonen
 * @author Terhi Kamula
 * @author Anssi Lepikko
 * @author Touko Miettinen
 * @author Joose Tikkanen
 * @license MIT
 * @date 24.5.2022
 */
import {Injectable} from "@angular/core";
import {CalendarEventTitleFormatter, CalendarEvent} from "angular-calendar";
import {TIMEventMeta} from "./calendar.component";

/**
 *  This CustomEventTitleFormatter makes it possible to customize the Tooltips and Event titles that are shown in
 *  the day-, week-, and month-view of the timCalendar.
 */
@Injectable()
export class CustomEventTitleFormatter extends CalendarEventTitleFormatter {
    constructor() {
        super();
    }

    /**
     * Customize week title
     * @param event current Event in the calendar
     */
    week(event: CalendarEvent<TIMEventMeta>): string {
        return ` ${event.title} (${event.meta!.enrollments}/${
            event.meta!.maxSize
        })`;
    }

    /**
     * Customize day title
     * @param event current Event in the calendar
     */
    day(event: CalendarEvent<TIMEventMeta>): string {
        return ` ${event.title} (${event.meta!.enrollments}/${
            event.meta!.maxSize
        })`;
    }

    /**
     * Customize week tooltip
     * @param event current Event in the calendar
     * @param title title of the Event
     */
    weekTooltip(event: CalendarEvent<TIMEventMeta>, title: string): string {
        return `${title} (${event.meta!.enrollments}/${event.meta!.maxSize})`;
    }

    /**
     * Customize day tooltip
     * @param event current Event in the calendar
     * @param title title of the Event
     */
    dayTooltip(event: CalendarEvent<TIMEventMeta>, title: string): string {
        return `${title} (${event.meta!.enrollments}/${event.meta!.maxSize})`;
    }

    /**
     * Customize month tooltip
     * @param event current Event in the calendar
     * @param title title of the Event
     */
    monthTooltip(event: CalendarEvent<TIMEventMeta>, title: string): string {
        return `${title} (${event.meta!.enrollments}/${event.meta!.maxSize})`;
    }
}
