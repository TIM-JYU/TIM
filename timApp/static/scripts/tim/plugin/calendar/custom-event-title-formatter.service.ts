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
import {CalendarEvent, CalendarEventTitleFormatter} from "angular-calendar";
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

    private getEventShortName(event: CalendarEvent<TIMEventMeta>) {
        if (!event.meta) {
            return "";
        }
        let res = `(${event.meta.enrollments}/${event.meta.maxSize}`;
        if (
            event.meta.extraEnrollments !== undefined &&
            event.meta.extraEnrollments !== null
        ) {
            res += ` +${event.meta.extraEnrollments}`;
        }
        res += `) ${event.title}`;
        console.log(event.meta, res);
        return res;
    }

    private getEventLongName(
        event: CalendarEvent<TIMEventMeta>,
        title: string
    ) {
        if (!event.meta) {
            return "";
        }
        let res = `${title} (${event.meta.enrollments}/${event.meta.maxSize}`;
        if (
            event.meta.extraEnrollments !== undefined &&
            event.meta.extraEnrollments !== null
        ) {
            res += `, ${event.meta.extraEnrollments} extra`;
        }
        res += ")";
        return res;
    }

    /**
     * Customize week title
     * @param event current Event in the calendar
     */
    week(event: CalendarEvent<TIMEventMeta>): string {
        return this.getEventShortName(event);
    }

    /**
     * Customize day title
     * @param event current Event in the calendar
     */
    day(event: CalendarEvent<TIMEventMeta>): string {
        return this.getEventShortName(event);
    }

    /**
     * Customize week tooltip
     * @param event current Event in the calendar
     * @param title title of the Event
     */
    weekTooltip(event: CalendarEvent<TIMEventMeta>, title: string): string {
        return this.getEventLongName(event, title);
    }

    /**
     * Customize day tooltip
     * @param event current Event in the calendar
     * @param title title of the Event
     */
    dayTooltip(event: CalendarEvent<TIMEventMeta>, title: string): string {
        return this.getEventLongName(event, title);
    }

    /**
     * Customize month tooltip
     * @param event current Event in the calendar
     * @param title title of the Event
     */
    monthTooltip(event: CalendarEvent<TIMEventMeta>, title: string): string {
        return this.getEventLongName(event, title);
    }
}
