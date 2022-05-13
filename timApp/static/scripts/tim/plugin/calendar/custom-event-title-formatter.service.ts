import {Injectable} from "@angular/core";
import {CalendarEventTitleFormatter, CalendarEvent} from "angular-calendar";
import {TIMEventMeta} from "./calendar.component";

@Injectable()
export class CustomEventTitleFormatter extends CalendarEventTitleFormatter {
    constructor() {
        super();
    }

    week(event: CalendarEvent<TIMEventMeta>): string {
        return ` ${event.title} (${event.meta!.enrollments}/${
            event.meta!.maxSize
        })`;
    }

    day(event: CalendarEvent<TIMEventMeta>): string {
        return ` ${event.title} (${event.meta!.enrollments}/${
            event.meta!.maxSize
        })`;
    }
}
