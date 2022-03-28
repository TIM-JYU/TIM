import {CalendarDateFormatter, DateFormatterParams} from "angular-calendar";
import {formatDate} from "@angular/common";
import {Injectable} from "@angular/core";

@Injectable()
export class CustomDateFormatter extends CalendarDateFormatter {
    public monthViewColumnHeader({date, locale}: DateFormatterParams): string {
        return formatDate(date, "EEE", "Fi-fi");
    }

    public monthViewTitle({date, locale}: DateFormatterParams): string {
        return formatDate(date, "MMMM y", "Fi-fi");
    }

    public weekViewColumnHeader({date, locale}: DateFormatterParams): string {
        return formatDate(date, "EEE", "Fi-fi");
    }

    public weekViewColumnSubHeader({
        date,
        locale,
    }: DateFormatterParams): string {
        return formatDate(date, "d.M", "Fi-fi");
    }

    public dayViewHour({date, locale}: DateFormatterParams): string {
        return formatDate(date, "HH:mm", "Fi-fi");
    }

    public weekViewHour({date, locale}: DateFormatterParams): string {
        return formatDate(date, "HH:mm", "Fi-fi");
    }
}
