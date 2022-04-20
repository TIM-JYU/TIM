import {CalendarDateFormatter, DateFormatterParams} from "angular-calendar";
import {formatDate} from "@angular/common";
import {Injectable} from "@angular/core";

@Injectable()
export class CustomDateFormatter extends CalendarDateFormatter {
    public monthViewColumnHeader({date, locale}: DateFormatterParams): string {
        return formatDate(date, "EEE", locale!);
    }

    public monthViewTitle({date, locale}: DateFormatterParams): string {
        return formatDate(date, "MMMM y", locale!);
    }

    public weekViewColumnHeader({date, locale}: DateFormatterParams): string {
        return formatDate(date, "EEE", locale!);
    }

    public weekViewColumnSubHeader({
        date,
        locale,
    }: DateFormatterParams): string {
        return formatDate(date, "d.M", locale!);
    }

    public dayViewHour({date, locale}: DateFormatterParams): string {
        return formatDate(date, "HH:mm", locale!);
    }

    public weekViewHour({date, locale}: DateFormatterParams): string {
        return formatDate(date, "HH:mm", locale!);
    }
}
