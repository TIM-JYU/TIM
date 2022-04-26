import {CalendarDateFormatter, DateFormatterParams} from "angular-calendar";
import {formatDate} from "@angular/common";
import {Injectable} from "@angular/core";

@Injectable()
export class CustomDateFormatter extends CalendarDateFormatter {
    public monthViewColumnHeader({date, locale}: DateFormatterParams): string {
        return formatDate(date, "EEE", locale!);
    }

    /**
     *  Custom formatted that takes into special account finnish language. Returns a formatted
     *  title with the name of the month and the year.
     *  There is a default 'ta' -ending in the default formatDate -function. This function removes the last 2 letters of all those
     *  huhtikuuta, tammikuuta etc. month names and brings them closer to natural finnish language.
     *
     * @param date date to be formatted
     * @param locale the locale of user
     */
    public monthViewTitle({date, locale}: DateFormatterParams): string {
        if (locale!.toLowerCase() == "fi" || locale!.toLowerCase() == "fi-fi") {
            const formattedYear = formatDate(date, "y", locale!);
            let formattedMonthName = formatDate(date, "MMMM", locale!);
            formattedMonthName = formattedMonthName.substring(
                0,
                formattedMonthName.length - 2
            );
            return formattedMonthName + " " + formattedYear;
        }

        return formatDate(date, "MMMM y", locale!);
    }

    /**
     * Custom formatted that takes into special account finnish language. Returns a formatted
     *  title with the name of the month.
     *  There is a default 'ta' -ending in the default formatDate -function. This function removes the last 2 letters of all those
     *  huhtikuuta, tammikuuta etc. month names and brings them closer to natural finnish language.
     * @param date date to be formatted
     * @param locale the locale of user
     */
    public viewMonth({date, locale}: DateFormatterParams): string {
        if (locale!.toLowerCase() == "fi" || locale!.toLowerCase() == "fi-fi") {
            let formattedMonthName = formatDate(date, "MMMM", locale!);
            formattedMonthName = formattedMonthName.substring(
                0,
                formattedMonthName.length - 2
            );
            return formattedMonthName;
        }
        return formatDate(date, "MMMM", locale!);
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

    public viewDay({date, locale}: DateFormatterParams): string {
        if (
            locale!.toLocaleLowerCase() == "fi-fi" ||
            locale!.toLocaleLowerCase() == "fi"
        ) {
            return formatDate(date, "EEEE, d.M.y", locale!);
        }
        return formatDate(date, "EEEE, M/d/y", locale!);
    }

    public dayViewHour({date, locale}: DateFormatterParams): string {
        return formatDate(date, "HH:mm", locale!);
    }

    public weekViewHour({date, locale}: DateFormatterParams): string {
        return formatDate(date, "HH:mm", locale!);
    }
}
