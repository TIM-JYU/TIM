/**
 * A custom date formatter for calendar plugin
 *
 * @author Miika Immonen
 * @author Terhi Kamula
 * @author Anssi Lepikko
 * @author Touko Miettinen
 * @author Joose Tikkanen
 * @license MIT
 * @date 24.5.2022
 */
import type {DateFormatterParams} from "angular-calendar";
import {CalendarDateFormatter, getWeekViewPeriod} from "angular-calendar";
import {formatDate} from "@angular/common";
import {Injectable} from "@angular/core";

@Injectable()
export class CustomDateFormatter extends CalendarDateFormatter {
    public monthViewColumnHeader({date, locale}: DateFormatterParams): string {
        return formatDate(date, "EEE", locale!);
    }

    public viewYear({date, locale}: DateFormatterParams): string {
        return formatDate(date, "y", locale!);
    }

    /**
     *  Custom formatter that takes into special account finnish language. Returns a formatted
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

    /**
     * Formats week view column header
     * @param date date
     * @param locale locale
     */
    public weekViewColumnHeader({date, locale}: DateFormatterParams): string {
        return formatDate(date, "EEE", locale!);
    }

    /**
     * Formats week view column sub-header
     * @param date date
     * @param locale locale
     */
    public weekViewColumnSubHeader({
        date,
        locale,
    }: DateFormatterParams): string {
        return formatDate(date, "d.M", locale!);
    }

    /**
     * Formats the date in week view to preferred form d.M. - d.M.yyyy or
     * M/d/- M/d/yyyy if any other than finnish locale
     *
     * @param date date
     * @param locale locale
     * @param weekStartsOn start day of week
     * @param excludeDays days to be excluded from view
     * @param daysInWeek number of days in a week
     */
    public weekViewTitle({
        date,
        locale,
        weekStartsOn,
        excludeDays,
        daysInWeek,
    }: DateFormatterParams): string {
        const {viewStart, viewEnd} = getWeekViewPeriod(
            this.dateAdapter,
            date,
            weekStartsOn!,
            excludeDays,
            daysInWeek
        );
        let formatType = "";
        if (
            locale!.toLocaleLowerCase() == "fi-fi" ||
            locale!.toLocaleLowerCase() == "fi"
        ) {
            formatType = "d.M.";
        } else {
            formatType = "M/d/";
        }

        const format = (dateToFormat: Date, showYear: boolean) =>
            formatDate(
                dateToFormat,
                formatType + (showYear ? "yyyy" : ""),
                locale!
            );
        return `${format(
            viewStart,
            viewStart.getUTCFullYear() !== viewEnd.getUTCFullYear()
        )} - ${format(viewEnd, true)}`;
    }

    /**
     *  Custom formatter to be used in the day-view of the calendar. Takes into special account
     *  finnish language and formats accordingly in d.M.y-format.
     *  All other locales are formatted in the M/d/y -format.
     * @param date current date
     * @param locale current locale of the user
     */
    public viewDay({date, locale}: DateFormatterParams): string {
        if (
            locale!.toLocaleLowerCase() == "fi-fi" ||
            locale!.toLocaleLowerCase() == "fi"
        ) {
            return formatDate(date, "EEEE, d.M.y", locale!);
        }
        return formatDate(date, "EEEE, M/d/y", locale!);
    }

    /**
     * Formats time in day view
     * @param date date
     * @param locale locale
     */
    public dayViewHour({date, locale}: DateFormatterParams): string {
        return formatDate(date, "HH:mm", locale!);
    }

    /**
     * Formats time in week view
     * @param date date
     * @param locale locale
     */
    public weekViewHour({date, locale}: DateFormatterParams): string {
        return formatDate(date, "HH:mm", locale!);
    }
}
