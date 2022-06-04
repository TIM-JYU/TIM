/**
 * Custom validator for date and time inputs in the calendar plugin
 *
 * @author Miika Immonen
 * @author Terhi Kamula
 * @author Anssi Lepikko
 * @author Touko Miettinen
 * @author Joose Tikkanen
 * @license MIT
 * @date 24.5.2022
 */
import {Directive, OnInit} from "@angular/core";
import {
    NG_VALIDATORS,
    Validator,
    AbstractControl,
    ValidatorFn,
    ValidationErrors,
} from "@angular/forms";

@Directive({
    selector: "[timCalDateTimeValidator]",
    providers: [
        {
            provide: NG_VALIDATORS,
            useClass: DateTimeValidatorDirective,
            multi: true,
        },
    ],
})

/**
 * Custom directive for date-/time -validation
 */
export class DateTimeValidatorDirective implements Validator, OnInit {
    ngOnInit() {}
    validate(control: AbstractControl): Record<string, boolean> | null {
        return dateCheckedValidator(control);
    }
}

/**
 *  Returns a map of ValidationErrors
 * @param control
 */
export const dateCheckedValidator: ValidatorFn = (
    control: AbstractControl
): ValidationErrors | null => {
    const values: {
        title: string;
        startDate: string;
        startTime: string;
        endDate: string;
        endTime: string;
        bookingStopTime: string;
        bookingStopDate: string;
    } = control.value;

    if (
        new Date(
            `${values.bookingStopDate}T${values.bookingStopTime}`
        ).getTime() >
        new Date(`${values.startDate}T${values.startTime}`).getTime()
    ) {
        return {bookingEndInvalid: true};
    }

    if (
        new Date(`${values.startDate}T${values.startTime}`).getTime() >
        new Date(`${values.endDate}T${values.endTime}`).getTime()
    ) {
        return {dateInvalid: true};
    }

    return null;
};
