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
    // TODO: Check that startTime + -date is before endTime + -date.

    console.log(control.value);
    // return {dateInvalid: true};
    return null;
};
