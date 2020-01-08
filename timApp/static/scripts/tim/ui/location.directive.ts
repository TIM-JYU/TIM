import {Directive} from "@angular/core";
import {AbstractControl, NG_VALIDATORS, Validator} from "@angular/forms";
import {createValidator} from "tim/util/utils";
import {isValidShortName} from "tim/ui/short-name.directive";

export function isValidLocation(viewValue: string) {
    const val = viewValue.replace(/\//g, "");
    return isValidShortName(val);
}

@Directive({
    selector: "[timLocation]",
    providers: [{provide: NG_VALIDATORS, useExisting: LocationDirective, multi: true}],
})
export class LocationDirective implements Validator {
    validate(control: AbstractControl) {
        return createValidator(isValidLocation, "timLocation")(control);
    }
}
