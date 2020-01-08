import {Directive} from "@angular/core";
import {AbstractControl, NG_VALIDATORS, Validator} from "@angular/forms";
import {createValidator} from "tim/util/utils";
import {slugify} from "tim/util/slugify";

export function isValidShortName(val: string) {
    return val.toLowerCase() === slugify(val);
}

@Directive({
    selector: "[timShortName]",
    providers: [{provide: NG_VALIDATORS, useExisting: ShortNameDirective, multi: true}],
})
export class ShortNameDirective implements Validator {
    validate(control: AbstractControl) {
        return createValidator(isValidShortName, "timShortName")(control);
    }
}
