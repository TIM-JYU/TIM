import moment from "moment";
import {timApp} from "../app";

class DurationPickerCtrl {
    private durationTypes: moment.unitOfTime.Base[];

    constructor() {
        this.durationTypes = durationTypes;
    }
}

export type DurationChoice =
    | "seconds"
    | "minutes"
    | "hours"
    | "days"
    | "weeks"
    | "months"
    | "years";
export const durationTypes: DurationChoice[] = [
    "seconds",
    "minutes",
    "hours",
    "days",
    "weeks",
    "months",
    "years",
];

timApp.component("timDurationPicker", {
    bindings: {
        amount: "=",
        disabled: "<?",
        required: "<?",
        type: "=",
    },
    controller: DurationPickerCtrl,
    template: `
<div class="form-group" tim-error-state>
    <div class="col-md-6">
        <input ng-model="$ctrl.amount"
               class="form-control"
               name="durAmount"
               placeholder="{{ $ctrl.required ? '' : 'Optional' }}"
               type="number"
               min="0"
               step="0.1"
               ng-required="!$ctrl.disabled && $ctrl.required"
               ng-disabled="$ctrl.disabled"/>
    </div>
    <div class="col-md-6">
        <select class="form-control" ng-model="$ctrl.type"
                ng-options="d for d in $ctrl.durationTypes"
                ng-disabled="$ctrl.disabled"></select>
    </div>
    <tim-error-message></tim-error-message>
</div>
    `,
});
