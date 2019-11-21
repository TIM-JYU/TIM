import {IAttributes, IController, INgModelController, IScope} from "angular";
import {timApp} from "tim/app";
import {slugify} from "../util/slugify";

timApp.directive("timShortName", [() => {
    return {
        require: "?ngModel",
        link(scope: IScope, elm: JQLite, attrs: IAttributes, c: IController | undefined) {
            if (c) {
                const ctrl = c as INgModelController;
                ctrl.$validators.timShortName = (modelValue: string, viewValue: string) => {
                    if (ctrl.$isEmpty(modelValue)) {
                        return true;
                    }
                    return viewValue.toLowerCase() === slugify(viewValue);
                };
            }
        },
    };
}]);

timApp.directive("timLocation", [() => {
    return {
        require: "?ngModel",
        link(scope: IScope, elm: JQLite, attrs: IAttributes, c: IController | undefined) {
            if (c) {
                const ctrl = c as INgModelController;
                ctrl.$validators.timLocation = (modelValue: string, viewValue: string) => {
                    if (ctrl.$isEmpty(modelValue)) {
                        return true;
                    }
                    viewValue = viewValue.replace(/\//g, "");
                    return viewValue.toLowerCase() === slugify(viewValue);
                };
            }
        },
    };
}]);
