import {IAttributes, IController, IRootElementService, IScope} from "angular";
import {timApp} from "tim/app";
import {slugify} from "../util/slugify";

timApp.directive("timShortName", [() => {
    return {
        require: "?ngModel",
        link(scope: IScope, elm: IRootElementService, attrs: IAttributes, ctrl: IController | undefined) {
            if (ctrl) {
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
        link(scope: IScope, elm: IRootElementService, attrs: IAttributes, ctrl: IController | undefined) {
            if (ctrl) {
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
