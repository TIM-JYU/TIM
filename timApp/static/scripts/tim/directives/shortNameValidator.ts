import {IAttributes, INgModelController, IRootElementService, IScope} from "angular";
import {timApp} from "tim/app";
import {slugify} from "../services/slugify";

timApp.directive("timShortName", [() => {
    return {
        require: "?ngModel",
        link(scope: IScope, elm: IRootElementService, attrs: IAttributes, ctrl: INgModelController) {
            if (ctrl) {
                ctrl.$validators.timShortName = (modelValue, viewValue) => {
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
        link(scope: IScope, elm: IRootElementService, attrs: IAttributes, ctrl: INgModelController) {
            if (ctrl) {
                ctrl.$validators.timLocation = (modelValue, viewValue) => {
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
