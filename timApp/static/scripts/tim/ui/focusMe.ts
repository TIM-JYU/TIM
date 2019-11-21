// from https://stackoverflow.com/a/14837021
import {IAttributes, IScope} from "angular";
import {timApp} from "tim/app";
import {$parse, $timeout} from "../util/ngimport";

timApp.directive("focusMe", [() => {
    return {
        link(scope: IScope, element: JQLite, attrs: IAttributes) {
            const model = $parse(attrs.focusMe as string);
            scope.$watch(model, (value) => {
                if (value === true) {
                    $timeout(() => {
                        element[0].focus();
                    });
                }
            });
            element.bind("blur", () => {
                // Catch "model.assign is not a function"
                try {
                    scope.$applyAsync(model.assign(scope, false) as string);
                } catch (e) {
                }
            });
        },
    };
}]);
