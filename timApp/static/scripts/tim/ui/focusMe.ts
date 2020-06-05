// from https://stackoverflow.com/a/14837021
import {IAttributes, IScope} from "angular";
import {timApp} from "tim/app";
import {$parse, $timeout} from "../util/ngimport";

// Some browsers (especially Firefox) require to blur elements before you can focus onto others
// This postfix tag blurs an element when the focus condition is false, thus allowing other elements to
// be focused right away
const BLUR_FALSE_TAG = "#blurWhenFalse";

timApp.directive("focusMe", [() => {
    return {
        link(scope: IScope, element: JQLite, attrs: IAttributes) {
            let attr = attrs.focusMe as string;
            let shouldBlurOnFalse = false;
            if (attr.endsWith(BLUR_FALSE_TAG)) {
                shouldBlurOnFalse = true;
                attr = attr.substring(0, attr.length - BLUR_FALSE_TAG.length);
            }
            const model = $parse(attr);
            scope.$watch(model, (value) => {
                if (value === true) {
                    $timeout(() => {
                        element[0].focus();
                    });
                } else if (value === false && shouldBlurOnFalse) {
                    $timeout(() => {
                        element[0].blur();
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
