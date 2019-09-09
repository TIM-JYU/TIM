import {timApp} from "../app";
import {KEY_ENTER} from "../util/keycodes";

timApp.directive("onEnter", () => (scope, element, attrs) => {
    element.bind("keydown keypress", (event) => {
        if (event.which === KEY_ENTER) {
            scope.$apply(() => {
                scope.$eval(attrs.onEnter as string);
            });
            event.preventDefault();
        }
    });
});
