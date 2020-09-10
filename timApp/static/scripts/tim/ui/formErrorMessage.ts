import {IController, IFormController, INgModelController} from "angular";
import {timApp} from "tim/app";
import {$timeout} from "../util/ngimport";

class ErrorStateCtrl implements IController {
    static $inject = ["$element"];
    private lastInvalid = false;
    private element: JQLite;
    public for: INgModelController | undefined;
    private formCtrl: IFormController | undefined;

    constructor(element: JQLite) {
        this.element = element;
    }

    async $onInit() {
        if (!this.for) {
            if (!this.formCtrl) {
                throw new Error(
                    "tim-error-state doesn't have 'for' attribute or parent form"
                );
            }
            const inputs = this.element.find(
                "input[type='text'], input[type='number'], textarea"
            );
            if (inputs.length === 0) {
                throw new Error(
                    "no inputs found for tim-error-state, so it needs 'for' attribute"
                );
            } else if (inputs.length === 1) {
                const input = inputs[0];
                const name = input.getAttribute("name");
                if (!name) {
                    return;
                }
                // formCtrl is not fully initialized yet (it doesn't have the inputs)
                await $timeout();
                this.for = this.formCtrl[name] as
                    | INgModelController
                    | undefined;
            } else {
                throw new Error(
                    "multiple inputs found for tim-error-state, so it needs 'for' attribute"
                );
            }
        }
    }

    $doCheck() {
        if (!this.for) {
            return;
        }
        const invalid = this.for.$invalid;
        if (this.lastInvalid !== invalid) {
            this.lastInvalid = invalid;
            if (invalid && this.for.$dirty) {
                this.element.addClass("has-error");
            } else {
                this.element.removeClass("has-error");
            }
        }
    }
}

/**
 * Displays an error message for the given form element when it is invalid.
 */
timApp.component("timErrorMessage", {
    bindings: {
        for: "=?",
    },
    require: {
        errState: "?^timErrorState",
    },
    controller: class implements IController {
        private for: INgModelController | undefined;
        private errState: ErrorStateCtrl | undefined;

        async $onInit() {
            if (!this.for) {
                if (!this.errState) {
                    throw new Error("for and errState undefined");
                }
                if (!this.errState.for) {
                    await $timeout();
                }
                if (!this.errState.for) {
                    throw new Error("errState.for was undefined");
                }
                this.for = this.errState.for;
            }
        }
    },
    templateUrl: "/static/templates/formErrorMessage.html",
});

/**
 * Adds 'has-error' class to the element if the given form element is invalid and dirty; otherwise removes it.
 */
timApp.directive("timErrorState", [
    () => {
        return {
            restrict: "A",
            bindToController: {
                for: "=?",
            },
            require: {
                formCtrl: "^form",
            },
            controller: ErrorStateCtrl,
        };
    },
]);

export type AlertSeverity = "danger" | "warning" | "success" | "info";
