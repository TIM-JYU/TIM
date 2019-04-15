import {Result} from "../util/utils";
import {DialogController, registerDialogComponent, showDialog} from "./dialog";

interface InputDialogParams<T> {
    defaultValue: string;
    validator: (s: string) => Promise<Result<T, string>>;
    title: string;
    text: string;
    okText?: string;
    cancelText?: string;
}

class InputDialogCtrl<T> extends DialogController<{params: InputDialogParams<T>}, T> {
    static component = "timInputDialog";
    static $inject = ["$element", "$scope"] as const;
    private value = "";
    private error?: string;
    private focus = false;

    protected getTitle() {
        return this.resolve.params.title;
    }

    async $onInit() {
        super.$onInit();
        this.value = this.resolve.params.defaultValue;
        await this.draggable.getLayoutPromise();
        this.focus = true;
    }

    async ok() {
        if (!this.value) {
            return;
        }
        const result = await this.resolve.params.validator(this.value);
        if (!result.ok) {
            this.error = result.result;
            return;
        }
        this.close(result.result);
    }

    clearError() {
        this.error = undefined;
    }

    okText(): string {
        return this.resolve.params.okText || "OK";
    }

    cancelText(): string {
        return this.resolve.params.cancelText || "Cancel";
    }

    text() {
        return this.resolve.params.text;
    }
}

registerDialogComponent(InputDialogCtrl,
    {
        template: `
<tim-dialog>
    <dialog-body>
        <p ng-bind="::$ctrl.text()"></p>
        <input on-save="$ctrl.ok()"
               class="form-control"
               focus-me="$ctrl.focus"
               type="text"
               ng-model="$ctrl.value"
               ng-change="$ctrl.clearError()">
        <tim-alert ng-if="$ctrl.error" severity="danger">
            {{ $ctrl.error }}
        </tim-alert>
    </dialog-body>
    <dialog-footer>
        <button data-ng-disabled="!$ctrl.value"
                class="timButton" type="button" ng-click="$ctrl.ok()">{{ ::$ctrl.okText() }}
        </button>
        <button class="btn btn-default" type="button" ng-click="$ctrl.dismiss()">{{ ::$ctrl.cancelText() }}</button>
    </dialog-footer>
</tim-dialog>
    `,
    });

export function showInputDialog<T>(p: InputDialogParams<T>) {
    return showDialog<InputDialogCtrl<T>, readonly ["$element", "$scope"]>(InputDialogCtrl, {params: () => p}).result;
}
