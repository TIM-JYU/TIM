import {DialogController, registerDialogComponent, showDialog} from "./dialog";

interface InputDialogParams {
    defaultValue: string;
    validator: (s: string) => Promise<{error?: string}>;
    title: string;
    text: string;
    okText?: string;
    cancelText?: string;
}

class InputDialogCtrl extends DialogController<{params: InputDialogParams}, string, "timInputDialog"> {
    private value = "";
    private error?: string;

    protected getTitle() {
        return this.resolve.params.title;
    }

    $onInit() {
        super.$onInit();
        this.value = this.resolve.params.defaultValue;
    }

    async ok() {
        const result = await this.resolve.params.validator(this.value);
        if (result.error) {
            this.error = result.error;
            return;
        }
        this.close(this.value);
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
}

registerDialogComponent("timInputDialog",
    InputDialogCtrl,
    {
        template: `
<tim-dialog>
    <dialog-body>
        <p ng-bind="$ctrl.text"></p>
        <input class="form-control" type="text" ng-model="$ctrl.value" ng-change="$ctrl.clearError()">
        <tim-alert severity="error">
            {{ $ctrl.error }}
        </tim-alert>
    </dialog-body>
    <dialog-footer>
        <button data-ng-disabled="!$ctrl.value"
                class="timButton" type="button" ng-click="$ctrl.ok()">{{ $ctrl.okText() }}
        </button>
        <button class="btn btn-default" type="button" ng-click="$ctrl.dismiss()">{{ $ctrl.cancelText() }}</button>
    </dialog-footer>
</tim-dialog>
    `,
    });

export function showInputDialog(p: InputDialogParams) {
    return showDialog<InputDialogCtrl>("timInputDialog", {params: () => p}).result;
}
