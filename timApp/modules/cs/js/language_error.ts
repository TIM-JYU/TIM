import {IScope} from "angular";
import {pluginBindings} from "tim/plugin/util";
import {csApp, CsBase} from "./csPlugin";

class CSErrorController extends CsBase {

    private error?: string;
    private own_error?: string;

    $onInit() {
        super.$onInit();

        this.error = this.attrsall.error ?? "";
        this.own_error = this.attrsall.own_error ?? "";
    }
}

csApp.component("csError", {
    bindings: pluginBindings,
    controller: CSErrorController,
    template: `
        <tim-markup-error ng-if="$ctrl.markupError" [data]="$ctrl.markupError"></tim-markup-error>
        <div ng-if="!$ctrl.markupError" class="error">
            <p>Error(s) initializing csPlugin:</p>
            <pre ng-if="$ctrl.error">{{$ctrl.error}}</pre>
            <pre ng-if="$ctrl.own_error">{{$ctrl.own_error}}</pre>
        </div>`,
});
