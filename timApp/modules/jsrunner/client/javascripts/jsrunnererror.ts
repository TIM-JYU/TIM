import {jsrunnerApp} from "./jsrunnerapp";

export class JsrunnerErrorController {
    showTrace = false;

    toggleStackTrace() {
        this.showTrace = !this.showTrace;
    }
}

jsrunnerApp.component("jsrunnerError", {
    bindings: {
        e: "<",
    },
    controller: JsrunnerErrorController,
    template: `
<tim-alert severity="danger">
  <span>{{ $ctrl.e.user }}:</span>
  <div ng-repeat="err in $ctrl.e.errors">
    <span>{{ err.msg }}</span>
    <button ng-if="err.stackTrace" class="timButton btn-sm" ng-click="$ctrl.toggleStackTrace()">Stack trace</button>
    <pre ng-if="err.stackTrace && $ctrl.showTrace">{{ err.stackTrace }}</pre>
  </div>
</tim-alert>
    `,
});
